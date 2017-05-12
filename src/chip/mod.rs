mod regs;
mod ops;

use wrapping::{cvt, Extensions, w, w8, w16};
use Pins;

use self::regs::Regs;
use self::ops::{Flag, Reg8, Reg16, Op};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum ArithmeticOp {
  Add,
  Adc,
  Sub,
  Sbc,
  And,
  Or,
  Xor,
  Cp,
}

impl ArithmeticOp {
  fn do_op(self, chip: &mut Chip, rhs: w8) {
    let lhs = chip.r.a;
    let (res, carry, subtraction, pv) = match self {
      ArithmeticOp::Add => {
        let (res, carry) = lhs.overflowing_add(rhs);
        (res, carry, false, res < lhs)
      },
      ArithmeticOp::Adc => {
        let (res, carry0) = lhs.overflowing_add(w(chip.r.flags.c() as u8));
        let (res, carry1) = res.overflowing_add(rhs);
        (res, carry0 | carry1, false, if chip.r.flags.c() {
          res <= lhs
        } else {
          res < lhs
        })
      },
      ArithmeticOp::Sub => {
        let (res, carry) = lhs.overflowing_sub(rhs);
        (res, carry, true, res > lhs)
      },
      ArithmeticOp::Sbc => {
        let (res, carry0) = lhs.overflowing_sub(w(chip.r.flags.c() as u8));
        let (res, carry1) = res.overflowing_sub(rhs);
        (res, carry0 | carry1, true, if chip.r.flags.c() {
          res >= lhs
        } else {
          res > lhs
        })
      },
      ArithmeticOp::Cp => {
        let (res, carry) = lhs.overflowing_sub(rhs);
        (res, carry, true, res > lhs)
      },
      ArithmeticOp::And => {
        let res = lhs & rhs;
        (res, false, false, res.count_ones() % 2 == 0)
      },
      ArithmeticOp::Or => {
        let res = lhs | rhs;
        (res, false, false, res.count_ones() % 2 == 0)
      },
      ArithmeticOp::Xor => {
        let res = lhs ^ rhs;
        (res, false, false, res.count_ones() % 2 == 0)
      },
    };

    if let ArithmeticOp::Cp = self {
      // --
    } else {
      chip.r.a = res;
    }

    chip.r.flags.set_s(res & w(0b1000_0000) != w(0));
    chip.r.flags.set_z(res == w(0));
    chip.r.flags.set_5(chip.r.a & w(0b1_0000) != w(0));
    // NOTE(ubsan): I don't feel like writing the code for half-carry
    // chip.r.flags.set_h();
    chip.r.flags.set_3(chip.r.a & w(0b1_0000) != w(0));
    chip.r.flags.set_v(pv);
    chip.r.flags.set_n(subtraction);
    chip.r.flags.set_c(carry);
    chip.state = State::FetchInstruction;
  }
}

#[derive(Copy, Clone, Debug)]
enum State {
  FetchInstruction,
  ReceiveInstruction,
  ReadInto8(Reg8),
  ReadInto16(Reg16, Option<w8>),
  OpAFromPins(ArithmeticOp),
  JumpRel(bool),
  Out,
}

pub struct Chip {
  r: Regs,
  _rp: Regs, // r'
  sp: w16,
  pc: w16,
  state: State,
}

impl Chip {
  pub fn new() -> Self {
    Chip {
      r: Regs::new(),
      _rp: Regs::new(),
      sp: w(0),
      pc: w(0),
      state: State::FetchInstruction,
    }
  }

  pub fn step(&mut self, pins: &mut Pins) {
    pins.zero_out();
    match self.state {
      State::FetchInstruction => {
        debug!("state: fetch instruction");
        pins.mreq = true;
        pins.rd = true;
        pins.address = self.pc.0;
        self.pc += w(1);
        self.state = State::ReceiveInstruction;
      },
      State::ReceiveInstruction => {
        debug!("state: receive instruction ({:02X})", pins.data);
        let op = Op::decode(pins.data);
        self.run_op(pins, op);
      },
      State::ReadInto8(into) => {
        debug!("state: read into {:?} ({:02X})", into, pins.data);
        let data = w(pins.data);
        self.load8_imm(pins, into, data);
      },
      State::ReadInto16(into, lower) => {
        if let Some(lower) = lower {
          debug!("state: read into {:?} ({:02X}, ())", into, pins.data);
          let upper = w(pins.data);
          self.load16_imm(/*pins,*/ into, upper, lower);
        } else {
          debug!("state: read into {:?} ((), {:02X})", into, pins.data);
          let lower = w(pins.data);
          pins.mreq = true;
          pins.rd = true;
          pins.address = self.pc.0;
          self.pc += w(1);
          self.state = State::ReadInto16(into, Some(lower));
        }
      },
      State::OpAFromPins(op) => {
        let data = w(pins.data);
        op.do_op(self, data);
      },
      State::JumpRel(cond) => {
        let data = w(pins.data as i8 as i16 as u16); // use sign-extension
        if cond { self.pc -= w(2); self.pc += data; }
        self.state = State::FetchInstruction;
      },
      State::Out => {
        let addr = pins.data;
        pins.iorq = true;
        pins.wr = true;
        pins.address = addr as u16;
        pins.data = self.r.a.0;
        self.state = State::FetchInstruction;
      }
    }
  }

  fn load8_imm(&mut self, pins: &mut Pins, into: Reg8, num: w8) {
    match into {
      Reg8::A => { self.r.a = num; },
      Reg8::B => { self.r.b = num; },
      Reg8::C => { self.r.c = num; },
      Reg8::D => { self.r.d = num; },
      Reg8::E => { self.r.e = num; },
      Reg8::H => { self.r.h = num; },
      Reg8::L => { self.r.l = num; },
      Reg8::Hld => {
        pins.mreq = true;
        pins.wr = true;
        pins.address = self.r.hl().0;
        pins.data = num.0;
      },
    }
    self.state = State::FetchInstruction;
  }

  fn load8(&mut self, pins: &mut Pins, into: Reg8, from: Option<Reg8>) {
    self.state = State::FetchInstruction;
    match from {
      None => {
        pins.mreq = true;
        pins.rd = true;
        pins.address = self.pc.0;
        self.pc += w(1);
        self.state = State::ReadInto8(into);
      },
      Some(from) => match from {
        Reg8::Hld => {
          pins.mreq = true;
          pins.rd = true;
          pins.address = self.r.hl().0;
          self.state = State::ReadInto8(into);
        },
        Reg8::A => { let num = self.r.a; self.load8_imm(pins, into, num); },
        Reg8::B => { let num = self.r.b; self.load8_imm(pins, into, num); },
        Reg8::C => { let num = self.r.c; self.load8_imm(pins, into, num); },
        Reg8::D => { let num = self.r.d; self.load8_imm(pins, into, num); },
        Reg8::E => { let num = self.r.e; self.load8_imm(pins, into, num); },
        Reg8::H => { let num = self.r.h; self.load8_imm(pins, into, num); },
        Reg8::L => { let num = self.r.l; self.load8_imm(pins, into, num); },
      },
    }
  }

  fn fetch16_imm(&mut self, from: Reg16) -> (w8, w8) {
    match from {
      Reg16::Bc => { (self.r.b, self.r.c) },
      Reg16::De => { (self.r.d, self.r.e) },
      Reg16::Hl => { (self.r.h, self.r.l) },
      Reg16::Sp => { (cvt(self.sp >> 8), cvt(self.sp)) },
    }
  }

  fn load16_imm(&mut self, into: Reg16, upper: w8, lower: w8) {
    self.state = State::FetchInstruction;
    match into {
      Reg16::Bc => { self.r.b = upper; self.r.c = lower; },
      Reg16::De => { self.r.d = upper; self.r.e = lower; },
      Reg16::Hl => { self.r.h = upper; self.r.l = lower; },
      Reg16::Sp => { self.sp = ((cvt(upper) as w16) << 8) | cvt(lower); },
    }
  }

  fn load16(&mut self, pins: &mut Pins, into: Reg16, from: Option<Reg16>) {
    match from {
      None => {
        pins.mreq = true;
        pins.rd = true;
        pins.address = self.pc.0;
        self.pc += w(1);
        self.state = State::ReadInto16(into, None);
      },
      Some(from) => match from {
        Reg16::Bc => {
          let (upper, lower) = (self.r.b, self.r.c);
          self.load16_imm(into, upper, lower);
        },
        Reg16::De => {
          let (upper, lower) = (self.r.d, self.r.e);
          self.load16_imm(into, upper, lower);
        }
        Reg16::Hl => {
          let (upper, lower) = (self.r.h, self.r.l);
          self.load16_imm(into, upper, lower);
        }
        Reg16::Sp => {
          let num = self.sp;
          self.load16_imm(into, cvt(num >> 8), cvt(num));
        }
      }
    }
  }

  fn arith_op_a(&mut self, pins: &mut Pins, op: ArithmeticOp, from: Reg8) {
    match from {
      Reg8::Hld => {
        pins.mreq = true;
        pins.rd = true;
        pins.address = self.r.hl().0;
        self.state = State::OpAFromPins(op);
      },
      Reg8::A => { let num = self.r.a; op.do_op(self, num); },
      Reg8::B => { let num = self.r.b; op.do_op(self, num); },
      Reg8::C => { let num = self.r.c; op.do_op(self, num); },
      Reg8::D => { let num = self.r.d; op.do_op(self, num); },
      Reg8::E => { let num = self.r.e; op.do_op(self, num); },
      Reg8::H => { let num = self.r.h; op.do_op(self, num); },
      Reg8::L => { let num = self.r.l; op.do_op(self, num); },
    }
  }

  fn inc_dec8(&mut self, _pins: &mut Pins, reg: Reg8, dec: bool) {
    let f = if dec {
      fn _dec(reg: &mut w8) -> w8 { *reg -= w(1); *reg } _dec
    } else {
      fn _inc(reg: &mut w8) -> w8 { *reg += w(1); *reg } _inc
    };
    let res = match reg {
      Reg8::Hld => unimplemented!(),
      Reg8::A => { f(&mut self.r.a) },
      Reg8::B => { f(&mut self.r.b) },
      Reg8::C => { f(&mut self.r.c) },
      Reg8::D => { f(&mut self.r.d) },
      Reg8::E => { f(&mut self.r.e) },
      Reg8::H => { f(&mut self.r.h) },
      Reg8::L => { f(&mut self.r.l) },
    };

    self.r.flags.set_n(dec);
    self.r.flags.set_v(if dec { res == w(0xFF) } else { res == w(0) });
    // self.r.flags.set_h();
    self.r.flags.set_z(res == w(0));
    self.r.flags.set_s(res & w(0b1000_0000) != w(0));
    self.state = State::FetchInstruction;
  }

  fn flag(&mut self, flag: Flag) -> bool {
    match flag {
      Flag::Nz => !self.r.flags.z(),
      Flag::Z  => self.r.flags.z(),
      Flag::Nc => !self.r.flags.c(),
      Flag::C  => self.r.flags.c(),
      Flag::Po => !self.r.flags.v(),
      Flag::Pe => self.r.flags.v(),
      Flag::P  => !self.r.flags.s(),
      Flag::M  => self.r.flags.s(),
    }
  }

  fn jump_rel(&mut self, pins: &mut Pins, flag: Option<Flag>) {
    let cond = if let Some(flag) = flag {
      self.flag(flag)
    } else {
      true
    };
    pins.mreq = true;
    pins.rd = true;
    pins.address = self.pc.0;
    self.pc += w(1); // should be moved back by the jump, if it jumps
    self.state = State::JumpRel(cond);
  }

  fn run_op(&mut self, pins: &mut Pins, op: Op) {
    match op {
      // --- MISC ---
      Op::Nop => {},
      Op::Halt => {
        debug!("register a: {:02X}", self.r.a);
        pins.halt = true;
      },
      Op::Out => {
        pins.mreq = true;
        pins.rd = true;
        pins.address = self.pc.0;
        self.pc += w(1);
        self.state = State::Out;
      },

      // --- LOADS ---
      Op::Ld8I(into) => self.load8(pins, into, None),
      Op::Ld8(into, from) => self.load8(pins, into, Some(from)),
      Op::Ld16i(into) => self.load16(pins, into, None),

      // --- INCS and DECS ---
      Op::Inc8(reg) => self.inc_dec8(pins, reg, false),
      Op::Dec8(reg) => self.inc_dec8(pins, reg, true),
      Op::Inc16(reg) => {
        // no flags are updated
        let (mut upper, mut lower) = self.fetch16_imm(reg);
        lower += w(1);
        if lower == w(0) { upper += w(1); }
        self.load16_imm(reg, upper, lower);
      },
      Op::Dec16(reg) => {
        // no flags are updated
        let (mut upper, mut lower) = self.fetch16_imm(reg);
        lower -= w(1);
        if lower == w(0) { upper -= w(1); }
        self.load16_imm(reg, upper, lower);
      },

      // --- ARITHMETIC ---
      Op::AddA(reg) => self.arith_op_a(pins, ArithmeticOp::Add, reg),
      Op::AdcA(reg) => self.arith_op_a(pins, ArithmeticOp::Adc, reg),
      Op::SubA(reg) => self.arith_op_a(pins, ArithmeticOp::Sub, reg),
      Op::SbcA(reg) => self.arith_op_a(pins, ArithmeticOp::Sbc, reg),
      Op::AndA(reg) => self.arith_op_a(pins, ArithmeticOp::And, reg),
      Op::XorA(reg) => self.arith_op_a(pins, ArithmeticOp::Xor, reg),
      Op::OrA(reg) => self.arith_op_a(pins, ArithmeticOp::Or, reg),
      Op::CpA(reg) => self.arith_op_a(pins, ArithmeticOp::Cp, reg),

      // --- JUMPS ---
      Op::Jr(flag) => self.jump_rel(pins, flag),
    }
  }
}
