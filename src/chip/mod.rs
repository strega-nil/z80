mod regs;
mod ops;

use std::mem;

use wrapping::{cvt, w, w8, w16};
use Pins;

use self::regs::Regs;
use self::ops::{Reg8, Reg16, Op};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum ArgsNeeded {
  Zero,
  One,
  Two,
}

#[derive(Copy, Clone, Debug)]
enum State {
  FetchInstruction,
  ReceiveInstruction,
  WriteOut8(w8),
  ReadInto8(Reg8),
  ReadInto16(Reg16, Option<w8>),
}

pub struct Chip {
  r: Regs,
  rp: Regs, // r'
  sp: w16,
  pc: w16,
  state: State,
}

impl Chip {
  pub fn new() -> Self {
    Chip {
      r: Regs::new(),
      rp: Regs::new(),
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
      State::WriteOut8(num) => {
        debug!("state: write out data ({:02X})", num);
        pins.mreq = true;
        pins.wr = true;
        pins.address = self.r.wz().0;
        pins.data = num.0;
        self.state = State::FetchInstruction;
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
      }
    }
  }

  fn load8_imm(&mut self, pins: &mut Pins, into: Reg8, num: w8) {
    self.state = State::FetchInstruction;
    match into {
      Reg8::A => { self.r.a = num; },
      Reg8::B => { self.r.b = num; },
      Reg8::C => { self.r.c = num; },
      Reg8::D => { self.r.d = num; },
      Reg8::E => { self.r.e = num; },
      Reg8::H => { self.r.h = num; },
      Reg8::L => { self.r.l = num; },
      Reg8::Hld => {
        self.r.w = self.r.h;
        self.r.z = self.r.l;
        self.state = State::WriteOut8(num);
      },
    }
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

  fn run_op(&mut self, pins: &mut Pins, op: Op) {
    // NOTE(ubsan): how op arguments work:
    // ArgsNeeded::Zero - no arguments, trivial
    // ArgsNeeded::One - one argument at pins.data
    // ArgsNeeded::Two -
    //   two arguments; the first is at z, the second is at pins.data
    // numbering goes backwards for arguments

    match op {
      Op::Nop => {},
      Op::Halt => {
        debug!("register a: {:02X}", self.r.a);
        pins.halt = true;
      },

      // --- LOADS ---
      Op::Ld8I(into) => self.load8(pins, into, None),
      Op::Ld8(into, from) => self.load8(pins, into, Some(from)),
      Op::Ld16i(into) => self.load16(pins, into, None),

      // --- INCS and DECS ---
      Op::Inc16(reg) => {
        // no flags are updated
        let (mut upper, mut lower) = self.fetch16_imm(reg);
        lower += w(1);
        if lower == w(0) { upper += w(1); }
        self.load16_imm(reg, upper, lower);
      }

      inst => panic!("unimplemented instruction: {:?}", inst),
    }
  }
}
