// TODO(ubsan): p/v flag
// TODO(ubsan): h flag
// TODO(ubsan): n flag

#[allow(dead_code)]

extern crate clap;
mod wrapping;

use std::fmt::{self, Debug, Formatter};

use clap::{App, Arg};
use wrapping::{w8, w16, w, cvt, Extensions};

// CALLING CONVENTION
// first, u16s get passed in BC, DE, HL in that order
// then, u8s get passed in A, B, C, D, E, H, L in that order;
//   if a reg is already taken by a u16, then skip those regs
// if a certain kind has to overflow the parameter regs, it's passed on the
//   stack, backwards order
// i.e., (1: u8, 2: u16, 3: u8, 4: u8, 5: u16, 6: u16, 7: u16, 8: u16) is
//   2 = BC,  5 = DE, 6 = HL, 7 = (SP + 4), 8 = (SP + 2),
//     1 = A, 4 = (SP + 6), 3 = (SP + 7)
//     RET = (SP)
// u8 returns are in A, u16 returns are in BC
//   if it returns two u8s, return in BC
// Otherwise, on the stack, with the address in BC
//   (taking up the first u16 slot)
// primes are saved
// IX and IY are saved
fn main() {
  let rom = {
    use std::io::Read;

    let matches = App::new("retro")
      .version("0.1.2")
      .author("Nicole Mazzuca <npmazzuca@gmail.com>")
      .about("An emulator for the `retro` computer")
      .arg(Arg::with_name("rom")
           .value_name("FILE")
           .help("sets the rom to boot from")
           .index(1))
      .get_matches();
    let filename =
      matches.value_of("input").expect("ROM to boot from required");
    let mut file = std::fs::File::open(filename).unwrap();
    let mut v = Vec::new();
    file.read_to_end(&mut v).unwrap();
    v
  };

  Processor::new(&rom).run();
}

pub struct Flags(pub u8);
impl Flags {
  pub fn new() -> Self { Flags(0) }

  pub fn s(&self) -> bool { self.0 >> 7 == 1 }
  pub fn z(&self) -> bool { self.0 >> 6 == 1 }
  pub fn h(&self) -> bool { unimplemented!() /*self.0 >> 4 == 1*/ }
  pub fn pv(&self) -> bool { unimplemented!() /*self.0 >> 2 == 1*/ }
  pub fn n(&self) -> bool { unimplemented!() /*self.0 >> 1 == 1*/ }
  pub fn c(&self) -> bool { self.0 >> 0 == 1 }
  pub fn set_s(&mut self, to: bool) {
    self.0 |= (to as u8) << 7;
    self.0 &= 0xFF ^ (!to as u8) << 7;
  }
  pub fn set_z(&mut self, to: bool) {
    self.0 |= (to as u8) << 6;
    self.0 &= 0xFF ^ (!to as u8) << 6;
  }
  pub fn set_h(&mut self, _to: bool) {
    unimplemented!() /*self.0 |= (to as u8) << 4*/
  }
  pub fn set_pv(&mut self, _to: bool) {
    unimplemented!() /*self.0 |= (to as u8) << 2*/
  }
  pub fn set_n(&mut self, _to: bool) {
    unimplemented!() /*self.0 |= (to as u8) << 1*/
  }
  pub fn set_c(&mut self, to: bool) {
    self.0 |= to as u8;
    self.0 &= 0xFF ^ (!to as u8);
  }
}
impl Debug for Flags {
  fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
    f.debug_struct("ConditionCodes").field("z", &self.z())
      .field("s", &self.s()).field("p", &false).field("c", &self.c())
      .field("n", &false).field("h", &false).finish()
  }
}

#[derive(Debug)]
pub struct Regs {
  pub a: w8,
  pub b: w8,
  pub c: w8,
  pub d: w8,
  pub e: w8,
  pub h: w8,
  pub l: w8,
  pub flags: Flags,
}

impl Regs {
  fn new() -> Self {
    Regs {
      a: w(0),
      b: w(0),
      c: w(0),
      d: w(0),
      e: w(0),
      h: w(0),
      l: w(0),
      flags: Flags::new(),
    }
  }

  fn hl(&self) -> w16 {
    (cvt(self.h) as w16) << 8 | (cvt(self.l) as w16)
  }
  fn set_hl(&mut self, to: w16) {
    self.h = cvt(to >> 8);
    self.l = cvt(to);
  }

  fn bc(&self) -> w16 {
    (cvt(self.b) as w16) << 8 | (cvt(self.c) as w16)
  }
  fn set_bc(&mut self, to: w16) {
    self.b = cvt(to >> 8);
    self.c = cvt(to);
  }

  fn de(&self) -> w16 {
    (cvt(self.d) as w16) << 8 | (cvt(self.e) as w16)
  }
  fn set_de(&mut self, to: w16) {
    self.d = cvt(to >> 8) as w8;
    self.e = cvt(to) as w8;
  }
}

#[derive(Copy, Clone, Debug)]
pub enum Op8 {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
  BCd,
  DEd,
  HLd,
  Imm,
  Immd,
}

#[derive(Copy, Clone, Debug)]
pub enum Op16 {
  AF,
  BC,
  DE,
  HL,
  SP,
  Imm,
  Immd,
}

#[derive(Copy, Clone, Debug)]
pub enum Flag {
  Djnz,
  Uncond,
  Nonzero,
  Zero,
  NoCarry,
  Carry,
}

struct Memory {
  banks: [Box<[u8; 0x4000]>; 4]
  // rom: [0x0000, 0x3FFF]
  // ram1: [0x4000, 0x7FFF]
  // ram2: [0x8000, 0xAFFF]
  // ram3: [0xB000, 0xFFFF]
}

impl Memory {
  fn new(rom: &[u8]) -> Memory {
    fn create_array() -> Box<[u8; 0x4000]> {
      unsafe {
        let backing = vec![0u8; 0x4000];
        let boxed = backing.into_boxed_slice();
        Box::from_raw(Box::into_raw(boxed) as *mut [u8; 0x4000])
      }
    }

    let mut ret = Memory {
      banks: [create_array(), create_array(), create_array(), create_array()],
    };
    ret.banks[0][..rom.len()].copy_from_slice(rom);
    ret
  }

  fn get_bank(n: w16) -> usize {
    (n.0 as usize) >> 14
  }
  fn get_idx(n: w16) -> usize {
    (n.0 as usize) & (!0 >> 2)
  }

  pub fn read8(&self, idx: w16) -> w8 {
    w(self.banks[Self::get_bank(idx)][Self::get_idx(idx)])
  }
  pub fn write8(&mut self, idx: w16, n: w8) {
    assert!(Self::get_bank(idx) != 0, "can't write to r/o memory");
    self.banks[Self::get_bank(idx)][Self::get_idx(idx)] = n.0;
  }
  pub fn read16(&self, idx: w16) -> w16 {
    cvt(self.read8(idx)) as w16
    | (cvt(self.read8(idx + w(1))) as w16) << 8
  }
  pub fn write16(&mut self, idx: w16, n: w16) {
    self.write8(idx, cvt(n));
    self.write8(idx + w(1), cvt(n >> 8));
  }
}

pub struct Processor {
  r: Regs,
  rp: Regs, // r'
  sp: w16,
  pc: w16,
  memory: Memory,
}

impl Processor {
  pub fn new(rom: &[u8]) -> Self {
    Processor {
      r: Regs::new(),
      rp: Regs::new(),
      sp: w(0),
      pc: w(0),
      memory: Memory::new(rom),
    }
  }

  pub fn get_imm8(&mut self) -> w8 {
    let ret = self.memory.read8(self.pc);
    self.pc += w(1);
    ret
  }
  pub fn get_imm16(&mut self) -> w16 {
    let ret = self.memory.read16(self.pc);
    self.pc += w(2);
    ret
  }

  pub fn peek_imm8(&self, ind: w16) -> w8 {
    self.memory.read8(self.pc + ind)
  }
  pub fn peek_imm16(&self, ind: w16) -> w16 {
    self.memory.read16(self.pc + ind)
  }

  pub fn pc_offset(&self, ind: w8) -> w16 {
    self.pc + w(ind.0 as i8 as u16)
  }

  pub fn get8(&mut self, op: Op8) -> w8 {
    match op {
      Op8::A => self.r.a,
      Op8::B => self.r.b,
      Op8::C => self.r.c,
      Op8::D => self.r.d,
      Op8::E => self.r.e,
      Op8::H => self.r.h,
      Op8::L => self.r.l,
      Op8::BCd => self.memory.read8(self.r.bc()),
      Op8::DEd => self.memory.read8(self.r.de()),
      Op8::HLd => self.memory.read8(self.r.hl()),
      Op8::Imm => self.get_imm8(),
      Op8::Immd => {let tmp = self.get_imm16(); self.memory.read8(tmp)},
    }
  }
  pub fn set8(&mut self, op: Op8, to: w8) {
    match op {
      Op8::A => self.r.a = to,
      Op8::B => self.r.b = to,
      Op8::C => self.r.c = to,
      Op8::D => self.r.d = to,
      Op8::E => self.r.e = to,
      Op8::H => self.r.h = to,
      Op8::L => self.r.l = to,
      Op8::BCd => self.memory.write8(self.r.bc(), to),
      Op8::DEd => self.memory.write8(self.r.de(), to),
      Op8::HLd => self.memory.write8(self.r.hl(), to),
      Op8::Immd => {let tmp = self.get_imm16(); self.memory.write8(tmp, to)},
      Op8::Imm => panic!("passed Op8::Imm to set8"),
    }
  }

  pub fn get16(&mut self, op: Op16) -> w16 {
    match op {
      Op16::AF => {
        let upper = self.r.a;
        let lower = w(self.r.flags.0);
        (cvt(upper) as w16) << 8 | cvt(lower) as w16
      }
      Op16::BC => self.r.bc(),
      Op16::DE => self.r.de(),
      Op16::HL => self.r.hl(),
      Op16::SP => self.sp,
      Op16::Imm => self.get_imm16(),
      Op16::Immd => {
        let idx = self.get_imm16();
        self.memory.read16(idx)
      }
    }
  }
  pub fn set16(&mut self, op: Op16, to: w16) {
    match op {
      Op16::AF => {
        self.r.a = cvt(to >> 8);
        self.r.flags.0 = to.0 as u8;
      }
      Op16::BC => self.r.set_bc(to),
      Op16::DE => self.r.set_de(to),
      Op16::HL => self.r.set_hl(to),
      Op16::SP => self.sp = to,
      Op16::Imm => panic!("Passed Op16::Imm to set16"),
      Op16::Immd => {
        let idx = self.get_imm16();
        self.memory.write16(idx, to);
      }
    }
  }

  pub fn flag(&mut self, f: Flag) -> bool {
    match f {
      Flag::Djnz => {
        self.r.b -= w(1);
        self.r.b != w(0)
      }
      Flag::Uncond => true,
      Flag::Nonzero => !self.r.flags.z(),
      Flag::Zero => self.r.flags.z(),
      Flag::NoCarry => !self.r.flags.c(),
      Flag::Carry => self.r.flags.c(),
    }
  }

  fn unimplemented_inst(&mut self) {
    panic!("Error: Unimplemented Instruction: {}",
      self.memory.read8(self.pc - w(1)));
  }
}

impl Processor {
  pub fn ld16(&mut self, into: Op16, from: Op16) {
    let tmp = self.get16(from);
    self.set16(into, tmp);
  }
  pub fn ld8(&mut self, into: Op8, from: Op8) {
    let tmp = self.get8(from);
    self.r.flags.set_z(tmp == w(0));
    self.r.flags.set_s((tmp.0 as i8) < 0);
    self.set8(into, tmp);
  }
  pub fn pop_imm16(&mut self) -> w16 {
    let tmp = self.memory.read16(self.sp);
    self.sp += w(2);
    tmp
  }
  pub fn pop16(&mut self, into: Op16) {
    let tmp = self.pop_imm16();
    self.set16(into, tmp);
  }
  pub fn push_imm16(&mut self, from: w16) {
    self.sp -= w(2);
    self.memory.write16(self.sp, from);
  }
  pub fn push16(&mut self, from: Op16) {
    let tmp = self.get16(from);
    self.push_imm16(tmp);
  }

  pub fn add16(&mut self, into: Op16, from: Op16) {
    let (v, o) = self.get16(into).overflowing_add(self.get16(from));
    self.r.flags.set_c(o);
    self.set16(into, v);
  }

  fn set_flags(&mut self, (v, o): (w8, bool)) -> w8 {
    self.r.flags.set_c(o);
    self.r.flags.set_z(v == w(0));
    self.r.flags.set_s(v.top_bit_set());
    v
  }

  pub fn add8(&mut self, from: Op8) {
    let tmp = self.r.a.overflowing_add(self.get8(from));
    let v = self.set_flags(tmp);
    self.r.a = v;
  }
  pub fn adc8(&mut self, from: Op8) {
    let v = {
      let carry = w(self.r.flags.c() as u8);
      let tmp = self.r.a.overflowing_add(self.get8(from));
      let (v, o) = (tmp.0).overflowing_add(carry);
      self.set_flags((v, o | tmp.1))
    };
    self.r.a = v;
  }

  pub fn sub8(&mut self, from: Op8) {
    let tmp = self.r.a.overflowing_sub(self.get8(from));
    let v = self.set_flags(tmp);
    self.r.a = v;
  }
  pub fn sbc8(&mut self, from: Op8) {
    let v = {
      let carry = w(self.r.flags.c() as u8);
      let tmp = self.r.a.overflowing_sub(self.get8(from));
      let (v, o) = (tmp.0).overflowing_sub(carry);
      self.set_flags((v, o | tmp.1))
    };
    self.r.a = v;
  }

  pub fn and8(&mut self, from: Op8) {
    let tmp = self.r.a & self.get8(from);
    let v = self.set_flags((tmp, false));
    self.r.a = v;
  }
  pub fn xor8(&mut self, from: Op8) {
    let tmp = self.r.a ^ self.get8(from);
    let v = self.set_flags((tmp, false));
    self.r.a = v;
  }
  pub fn or8(&mut self, from: Op8) {
    let tmp = self.r.a | self.get8(from);
    let v = self.set_flags((tmp, false));
    self.r.a = v;
  }
  pub fn cp8(&mut self, from: Op8) {
    let tmp = self.r.a.overflowing_sub(self.get8(from));
    self.set_flags(tmp);
  }

  pub fn not(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    self.set8(arg, !tmp);
  }

  pub fn inc16(&mut self, arg: Op16) {
    let v = self.get16(arg) + w(1);
    self.set16(arg, v);
  }
  pub fn dec16(&mut self, arg: Op16) {
    let v = self.get16(arg) - w(1);
    self.set16(arg, v);
  }
  pub fn inc8(&mut self, arg: Op8) {
    let tmp = self.get8(arg) + w(1);
    let v = self.set_flags((tmp, false));
    self.set8(arg, v);
  }
  pub fn dec8(&mut self, arg: Op8) {
    let tmp = self.get8(arg) - w(1);
    let v = self.set_flags((tmp, false));
    self.set8(arg, v);
  }

  pub fn rlc(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry = tmp >> 7;
    self.r.flags.set_c(carry != w(0));
    self.set8(arg, tmp << 1 | carry);
  }
  pub fn rrc(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry = tmp << 7;
    self.r.flags.set_c(carry != w(0));
    self.set8(arg, tmp >> 1 | carry);
  }
  pub fn rl(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry_flag = tmp >> 7;
    let carry = w(self.r.flags.c() as u8);
    self.r.flags.set_c(carry_flag != w(0));
    self.set8(arg, tmp << 1 | carry);
  }
  pub fn rr(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry_flag = tmp << 7;
    let carry = w(self.r.flags.c() as u8);
    self.r.flags.set_c(carry_flag != w(0));
    self.set8(arg, tmp >> 1 | carry << 7);
  }

  pub fn jr(&mut self, f: Flag) {
    let label = self.get_imm8();
    if self.flag(f) {
      self.pc -= w(2); // get to start of opcode
      self.pc = self.pc_offset(label);
    }
  }
  pub fn jp(&mut self, f: Flag) {
    let label = self.get_imm16();
    if self.flag(f) {
      self.pc = label;
    }
  }
  pub fn call(&mut self, f: Flag) {
    let label = self.get_imm16();
    if self.flag(f) {
      let tmp = self.pc;
      self.push_imm16(tmp);
      self.pc = label;
    }
  }
  pub fn ret(&mut self, f: Flag) {
    if self.flag(f) {
      let ret_label = self.pop_imm16();
      self.pc = ret_label;
    }
  }

  pub fn ex_af(&mut self) {
    std::mem::swap(&mut self.r.a, &mut self.rp.a);
    std::mem::swap(&mut self.r.flags, &mut self.rp.flags);
  }

  pub fn ex_de_hl(&mut self) {
    std::mem::swap(&mut self.r.d, &mut self.r.h);
    std::mem::swap(&mut self.r.e, &mut self.r.l);
  }

  pub fn ex_spd_hl(&mut self) {
    let tmp = self.r.hl();
    self.r.set_hl(self.memory.read16(self.sp));
    self.memory.write16(self.sp, tmp);
  }

  pub fn exx(&mut self) {
    std::mem::swap(&mut self.r.b, &mut self.rp.b);
    std::mem::swap(&mut self.r.c, &mut self.rp.c);
    std::mem::swap(&mut self.r.d, &mut self.rp.d);
    std::mem::swap(&mut self.r.e, &mut self.rp.e);
    std::mem::swap(&mut self.r.h, &mut self.rp.h);
    std::mem::swap(&mut self.r.l, &mut self.rp.l);
  }

  pub fn halt(&mut self) {}
}

impl Processor {
  pub fn is_ret_op(op: u8) -> bool {
    op == 0xC9 || // RET
      op == 0xC0 || // RET NZ
      op == 0xC8 || // RET Z
      op == 0xD0 || // RET NC
      op == 0xD8 || // RET C
      op == 0xE0 || // RET PO
      op == 0xE8 || // RET PE
      op == 0xF0 || // RET P
      op == 0xF8    // RET M
  }
  pub fn ret_op_flag(op: u8) -> Flag {
    match op {
      0xC9 => Flag::Uncond,  // RET
      0xC0 => Flag::Nonzero, // RET NZ
      0xC8 => Flag::Zero,    // RET Z
      0xD0 => Flag::NoCarry, // RET NC
      0xD8 => Flag::Carry,   // RET C

      0xE0 => unimplemented!(), // Flag::ParOdd,  // RET PO
      0xE8 => unimplemented!(), // Flag::ParEven, // RET PE
      0xF0 => unimplemented!(), // Flag::Plus,    // RET P
      0xF8 => unimplemented!(), // Flag::Minus,   // RET M
      n => panic!("unreachable op: {}", n),
    }
  }
  pub fn is_call_op(op: u8) -> bool {
    op == 0xCD || // CALL
      op == 0xC4 || // CALL NZ
      op == 0xCC || // CALL Z
      op == 0xD4 || // CALL NC
      op == 0xDC || // CALL C
      op == 0xE4 || // CALL PO
      op == 0xEC || // CALL PE
      op == 0xF4 || // CALL P
      op == 0xFC    // CALL M
  }
}

// at the end because lots of lines
impl Processor {
  pub fn run(&mut self) {
    while self.step() { }
  }

  // returns true if the machine should continue
  // returns false if it shut off
  // TODO(ubsan): write this better
  pub fn step(&mut self) -> bool {
    let opcode = self.get_imm8();
    match opcode.0 {
      0x00 => {}                                // NOP
      0x01 => self.ld16(Op16::BC, Op16::Imm),   // LD BC, NN
      0x02 => self.ld8(Op8::BCd, Op8::A),       // LD (BC), A
      0x03 => self.inc16(Op16::BC),             // INC BC
      0x04 => self.inc8(Op8::B),                // INC B
      0x05 => self.dec8(Op8::B),                // DEC B
      0x06 => self.ld8(Op8::B, Op8::Imm),       // LD B, N
      0x07 => self.rlc(Op8::A),                 // RLCA
      0x08 => self.ex_af(),                     // EX AF, AF'
      0x09 => self.add16(Op16::HL, Op16::BC),   // ADD HL, BC
      0x0A => self.ld8(Op8::A, Op8::BCd),       // LD A, (BC)
      0x0B => self.dec16(Op16::BC),             // DEC BC
      0x0C => self.inc8(Op8::C),                // INC C
      0x0D => self.dec8(Op8::C),                // DEC C
      0x0E => self.ld8(Op8::C, Op8::Imm),       // LD C, N
      0x0F => self.rrc(Op8::A),                 // RRCA

      0x10 => self.jr(Flag::Djnz),              // DJNZ N
      0x11 => self.ld16(Op16::DE, Op16::Imm),   // LD DE, NN
      0x12 => self.ld8(Op8::DEd, Op8::A),       // LD (DE), A
      0x13 => self.inc16(Op16::DE),             // INC DE
      0x14 => self.inc8(Op8::D),                // INC D
      0x15 => self.dec8(Op8::D),                // DEC D
      0x16 => self.ld8(Op8::D, Op8::Imm),       // LD D, N
      0x17 => self.rl(Op8::A),                  // RLA
      0x18 => self.jr(Flag::Uncond),            // JR N
      0x19 => self.add16(Op16::HL, Op16::DE),   // ADD HL, DE
      0x1A => self.ld8(Op8::A, Op8::DEd),       // LD A, (DE)
      0x1B => self.dec16(Op16::DE),             // DEC DE
      0x1C => self.inc8(Op8::E),                // INC E
      0x1D => self.dec8(Op8::E),                // DEC E
      0x1E => self.ld8(Op8::E, Op8::Imm),       // LD E, N
      0x1F => self.rr(Op8::A),                  // RRA

      0x20 => self.jr(Flag::Nonzero),           // JR NZ, NN
      0x21 => self.ld16(Op16::HL, Op16::Imm),   // LD HL, NN
      0x22 => self.ld16(Op16::Immd, Op16::HL),  // LD (NN), HL
      0x23 => self.inc16(Op16::HL),             // INC HL
      0x24 => self.inc8(Op8::H),                // INC H
      0x25 => self.dec8(Op8::H),                // DEC H
      0x26 => self.ld8(Op8::H, Op8::Imm),       // LD H, N
      0x27 => self.unimplemented_inst(),        // DAA
      0x28 => self.jr(Flag::Zero),              // JR Z, N
      0x29 => self.add16(Op16::HL, Op16::HL),   // ADD HL, HL
      0x2A => self.ld16(Op16::HL, Op16::Immd),  // LD HL, (NN)
      0x2B => self.dec16(Op16::HL),             // DEC HL
      0x2C => self.inc8(Op8::L),                // INC L
      0x2D => self.dec8(Op8::L),                // DEC L
      0x2E => self.ld8(Op8::L, Op8::Imm),       // LD L, N
      0x2F => self.not(Op8::A),                 // CPL

      0x30 => self.jr(Flag::NoCarry),           // JR NC, N
      0x31 => self.ld16(Op16::SP, Op16::Imm),   // LD SP, NN
      0x32 => self.ld8(Op8::Immd, Op8::A),      // LD (NN), A
      0x33 => self.inc16(Op16::SP),             // INC SP
      0x34 => self.inc8(Op8::HLd),              // INC (HL)
      0x35 => self.dec8(Op8::HLd),              // DEC (HL)
      0x36 => self.ld8(Op8::HLd, Op8::Imm),     // LD (HL), N
      0x37 => self.r.flags.set_c(true),         // SCF
      0x38 => self.jr(Flag::Carry),             // JR C, N
      0x39 => self.add16(Op16::HL, Op16::SP),   // ADD HL, SP
      0x3A => self.ld8(Op8::A, Op8::Immd),      // LD A, (NN)
      0x3B => self.dec16(Op16::SP),             // DEC SP
      0x3C => self.inc8(Op8::A),                // INC A
      0x3D => self.dec8(Op8::A),                // DEC A
      0x3E => self.ld8(Op8::A, Op8::Imm),       // LD A, N
      0x3F => {                                 // CCF
        let c = self.r.flags.c();
        self.r.flags.set_c(!c);
      },

      0x40 => self.ld8(Op8::B, Op8::B),         // LD B, B
      0x41 => self.ld8(Op8::B, Op8::C),         // LD B, C
      0x42 => self.ld8(Op8::B, Op8::D),         // LD B, D
      0x43 => self.ld8(Op8::B, Op8::E),         // LD B, E
      0x44 => self.ld8(Op8::B, Op8::H),         // LD B, H
      0x45 => self.ld8(Op8::B, Op8::L),         // LD B, L
      0x46 => self.ld8(Op8::B, Op8::HLd),       // LD B, (HL)
      0x47 => self.ld8(Op8::B, Op8::A),         // LD B, A
      0x48 => self.ld8(Op8::C, Op8::B),         // LD C, B
      0x49 => self.ld8(Op8::C, Op8::C),         // LD C, C
      0x4A => self.ld8(Op8::C, Op8::D),         // LD C, D
      0x4B => self.ld8(Op8::C, Op8::E),         // LD C, E
      0x4C => self.ld8(Op8::C, Op8::H),         // LD C, H
      0x4D => self.ld8(Op8::C, Op8::L),         // LD C, L
      0x4E => self.ld8(Op8::C, Op8::HLd),       // LD C, (HL)
      0x4F => self.ld8(Op8::C, Op8::A),         // LD C, A

      0x50 => self.ld8(Op8::D, Op8::B),         // LD D, B
      0x51 => self.ld8(Op8::D, Op8::C),         // LD D, C
      0x52 => self.ld8(Op8::D, Op8::D),         // LD D, D
      0x53 => self.ld8(Op8::D, Op8::E),         // LD D, E
      0x54 => self.ld8(Op8::D, Op8::H),         // LD D, H
      0x55 => self.ld8(Op8::D, Op8::L),         // LD D, L
      0x56 => self.ld8(Op8::D, Op8::HLd),       // LD D, (HL)
      0x57 => self.ld8(Op8::D, Op8::A),         // LD D, A
      0x58 => self.ld8(Op8::E, Op8::B),         // LD E, B
      0x59 => self.ld8(Op8::E, Op8::C),         // LD E, C
      0x5A => self.ld8(Op8::E, Op8::D),         // LD E, D
      0x5B => self.ld8(Op8::E, Op8::E),         // LD E, E
      0x5C => self.ld8(Op8::E, Op8::H),         // LD E, H
      0x5D => self.ld8(Op8::E, Op8::L),         // LD E, L
      0x5E => self.ld8(Op8::E, Op8::HLd),       // LD E, (HL)
      0x5F => self.ld8(Op8::E, Op8::A),         // LD E, A

      0x60 => self.ld8(Op8::H, Op8::B),         // LD H, B
      0x61 => self.ld8(Op8::H, Op8::C),         // LD H, C
      0x62 => self.ld8(Op8::H, Op8::D),         // LD H, D
      0x63 => self.ld8(Op8::H, Op8::E),         // LD H, E
      0x64 => self.ld8(Op8::H, Op8::H),         // LD H, H
      0x65 => self.ld8(Op8::H, Op8::L),         // LD H, L
      0x66 => self.ld8(Op8::H, Op8::HLd),       // LD H, (HL)
      0x67 => self.ld8(Op8::H, Op8::A),         // LD H, A
      0x68 => self.ld8(Op8::L, Op8::B),         // LD L, B
      0x69 => self.ld8(Op8::L, Op8::C),         // LD L, C
      0x6A => self.ld8(Op8::L, Op8::D),         // LD L, D
      0x6B => self.ld8(Op8::L, Op8::E),         // LD L, E
      0x6C => self.ld8(Op8::L, Op8::H),         // LD L, H
      0x6D => self.ld8(Op8::L, Op8::L),         // LD L, L
      0x6E => self.ld8(Op8::L, Op8::HLd),       // LD L, (HL)
      0x6F => self.ld8(Op8::L, Op8::A),         // LD L, A

      0x70 => self.ld8(Op8::HLd, Op8::B),       // LD H, B
      0x71 => self.ld8(Op8::HLd, Op8::C),       // LD H, C
      0x72 => self.ld8(Op8::HLd, Op8::D),       // LD H, D
      0x73 => self.ld8(Op8::HLd, Op8::E),       // LD H, E
      0x74 => self.ld8(Op8::HLd, Op8::H),       // LD H, H
      0x75 => self.ld8(Op8::HLd, Op8::L),       // LD H, L
      0x76 => { self.halt(); return false }     // HALT
      0x77 => self.ld8(Op8::HLd, Op8::A),       // LD H, A
      0x78 => self.ld8(Op8::A, Op8::B),         // LD A, B
      0x79 => self.ld8(Op8::A, Op8::C),         // LD A, C
      0x7A => self.ld8(Op8::A, Op8::D),         // LD A, D
      0x7B => self.ld8(Op8::A, Op8::E),         // LD A, E
      0x7C => self.ld8(Op8::A, Op8::H),         // LD A, H
      0x7D => self.ld8(Op8::A, Op8::L),         // LD A, L
      0x7E => self.ld8(Op8::A, Op8::HLd),       // LD A, (HL)
      0x7F => self.ld8(Op8::A, Op8::A),         // LD A, A

      0x80 => self.add8(Op8::B),                // ADD A, B
      0x81 => self.add8(Op8::C),                // ADD A, B
      0x82 => self.add8(Op8::D),                // ADD A, B
      0x83 => self.add8(Op8::E),                // ADD A, B
      0x84 => self.add8(Op8::H),                // ADD A, B
      0x85 => self.add8(Op8::L),                // ADD A, B
      0x86 => self.add8(Op8::HLd),              // ADD A, B
      0x87 => self.add8(Op8::A),                // ADD A, B
      0x88 => self.adc8(Op8::B),                // ADC A, B
      0x89 => self.adc8(Op8::C),                // ADC A, B
      0x8A => self.adc8(Op8::D),                // ADC A, B
      0x8B => self.adc8(Op8::E),                // ADC A, B
      0x8C => self.adc8(Op8::H),                // ADC A, B
      0x8D => self.adc8(Op8::L),                // ADC A, B
      0x8E => self.adc8(Op8::HLd),              // ADC A, B
      0x8F => self.adc8(Op8::A),                // ADC A, B

      0x90 => self.sub8(Op8::B),                // SUB B
      0x91 => self.sub8(Op8::C),                // SUB C
      0x92 => self.sub8(Op8::D),                // SUB D
      0x93 => self.sub8(Op8::E),                // SUB E
      0x94 => self.sub8(Op8::H),                // SUB H
      0x95 => self.sub8(Op8::L),                // SUB L
      0x96 => self.sub8(Op8::HLd),              // SUB (HL)
      0x97 => self.sub8(Op8::A),                // SBC A
      0x98 => self.sbc8(Op8::B),                // SBC B
      0x99 => self.sbc8(Op8::C),                // SBC C
      0x9A => self.sbc8(Op8::D),                // SBC D
      0x9B => self.sbc8(Op8::E),                // SBC E
      0x9C => self.sbc8(Op8::H),                // SBC H
      0x9D => self.sbc8(Op8::L),                // SBC L
      0x9E => self.sbc8(Op8::HLd),              // SBC (HL)
      0x9F => self.sbc8(Op8::A),                // SBC A

      0xA0 => self.and8(Op8::B),                // AND B
      0xA1 => self.and8(Op8::C),                // AND C
      0xA2 => self.and8(Op8::D),                // AND D
      0xA3 => self.and8(Op8::E),                // AND E
      0xA4 => self.and8(Op8::H),                // AND H
      0xA5 => self.and8(Op8::L),                // AND L
      0xA6 => self.and8(Op8::HLd),              // AND (HL)
      0xA7 => self.and8(Op8::A),                // AND A
      0xA8 => self.xor8(Op8::B),                // XOR B
      0xA9 => self.xor8(Op8::C),                // XOR C
      0xAA => self.xor8(Op8::D),                // XOR D
      0xAB => self.xor8(Op8::E),                // XOR E
      0xAC => self.xor8(Op8::H),                // XOR H
      0xAD => self.xor8(Op8::L),                // XOR L
      0xAE => self.xor8(Op8::HLd),              // XOR (HL)
      0xAF => self.xor8(Op8::A),                // XOR A

      0xB0 => self.or8(Op8::B),                 // OR B
      0xB1 => self.or8(Op8::B),                 // OR C
      0xB2 => self.or8(Op8::B),                 // OR D
      0xB3 => self.or8(Op8::B),                 // OR E
      0xB4 => self.or8(Op8::B),                 // OR H
      0xB5 => self.or8(Op8::B),                 // OR L
      0xB6 => self.or8(Op8::B),                 // OR (HL)
      0xB7 => self.or8(Op8::B),                 // OR A
      0xB8 => self.cp8(Op8::B),                 // CP B
      0xB9 => self.cp8(Op8::C),                 // CP C
      0xBA => self.cp8(Op8::D),                 // CP D
      0xBB => self.cp8(Op8::E),                 // CP E
      0xBC => self.cp8(Op8::H),                 // CP H
      0xBD => self.cp8(Op8::L),                 // CP L
      0xBE => self.cp8(Op8::HLd),               // CP (HL)
      0xBF => self.cp8(Op8::A),                 // CP A

      0xC0 => self.ret(Flag::Nonzero),          // RET NZ
      0xC1 => self.pop16(Op16::BC),             // POP BC
      0xC2 => self.jp(Flag::Nonzero),           // JP NZ, NN
      0xC3 => self.jp(Flag::Uncond),            // JP NN
      0xC4 => self.call(Flag::Nonzero),         // CALL NZ, NN
      0xC5 => self.push16(Op16::BC),            // PUSH BC
      0xC6 => self.add8(Op8::Imm),              // ADD A, N
      0xC7 => self.unimplemented_inst(),        // RST 00h
      0xC8 => self.ret(Flag::Zero),             // RET Z
      0xC9 => self.ret(Flag::Uncond),           // RET
      0xCA => self.jp(Flag::Zero),              // JP Z, NN
      0xCB => self.unimplemented_inst(),
      0xCC => self.call(Flag::Zero),            // CALL Z, NN
      0xCD => self.call(Flag::Uncond),          // CALL, NN
      0xCE => self.adc8(Op8::Imm),              // ADC A, N
      0xCF => self.unimplemented_inst(),        // RST 08h

      0xD0 => self.ret(Flag::NoCarry),          // RET NC
      0xD1 => self.pop16(Op16::DE),             // POP DE
      0xD2 => self.jp(Flag::NoCarry),           // JP NC, NN
      0xD3 => self.unimplemented_inst(),        // OUT (N), A
      0xD4 => self.call(Flag::NoCarry),         // CALL NC, NN
      0xD5 => self.push16(Op16::DE),            // PUSH DE
      0xD6 => self.sub8(Op8::Imm),              // SUB N
      0xD7 => self.unimplemented_inst(),        // RST 10h
      0xD8 => self.ret(Flag::Carry),            // RET C
      0xD9 => self.exx(),                       // EXX
      0xDA => self.jp(Flag::Carry),             // JP C, NN
      0xDB => self.unimplemented_inst(),        // IN A, (N)
      0xDC => self.call(Flag::Carry),           // CALL C, NN
      0xDD => self.unimplemented_inst(),
      0xDE => self.sbc8(Op8::Imm),              // SBC A, N
      0xDF => self.unimplemented_inst(),        // RST 18h

      0xE0 => self.unimplemented_inst(),        // RET PO
      0xE1 => self.pop16(Op16::HL),             // POP HL
      0xE2 => self.unimplemented_inst(),       // JP PO, NN
      0xE3 => self.ex_spd_hl(),                 // EX (SP), HL
      0xE4 => self.unimplemented_inst(),        // CALL PO, NN
      0xE5 => self.push16(Op16::HL),            // PUSH HL
      0xE6 => self.and8(Op8::Imm),              // AND N
      0xE7 => self.unimplemented_inst(),        // RST 20h
      0xE8 => self.unimplemented_inst(),        // RET PE
      0xE9 => self.unimplemented_inst(),        // JP (HL)
      0xEA => self.unimplemented_inst(),        // JP PE, NN
      0xEB => self.ex_de_hl(),                  // EX DE, HL
      0xEC => self.unimplemented_inst(),        // CALL PE, NN
      0xED => self.unimplemented_inst(),
      0xEE => self.unimplemented_inst(),        // XOR N
      0xEF => self.unimplemented_inst(),        // RST 28h

      0xF0 => self.unimplemented_inst(),        // RET P
      0xF1 => self.pop16(Op16::AF),             // POP AF
      0xF2 => self.unimplemented_inst(),        // JP P, NN
      0xF3 => self.unimplemented_inst(),        // DI
      0xF4 => self.unimplemented_inst(),        // CALL P, NN
      0xF5 => self.push16(Op16::AF),            // PUSH AF
      0xF6 => self.or8(Op8::Imm),               // OR N
      0xF7 => self.unimplemented_inst(),        // RST 30h
      0xF8 => self.unimplemented_inst(),        // RET M
      0xF9 => self.ld16(Op16::SP, Op16::HL),    // LD SP, HL
      0xFA => self.unimplemented_inst(),        // JP M, NN
      0xFB => self.unimplemented_inst(),        // EI
      0xFC => self.unimplemented_inst(),        // CALL M, NN
      0xFD => self.unimplemented_inst(),
      0xFE => self.cp8(Op8::Imm),               // CP N
      0xFF => self.unimplemented_inst(),        // RST 38h
      _ => unreachable!(),
    }

    true
  }

  pub fn print_op(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
    let opcode = self.peek_imm8(w(0));
    match opcode.0 {
      0x00 => write!(f, "NOP"),
      0x01 => write!(f, "LD BC, {:04X}", self.peek_imm16(w(1))),
      0x02 => write!(f, "LD (BC), A"),
      0x03 => write!(f, "INC BC"),
      0x04 => write!(f, "INC B"),
      0x05 => write!(f, "DEC B"),
      0x06 => write!(f, "LD B, {:02X}", self.peek_imm8(w(1))),
      0x07 => write!(f, "RLCA"),
      0x08 => write!(f, "EX AF, AF'"),
      0x09 => write!(f, "ADD HL, BC"),
      0x0A => write!(f, "LD A, (BC)"),
      0x0B => write!(f, "DEC BC"),
      0x0C => write!(f, "INC C"),
      0x0D => write!(f, "DEC C"),
      0x0E => write!(f, "LD C, {:02X}", self.peek_imm8(w(1))),
      0x0F => write!(f, "RRCA"),

      0x10 => write!(f, "DJNZ {:04X}", self.pc_offset(self.peek_imm8(w(1)))),
      0x11 => write!(f, "LD DE, {:04X}", self.peek_imm16(w(1))),
      0x12 => write!(f, "LD (DE), A"),
      0x13 => write!(f, "INC DE"),
      0x14 => write!(f, "INC D"),
      0x15 => write!(f, "DEC D"),
      0x16 => write!(f, "LD D, {:02X}", self.peek_imm8(w(1))),
      0x17 => write!(f, "RLA"),
      0x18 => write!(f, "JR {:04X}", self.pc_offset(self.peek_imm8(w(1)))),
      0x19 => write!(f, "ADD HL, DE"),
      0x1A => write!(f, "LD A, (DE)"),
      0x1B => write!(f, "DEC DE"),
      0x1C => write!(f, "INC E"),
      0x1D => write!(f, "DEC E"),
      0x1E => write!(f, "LD E, {:02X}", self.peek_imm8(w(1))),
      0x1F => write!(f, "RRA"),

      0x20 => write!(f, "JR NZ, {:04X}", self.pc_offset(self.peek_imm8(w(1)))),
      0x21 => write!(f, "LD HL, {:04X}", self.peek_imm16(w(1))),
      0x22 => write!(f, "LD ({:04X}), HL", self.peek_imm16(w(1))),
      0x23 => write!(f, "INC HL"),
      0x24 => write!(f, "INC H"),
      0x25 => write!(f, "DEC H"),
      0x26 => write!(f, "LD H, {:02X}", self.peek_imm8(w(1))),
      0x27 => write!(f, "DAA"),
      0x28 => write!(f, "JR Z, {:04X}", self.pc_offset(self.peek_imm8(w(1)))),
      0x29 => write!(f, "ADD HL, HL"),
      0x2A => write!(f, "LD HL, ({:04X})", self.peek_imm16(w(1))),
      0x2B => write!(f, "DEC HL"),
      0x2C => write!(f, "INC L"),
      0x2D => write!(f, "DEC L"),
      0x2E => write!(f, "LD L, {:02X}", self.peek_imm8(w(1))),
      0x2F => write!(f, "CPL"),

      0x30 => write!(f, "JR NC, {:04X}", self.pc_offset(self.peek_imm8(w(1)))),
      0x31 => write!(f, "LD SP, {:04X}", self.peek_imm16(w(1))),
      0x32 => write!(f, "LD ({:04X}), A", self.peek_imm16(w(1))),
      0x33 => write!(f, "INC SP"),
      0x34 => write!(f, "INC (HL)"),
      0x35 => write!(f, "DEC (HL)"),
      0x36 => write!(f, "LD (HL), {:02X}", self.peek_imm8(w(1))),
      0x37 => write!(f, "SCF"),
      0x38 => write!(f, "JR C, {:04X}", self.pc_offset(self.peek_imm8(w(1)))),
      0x39 => write!(f, "ADD HL, SP"),
      0x3A => write!(f, "LD A, ({:04X})", self.peek_imm16(w(1))),
      0x3B => write!(f, "DEC SP"),
      0x3C => write!(f, "INC A"),
      0x3D => write!(f, "DEC A"),
      0x3E => write!(f, "LD A, {:02X}", self.peek_imm8(w(1))),
      0x3F => write!(f, "CCF"),

      0x40 => write!(f, "LD B, B"),
      0x41 => write!(f, "LD B, C"),
      0x42 => write!(f, "LD B, D"),
      0x43 => write!(f, "LD B, E"),
      0x44 => write!(f, "LD B, H"),
      0x45 => write!(f, "LD B, L"),
      0x46 => write!(f, "LD B, (HL)"),
      0x47 => write!(f, "LD B, A"),
      0x48 => write!(f, "LD C, B"),
      0x49 => write!(f, "LD C, C"),
      0x4A => write!(f, "LD C, D"),
      0x4B => write!(f, "LD C, E"),
      0x4C => write!(f, "LD C, H"),
      0x4D => write!(f, "LD C, L"),
      0x4E => write!(f, "LD C, (HL)"),
      0x4F => write!(f, "LD C, A"),

      0x50 => write!(f, "LD D, B"),
      0x51 => write!(f, "LD D, C"),
      0x52 => write!(f, "LD D, D"),
      0x53 => write!(f, "LD D, E"),
      0x54 => write!(f, "LD D, H"),
      0x55 => write!(f, "LD D, L"),
      0x56 => write!(f, "LD D, (HL)"),
      0x57 => write!(f, "LD D, A"),
      0x58 => write!(f, "LD E, B"),
      0x59 => write!(f, "LD E, C"),
      0x5A => write!(f, "LD E, D"),
      0x5B => write!(f, "LD E, E"),
      0x5C => write!(f, "LD E, H"),
      0x5D => write!(f, "LD E, L"),
      0x5E => write!(f, "LD E, (HL)"),
      0x5F => write!(f, "LD E, A"),

      0x60 => write!(f, "LD H, B"),
      0x61 => write!(f, "LD H, C"),
      0x62 => write!(f, "LD H, D"),
      0x63 => write!(f, "LD H, E"),
      0x64 => write!(f, "LD H, H"),
      0x65 => write!(f, "LD H, L"),
      0x66 => write!(f, "LD H, (HL)"),
      0x67 => write!(f, "LD H, A"),
      0x68 => write!(f, "LD L, B"),
      0x69 => write!(f, "LD L, C"),
      0x6A => write!(f, "LD L, D"),
      0x6B => write!(f, "LD L, E"),
      0x6C => write!(f, "LD L, H"),
      0x6D => write!(f, "LD L, L"),
      0x6E => write!(f, "LD L, (HL)"),
      0x6F => write!(f, "LD L, A"),

      0x70 => write!(f, "LD H, B"),
      0x71 => write!(f, "LD H, C"),
      0x72 => write!(f, "LD H, D"),
      0x73 => write!(f, "LD H, E"),
      0x74 => write!(f, "LD H, H"),
      0x75 => write!(f, "LD H, L"),
      0x76 => write!(f, "HALT"),
      0x77 => write!(f, "LD H, A"),
      0x78 => write!(f, "LD A, B"),
      0x79 => write!(f, "LD A, C"),
      0x7A => write!(f, "LD A, D"),
      0x7B => write!(f, "LD A, E"),
      0x7C => write!(f, "LD A, H"),
      0x7D => write!(f, "LD A, L"),
      0x7E => write!(f, "LD A, (HL)"),
      0x7F => write!(f, "LD A, A"),

      0x80 => write!(f, "ADD A, B"),
      0x81 => write!(f, "ADD A, C"),
      0x82 => write!(f, "ADD A, D"),
      0x83 => write!(f, "ADD A, E"),
      0x84 => write!(f, "ADD A, H"),
      0x85 => write!(f, "ADD A, L"),
      0x86 => write!(f, "ADD A, (HL)"),
      0x87 => write!(f, "ADD A, A"),
      0x88 => write!(f, "ADC A, B"),
      0x89 => write!(f, "ADC A, C"),
      0x8A => write!(f, "ADC A, D"),
      0x8B => write!(f, "ADC A, E"),
      0x8C => write!(f, "ADC A, H"),
      0x8D => write!(f, "ADC A, L"),
      0x8E => write!(f, "ADC A, (HL)"),
      0x8F => write!(f, "ADC A, A"),

      0x90 => write!(f, "SUB B"),
      0x91 => write!(f, "SUB C"),
      0x92 => write!(f, "SUB D"),
      0x93 => write!(f, "SUB E"),
      0x94 => write!(f, "SUB H"),
      0x95 => write!(f, "SUB L"),
      0x96 => write!(f, "SUB (HL)"),
      0x97 => write!(f, "SBC A"),
      0x98 => write!(f, "SBC B"),
      0x99 => write!(f, "SBC C"),
      0x9A => write!(f, "SBC D"),
      0x9B => write!(f, "SBC E"),
      0x9C => write!(f, "SBC H"),
      0x9D => write!(f, "SBC L"),
      0x9E => write!(f, "SBC (HL)"),
      0x9F => write!(f, "SBC A"),

      0xA0 => write!(f, "AND B"),
      0xA1 => write!(f, "AND C"),
      0xA2 => write!(f, "AND D"),
      0xA3 => write!(f, "AND E"),
      0xA4 => write!(f, "AND H"),
      0xA5 => write!(f, "AND L"),
      0xA6 => write!(f, "AND (HL)"),
      0xA7 => write!(f, "AND A"),
      0xA8 => write!(f, "XOR B"),
      0xA9 => write!(f, "XOR C"),
      0xAA => write!(f, "XOR D"),
      0xAB => write!(f, "XOR E"),
      0xAC => write!(f, "XOR H"),
      0xAD => write!(f, "XOR L"),
      0xAE => write!(f, "XOR (HL)"),
      0xAF => write!(f, "XOR A"),

      0xB0 => write!(f, "OR B"),
      0xB1 => write!(f, "OR C"),
      0xB2 => write!(f, "OR D"),
      0xB3 => write!(f, "OR E"),
      0xB4 => write!(f, "OR H"),
      0xB5 => write!(f, "OR L"),
      0xB6 => write!(f, "OR (HL)"),
      0xB7 => write!(f, "OR A"),
      0xB8 => write!(f, "CP B"),
      0xB9 => write!(f, "CP C"),
      0xBA => write!(f, "CP D"),
      0xBB => write!(f, "CP E"),
      0xBC => write!(f, "CP H"),
      0xBD => write!(f, "CP L"),
      0xBE => write!(f, "CP (HL)"),
      0xBF => write!(f, "CP A"),

      0xC0 => write!(f, "RET NZ"),
      0xC1 => write!(f, "POP BC"),
      0xC2 => write!(f, "JP NZ, {:04X}", self.peek_imm16(w(1))),
      0xC3 => write!(f, "JP {:04X}", self.peek_imm16(w(1))),
      0xC4 => write!(f, "CALL NZ, {:04X}", self.peek_imm16(w(1))),
      0xC5 => write!(f, "PUSH BC"),
      0xC6 => write!(f, "ADD A, {:02X}", self.peek_imm8(w(1))),
      0xC7 => write!(f, "RST 00h"),
      0xC8 => write!(f, "RET Z"),
      0xC9 => write!(f, "RET"),
      0xCA => write!(f, "JP Z, {:04X}", self.peek_imm16(w(1))),
      0xCB => write!(f, "unimplemented"),// self.get_bit_op(f),
      0xCC => write!(f, "CALL Z, {:04X}", self.peek_imm16(w(1))),
      0xCD => write!(f, "CALL {:04X}", self.peek_imm16(w(1))),
      0xCE => write!(f, "ADC A, {:02X}", self.peek_imm8(w(1))),
      0xCF => write!(f, "RST 08h"),

      0xD0 => write!(f, "RET NC"),
      0xD1 => write!(f, "POP DE"),
      0xD2 => write!(f, "JP NC, {:04X}", self.peek_imm16(w(1))),
      0xD3 => write!(f, "unimplemented"), //write!(f, "OUT ({:02X}), A", self.peek_imm8(w(1))),
      0xD4 => write!(f, "CALL NC, {:04X}", self.peek_imm16(w(1))),
      0xD5 => write!(f, "PUSH DE"),
      0xD6 => write!(f, "SUB {:02X}", self.peek_imm8(w(1))),
      0xD7 => write!(f, "RST 10h"),
      0xD8 => write!(f, "RET C"),
      0xD9 => write!(f, "EXX"),
      0xDA => write!(f, "JP C, {:04X}", self.peek_imm16(w(1))),
      0xDB => write!(f, "unimplemented"), //write!(f, "IN A, {:02X}", self.peek_imm8(w(1))),
      0xDC => write!(f, "CALL C, {:04X}", self.peek_imm16(w(1))),
      0xDD => write!(f, "unimplemented"), // self.print_ix_op(f),
      0xDE => write!(f, "SBC A, {:02X}", self.peek_imm8(w(1))),
      0xDF => write!(f, "RST 18h"),

      0xE0 => write!(f, "RET PO"),
      0xE1 => write!(f, "POP HL"),
      0xE2 => write!(f, "JP PO, {:04X}", self.peek_imm16(w(1))),
      0xE3 => write!(f, "EX (SP), HL"),
      0xE4 => write!(f, "CALL PO, {:04X}", self.peek_imm16(w(1))),
      0xE5 => write!(f, "PUSH HL"),
      0xE6 => write!(f, "AND {:02X}", self.peek_imm8(w(1))),
      0xE7 => write!(f, "RST 20h"),
      0xE8 => write!(f, "RET PE"),
      0xE9 => write!(f, "JP (HL)"),
      0xEA => write!(f, "JP PE, {:04X}", self.peek_imm16(w(1))),
      0xEB => write!(f, "EX DE, HL"),
      0xEC => write!(f, "CALL PE, {:04X}", self.peek_imm16(w(1))),
      0xED => write!(f, "unimplemented"), // self.print_extd_op(f),
      0xEE => write!(f, "XOR {:02X}", self.peek_imm8(w(1))),
      0xEF => write!(f, "RST 28h"),

      0xF0 => write!(f, "RET P"),
      0xF1 => write!(f, "POP AF"),
      0xF2 => write!(f, "JP P, {:04X}", self.peek_imm16(w(1))),
      0xF3 => write!(f, "DI"),
      0xF4 => write!(f, "CALL P, {:04X}", self.peek_imm16(w(1))),
      0xF5 => write!(f, "PUSH AF"),
      0xF6 => write!(f, "OR {:02X}", self.peek_imm8(w(1))),
      0xF7 => write!(f, "RST 30h"),
      0xF8 => write!(f, "RET M"),
      0xF9 => write!(f, "LD SP, HL"),
      0xFA => write!(f, "JP M, {:04X}", self.peek_imm16(w(1))),
      0xFB => write!(f, "EI"),
      0xFC => write!(f, "CALL M, {:04X}", self.peek_imm16(w(1))),
      0xFD => write!(f, "unimplemented"), // self.print_iy_op(f),
      0xFE => write!(f, "CP {:02X}", self.peek_imm8(w(1))),
      0xFF => write!(f, "RST 38h"),
      _ => unreachable!(),
    }
  }
}
