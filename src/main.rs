// TODO(ubsan): p flag
// TODO(ubsan): h flag
// TODO(ubsan): n flag

#![feature(box_syntax, question_mark)]
fn main() {
  use std::io::Read;
  let mut state = State::with_memory(&[
      0x0C, // inc b
      0x0D, // dec b
      0x28, 0xFF,
  ]);
  println!("{:?}", state);
  for _ in std::io::stdin().bytes() {
    if state.memory[state.pc as usize] == 0 { break; }
    state.step();
    println!("{:?}", state);
  }
}

use std::fmt::{self, Debug, Formatter};
pub struct Flags(u8);
impl Flags {
  pub fn zeroed() -> Self { Flags(0) }

  pub fn s(&self) -> bool { self.0 >> 7 == 1 }
  pub fn z(&self) -> bool { self.0 >> 6 == 1 }
  pub fn h(&self) -> bool { unimplemented!() /*self.0 >> 4 == 1*/ }
  pub fn p(&self) -> bool { unimplemented!() /*self.0 >> 2 == 1*/ }
  pub fn n(&self) -> bool { unimplemented!() /*self.0 >> 1 == 1*/ }
  pub fn c(&self) -> bool { self.0 >> 0 == 1 }
  pub fn set_s(&mut self, to: bool) { self.0 |= (to as u8) << 7 }
  pub fn set_z(&mut self, to: bool) { self.0 |= (to as u8) << 6 }
  pub fn set_h(&mut self, _to: bool) {
    unimplemented!() /*self.0 |= (to as u8) << 4*/
  }
  pub fn set_p(&mut self, _to: bool) {
    unimplemented!() /*self.0 |= (to as u8) << 2*/
  }
  pub fn set_n(&mut self, _to: bool) {
    unimplemented!() /*self.0 |= (to as u8) << 1*/
  }
  pub fn set_c(&mut self, to: bool) { self.0 |= (to as u8) << 0 }
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
  pub a: u8,
  pub b: u8,
  pub c: u8,
  pub d: u8,
  pub e: u8,
  pub h: u8,
  pub l: u8,
  pub flags: Flags,
}

impl Regs {
  fn hl(&self) -> u16 {
    (self.h as u16) << 8 | self.l as u16
  }
  fn set_hl(&mut self, to: u16) {
    self.h = (to >> 8) as u8;
    self.l = to as u8;
  }

  fn bc(&self) -> u16 {
    (self.b as u16) << 8 | self.c as u16
  }
  fn set_bc(&mut self, to: u16) {
    self.b = (to >> 8) as u8;
    self.c = to as u8;
  }

  fn de(&self) -> u16 {
    (self.d as u16) << 8 | self.e as u16
  }
  fn set_de(&mut self, to: u16) {
    self.d = (to >> 8) as u8;
    self.e = to as u8;
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

pub struct State {
  r: Regs,
  rp: Regs, // r'
  sp: u16,
  pc: u16,
  memory: Box<[u8; std::u16::MAX as usize + 1]>,
  _enable_int: u8,
}

impl Debug for State {
  fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
    writeln!(f, "A  CZPSNH  BC   DE   HL  A' CZPSNH' BC'  DE'  HL' v(PC)")?;
    writeln!(f, "{:02X} {}{}{}{}{}{} {:04X} {:04X} {:04X} {:02X} {}{}{}{}{}{} \
        {:04X} {:04X} {:04X} {:02X}{:02X}{:02X}",
        // normal regs
        self.r.a,
          // flags
        self.r.flags.c() as u8, self.r.flags.z() as u8,
        0/*self.r.flags.p() as u8*/, self.r.flags.s() as u8,
        0/*self.r.flags.n() as u8*/, 0/*self.flags.h() as u8*/,
          // 16-bit
        self.r.bc(), self.r.de(), self.r.hl(),

        // prime regs
        self.rp.a,
          // flags
        self.rp.flags.c() as u8, self.rp.flags.z() as u8,
        0/*self.rp.flags.p() as u8*/, self.rp.flags.s() as u8,
        0/*self.rp.flags.n() as u8*/, 0/*self.rp.flags.h()*/,
          // 16-bit
        self.rp.bc(), self.rp.de(), self.rp.hl(),

        // (pc)
        self.memory[self.pc as usize], self.memory[(self.pc + 1) as usize],
        self.memory[(self.pc + 2) as usize])?;
    Ok(())
  }
}

impl State {
  pub fn zeroed() -> Self {
    use std::{mem, ptr};
    unsafe {
      let mut ret: Self = mem::zeroed();
      ptr::write(&mut ret.memory, box [0; std::u16::MAX as usize + 1]);
      ret
    }
  }

  pub fn with_memory(mem: &[u8]) -> Self {
    let mut ret = Self::zeroed();
    ret.memory[..mem.len()].copy_from_slice(mem);
    ret
  }

  pub fn get_imm8(&mut self) -> u8 {
    let ret = self.memory[self.pc as usize];
    self.pc += 1;
    ret
  }
  pub fn get_imm16(&mut self) -> u16 {
    let lower = self.get_imm8();
    let upper = self.get_imm8();
    (upper as u16) << 8 | lower as u16
  }

  pub fn get8(&mut self, op: Op8) -> u8 {
    match op {
      Op8::A => self.r.a,
      Op8::B => self.r.b,
      Op8::C => self.r.c,
      Op8::D => self.r.d,
      Op8::E => self.r.e,
      Op8::H => self.r.h,
      Op8::L => self.r.l,
      Op8::BCd => self.memory[self.r.bc() as usize],
      Op8::DEd => self.memory[self.r.de() as usize],
      Op8::HLd => self.memory[self.r.hl() as usize],
      Op8::Imm => self.get_imm8(),
      Op8::Immd => self.memory[self.get_imm16() as usize],
    }
  }
  pub fn set8(&mut self, op: Op8, to: u8) {
    match op {
      Op8::A => self.r.a = to,
      Op8::B => self.r.b = to,
      Op8::C => self.r.c = to,
      Op8::D => self.r.d = to,
      Op8::E => self.r.e = to,
      Op8::H => self.r.h = to,
      Op8::L => self.r.l = to,
      Op8::BCd => self.memory[self.r.bc() as usize] = to,
      Op8::DEd => self.memory[self.r.de() as usize] = to,
      Op8::HLd => self.memory[self.r.hl() as usize] = to,
      Op8::Immd => self.memory[self.get_imm16() as usize] = to,
      Op8::Imm => panic!("passed Op8::Imm to set8"),
    }
  }

  pub fn get16(&mut self, op: Op16) -> u16 {
    match op {
      Op16::AF => unimplemented!(),
      Op16::BC => self.r.bc(),
      Op16::DE => self.r.de(),
      Op16::HL => self.r.hl(),
      Op16::SP => self.sp,
      Op16::Imm => self.get_imm16(),
      Op16::Immd => {
        let idx = self.get_imm16();
        let lower = self.memory[idx as usize];
        let upper = self.memory[(idx + 1) as usize];
        (upper as u16) << 8 | lower as u16
      }
    }
  }
  pub fn set16(&mut self, op: Op16, to: u16) {
    match op {
      Op16::AF => unimplemented!(),
      Op16::BC => self.r.set_bc(to),
      Op16::DE => self.r.set_de(to),
      Op16::HL => self.r.set_hl(to),
      Op16::SP => self.sp = to,
      Op16::Imm => panic!("Passed Op16::Imm to set16"),
      Op16::Immd => {
        let idx = self.get_imm16();
        self.memory[idx as usize] = to as u8;
        self.memory[(idx + 1) as usize] = (to >> 8) as u8;
      }
    }
  }

  pub fn flag(&mut self, f: Flag) -> bool {
    match f {
      Flag::Djnz => {
        self.r.b -= 1;
        self.r.b == 0
      }
      Flag::Uncond => true,
      Flag::Nonzero => !self.r.flags.z(),
      Flag::Zero => self.r.flags.z(),
      Flag::NoCarry => !self.r.flags.c(),
      Flag::Carry => self.r.flags.c(),
    }
  }

  fn unimplemented_inst(&mut self) {
    self.pc -= 1;
    panic!("Error: Unimplemented Instruction: {} \n{:?}",
      self.memory[self.pc as usize], self);
  }

  fn _not_an_inst(&mut self) {
    self.pc -= 1;
    panic!("Error: Unimplemented Instruction: {:#?} ({})", self,
      self.memory[self.pc as usize]);
  }
}

impl State {
  pub fn ld16(&mut self, into: Op16, from: Op16) {
    let tmp = self.get16(from);
    self.set16(into, tmp);
  }
  pub fn ld8(&mut self, into: Op8, from: Op8) {
    let tmp = self.get8(from);
    self.set8(into, tmp);
  }

  pub fn add16(&mut self, into: Op16, from: Op16) {
    let tmp = self.get16(into) as u32 + self.get16(from) as u32;
    self.r.flags.set_c(tmp > 0xFFFF);
    self.set16(into, tmp as u16);
  }

  pub fn not(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    self.set8(arg, !tmp);
  }

  pub fn inc16(&mut self, arg: Op16) {
    let tmp = self.get16(arg) + 1;
    self.set16(arg, tmp);
  }
  pub fn dec16(&mut self, arg: Op16) {
    let tmp = self.get16(arg) - 1;
    self.set16(arg, tmp);
  }
  pub fn inc8(&mut self, arg: Op8) {
    let tmp = self.get8(arg) as u16 + 1;
    self.r.flags.set_c(tmp > 0xFF);
    let tmp = tmp as u8;
    self.r.flags.set_z(tmp == 0);
    self.r.flags.set_s((tmp as i8) < 0);

    self.set8(arg, tmp as u8);
  }
  pub fn dec8(&mut self, arg: Op8) {
    let tmp = self.get8(arg) as i8 - 1;
    self.r.flags.set_c(tmp < 0);
    self.r.flags.set_z(tmp == 0);
    self.r.flags.set_s(tmp < 0);

    self.set8(arg, tmp as u8);
  }

  pub fn rlc(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry = tmp >> 7;
    self.r.flags.set_c(carry != 0);
    self.set8(arg, tmp << 1 | carry);
  }
  pub fn rrc(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry = tmp << 7;
    self.r.flags.set_c(carry != 0);
    self.set8(arg, tmp >> 1 | carry);
  }
  pub fn rl(&mut self, arg: Op8) {
    let mut tmp = self.get8(arg);
    let carry = tmp >> 7;
    tmp = tmp << 1 | self.r.flags.c() as u8;
    self.set8(arg, tmp);
    self.r.flags.set_c(carry != 0);
  }
  pub fn rr(&mut self, arg: Op8) {
    let mut tmp = self.get8(arg);
    let carry = tmp << 7;
    tmp = tmp >> 1 | (self.r.flags.c() as u8) << 7;
    self.set8(arg, tmp);
    self.r.flags.set_c(carry != 0);
  }

  pub fn jr(&mut self, f: Flag) {
    let label = self.get_imm8();
    if self.flag(f) {
      self.pc -= 2; // get to start of opcode
      self.pc += label as i8 as u16;
    }
  }

  pub fn ex_af(&mut self) {
    std::mem::swap(&mut self.r.a, &mut self.rp.a);
    std::mem::swap(&mut self.r.flags, &mut self.rp.flags);
  }
}

// at the end because lots of lines
impl State {
  pub fn step(&mut self) {
    let opcode = self.get_imm8();
    match opcode {
      0x00 => {} // NOP
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
      0x1E => self.ld8(Op8::C, Op8::Imm),       // LD C, N
      0x1F => self.rr(Op8::A),                  // RRA

      0x20 => self.jr(Flag::Nonzero),           // JRNZ NN
      0x21 => self.ld16(Op16::HL, Op16::Imm),   // LD HL, NN
      0x22 => self.ld16(Op16::Immd, Op16::HL),  // LD (NN), HL
      0x23 => self.inc16(Op16::HL),             // INC HL
      0x24 => self.inc8(Op8::H),                // INC H
      0x25 => self.dec8(Op8::H),                // DEC H
      0x26 => self.ld8(Op8::HLd, Op8::Imm),     // LD (HL), N
      0x27 => self.unimplemented_inst(),        // DAA
      0x28 => self.jr(Flag::Zero),              // JRZ N
      0x29 => self.add16(Op16::HL, Op16::HL),   // ADD HL, HL
      0x2A => self.ld16(Op16::HL, Op16::Immd),  // LD HL, (NN)
      0x2B => self.dec16(Op16::HL),             // DEC HL
      0x2C => self.inc8(Op8::L),                // INC L
      0x2D => self.dec8(Op8::L),                // DEC L
      0x2E => self.ld8(Op8::L, Op8::Imm),       // LD L, N
      0x2F => self.not(Op8::A),                 // CPL

      0x30 => self.jr(Flag::NoCarry),           // JRNC N
      0x31 => self.ld16(Op16::SP, Op16::Imm),   // LD SP, NN
      0x32 => self.ld8(Op8::Immd, Op8::A),      // LD (NN), A
      0x33 => self.inc16(Op16::SP),             // INC SP
      0x34 => self.inc8(Op8::HLd),              // INC (HL)
      0x35 => self.dec8(Op8::HLd),              // DEC (HL)
      0x36 => self.ld8(Op8::HLd, Op8::Imm),     // LD (HL), N
      0x37 => self.r.flags.set_c(true),         // SCF
      0x38 => self.jr(Flag::Carry),             // JRC N
      0x39 => self.add16(Op16::HL, Op16::HL),   // ADD HL, HL
      0x3A => self.add16(Op16::HL, Op16::Immd), // ADD HL, (NN)
      0x3B => self.dec16(Op16::HL),             // DEC HL
      0x3C => self.inc8(Op8::L),
      0x3D => self.dec8(Op8::L),
      0x3E => self.ld8(Op8::L, Op8::Imm),
      0x3F => {
        let c = self.r.flags.c();
        self.r.flags.set_c(!c);
      },

      0x40 => self.unimplemented_inst(),
      0x41 => self.unimplemented_inst(),
      0x42 => self.unimplemented_inst(),
      0x43 => self.unimplemented_inst(),
      0x44 => self.unimplemented_inst(),
      0x45 => self.unimplemented_inst(),
      0x46 => self.unimplemented_inst(),
      0x47 => self.unimplemented_inst(),
      0x48 => self.unimplemented_inst(),
      0x49 => self.unimplemented_inst(),
      0x4A => self.unimplemented_inst(),
      0x4B => self.unimplemented_inst(),
      0x4C => self.unimplemented_inst(),
      0x4D => self.unimplemented_inst(),
      0x4E => self.unimplemented_inst(),
      0x4F => self.unimplemented_inst(),

      0x50 => self.unimplemented_inst(),
      0x51 => self.unimplemented_inst(),
      0x52 => self.unimplemented_inst(),
      0x53 => self.unimplemented_inst(),
      0x54 => self.unimplemented_inst(),
      0x55 => self.unimplemented_inst(),
      0x56 => self.unimplemented_inst(),
      0x57 => self.unimplemented_inst(),
      0x58 => self.unimplemented_inst(),
      0x59 => self.unimplemented_inst(),
      0x5A => self.unimplemented_inst(),
      0x5B => self.unimplemented_inst(),
      0x5C => self.unimplemented_inst(),
      0x5D => self.unimplemented_inst(),
      0x5E => self.unimplemented_inst(),
      0x5F => self.unimplemented_inst(),
      0x60 => self.unimplemented_inst(),
      0x61 => self.unimplemented_inst(),
      0x62 => self.unimplemented_inst(),
      0x63 => self.unimplemented_inst(),
      0x64 => self.unimplemented_inst(),
      0x65 => self.unimplemented_inst(),
      0x66 => self.unimplemented_inst(),
      0x67 => self.unimplemented_inst(),
      0x68 => self.unimplemented_inst(),
      0x69 => self.unimplemented_inst(),
      0x6A => self.unimplemented_inst(),
      0x6B => self.unimplemented_inst(),
      0x6C => self.unimplemented_inst(),
      0x6D => self.unimplemented_inst(),
      0x6E => self.unimplemented_inst(),
      0x6F => self.unimplemented_inst(),
      0x70 => self.unimplemented_inst(),
      0x71 => self.unimplemented_inst(),
      0x72 => self.unimplemented_inst(),
      0x73 => self.unimplemented_inst(),
      0x74 => self.unimplemented_inst(),
      0x75 => self.unimplemented_inst(),
      0x76 => self.unimplemented_inst(),
      0x77 => self.unimplemented_inst(),
      0x78 => self.unimplemented_inst(),
      0x79 => self.unimplemented_inst(),
      0x7A => self.unimplemented_inst(),
      0x7B => self.unimplemented_inst(),
      0x7C => self.unimplemented_inst(),
      0x7D => self.unimplemented_inst(),
      0x7E => self.unimplemented_inst(),
      0x7F => self.unimplemented_inst(),
      0x80 => self.unimplemented_inst(),
      0x81 => self.unimplemented_inst(),
      0x82 => self.unimplemented_inst(),
      0x83 => self.unimplemented_inst(),
      0x84 => self.unimplemented_inst(),
      0x85 => self.unimplemented_inst(),
      0x86 => self.unimplemented_inst(),
      0x87 => self.unimplemented_inst(),
      0x88 => self.unimplemented_inst(),
      0x89 => self.unimplemented_inst(),
      0x8A => self.unimplemented_inst(),
      0x8B => self.unimplemented_inst(),
      0x8C => self.unimplemented_inst(),
      0x8D => self.unimplemented_inst(),
      0x8E => self.unimplemented_inst(),
      0x8F => self.unimplemented_inst(),
      0x90 => self.unimplemented_inst(),
      0x91 => self.unimplemented_inst(),
      0x92 => self.unimplemented_inst(),
      0x93 => self.unimplemented_inst(),
      0x94 => self.unimplemented_inst(),
      0x95 => self.unimplemented_inst(),
      0x96 => self.unimplemented_inst(),
      0x97 => self.unimplemented_inst(),
      0x98 => self.unimplemented_inst(),
      0x99 => self.unimplemented_inst(),
      0x9A => self.unimplemented_inst(),
      0x9B => self.unimplemented_inst(),
      0x9C => self.unimplemented_inst(),
      0x9D => self.unimplemented_inst(),
      0x9E => self.unimplemented_inst(),
      0x9F => self.unimplemented_inst(),
      0xA0 => self.unimplemented_inst(),
      0xA1 => self.unimplemented_inst(),
      0xA2 => self.unimplemented_inst(),
      0xA3 => self.unimplemented_inst(),
      0xA4 => self.unimplemented_inst(),
      0xA5 => self.unimplemented_inst(),
      0xA6 => self.unimplemented_inst(),
      0xA7 => self.unimplemented_inst(),
      0xA8 => self.unimplemented_inst(),
      0xA9 => self.unimplemented_inst(),
      0xAA => self.unimplemented_inst(),
      0xAB => self.unimplemented_inst(),
      0xAC => self.unimplemented_inst(),
      0xAD => self.unimplemented_inst(),
      0xAE => self.unimplemented_inst(),
      0xAF => self.unimplemented_inst(),
      0xB0 => self.unimplemented_inst(),
      0xB1 => self.unimplemented_inst(),
      0xB2 => self.unimplemented_inst(),
      0xB3 => self.unimplemented_inst(),
      0xB4 => self.unimplemented_inst(),
      0xB5 => self.unimplemented_inst(),
      0xB6 => self.unimplemented_inst(),
      0xB7 => self.unimplemented_inst(),
      0xB8 => self.unimplemented_inst(),
      0xB9 => self.unimplemented_inst(),
      0xBA => self.unimplemented_inst(),
      0xBB => self.unimplemented_inst(),
      0xBC => self.unimplemented_inst(),
      0xBD => self.unimplemented_inst(),
      0xBE => self.unimplemented_inst(),
      0xBF => self.unimplemented_inst(),
      0xC0 => self.unimplemented_inst(),
      0xC1 => self.unimplemented_inst(),
      0xC2 => self.unimplemented_inst(),
      0xC3 => self.unimplemented_inst(),
      0xC4 => self.unimplemented_inst(),
      0xC5 => self.unimplemented_inst(),
      0xC6 => self.unimplemented_inst(),
      0xC7 => self.unimplemented_inst(),
      0xC8 => self.unimplemented_inst(),
      0xC9 => self.unimplemented_inst(),
      0xCA => self.unimplemented_inst(),
      0xCB => self.unimplemented_inst(),
      0xCC => self.unimplemented_inst(),
      0xCD => self.unimplemented_inst(),
      0xCE => self.unimplemented_inst(),
      0xCF => self.unimplemented_inst(),
      0xD0 => self.unimplemented_inst(),
      0xD1 => self.unimplemented_inst(),
      0xD2 => self.unimplemented_inst(),
      0xD3 => self.unimplemented_inst(),
      0xD4 => self.unimplemented_inst(),
      0xD5 => self.unimplemented_inst(),
      0xD6 => self.unimplemented_inst(),
      0xD7 => self.unimplemented_inst(),
      0xD8 => self.unimplemented_inst(),
      0xD9 => self.unimplemented_inst(),
      0xDA => self.unimplemented_inst(),
      0xDB => self.unimplemented_inst(),
      0xDC => self.unimplemented_inst(),
      0xDD => self.unimplemented_inst(),
      0xDE => self.unimplemented_inst(),
      0xDF => self.unimplemented_inst(),
      0xE0 => self.unimplemented_inst(),
      0xE1 => self.unimplemented_inst(),
      0xE2 => self.unimplemented_inst(),
      0xE3 => self.unimplemented_inst(),
      0xE4 => self.unimplemented_inst(),
      0xE5 => self.unimplemented_inst(),
      0xE6 => self.unimplemented_inst(),
      0xE7 => self.unimplemented_inst(),
      0xE8 => self.unimplemented_inst(),
      0xE9 => self.unimplemented_inst(),
      0xEA => self.unimplemented_inst(),
      0xEB => self.unimplemented_inst(),
      0xEC => self.unimplemented_inst(),
      0xED => self.unimplemented_inst(),
      0xEE => self.unimplemented_inst(),
      0xEF => self.unimplemented_inst(),
      0xF0 => self.unimplemented_inst(),
      0xF1 => self.unimplemented_inst(),
      0xF2 => self.unimplemented_inst(),
      0xF3 => self.unimplemented_inst(),
      0xF4 => self.unimplemented_inst(),
      0xF5 => self.unimplemented_inst(),
      0xF6 => self.unimplemented_inst(),
      0xF7 => self.unimplemented_inst(),
      0xF8 => self.unimplemented_inst(),
      0xF9 => self.unimplemented_inst(),
      0xFA => self.unimplemented_inst(),
      0xFB => self.unimplemented_inst(),
      0xFC => self.unimplemented_inst(),
      0xFD => self.unimplemented_inst(),
      0xFE => self.unimplemented_inst(),
      0xFF => self.unimplemented_inst(),
      _ => unreachable!(),
    }
  }
}
