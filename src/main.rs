// TODO(ubsan): p flag
// TODO(ubsan): h flag
// TODO(ubsan): n flag
#![feature(box_syntax, question_mark)]

//use std::io::BufRead;
use std::io::Read;

fn main() {
  let mut state = State::with_memory(&[
      0x06, 0x05,       // LD B, 05
      0x3E, 0x00,       // LD A, 00
      0x16, 0x01,       // LD D, 01
      0x26, 0x00,       // LD H, 00
      // loop:
      0x82,             // ADD A, D
      0x67,             // LD H, A
      0xEB,             // EX DE, HL
      0x10, 0xFD,       // DJNZ loop

      0x76,             // HALT
  ]);

  println!("{:?}", state);
  'dbg: while state.step() {
    'input: for c in std::io::stdin().bytes() {
      match c.unwrap() {
        b'q' => break 'dbg,
        b'\n' | b'n' => break 'input,
        _ => {
          println!("?");
          break 'input;
        }
      }
    }
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
    writeln!(f, "A  CZPSNH  BC   DE   HL   PC  v(PC)")?;
    writeln!(f, "{:02X} {}{}{}{}{}{} {:04X} {:04X} {:04X} \
        {:04X} {:02X}{:02X}{:02X}",
        self.r.a,
        self.r.flags.c() as u8, self.r.flags.z() as u8,
        0/*self.r.flags.p() as u8*/, self.r.flags.s() as u8,
        0/*self.r.flags.n() as u8*/, 0/*self.flags.h() as u8*/,
        self.r.bc(), self.r.de(), self.r.hl(),

        self.pc,
        self.memory[self.pc as usize], self.memory[(self.pc + 1) as usize],
        self.memory[(self.pc + 2) as usize])?;

    // prime regs
    writeln!(f, "A' CZPSNH' BC'  DE'  HL'  SP  v(SP)")?;
    writeln!(f, "{:02X} {}{}{}{}{}{} {:04X} {:04X} {:04X} \
        {:04X} {:02X}{:02X}{:02X}",
        self.rp.a,
        self.rp.flags.c() as u8, self.rp.flags.z() as u8,
        0/*self.rp.flags.p() as u8*/, self.rp.flags.s() as u8,
        0/*self.rp.flags.n() as u8*/, 0/*self.rp.flags.h()*/,
        self.rp.bc(), self.rp.de(), self.rp.hl(),

        self.sp,
        self.memory[self.sp as usize], self.memory[(self.sp + 1) as usize],
        self.memory[(self.sp + 2) as usize])?;
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
        self.r.b != 0
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

  pub fn add8(&mut self, from: Op8) {
    let tmp = self.r.a as u16 + self.get8(from) as u16;
    self.r.flags.set_c(tmp > 0xFF);
    let tmp = tmp as u8;
    self.r.flags.set_z(tmp == 0);
    self.r.flags.set_s((tmp as i8) < 0);

    self.r.a = tmp;
  }
  pub fn adc8(&mut self, from: Op8) {
    let tmp = self.r.a as u16 + self.get8(from) as u16
        + self.r.flags.c() as u16;
    self.r.flags.set_c(tmp > 0xFF);
    let tmp = tmp as u8;
    self.r.flags.set_z(tmp == 0);
    self.r.flags.set_s((tmp as i8) < 0);

    self.r.a = tmp;
  }
  pub fn sub8(&mut self, from: Op8) {
    let tmp = self.r.a as i16 - self.get8(from) as i16;
    self.r.flags.set_c(tmp < 0);
    let tmp = tmp as u8;
    self.r.flags.set_z(tmp == 0);
    self.r.flags.set_s((tmp as i8) < 0);

    self.r.a = tmp;
  }
  pub fn sbc8(&mut self, from: Op8) {
    let tmp = self.r.a as i16 - self.get8(from) as i16
        - self.r.flags.c() as i16;
    self.r.flags.set_c(tmp < 0);
    let tmp = tmp as u8;
    self.r.flags.set_z(tmp == 0);
    self.r.flags.set_s((tmp as i8) < 0);

    self.r.a = tmp;
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

  pub fn ex_de_hl(&mut self) {
    std::mem::swap(&mut self.r.d, &mut self.r.h);
    std::mem::swap(&mut self.r.e, &mut self.r.l);
  }

  pub fn halt(&mut self) {}
}

// at the end because lots of lines
impl State {
  // returns true if the machine should continue
  // returns false if it shut off
  pub fn step(&mut self) -> bool {
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
      0x1E => self.ld8(Op8::E, Op8::Imm),       // LD E, N
      0x1F => self.rr(Op8::A),                  // RRA

      0x20 => self.jr(Flag::Nonzero),           // JRNZ NN
      0x21 => self.ld16(Op16::HL, Op16::Imm),   // LD HL, NN
      0x22 => self.ld16(Op16::Immd, Op16::HL),  // LD (NN), HL
      0x23 => self.inc16(Op16::HL),             // INC HL
      0x24 => self.inc8(Op8::H),                // INC H
      0x25 => self.dec8(Op8::H),                // DEC H
      0x26 => self.ld8(Op8::H, Op8::Imm),       // LD H, N
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
      0x39 => self.add16(Op16::HL, Op16::SP),   // ADD HL, SP
      0x3A => self.add16(Op16::HL, Op16::Immd), // ADD HL, (NN)
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
      0xEB => self.ex_de_hl(),                  // EX DE, HL
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

    true
  }
}
