mod regs;
mod decoder;

use std::mem;

use wrapping::{w8, w16, w, cvt, Extensions};

use self::regs::Regs;

#[derive(Copy, Clone, Debug)]
enum Op8 {
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
enum Op16 {
  AF,
  BC,
  DE,
  HL,
  SP,
  Imm,
  Immd,
}

#[derive(Copy, Clone, Debug)]
enum Flag {
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

  fn read8(&self, idx: w16) -> w8 {
    w(self.banks[Self::get_bank(idx)][Self::get_idx(idx)])
  }
  fn write8(&mut self, idx: w16, n: w8) {
    assert!(Self::get_bank(idx) != 0, "can't write to r/o memory");
    self.banks[Self::get_bank(idx)][Self::get_idx(idx)] = n.0;
  }
  fn read16(&self, idx: w16) -> w16 {
    cvt(self.read8(idx)) as w16
    | (cvt(self.read8(idx + w(1))) as w16) << 8
  }
  fn write16(&mut self, idx: w16, n: w16) {
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

  pub fn run(&mut self) {
    while self.step() { }
  }

  fn get_imm8(&mut self) -> w8 {
    let ret = self.memory.read8(self.pc);
    self.pc += w(1);
    ret
  }
  fn get_imm16(&mut self) -> w16 {
    let ret = self.memory.read16(self.pc);
    self.pc += w(2);
    ret
  }

  fn pc_offset(&self, ind: w8) -> w16 {
    self.pc + w(ind.0 as i8 as u16)
  }

  fn get8(&mut self, op: Op8) -> w8 {
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
  fn set8(&mut self, op: Op8, to: w8) {
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

  fn get16(&mut self, op: Op16) -> w16 {
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
  fn set16(&mut self, op: Op16, to: w16) {
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

  fn flag(&mut self, f: Flag) -> bool {
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

// instructions
impl Processor {
  fn ld16(&mut self, into: Op16, from: Op16) {
    let tmp = self.get16(from);
    self.set16(into, tmp);
  }
  fn ld8(&mut self, into: Op8, from: Op8) {
    let tmp = self.get8(from);
    self.r.flags.set_z(tmp == w(0));
    self.r.flags.set_s((tmp.0 as i8) < 0);
    self.set8(into, tmp);
  }
  fn pop_imm16(&mut self) -> w16 {
    let tmp = self.memory.read16(self.sp);
    self.sp += w(2);
    tmp
  }
  fn pop16(&mut self, into: Op16) {
    let tmp = self.pop_imm16();
    self.set16(into, tmp);
  }
  fn push_imm16(&mut self, from: w16) {
    self.sp -= w(2);
    self.memory.write16(self.sp, from);
  }
  fn push16(&mut self, from: Op16) {
    let tmp = self.get16(from);
    self.push_imm16(tmp);
  }

  fn add16(&mut self, into: Op16, from: Op16) {
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

  fn add8(&mut self, from: Op8) {
    let tmp = self.r.a.overflowing_add(self.get8(from));
    let v = self.set_flags(tmp);
    self.r.a = v;
  }
  fn adc8(&mut self, from: Op8) {
    let v = {
      let carry = w(self.r.flags.c() as u8);
      let tmp = self.r.a.overflowing_add(self.get8(from));
      let (v, o) = (tmp.0).overflowing_add(carry);
      self.set_flags((v, o | tmp.1))
    };
    self.r.a = v;
  }

  fn sub8(&mut self, from: Op8) {
    let tmp = self.r.a.overflowing_sub(self.get8(from));
    let v = self.set_flags(tmp);
    self.r.a = v;
  }
  fn sbc8(&mut self, from: Op8) {
    let v = {
      let carry = w(self.r.flags.c() as u8);
      let tmp = self.r.a.overflowing_sub(self.get8(from));
      let (v, o) = (tmp.0).overflowing_sub(carry);
      self.set_flags((v, o | tmp.1))
    };
    self.r.a = v;
  }

  fn and8(&mut self, from: Op8) {
    let tmp = self.r.a & self.get8(from);
    let v = self.set_flags((tmp, false));
    self.r.a = v;
  }
  fn xor8(&mut self, from: Op8) {
    let tmp = self.r.a ^ self.get8(from);
    let v = self.set_flags((tmp, false));
    self.r.a = v;
  }
  fn or8(&mut self, from: Op8) {
    let tmp = self.r.a | self.get8(from);
    let v = self.set_flags((tmp, false));
    self.r.a = v;
  }
  fn cp8(&mut self, from: Op8) {
    let tmp = self.r.a.overflowing_sub(self.get8(from));
    self.set_flags(tmp);
  }

  fn not(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    self.set8(arg, !tmp);
  }

  fn inc16(&mut self, arg: Op16) {
    let v = self.get16(arg) + w(1);
    self.set16(arg, v);
  }
  fn dec16(&mut self, arg: Op16) {
    let v = self.get16(arg) - w(1);
    self.set16(arg, v);
  }
  fn inc8(&mut self, arg: Op8) {
    let tmp = self.get8(arg) + w(1);
    let v = self.set_flags((tmp, false));
    self.set8(arg, v);
  }
  fn dec8(&mut self, arg: Op8) {
    let tmp = self.get8(arg) - w(1);
    let v = self.set_flags((tmp, false));
    self.set8(arg, v);
  }

  fn rlc(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry = tmp >> 7;
    self.r.flags.set_c(carry != w(0));
    self.set8(arg, tmp << 1 | carry);
  }
  fn rrc(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry = tmp << 7;
    self.r.flags.set_c(carry != w(0));
    self.set8(arg, tmp >> 1 | carry);
  }
  fn rl(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry_flag = tmp >> 7;
    let carry = w(self.r.flags.c() as u8);
    self.r.flags.set_c(carry_flag != w(0));
    self.set8(arg, tmp << 1 | carry);
  }
  fn rr(&mut self, arg: Op8) {
    let tmp = self.get8(arg);
    let carry_flag = tmp << 7;
    let carry = w(self.r.flags.c() as u8);
    self.r.flags.set_c(carry_flag != w(0));
    self.set8(arg, tmp >> 1 | carry << 7);
  }

  fn jr(&mut self, f: Flag) {
    let label = self.get_imm8();
    if self.flag(f) {
      self.pc -= w(2); // get to start of opcode
      self.pc = self.pc_offset(label);
    }
  }
  fn jp(&mut self, f: Flag) {
    let label = self.get_imm16();
    if self.flag(f) {
      self.pc = label;
    }
  }
  fn call(&mut self, f: Flag) {
    let label = self.get_imm16();
    if self.flag(f) {
      let tmp = self.pc;
      self.push_imm16(tmp);
      self.pc = label;
    }
  }
  fn ret(&mut self, f: Flag) {
    if self.flag(f) {
      let ret_label = self.pop_imm16();
      self.pc = ret_label;
    }
  }

  fn ex_af(&mut self) {
    mem::swap(&mut self.r.a, &mut self.rp.a);
    mem::swap(&mut self.r.flags, &mut self.rp.flags);
  }

  fn ex_de_hl(&mut self) {
    mem::swap(&mut self.r.d, &mut self.r.h);
    mem::swap(&mut self.r.e, &mut self.r.l);
  }

  fn ex_spd_hl(&mut self) {
    let tmp = self.r.hl();
    self.r.set_hl(self.memory.read16(self.sp));
    self.memory.write16(self.sp, tmp);
  }

  fn exx(&mut self) {
    mem::swap(&mut self.r.b, &mut self.rp.b);
    mem::swap(&mut self.r.c, &mut self.rp.c);
    mem::swap(&mut self.r.d, &mut self.rp.d);
    mem::swap(&mut self.r.e, &mut self.rp.e);
    mem::swap(&mut self.r.h, &mut self.rp.h);
    mem::swap(&mut self.r.l, &mut self.rp.l);
  }

  fn halt(&mut self) {}
}
