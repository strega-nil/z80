use std::fmt::{self, Debug, Formatter};

use wrapping::{w, cvt, w8, w16};

pub struct Flags(pub u8);
impl Flags {
  pub fn new() -> Self { Flags(0) }

  pub fn s(&self) -> bool  { self.0 & 0b1000_0000 != 0 }
  pub fn z(&self) -> bool  { self.0 & 0b0100_0000 != 0 }
  pub fn f5(&self) -> bool { self.0 & 0b0010_0000 != 0 }
  pub fn h(&self) -> bool  { self.0 & 0b0001_0000 != 0 }
  pub fn f3(&self) -> bool { self.0 & 0b0000_1000 != 0 }
  pub fn pv(&self) -> bool { self.0 & 0b0000_0100 != 0 }
  pub fn n(&self) -> bool  { self.0 & 0b0000_0010 != 0 }
  pub fn c(&self) -> bool  { self.0 & 0b0000_0001 != 0 }

  #[inline(always)]
  fn set_bit(&mut self, n: usize, to: bool) {
    self.0 |= (to as u8) << n;
    self.0 &= !((!to as u8) << n);
  }

  pub fn set_s(&mut self, to: bool)  { self.set_bit(7, to) }
  pub fn set_z(&mut self, to: bool)  { self.set_bit(6, to) }
  pub fn set_f5(&mut self, to: bool) { self.set_bit(5, to) }
  pub fn set_h(&mut self, to: bool)  { self.set_bit(4, to) }
  pub fn set_f3(&mut self, to: bool) { self.set_bit(3, to) }
  pub fn set_pv(&mut self, to: bool) { self.set_bit(2, to) }
  pub fn set_n(&mut self, to: bool)  { self.set_bit(1, to) }
  pub fn set_c(&mut self, to: bool)  { self.set_bit(0, to) }
}
impl Debug for Flags {
  fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
    f.debug_struct("ConditionCodes")
      .field("s", &self.s())
      .field("z", &self.z())
      .field("f5", &self.f5())
      .field("h", &self.h())
      .field("f3", &self.f3())
      .field("p/v", &self.pv())
      .field("n", &self.n())
      .field("c", &self.c())
      .finish()
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
  pub w: w8,
  pub z: w8,
  pub flags: Flags,
}

impl Regs {
  pub fn new() -> Self {
    Regs {
      a: w(0),
      b: w(0),
      c: w(0),
      d: w(0),
      e: w(0),
      h: w(0),
      l: w(0),
      w: w(0),
      z: w(0),
      flags: Flags::new(),
    }
  }

  pub fn hl(&self) -> w16 {
    (cvt(self.h) as w16) << 8 | (cvt(self.l) as w16)
  }
  pub fn set_hl(&mut self, to: w16) {
    self.h = cvt(to >> 8);
    self.l = cvt(to);
  }

  pub fn bc(&self) -> w16 {
    (cvt(self.b) as w16) << 8 | (cvt(self.c) as w16)
  }
  pub fn set_bc(&mut self, to: w16) {
    self.b = cvt(to >> 8);
    self.c = cvt(to);
  }

  pub fn de(&self) -> w16 {
    (cvt(self.d) as w16) << 8 | (cvt(self.e) as w16)
  }
  pub fn set_de(&mut self, to: w16) {
    self.d = cvt(to >> 8) as w8;
    self.e = cvt(to) as w8;
  }

  pub fn wz(&self) -> w16 {
    (cvt(self.w) as w16) << 8 | (cvt(self.z) as w16)
  }
  pub fn set_wz(&mut self, to: w16) {
    self.w = cvt(to >> 8) as w8;
    self.z = cvt(to) as w8;
  }
}

