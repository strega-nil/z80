use std::fmt::{self, Debug, Formatter};

use wrapping::{w, cvt, w8, w16};

pub struct Flags(pub u8);
impl Flags {
  pub fn new() -> Self { Flags(0) }

  pub fn s(&self) -> bool { self.0 >> 7 == 1 }
  pub fn z(&self) -> bool { self.0 >> 6 == 1 }
  #[allow(dead_code)]
  pub fn h(&self) -> bool { unimplemented!() /*self.0 >> 4 == 1*/ }
  #[allow(dead_code)]
  pub fn pv(&self) -> bool { unimplemented!() /*self.0 >> 2 == 1*/ }
  #[allow(dead_code)]
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
  #[allow(dead_code)]
  pub fn set_h(&mut self, _to: bool) {
    unimplemented!() /*self.0 |= (to as u8) << 4*/
  }
  #[allow(dead_code)]
  pub fn set_pv(&mut self, _to: bool) {
    unimplemented!() /*self.0 |= (to as u8) << 2*/
  }
  #[allow(dead_code)]
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
  pub fn new() -> Self {
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
}

