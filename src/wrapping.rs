#![allow(non_camel_case_types)]

use std::num::Wrapping;

pub type w8 = Wrapping<u8>;
pub type w16 = Wrapping<u16>;

pub trait As<T> {
  fn convert(self) -> T;
}

impl As<u8> for u16 {
  fn convert(self) -> u8 {
    self as u8
  }
}
impl As<u16> for u8 {
  fn convert(self) -> u16 {
    self as u16
  }
}

pub fn cvt<U, T>(w: Wrapping<T>) -> Wrapping<U>
  where T: As<U>
{
  Wrapping(w.0.convert())
}

pub fn w<T>(t: T) -> Wrapping<T> { Wrapping(t) }

pub trait Extensions: Sized {
  fn overflowing_add(self, other: Self) -> (Self, bool);
  fn overflowing_sub(self, other: Self) -> (Self, bool);
  fn overflowing_mul(self, other: Self) -> (Self, bool);
  fn overflowing_div(self, other: Self) -> (Self, bool);
  fn overflowing_rem(self, other: Self) -> (Self, bool);
  fn overflowing_neg(self) -> (Self, bool);
  fn count_ones(self) -> u32;
}

macro_rules! impl_we {
  ($($typ:ty),* $(,)*) => (
    $(impl Extensions for $typ {
      fn overflowing_add(self, other: Self) -> (Self, bool) {
        let (v, o) = self.0.overflowing_add(other.0);
        (w(v), o)
      }
      fn overflowing_sub(self, other: Self) -> (Self, bool) {
        let (v, o) = self.0.overflowing_sub(other.0);
        (w(v), o)
      }
      fn overflowing_mul(self, other: Self) -> (Self, bool) {
        let (v, o) = self.0.overflowing_mul(other.0);
        (w(v), o)
      }
      fn overflowing_div(self, other: Self) -> (Self, bool) {
        let (v, o) = self.0.overflowing_div(other.0);
        (w(v), o)
      }
      fn overflowing_rem(self, other: Self) -> (Self, bool) {
        let (v, o) = self.0.overflowing_rem(other.0);
        (w(v), o)
      }
      fn overflowing_neg(self) -> (Self, bool) {
        let (v, o) = self.0.overflowing_neg();
        (w(v), o)
      }
      fn count_ones(self) -> u32 {
        self.0.count_ones()
      }
    })*
  )
}

impl_we!{w8, w16}
