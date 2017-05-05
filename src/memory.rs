use wrapping::{w, w8, w16};

enum Void { }
impl Void {
  #[inline(always)]
  unsafe fn unreachable() -> ! {
    match *(0x1 as *const Void) { }
  }
}

struct MemBox {
  rom: [u8; 0x4000],  // [0x0000, 0x3FFF]
  ram1: [u8; 0x4000], // [0x4000, 0x7FFF]
  ram2: [u8; 0x4000], // [0x8000, 0xAFFF]
  ram3: [u8; 0x4000], // [0xB000, 0xFFFF]
}

pub struct Memory(Box<MemBox>);

impl Memory {
  pub fn new(rom: &[u8]) -> Memory {
    use std::mem;
    let mut ret = unsafe {
      let v = vec![0; mem::size_of::<Memory>()];
      let b = v.into_boxed_slice();
      let ptr = Box::into_raw(b) as *mut u8 as *mut MemBox;
      Box::from_raw(ptr)
    };
    ret.rom[..rom.len()].copy_from_slice(rom);
    Memory(ret)
  }

  fn get_idx(n: w16) -> usize {
    (n.0 as usize) & (!0 >> 2)
  }
  fn banks(&self, bn: w16) -> &[u8; 0x4000] {
    match bn.0 >> 14 {
      0 => &self.0.rom,
      1 => &self.0.ram1,
      2 => &self.0.ram2,
      3 => &self.0.ram3,
      _ => unsafe { Void::unreachable() }, // unreachable
    }
  }
  fn banks_mut(&mut self, bn: w16) -> &mut [u8; 0x4000] {
    match bn.0 >> 14 {
      0 => panic!("can't write to r/o memory"),
      1 => &mut self.0.ram1,
      2 => &mut self.0.ram2,
      3 => &mut self.0.ram3,
      _ => unsafe { Void::unreachable() }, // unreachable
    }
  }

  pub fn read8(&self, idx: w16) -> w8 {
    w(self.banks(idx)[Self::get_idx(idx)])
  }
  pub fn write8(&mut self, idx: w16, n: w8) {
    self.banks_mut(idx)[Self::get_idx(idx)] = n.0;
  }
}

