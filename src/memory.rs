use Pins;
use Peripheral;

struct MemBox {
  rom: [u8; 0x4000],  // [0x0000, 0x3FFF]
  ram1: [u8; 0x4000], // [0x4000, 0x7FFF]
  ram2: [u8; 0x4000], // [0x8000, 0xAFFF]
  ram3: [u8; 0x4000], // [0xB000, 0xFFFF]
}

pub struct Memory(Box<MemBox>);

impl Peripheral for Memory {
  fn step(&mut self, pins: &mut Pins) {
    if pins.mreq {
      if pins.rd {
        pins.data = self.read(pins.address);
      } else if pins.wr {
        self.write(pins.address, pins.data);
      }
    }
  }
}

impl Memory {
  pub fn new(rom: &[u8]) -> Memory {
    use std::mem;
    let mut ret = unsafe {
      let v = vec![0u8; mem::size_of::<MemBox>()];
      let b = v.into_boxed_slice();
      let ptr = Box::into_raw(b) as *mut u8 as *mut MemBox;
      Box::from_raw(ptr)
    };
    ret.rom[..rom.len()].copy_from_slice(rom);
    Memory(ret)
  }

  fn get_idx(n: u16) -> usize {
    (n as usize) & 0x3FFF
  }
  fn banks(&self, bn: u16) -> &[u8; 0x4000] {
    match bn >> 14 {
      0 => &self.0.rom,
      1 => &self.0.ram1,
      2 => &self.0.ram2,
      3 => &self.0.ram3,
      _ => unreachable!(),
    }
  }
  fn banks_mut(&mut self, bn: u16) -> &mut [u8; 0x4000] {
    match bn >> 14 {
      0 => panic!("can't write to r/o memory"),
      1 => &mut self.0.ram1,
      2 => &mut self.0.ram2,
      3 => &mut self.0.ram3,
      _ => unreachable!(),
    }
  }

  fn read(&self, idx: u16) -> u8 {
    self.banks(idx)[Self::get_idx(idx)]
  }
  fn write(&mut self, idx: u16, n: u8) {
    self.banks_mut(idx)[Self::get_idx(idx)] = n;
  }
}
