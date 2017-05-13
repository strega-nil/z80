extern crate clap;

#[macro_use]
mod macros;
mod wrapping;

mod chip;
mod memory;
mod lprint;

use clap::{App, Arg};
use chip::Chip;
use memory::Memory;
use lprint::LinePrinter;

// NOTE(ubsan): this emulator is *not* cycle-accurate

pub trait Peripheral {
  fn step(&mut self, pins: &mut Pins);
}

// NOTE(ubsan): bool(true) = HIGH, bool(false) = LOW
// NOTE(ubsan): clock is implicit, in calling "step"
// +5v and ground are also implicit, as they aren't necessary in software
// TODO(ubsan): make the booleans bitflags
pub struct Pins {
  // NOTE(ubsan): inout
  pub data: u8,

  // NOTE(ubsan): output
  pub address: u16,
  pub _m1: bool,
  pub mreq: bool,
  pub iorq: bool,
  pub rd: bool,
  pub wr: bool,
  pub _rfsh: bool,
  pub halt: bool,
  pub _busack: bool,

  // NOTE(ubsan): input
  pub _busreq: bool,
  pub _wait: bool,
  pub _int: bool,
  pub _nmi: bool,
  pub _reset: bool,
}

impl Pins {
  fn new() -> Self {
    Pins {
      address: 0,
      data: 0,
      _m1: false,
      mreq: false,
      iorq: false,
      rd: false,
      wr: false,
      _rfsh: false,
      halt: false,
      _busack: false,
      _busreq: false,
      _wait: false,
      _int: false,
      _nmi: false,
      _reset: false,
    }
  }
  fn zero_out(&mut self) {
    self.address = 0;
    self._m1= false;
    self.mreq= false;
    self.iorq= false;
    self.rd= false;
    self.wr= false;
    self._rfsh= false;
    self.halt= false;
    self._busack= false;
  }
}

struct Board<'a> {
  pins: Pins, // communication!
  chip: Chip,
  peripherals: Box<[&'a mut Peripheral]>,
}

impl<'a> Board<'a> {
  fn new(peripherals: Box<[&'a mut Peripheral]>) -> Self {
    Board {
      pins: Pins::new(),
      chip: Chip::new(),
      peripherals,
    }
  }

  fn step(&mut self) {
    self.chip.step(&mut self.pins);
    for p in &mut *self.peripherals {
      p.step(&mut self.pins);
    }
  }

  fn halted(&self) -> bool {
    self.pins.halt
  }
}

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
        .index(1)
      ).get_matches();
    let filename =
      matches.value_of("rom").expect("ROM to boot from required");
    let mut file = std::fs::File::open(filename).unwrap();
    let mut v = Vec::new();
    file.read_to_end(&mut v).unwrap();
    v
  };

  let mut memory = Memory::new(&rom);
  let mut lprint = LinePrinter::new(0);
  let mut board = Board::new(Box::new([&mut memory, &mut lprint]));
  while !board.halted() {
    board.step();
    ::std::thread::sleep(::std::time::Duration::from_millis(1));
  }
}

