#![allow(unused)]

extern crate clap;

#[macro_use]
mod macros;
mod wrapping;

mod chip;
mod memory;

use clap::{App, Arg};
use chip::Chip;
use memory::Memory;

// NOTE(ubsan): bool(true) = HIGH, bool(false) = LOW
// NOTE(ubsan): clock is implicit, in calling "step"
// +5v and ground are also implicit, as they aren't necessary in software
pub struct Pins {
  pub address: u16,
  pub data: u8,
  // TODO(ubsan): make these bitflags
  pub _m1: bool,
  pub mreq: bool,
  pub _iorq: bool,
  pub rd: bool,
  pub wr: bool,
  pub _rfsh: bool,
  pub halt: bool,
  pub _busack: bool,
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
      _iorq: false,
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
  fn zero(&mut self) {
    *self = Self::new();
  }
}

struct Board {
  pins: Pins, // communication!
  chip: Chip,
  memory: Memory,
}

impl Board {
  fn new(rom: &[u8]) -> Self {
    Board {
      pins: Pins::new(),
      memory: Memory::new(rom),
      chip: Chip::new(),
    }
  }

  fn step(&mut self) {
    self.chip.step(&mut self.pins);
    self.memory.step(&mut self.pins);
  }

  fn halted(&self) -> bool {
    self.pins.halt
  }
}

fn main() {
  use std::io::BufRead;
  /*
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
      matches.value_of("input").expect("ROM to boot from required");
    let mut file = std::fs::File::open(filename).unwrap();
    let mut v = Vec::new();
    file.read_to_end(&mut v).unwrap();
    v
  };
  */
  let rom = [32];

  let mut board = Board::new(&rom);
  let stdin = std::io::stdin();
  for _ in stdin.lock().lines() {
    if board.halted() {
      break;
    } else {
      board.step();
    }
  }
}

