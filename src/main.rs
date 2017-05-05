#![deny(warnings)]

extern crate clap;

mod wrapping;

mod chip;
mod memory;

use clap::{App, Arg};
use chip::Chip;
use memory::Memory;

// NOTE(ubsan): bool(true) = HIGH, bool(false) = LOW

struct Out {
  address: u16,
  data: u8,
  // TODO(ubsan): make these bitflags
  _m1: bool,
  _mreq: bool,
  _iorq: bool,
  _rd: bool,
  _wr: bool,
  _rfsh: bool,
  halt: bool,
  _busack: bool,
}

struct In {
  data: u8,
  // TODO(ubsan): make these bitflags
  // NOTE(ubsan): clock is implicit, in calling "step"
  // +5v and ground are also implicit, as they aren't necessary in software
  _busreq: bool,
  _wait: bool,
  _int: bool,
  _nmi: bool,
  _reset: bool,
}

struct Board {
  chip: Chip,
  memory: Memory,
}

impl Board {
  fn new(rom: &[u8]) -> Self {
    Board {
      memory: Memory::new(rom),
      chip: Chip::new(),
    }
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
      matches.value_of("input").expect("ROM to boot from required");
    let mut file = std::fs::File::open(filename).unwrap();
    let mut v = Vec::new();
    file.read_to_end(&mut v).unwrap();
    v
  };

  let _board = Board::new(&rom);
}

