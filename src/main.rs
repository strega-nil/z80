// TODO(ubsan): p/v flag
// TODO(ubsan): h flag
// TODO(ubsan): n flag

#[allow(dead_code)]

extern crate clap;
mod wrapping;
mod processor;

use clap::{App, Arg};
use processor::Processor;

// CALLING CONVENTION
// first, u16s get passed in BC, DE, HL in that order
// then, u8s get passed in A, B, C, D, E, H, L in that order;
//   if a reg is already taken by a u16, then skip those regs
// if a certain kind has to overflow the parameter regs, it's passed on the
//   stack, backwards order
// i.e., (1: u8, 2: u16, 3: u8, 4: u8, 5: u16, 6: u16, 7: u16, 8: u16) is
//   2 = BC,  5 = DE, 6 = HL, 7 = (SP + 4), 8 = (SP + 2),
//     1 = A, 4 = (SP + 6), 3 = (SP + 7)
//     RET = (SP)
// u8 returns are in A, u16 returns are in BC
//   if it returns two u8s, return in BC
// Otherwise, on the stack, with the address in BC
//   (taking up the first u16 slot)
// primes are saved
// IX and IY are saved
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
           .index(1))
      .get_matches();
    let filename =
      matches.value_of("input").expect("ROM to boot from required");
    let mut file = std::fs::File::open(filename).unwrap();
    let mut v = Vec::new();
    file.read_to_end(&mut v).unwrap();
    v
  };

  Processor::new(&rom).run();
}

