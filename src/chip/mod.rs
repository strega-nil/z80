// TODO(ubsan): p/v flag
// TODO(ubsan): h flag
// TODO(ubsan): n flag

mod regs;

use std::mem;

use wrapping::{w8, w16, w};
use Pins;

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

enum State {
  FetchInstruction,
  ReceiveInstruction,
}

pub struct Chip {
  r: Regs,
  rp: Regs, // r'
  sp: w16,
  pc: w16,
  state: State,
}

impl Chip {
  pub fn new() -> Self {
    Chip {
      r: Regs::new(),
      rp: Regs::new(),
      sp: w(0),
      pc: w(0),
      state: State::FetchInstruction,
    }
  }

  pub fn step(&mut self, pins: &mut Pins) {
    match self.state {
      State::FetchInstruction => {
        debug!("state: fetch instruction");
        pins.zero();
        pins.mreq = true;
        pins.rd = true;
        pins.address = self.pc.0;
        self.pc += w(1);
        self.state = State::ReceiveInstruction;
      },
      State::ReceiveInstruction => {
        debug!("state: receive instruction ({:02X})", pins.data);
        panic!("");
      }
    }
  }
}
