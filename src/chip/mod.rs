// TODO(ubsan): p/v flag
// TODO(ubsan): h flag
// TODO(ubsan): n flag

mod regs;

use std::mem;

use wrapping::{w8, w16, w};
use Pins;

use self::regs::Regs;

#[derive(Copy, Clone, Debug)]
enum CompleteOp {
  Nop,
  Halt
}

#[derive(Copy, Clone, Debug)]
enum IncompleteOp {
  Complete(CompleteOp),
}

impl IncompleteOp {
  fn complete(self) -> Option<CompleteOp> {
    use self::IncompleteOp::*;
    match self {
      Complete(c) => Some(c),
    }
  }
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
  NeedMoreData(IncompleteOp),
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
        let op = self.decode_op(pins.data);
        if let Some(op) = op.complete() {
          self.run_op(pins, op);
          self.state = State::FetchInstruction;
        } else {
          panic!("");
        }
      }
      State::NeedMoreData(_) => { unreachable!() },
    }
  }

  fn decode_op(&self, data: u8) -> IncompleteOp {
    match data {
      0x00 => IncompleteOp::Complete(CompleteOp::Nop),
      0x76 => IncompleteOp::Complete(CompleteOp::Halt),
      n => panic!("unrecognized instruction: {:X}", n),
    }
  }

  fn run_op(&mut self, pins: &mut Pins, op: CompleteOp) {
    match op {
      CompleteOp::Nop => {},
      CompleteOp::Halt => {
        pins.halt = true;
      },
    }
  }
}
