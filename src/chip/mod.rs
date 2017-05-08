mod regs;

use std::mem;

use wrapping::{w8, w16, w};
use Pins;

use self::regs::Regs;


// NOTE(ubsan): these numbers are how many arguments the operands take
#[derive(Copy, Clone, Debug)]
enum OneArgOp {
  LdAImm,
}

#[derive(Copy, Clone, Debug)]
enum CompleteOp {
  Nop,
  Halt,
  Arg(OneArgOp, w8),
}

#[derive(Copy, Clone, Debug)]
enum IncompleteOp {
  Complete(CompleteOp),
  OneArg(OneArgOp),
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
    pins.zero_out();
    match self.state {
      State::FetchInstruction => {
        debug!("state: fetch instruction");
        pins.mreq = true;
        pins.rd = true;
        pins.address = self.pc.0;
        self.pc += w(1);
        self.state = State::ReceiveInstruction;
      },
      State::ReceiveInstruction => {
        debug!("state: receive instruction ({:02X})", pins.data);
        let op = self.decode_op(pins.data);
        match op {
          IncompleteOp::Complete(op) => {
            self.run_op(pins, op);
            self.state = State::FetchInstruction;
          },
          IncompleteOp::OneArg(_) => {
            pins.mreq = true;
            pins.rd = true;
            pins.address = self.pc.0;
            self.pc += w(1);
            self.state = State::NeedMoreData(op);
          }
        }
      }
      State::NeedMoreData(op) => {
        debug!("state: receive data ({:02X})", pins.data);
        match op {
          IncompleteOp::Complete(_) => unreachable!(),
          IncompleteOp::OneArg(op) => {
            let op = CompleteOp::Arg(op, w(pins.data));
            self.run_op(pins, op);
            self.state = State::FetchInstruction;
          }
        }
      },
    }
  }

  fn decode_op(&self, data: u8) -> IncompleteOp {
    match data {
      0x00 => IncompleteOp::Complete(CompleteOp::Nop),
      0x3E => IncompleteOp::OneArg(OneArgOp::LdAImm),
      0x76 => IncompleteOp::Complete(CompleteOp::Halt),
      n => panic!("unrecognized instruction: {:02X}", n),
    }
  }

  fn run_op(&mut self, pins: &mut Pins, op: CompleteOp) {
    match op {
      CompleteOp::Nop => {},
      CompleteOp::Halt => {
        debug!("register a: {:02X}", self.r.a);
        pins.halt = true;
      },
      CompleteOp::Arg(op, arg) => match op {
        OneArgOp::LdAImm => {
          self.r.a = arg;
        }
      },
    }
  }
}
