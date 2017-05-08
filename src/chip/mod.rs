mod regs;
mod ops;

use std::mem;

use wrapping::{w8, w16, w};
use Pins;

use self::regs::Regs;
use self::ops::Op;

#[derive(Copy, Clone, Debug)]
enum ArgsNeeded {
  Zero,
  One,
  Two,
}

#[derive(Copy, Clone, Debug)]
enum State {
  FetchInstruction,
  ReceiveInstruction,
  NeedMoreData(Op, ArgsNeeded),
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
        let op = Op::decode(pins.data);
        match op.args_needed() {
          ArgsNeeded::Zero => {
            self.run_op(pins, op);
            self.state = State::FetchInstruction;
          },
          an => {
            pins.mreq = true;
            pins.rd = true;
            pins.address = self.pc.0;
            self.pc += w(1);
            self.state = State::NeedMoreData(op, an);
          }
        }
      }
      State::NeedMoreData(op, args_needed) => {
        debug!("state: receive data ({:02X})", pins.data);
        match op.args_needed() {
          ArgsNeeded::Zero => unreachable!(),
          ArgsNeeded::One => {
            self.run_op(pins, op);
            self.state = State::FetchInstruction;
          }
          ArgsNeeded::Two => {
            self.r.z = w(pins.data);
            pins.mreq = true;
            pins.rd = true;
            pins.address = self.pc.0;
            self.pc += w(1);
            self.state = State::NeedMoreData(op, ArgsNeeded::One);
          }
        }
      },
    }
  }

  fn run_op(&mut self, pins: &mut Pins, op: Op) {
    // NOTE(ubsan): how op arguments work:
    // ArgsNeeded::Zero - no arguments, trivial
    // ArgsNeeded::One - one argument at pins.data
    // ArgsNeeded::Two -
    //   two arguments; the first is at z, the second is at pins.data
    // numbering goes backwards for arguments
    let arg0 = w(pins.data);
    let arg1 = self.r.z;
    let _arg2 = self.r.w;

    match op {
      Op::Nop => {},
      Op::Halt => {
        debug!("register a: {:02X}", self.r.a);
        pins.halt = true;
      },
      Op::LdAImm => {
        self.r.a = arg0;
      },
      Op::LdBcImm => {
        self.r.b = arg0; // higher
        self.r.c = arg1; // lower
      }
      inst => panic!("unimplemented instruction: {:?}", inst),
    }
  }
}
