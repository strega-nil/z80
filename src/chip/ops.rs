use super::ArgsNeeded;

#[derive(Copy, Clone, Debug)]
pub(super) enum Conditional {
}

#[derive(Copy, Clone, Debug)]
pub(super) enum Reg8 {
  B,
  C,
  D,
  E,
  H,
  L,
  Hld,
  A,
}

impl Reg8 {
  fn arith_ld_arg(n: u8) -> (Self, Self) {
    (Self::single_arg((n & 0b111000) >> 3), Self::single_arg(n & 0b111))
  }

  fn single_arg(n: u8) -> Self {
    match n {
      0x0 => Reg8::B,
      0x1 => Reg8::C,
      0x2 => Reg8::D,
      0x3 => Reg8::E,
      0x4 => Reg8::H,
      0x5 => Reg8::L,
      0x6 => Reg8::Hld,
      0x7 => Reg8::A,
      _ => unreachable!(),
    }
  }
}

#[derive(Copy, Clone, Debug)]
pub(super) enum Reg16 {
  Bc,
  De,
  Hl,
  Sp,
}

enum WhichOp {
  First,
  Second,
}

impl Reg16 {
  fn op_arg(n: u8) -> (WhichOp, Self) {
    let r1 = match (n & 0b110000) >> 4 {
      0x0 => Reg16::Bc,
      0x1 => Reg16::De,
      0x2 => Reg16::Hl,
      0x3 => Reg16::Sp,
      _ => unreachable!(),
    };
    if n & 0b1000 == 0 {
      (WhichOp::First, r1)
    } else {
      (WhichOp::Second, r1)
    }
  }
}

#[derive(Copy, Clone, Debug)]
pub(super) enum Op {
  Nop,
  Halt,
  Ld16i(Reg16),
  AddHl(Reg16),

  Ld8I(Reg8),
  Ld8(Reg8, Reg8),

  Inc8(Reg8),
  Inc16(Reg16),
  Dec16(Reg16),
}

impl Op {
  fn misc_block(n: u8) -> Self {
    let (arg0, arg1) = ((n & 0b111000) >> 3, n & 0b111);
    match arg1 {
      0x0 => { // nop, ex af, and the jrs
        if arg0 == 0x0 {
          Op::Nop
        } else {
          unimplemented!()
        }
      },
      0x1 => { // 16-bit loads and adds
        let (op, arg) = Reg16::op_arg(n);
        match op {
          WhichOp::First => Op::Ld16i(arg),
          WhichOp::Second => Op::AddHl(arg),
        }
      },
      0x2 => { // deref loads
        unimplemented!()
      },
      0x3 => { // 16-bit incs and decs
        let (op, arg) = Reg16::op_arg(n);
        match op {
          WhichOp::First => Op::Inc16(arg),
          WhichOp::Second => Op::Dec16(arg),
        }
      },
      0x4 => { // 8-bit incs
        unimplemented!()
      },
      0x5 => { // 8-bit decs
        unimplemented!()
      },
      0x6 => { // immediate loads
        Op::Ld8I(Reg8::single_arg(arg0))
      },
      0x7 => { // misc
        unimplemented!()
      },
      _ => unreachable!(),
    }
  }
  fn load_block(n: u8) -> Self {
    match Reg8::arith_ld_arg(n) {
      (Reg8::Hld, Reg8::Hld) => Op::Halt,
      (a0, a1) => Op::Ld8(a0, a1),
    }
  }
  fn arith_block(n: u8) -> Self {
    panic!("unimplemented: arith ({:02X})", n)
  }
  fn jmp_block(n: u8) -> Self {
    unimplemented!()
  }

  pub(super) fn decode(n: u8) -> Self {
    match (n & 0b11000000) >> 6 {
      0x0 => Self::misc_block(n & 0b111111),
      0x1 => Self::load_block(n & 0b111111),
      0x2 => Self::arith_block(n & 0b111111),
      0x3 => Self::jmp_block(n & 0b111111),
      _ => unreachable!(),
    }
  }

  pub(super) fn args_needed(&self) -> ArgsNeeded {
    match *self {
      Op::Nop | Op::Halt => ArgsNeeded::Zero,

      Op::Ld8I(_) => ArgsNeeded::One,
      Op::Ld8(_, _) => ArgsNeeded::Zero,

      Op::Ld16i(_) => ArgsNeeded::Two,
      Op::Inc16(_) => ArgsNeeded::Zero,
      op => panic!("unimplemented inst: {:?}", op),
    }
  }
}

