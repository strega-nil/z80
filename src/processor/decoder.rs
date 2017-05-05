use super::{Processor, Flag, Op8, Op16};

impl Processor {
  // returns true if the machine should continue
  // returns false if it shut off
  // TODO(ubsan): write this better
  pub fn step(&mut self) -> bool {
    let opcode = self.get_imm8();
    match opcode.0 {
      0x00 => {}                                // NOP
      0x01 => self.ld16(Op16::BC, Op16::Imm),   // LD BC, NN
      0x02 => self.ld8(Op8::BCd, Op8::A),       // LD (BC), A
      0x03 => self.inc16(Op16::BC),             // INC BC
      0x04 => self.inc8(Op8::B),                // INC B
      0x05 => self.dec8(Op8::B),                // DEC B
      0x06 => self.ld8(Op8::B, Op8::Imm),       // LD B, N
      0x07 => self.rlc(Op8::A),                 // RLCA
      0x08 => self.ex_af(),                     // EX AF, AF'
      0x09 => self.add16(Op16::HL, Op16::BC),   // ADD HL, BC
      0x0A => self.ld8(Op8::A, Op8::BCd),       // LD A, (BC)
      0x0B => self.dec16(Op16::BC),             // DEC BC
      0x0C => self.inc8(Op8::C),                // INC C
      0x0D => self.dec8(Op8::C),                // DEC C
      0x0E => self.ld8(Op8::C, Op8::Imm),       // LD C, N
      0x0F => self.rrc(Op8::A),                 // RRCA

      0x10 => self.jr(Flag::Djnz),              // DJNZ N
      0x11 => self.ld16(Op16::DE, Op16::Imm),   // LD DE, NN
      0x12 => self.ld8(Op8::DEd, Op8::A),       // LD (DE), A
      0x13 => self.inc16(Op16::DE),             // INC DE
      0x14 => self.inc8(Op8::D),                // INC D
      0x15 => self.dec8(Op8::D),                // DEC D
      0x16 => self.ld8(Op8::D, Op8::Imm),       // LD D, N
      0x17 => self.rl(Op8::A),                  // RLA
      0x18 => self.jr(Flag::Uncond),            // JR N
      0x19 => self.add16(Op16::HL, Op16::DE),   // ADD HL, DE
      0x1A => self.ld8(Op8::A, Op8::DEd),       // LD A, (DE)
      0x1B => self.dec16(Op16::DE),             // DEC DE
      0x1C => self.inc8(Op8::E),                // INC E
      0x1D => self.dec8(Op8::E),                // DEC E
      0x1E => self.ld8(Op8::E, Op8::Imm),       // LD E, N
      0x1F => self.rr(Op8::A),                  // RRA

      0x20 => self.jr(Flag::Nonzero),           // JR NZ, NN
      0x21 => self.ld16(Op16::HL, Op16::Imm),   // LD HL, NN
      0x22 => self.ld16(Op16::Immd, Op16::HL),  // LD (NN), HL
      0x23 => self.inc16(Op16::HL),             // INC HL
      0x24 => self.inc8(Op8::H),                // INC H
      0x25 => self.dec8(Op8::H),                // DEC H
      0x26 => self.ld8(Op8::H, Op8::Imm),       // LD H, N
      0x27 => self.unimplemented_inst(),        // DAA
      0x28 => self.jr(Flag::Zero),              // JR Z, N
      0x29 => self.add16(Op16::HL, Op16::HL),   // ADD HL, HL
      0x2A => self.ld16(Op16::HL, Op16::Immd),  // LD HL, (NN)
      0x2B => self.dec16(Op16::HL),             // DEC HL
      0x2C => self.inc8(Op8::L),                // INC L
      0x2D => self.dec8(Op8::L),                // DEC L
      0x2E => self.ld8(Op8::L, Op8::Imm),       // LD L, N
      0x2F => self.not(Op8::A),                 // CPL

      0x30 => self.jr(Flag::NoCarry),           // JR NC, N
      0x31 => self.ld16(Op16::SP, Op16::Imm),   // LD SP, NN
      0x32 => self.ld8(Op8::Immd, Op8::A),      // LD (NN), A
      0x33 => self.inc16(Op16::SP),             // INC SP
      0x34 => self.inc8(Op8::HLd),              // INC (HL)
      0x35 => self.dec8(Op8::HLd),              // DEC (HL)
      0x36 => self.ld8(Op8::HLd, Op8::Imm),     // LD (HL), N
      0x37 => self.r.flags.set_c(true),         // SCF
      0x38 => self.jr(Flag::Carry),             // JR C, N
      0x39 => self.add16(Op16::HL, Op16::SP),   // ADD HL, SP
      0x3A => self.ld8(Op8::A, Op8::Immd),      // LD A, (NN)
      0x3B => self.dec16(Op16::SP),             // DEC SP
      0x3C => self.inc8(Op8::A),                // INC A
      0x3D => self.dec8(Op8::A),                // DEC A
      0x3E => self.ld8(Op8::A, Op8::Imm),       // LD A, N
      0x3F => {                                 // CCF
        let c = self.r.flags.c();
        self.r.flags.set_c(!c);
      },

      0x40 => self.ld8(Op8::B, Op8::B),         // LD B, B
      0x41 => self.ld8(Op8::B, Op8::C),         // LD B, C
      0x42 => self.ld8(Op8::B, Op8::D),         // LD B, D
      0x43 => self.ld8(Op8::B, Op8::E),         // LD B, E
      0x44 => self.ld8(Op8::B, Op8::H),         // LD B, H
      0x45 => self.ld8(Op8::B, Op8::L),         // LD B, L
      0x46 => self.ld8(Op8::B, Op8::HLd),       // LD B, (HL)
      0x47 => self.ld8(Op8::B, Op8::A),         // LD B, A
      0x48 => self.ld8(Op8::C, Op8::B),         // LD C, B
      0x49 => self.ld8(Op8::C, Op8::C),         // LD C, C
      0x4A => self.ld8(Op8::C, Op8::D),         // LD C, D
      0x4B => self.ld8(Op8::C, Op8::E),         // LD C, E
      0x4C => self.ld8(Op8::C, Op8::H),         // LD C, H
      0x4D => self.ld8(Op8::C, Op8::L),         // LD C, L
      0x4E => self.ld8(Op8::C, Op8::HLd),       // LD C, (HL)
      0x4F => self.ld8(Op8::C, Op8::A),         // LD C, A

      0x50 => self.ld8(Op8::D, Op8::B),         // LD D, B
      0x51 => self.ld8(Op8::D, Op8::C),         // LD D, C
      0x52 => self.ld8(Op8::D, Op8::D),         // LD D, D
      0x53 => self.ld8(Op8::D, Op8::E),         // LD D, E
      0x54 => self.ld8(Op8::D, Op8::H),         // LD D, H
      0x55 => self.ld8(Op8::D, Op8::L),         // LD D, L
      0x56 => self.ld8(Op8::D, Op8::HLd),       // LD D, (HL)
      0x57 => self.ld8(Op8::D, Op8::A),         // LD D, A
      0x58 => self.ld8(Op8::E, Op8::B),         // LD E, B
      0x59 => self.ld8(Op8::E, Op8::C),         // LD E, C
      0x5A => self.ld8(Op8::E, Op8::D),         // LD E, D
      0x5B => self.ld8(Op8::E, Op8::E),         // LD E, E
      0x5C => self.ld8(Op8::E, Op8::H),         // LD E, H
      0x5D => self.ld8(Op8::E, Op8::L),         // LD E, L
      0x5E => self.ld8(Op8::E, Op8::HLd),       // LD E, (HL)
      0x5F => self.ld8(Op8::E, Op8::A),         // LD E, A

      0x60 => self.ld8(Op8::H, Op8::B),         // LD H, B
      0x61 => self.ld8(Op8::H, Op8::C),         // LD H, C
      0x62 => self.ld8(Op8::H, Op8::D),         // LD H, D
      0x63 => self.ld8(Op8::H, Op8::E),         // LD H, E
      0x64 => self.ld8(Op8::H, Op8::H),         // LD H, H
      0x65 => self.ld8(Op8::H, Op8::L),         // LD H, L
      0x66 => self.ld8(Op8::H, Op8::HLd),       // LD H, (HL)
      0x67 => self.ld8(Op8::H, Op8::A),         // LD H, A
      0x68 => self.ld8(Op8::L, Op8::B),         // LD L, B
      0x69 => self.ld8(Op8::L, Op8::C),         // LD L, C
      0x6A => self.ld8(Op8::L, Op8::D),         // LD L, D
      0x6B => self.ld8(Op8::L, Op8::E),         // LD L, E
      0x6C => self.ld8(Op8::L, Op8::H),         // LD L, H
      0x6D => self.ld8(Op8::L, Op8::L),         // LD L, L
      0x6E => self.ld8(Op8::L, Op8::HLd),       // LD L, (HL)
      0x6F => self.ld8(Op8::L, Op8::A),         // LD L, A

      0x70 => self.ld8(Op8::HLd, Op8::B),       // LD H, B
      0x71 => self.ld8(Op8::HLd, Op8::C),       // LD H, C
      0x72 => self.ld8(Op8::HLd, Op8::D),       // LD H, D
      0x73 => self.ld8(Op8::HLd, Op8::E),       // LD H, E
      0x74 => self.ld8(Op8::HLd, Op8::H),       // LD H, H
      0x75 => self.ld8(Op8::HLd, Op8::L),       // LD H, L
      0x76 => { self.halt(); return false }     // HALT
      0x77 => self.ld8(Op8::HLd, Op8::A),       // LD H, A
      0x78 => self.ld8(Op8::A, Op8::B),         // LD A, B
      0x79 => self.ld8(Op8::A, Op8::C),         // LD A, C
      0x7A => self.ld8(Op8::A, Op8::D),         // LD A, D
      0x7B => self.ld8(Op8::A, Op8::E),         // LD A, E
      0x7C => self.ld8(Op8::A, Op8::H),         // LD A, H
      0x7D => self.ld8(Op8::A, Op8::L),         // LD A, L
      0x7E => self.ld8(Op8::A, Op8::HLd),       // LD A, (HL)
      0x7F => self.ld8(Op8::A, Op8::A),         // LD A, A

      0x80 => self.add8(Op8::B),                // ADD A, B
      0x81 => self.add8(Op8::C),                // ADD A, B
      0x82 => self.add8(Op8::D),                // ADD A, B
      0x83 => self.add8(Op8::E),                // ADD A, B
      0x84 => self.add8(Op8::H),                // ADD A, B
      0x85 => self.add8(Op8::L),                // ADD A, B
      0x86 => self.add8(Op8::HLd),              // ADD A, B
      0x87 => self.add8(Op8::A),                // ADD A, B
      0x88 => self.adc8(Op8::B),                // ADC A, B
      0x89 => self.adc8(Op8::C),                // ADC A, B
      0x8A => self.adc8(Op8::D),                // ADC A, B
      0x8B => self.adc8(Op8::E),                // ADC A, B
      0x8C => self.adc8(Op8::H),                // ADC A, B
      0x8D => self.adc8(Op8::L),                // ADC A, B
      0x8E => self.adc8(Op8::HLd),              // ADC A, B
      0x8F => self.adc8(Op8::A),                // ADC A, B

      0x90 => self.sub8(Op8::B),                // SUB B
      0x91 => self.sub8(Op8::C),                // SUB C
      0x92 => self.sub8(Op8::D),                // SUB D
      0x93 => self.sub8(Op8::E),                // SUB E
      0x94 => self.sub8(Op8::H),                // SUB H
      0x95 => self.sub8(Op8::L),                // SUB L
      0x96 => self.sub8(Op8::HLd),              // SUB (HL)
      0x97 => self.sub8(Op8::A),                // SBC A
      0x98 => self.sbc8(Op8::B),                // SBC B
      0x99 => self.sbc8(Op8::C),                // SBC C
      0x9A => self.sbc8(Op8::D),                // SBC D
      0x9B => self.sbc8(Op8::E),                // SBC E
      0x9C => self.sbc8(Op8::H),                // SBC H
      0x9D => self.sbc8(Op8::L),                // SBC L
      0x9E => self.sbc8(Op8::HLd),              // SBC (HL)
      0x9F => self.sbc8(Op8::A),                // SBC A

      0xA0 => self.and8(Op8::B),                // AND B
      0xA1 => self.and8(Op8::C),                // AND C
      0xA2 => self.and8(Op8::D),                // AND D
      0xA3 => self.and8(Op8::E),                // AND E
      0xA4 => self.and8(Op8::H),                // AND H
      0xA5 => self.and8(Op8::L),                // AND L
      0xA6 => self.and8(Op8::HLd),              // AND (HL)
      0xA7 => self.and8(Op8::A),                // AND A
      0xA8 => self.xor8(Op8::B),                // XOR B
      0xA9 => self.xor8(Op8::C),                // XOR C
      0xAA => self.xor8(Op8::D),                // XOR D
      0xAB => self.xor8(Op8::E),                // XOR E
      0xAC => self.xor8(Op8::H),                // XOR H
      0xAD => self.xor8(Op8::L),                // XOR L
      0xAE => self.xor8(Op8::HLd),              // XOR (HL)
      0xAF => self.xor8(Op8::A),                // XOR A

      0xB0 => self.or8(Op8::B),                 // OR B
      0xB1 => self.or8(Op8::B),                 // OR C
      0xB2 => self.or8(Op8::B),                 // OR D
      0xB3 => self.or8(Op8::B),                 // OR E
      0xB4 => self.or8(Op8::B),                 // OR H
      0xB5 => self.or8(Op8::B),                 // OR L
      0xB6 => self.or8(Op8::B),                 // OR (HL)
      0xB7 => self.or8(Op8::B),                 // OR A
      0xB8 => self.cp8(Op8::B),                 // CP B
      0xB9 => self.cp8(Op8::C),                 // CP C
      0xBA => self.cp8(Op8::D),                 // CP D
      0xBB => self.cp8(Op8::E),                 // CP E
      0xBC => self.cp8(Op8::H),                 // CP H
      0xBD => self.cp8(Op8::L),                 // CP L
      0xBE => self.cp8(Op8::HLd),               // CP (HL)
      0xBF => self.cp8(Op8::A),                 // CP A

      0xC0 => self.ret(Flag::Nonzero),          // RET NZ
      0xC1 => self.pop16(Op16::BC),             // POP BC
      0xC2 => self.jp(Flag::Nonzero),           // JP NZ, NN
      0xC3 => self.jp(Flag::Uncond),            // JP NN
      0xC4 => self.call(Flag::Nonzero),         // CALL NZ, NN
      0xC5 => self.push16(Op16::BC),            // PUSH BC
      0xC6 => self.add8(Op8::Imm),              // ADD A, N
      0xC7 => self.unimplemented_inst(),        // RST 00h
      0xC8 => self.ret(Flag::Zero),             // RET Z
      0xC9 => self.ret(Flag::Uncond),           // RET
      0xCA => self.jp(Flag::Zero),              // JP Z, NN
      0xCB => self.unimplemented_inst(),
      0xCC => self.call(Flag::Zero),            // CALL Z, NN
      0xCD => self.call(Flag::Uncond),          // CALL, NN
      0xCE => self.adc8(Op8::Imm),              // ADC A, N
      0xCF => self.unimplemented_inst(),        // RST 08h

      0xD0 => self.ret(Flag::NoCarry),          // RET NC
      0xD1 => self.pop16(Op16::DE),             // POP DE
      0xD2 => self.jp(Flag::NoCarry),           // JP NC, NN
      0xD3 => self.unimplemented_inst(),        // OUT (N), A
      0xD4 => self.call(Flag::NoCarry),         // CALL NC, NN
      0xD5 => self.push16(Op16::DE),            // PUSH DE
      0xD6 => self.sub8(Op8::Imm),              // SUB N
      0xD7 => self.unimplemented_inst(),        // RST 10h
      0xD8 => self.ret(Flag::Carry),            // RET C
      0xD9 => self.exx(),                       // EXX
      0xDA => self.jp(Flag::Carry),             // JP C, NN
      0xDB => self.unimplemented_inst(),        // IN A, (N)
      0xDC => self.call(Flag::Carry),           // CALL C, NN
      0xDD => self.unimplemented_inst(),
      0xDE => self.sbc8(Op8::Imm),              // SBC A, N
      0xDF => self.unimplemented_inst(),        // RST 18h

      0xE0 => self.unimplemented_inst(),        // RET PO
      0xE1 => self.pop16(Op16::HL),             // POP HL
      0xE2 => self.unimplemented_inst(),       // JP PO, NN
      0xE3 => self.ex_spd_hl(),                 // EX (SP), HL
      0xE4 => self.unimplemented_inst(),        // CALL PO, NN
      0xE5 => self.push16(Op16::HL),            // PUSH HL
      0xE6 => self.and8(Op8::Imm),              // AND N
      0xE7 => self.unimplemented_inst(),        // RST 20h
      0xE8 => self.unimplemented_inst(),        // RET PE
      0xE9 => self.unimplemented_inst(),        // JP (HL)
      0xEA => self.unimplemented_inst(),        // JP PE, NN
      0xEB => self.ex_de_hl(),                  // EX DE, HL
      0xEC => self.unimplemented_inst(),        // CALL PE, NN
      0xED => self.unimplemented_inst(),
      0xEE => self.unimplemented_inst(),        // XOR N
      0xEF => self.unimplemented_inst(),        // RST 28h

      0xF0 => self.unimplemented_inst(),        // RET P
      0xF1 => self.pop16(Op16::AF),             // POP AF
      0xF2 => self.unimplemented_inst(),        // JP P, NN
      0xF3 => self.unimplemented_inst(),        // DI
      0xF4 => self.unimplemented_inst(),        // CALL P, NN
      0xF5 => self.push16(Op16::AF),            // PUSH AF
      0xF6 => self.or8(Op8::Imm),               // OR N
      0xF7 => self.unimplemented_inst(),        // RST 30h
      0xF8 => self.unimplemented_inst(),        // RET M
      0xF9 => self.ld16(Op16::SP, Op16::HL),    // LD SP, HL
      0xFA => self.unimplemented_inst(),        // JP M, NN
      0xFB => self.unimplemented_inst(),        // EI
      0xFC => self.unimplemented_inst(),        // CALL M, NN
      0xFD => self.unimplemented_inst(),
      0xFE => self.cp8(Op8::Imm),               // CP N
      0xFF => self.unimplemented_inst(),        // RST 38h
      _ => unreachable!(),
    }

    true
  }
}
