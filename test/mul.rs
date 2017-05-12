use std::fs::File;
use std::io::Write;

static ROM: &[u8] = &[
  // data setup
  0x21, 0x00, 0x40,
  0x36, 0x10,
  0x23,
  0x36, 0x08,

  // multiplication
  // n1 in b
  // n2 in c
  // res in a
  0x21, 0x00, 0x40,
  0x46,
  0x23,
  0x4E,
  0xAF,

  0xB9,
  0x28, 0x0A, // test for zero c
  0xB8,
  0x28, 0x07, // test for zero b

  //start:
  0x80,
  0x0D,
  0x20, 0xFE,

  //end:
  0x76,
];

static FILENAME: &str = "mul.rom";

fn main() {
  let mut file = File::create(FILENAME).unwrap();
  file.write_all(ROM).unwrap();
  file.flush().unwrap();
}
