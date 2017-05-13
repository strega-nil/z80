use {Peripheral, Pins};
use std::io::{Write, Stdout, stdout};

pub struct LinePrinter {
  stdout: Stdout,
  port: u8,
  column: u8,
}

impl LinePrinter {
  pub fn new(port: u8) -> Self {
    LinePrinter {
      stdout: stdout(),
      port,
      column: 0,
    }
  }
}

impl Peripheral for LinePrinter {
  fn step(&mut self, pins: &mut Pins) {
    if pins.iorq && pins.address as u8 == self.port {
      debug!("Printing: ");
      if pins.data == b'\n' {
        self.column = 0;
      } else {
        self.column += 1;
      }
      assert!(self.column < 80, "PRINTER ON FIRE!!!!");
      self.stdout.write_all(&[pins.data]).unwrap();
      self.stdout.flush().unwrap();
    }
  }
}
