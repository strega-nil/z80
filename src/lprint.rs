use {Peripheral, Pins};
use std::io::{Write, Stdout, stdout};

pub struct LinePrinter {
  stdout: Stdout,
  port: u8,
}

impl LinePrinter {
  pub fn new(port: u8) -> Self {
    LinePrinter {
      stdout: stdout(),
      port,
    }
  }
}

impl Peripheral for LinePrinter {
  fn step(&mut self, pins: &mut Pins) {
    if pins.iorq && pins.address as u8 == self.port {
      debug!("Printing: ");
      self.stdout.write_all(&[pins.data]).unwrap();
      self.stdout.flush().unwrap();
    }
  }
}
