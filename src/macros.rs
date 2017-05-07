macro_rules! debug {
  ($($args:expr),*$(,)*) => (
    println!($($args),*);
  )
}
