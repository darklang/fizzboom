#![feature(trace_macros)]
#![feature(box_syntax)]
#![feature(log_syntax)]
#![feature(type_alias_impl_trait)]

#[macro_export]
macro_rules! ivec {
  () => (
      im::Vector::new()
  );
  ($($x:expr),+ $(,)?) => (
      im::Vector::from(<[_]>::into_vec(box [$($x),+]))
  );
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}

pub mod dval;
pub mod errors;
pub mod eval;
pub mod expr;
pub mod op;
pub mod runtime;
