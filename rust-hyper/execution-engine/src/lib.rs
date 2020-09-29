#![feature(trace_macros)]
#![feature(box_syntax)]
#![feature(log_syntax)]
#![feature(box_patterns)]

#[macro_export]
macro_rules! ivec {
  () => (
      im_rc::Vector::new()
  );
  ($($x:expr),+ $(,)?) => (
      std::borrow::Cow::Owned(im_rc::Vector::from(<[_]>::into_vec(box [$($x),+])))
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
