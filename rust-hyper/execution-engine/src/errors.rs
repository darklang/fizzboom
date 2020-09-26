use crate::{
  dval::{DType, Dval},
  runtime,
};
use std::fmt;

#[derive(Debug)]
pub enum Error<'a> {
  MissingFunction(runtime::FunctionDesc_),
  IncorrectArguments(runtime::FunctionDesc_, Vec<Dval<'a>>),
  InvalidType(Dval<'a>, DType<'a>),
}

impl<'a> fmt::Display for Error<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::MissingFunction(fun) => {
        write!(f, "Missing function: {}", fun)
      }
      Error::IncorrectArguments(fun, actuals) => write!(f,
               "Incorrect arguments calling {}, with {:?}",
               fun, actuals),
      Error::InvalidType(val, ty) => {
        write!(f, "Invalid type, expected {}, got {}", ty, val)
      }
    }
  }
}

impl<'a> std::error::Error for Error<'a> {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    // Some(self)
    None
  }
}
