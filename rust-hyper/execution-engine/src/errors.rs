use crate::{
  dval::{DType, Dval},
  runtime,
};
use std::fmt;

#[derive(Debug)]
pub enum Error {
  MissingFunction(runtime::FunctionDesc_),
  IncorrectArguments(runtime::FunctionDesc_, Vec<Dval>),
  InvalidType(Dval, DType),
}

impl fmt::Display for Error {
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

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    Some(self)
  }
}
