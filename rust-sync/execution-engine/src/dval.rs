use crate::expr;
use im_rc as im;
use std::{fmt, sync::Arc};

// use crate::{errors, expr};
use crate::{errors, runtime};
use ramp;

// These are types that aren't real values, but are used to hold other information
#[derive(Debug)]
pub enum Special {
  Error(runtime::Caller, errors::Error),
  Incomplete(runtime::Caller),
}

#[derive(Debug)]
pub enum Dval_ {
  DBool(bool),
  DInt(ramp::Int),
  DStr(String),
  DList(im::Vector<Dval>),
  DLambda(runtime::SymTable, im::Vector<String>, expr::Expr),
  DSpecial(Special),
}

impl Dval_ {
  pub fn is_special(&self) -> bool {
    matches!(self, Dval_::DSpecial(_))
  }

  pub fn to_json(&self) -> serde_json::Value {
    match self {
      Dval_::DBool(bool) => serde_json::Value::Bool(*bool),
      Dval_::DInt(i) => {
        if i.bit_length() > 53 {
          serde_json::Value::String(i.to_str_radix(10, false))
        } else {
          serde_json::Value::Number(serde_json::Number::from_f64(i.to_f64()).unwrap())
        }
      }
      Dval_::DStr(str) => serde_json::Value::String(str.clone()),
      Dval_::DList(l) => {
        serde_json::Value::Array(l.iter()
                                  .map(|dv| dv.to_json())
                                  .collect())
      }

      Dval_::DLambda(_, _, _) => serde_json::Value::Null,
      Dval_::DSpecial(_) => serde_json::Value::Null,
    }
  }
}

pub type Dval = Arc<Dval_>;

unsafe impl Send for Dval_ {}
unsafe impl Sync for Dval_ {}

#[derive(Debug)]
pub enum DType {
  TList(Arc<DType>),
  TLambda,
  TBool,
  NamedType(String),
}

impl fmt::Display for Dval_ {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!("{:?}", self))?;
    Ok(())
  }
}

impl fmt::Display for DType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!("{:?}", self))?;
    Ok(())
  }
}

pub fn derror(caller: &runtime::Caller,
              error: errors::Error)
              -> Dval {
  Arc::new(Dval_::DSpecial(Special::Error(*caller, error)))
}

pub fn dcode_error(caller: &runtime::Caller,
                   id: runtime::ID,
                   error: errors::Error)
                   -> Dval {
  Arc::new(Dval_::DSpecial(Special::Error(
        runtime::Caller::Code(caller.to_tlid(), id),
        error,
    )))
}

pub fn dincomplete(caller: &runtime::Caller) -> Dval {
  Arc::new(Dval_::DSpecial(Special::Incomplete(*caller)))
}

pub fn dbool(val: bool) -> Dval {
  Arc::new(Dval_::DBool(val))
}
pub fn dint(i: ramp::Int) -> Dval {
  Arc::new(Dval_::DInt(i))
}

pub fn dstr(val: &str) -> Dval {
  Arc::new(Dval_::DStr(val.to_string()))
}

pub fn dlist(l: im::Vector<Dval>) -> Dval {
  Arc::new(Dval_::DList(l))
}
