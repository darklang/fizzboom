use crate::expr;
use im_rc as im;
use std::{fmt};
use std::borrow::Cow;
use serde::ser::{Serialize, SerializeSeq};

// use crate::{errors, expr};
use crate::{errors, runtime};
use ramp;

// These are types that aren't real values, but are used to hold other information
#[derive(Debug, Clone)]
pub enum Special<'a> {
  Error(runtime::Caller, errors::Error<'a>),
  Incomplete(runtime::Caller),
}

#[derive(Debug, Clone)]
pub enum Dval<'a> {
  DBool(bool),
  DInt(ramp::Int),
  DStr(Cow<'a, str>),
  DList(im::Vector<Dval<'a>>),
  DLambda(runtime::SymTable<'a>, &'a Cow<'a, [&'a str]>, &'a expr::Expr<'a>),
  DSpecial(Box<Special<'a>>),
}

impl<'a> Serialize for Dval<'a> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
      use Dval::*;
      match self {
        DBool(v) => serializer.serialize_bool(*v),
        DInt(i) => {
          if i.bit_length() > 53 {
            serializer.serialize_str(&i.to_str_radix(10, false))
          } else {
            serializer.serialize_f64(i.to_f64())
          }
        },
        DStr(s) => serializer.serialize_str(&s),
        DList(lst) => {
          
          let mut seq = serializer.serialize_seq(Some(lst.len()))?;

          for elem in lst {
            seq.serialize_element(elem)?;
          }

          seq.end()
        },
        DLambda(..) | DSpecial(..) => serializer.serialize_none(),
      }
    }
}

impl<'a> Dval<'a> {
  pub fn is_special(&self) -> bool {
    matches!(self, Dval::DSpecial(_))
  }
}

// pub type Dval<'a> = Rc<Dval_<'a>>;

#[derive(Debug, Clone)]
pub enum DType<'a> {
  TList(&'a [DType<'a>]),
  TLambda,
  TBool,
  NamedType(&'a str),
}

impl<'a> fmt::Display for Dval<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!("{:?}", self))?;
    Ok(())
  }
}

impl<'a> fmt::Display for DType<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!("{:?}", self))?;
    Ok(())
  }
}

pub fn derror<'a>(caller: &runtime::Caller,
              error: errors::Error<'a>)
              -> Dval<'a> {
  Dval::DSpecial(Box::new(Special::Error(*caller, error)))
}

pub fn dcode_error<'a>(caller: &runtime::Caller,
                   id: runtime::ID,
                   error: errors::Error<'a>)
                   -> Dval<'a> {
  Dval::DSpecial(Box::new(Special::Error(
        runtime::Caller::Code(caller.to_tlid(), id),
        error,
    )))
}

pub fn dincomplete<'a>(caller: &runtime::Caller) -> Dval<'a> {
  Dval::DSpecial(Box::new(Special::Incomplete(*caller)))
}

pub fn dbool<'a>(val: bool) -> Dval<'a> {
  Dval::DBool(val)
}
pub fn dint<'a>(i: ramp::Int) -> Dval<'a> {
  Dval::DInt(i)
}

pub fn dstr<'a>(val: Cow<'a, str>) -> Dval<'a> {
  Dval::DStr(val)
}

pub fn dlist<'a>(l: im::Vector<Dval<'a>>) -> Dval<'a> {
  Dval::DList(l)
}
