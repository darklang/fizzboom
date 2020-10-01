use crate::dval::Dval;
use im_rc as im;
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunctionDesc_<'a> {
  FunctionDesc(&'a str, &'a str, &'a str, &'a str, u32),
}

impl<'a> fmt::Display for FunctionDesc_<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let FunctionDesc_::FunctionDesc(owner,
                                    package,
                                    module,
                                    name,
                                    version) = self;
    write!(f,
           "{}/{}/{}::{}_v{}",
           owner, package, module, name, version)
  }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
pub enum Caller {
  Toplevel(TLID),
  Code(TLID, ID),
}

impl Caller {
  pub fn to_tlid(&self) -> TLID {
    match self {
      Caller::Toplevel(tlid) => *tlid,
      Caller::Code(tlid, _) => *tlid,
    }
  }
}

pub type FuncSig =
  Box<dyn for<'s, 'a> Fn(&'s crate::eval::ExecState, &[Dval<'a>]) -> Dval<'a>>;

pub type SymTable<'a> = im::HashMap<&'a str, Dval<'a>>;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
pub enum TLID {
  TLID(u64),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
pub enum ID {
  ID(u64),
}

pub fn gid() -> ID {
  ID::ID(rand::random())
}

pub fn gtlid() -> TLID {
  TLID::TLID(rand::random())
}

pub struct StdlibFunction {
  pub f: FuncSig,
}

impl fmt::Debug for StdlibFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("function")
  }
}

pub type StdlibDef<'a> =
  std::collections::HashMap<&'a FunctionDesc_<'a>, StdlibFunction>;

pub struct Environment<'a> {
  pub functions: StdlibDef<'a>,
}
