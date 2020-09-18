use crate::dval::Dval;
use std::{fmt, sync::Arc};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunctionDesc_ {
  FunctionDesc(String, String, String, String, u32),
}

impl fmt::Display for FunctionDesc_ {
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
  Arc<dyn Fn(&crate::eval::ExecState, Vec<Dval>) -> Dval
        + Send
        + Sync>;

pub type SymTable = im::HashMap<String, Dval>;

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

pub type StdlibDef =
  std::collections::HashMap<FunctionDesc_, StdlibFunction>;

pub struct Environment {
  pub functions: StdlibDef,
}

unsafe impl Send for Environment {}
