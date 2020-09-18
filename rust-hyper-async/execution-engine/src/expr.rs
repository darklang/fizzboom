use std::sync::Arc;

use crate::runtime::*;
use ramp;

#[derive(Debug)]
pub enum Expr_ {
  Let {
    id:   ID,
    lhs:  String,
    rhs:  Expr,
    body: Expr,
  },
  FnCall {
    id:   ID,
    name: FunctionDesc_,
    args: im::Vector<Expr>,
  },
  Lambda {
    id:     ID,
    params: im::Vector<String>,
    body:   Expr,
  },
  BinOp {
    id:  ID,
    lhs: Expr,
    op:  FunctionDesc_,
    rhs: Expr,
  },
  If {
    id:        ID,
    cond:      Expr,
    then_body: Expr,
    else_body: Expr,
  },
  Variable {
    id:   ID,
    name: String,
  },
  IntLiteral {
    id:  ID,
    val: ramp::Int,
  },
  StringLiteral {
    id:  ID,
    val: String,
  },
  Blank {
    id: ID,
  },
}

pub type Expr = Arc<Expr_>;
unsafe impl Send for Expr_ {}
unsafe impl Sync for Expr_ {}

use Expr_::*;

pub fn elet(lhs: &str, rhs: Expr, body: Expr) -> Expr {
  Arc::new(Let { id: gid(),
                 lhs: lhs.to_string(),
                 rhs,
                 body })
}

pub fn estr(val: &str) -> Expr {
  Arc::new(StringLiteral { id:  gid(),
                           val: val.to_string(), })
}
pub fn eint(val: i64) -> Expr {
  Arc::new(IntLiteral { id:  gid(),
                        val: ramp::Int::from(val), })
}

pub fn evar(name: &str) -> Expr {
  Arc::new(Variable { id:   gid(),
                      name: name.to_string(), })
}

pub fn elambda(names: im::Vector<&str>, body: Expr) -> Expr {
  Arc::new(Lambda { id: gid(),
                    params: names.iter()
                                 .map(|n| n.to_string())
                                 .collect(),
                    body })
}

pub fn eif(cond: Expr, then_body: Expr, else_body: Expr) -> Expr {
  Arc::new(If { id: gid(),
                cond,
                then_body,
                else_body })
}

pub fn ebinop(lhs: Expr,
              module: &str,
              op: &str,
              version: u32,
              rhs: Expr)
              -> Expr {
  Arc::new(BinOp { id: gid(),
                   lhs,
                   op:
                     FunctionDesc_::FunctionDesc("dark".to_string(),
                                                 "stdlib".to_string(),
                                                 module.to_string(),
                                                 op.to_string(),
                                                 version),
                   rhs })
}

pub fn eblank() -> Expr {
  Arc::new(Blank { id: gid() })
}

pub fn efn(owner: &str,
           package: &str,
           module: &str,
           name: &str,
           version: u32,
           args: im::Vector<Expr>)
           -> Expr {
  Arc::new(FnCall { id: gid(),
                    name:
                      FunctionDesc_::FunctionDesc(owner.to_string(),
                                                  package.to_string(),
                                                  module.to_string(),
                                                  name.to_string(),
                                                  version),
                    args })
}

// Stdlib function
pub fn esfn(module: &str,
            name: &str,
            version: u32,
            args: im::Vector<Expr>)
            -> Expr {
  efn("dark", "stdlib", module, name, version, args)
}
