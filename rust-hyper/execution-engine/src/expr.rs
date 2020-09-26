use im_rc as im;
use std::sync::Arc;

use crate::runtime::*;
use ramp;

#[derive(Debug)]
pub enum Expr_<'a> {
  Let {
    id:   ID,
    lhs:  &'a str,
    rhs:  Expr<'a>,
    body: Expr<'a>,
  },
  FnCall {
    id:   ID,
    name: FunctionDesc_,
    args: im::Vector<Expr<'a>>,
  },
  Lambda {
    id:     ID,
    params: im::Vector<&'a str>,
    body:   Expr<'a>,
  },
  BinOp {
    id:  ID,
    lhs: Expr<'a>,
    op:  FunctionDesc_,
    rhs: Expr<'a>,
  },
  If {
    id:        ID,
    cond:      Expr<'a>,
    then_body: Expr<'a>,
    else_body: Expr<'a>,
  },
  Variable {
    id:   ID,
    name: &'a str,
  },
  IntLiteral {
    id:  ID,
    val: ramp::Int,
  },
  StringLiteral {
    id:  ID,
    val: &'a str,
  },
  Blank {
    id: ID,
  },
}

pub type Expr<'a> = Arc<Expr_<'a>>;
unsafe impl<'a> Send for Expr_<'a> {}
unsafe impl<'a> Sync for Expr_<'a> {}

use Expr_::*;

pub fn elet<'a>(lhs: &'a str, rhs: Expr<'a>, body: Expr<'a>) -> Expr<'a> {
  Arc::new(Let { id: gid(),
                 lhs,
                 rhs,
                 body })
}

pub fn estr<'a>(val: &'a str) -> Expr<'a> {
  Arc::new(StringLiteral { id:  gid(),
                           val: val, })
}
pub fn eint<'a>(val: i64) -> Expr<'a> {
  Arc::new(IntLiteral { id:  gid(),
                        val: ramp::Int::from(val), })
}

pub fn evar<'a>(name: &'a str) -> Expr<'a> {
  Arc::new(Variable { id:   gid(),
                      name: name, })
}

pub fn elambda<'a>(names: im::Vector<&'a str>, body: Expr<'a>) -> Expr<'a> {
  Arc::new(Lambda { id: gid(),
                    params: names.iter()
                                 .map(|&n| n)
                                 .collect(),
                    body })
}

pub fn eif<'a>(cond: Expr<'a>, then_body: Expr<'a>, else_body: Expr<'a>) -> Expr<'a> {
  Arc::new(If { id: gid(),
                cond,
                then_body,
                else_body })
}

pub fn ebinop<'a>(lhs: Expr<'a>,
              module: &'a str,
              op: &'a str,
              version: u32,
              rhs: Expr<'a>)
              -> Expr<'a> {
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

pub fn eblank<'a>() -> Expr<'a> {
  Arc::new(Blank { id: gid() })
}

pub fn efn<'a>(owner: &'a str,
           package: &'a str,
           module: &'a str,
           name: &'a str,
           version: u32,
           args: im::Vector<Expr<'a>>)
           -> Expr<'a> {
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
pub fn esfn<'a>(module: &'a str,
            name: &'a str,
            version: u32,
            args: im::Vector<Expr<'a>>)
            -> Expr<'a> {
  efn("dark", "stdlib", module, name, version, args)
}
