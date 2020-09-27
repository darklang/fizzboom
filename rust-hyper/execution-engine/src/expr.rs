use std::borrow::Cow;

use crate::runtime::*;
use ramp;

#[derive(Debug)]
pub enum Expr<'a> {
  Let {
    id:   ID,
    lhs:  &'a str,
    rhs:  Box<Expr<'a>>,
    body: Box<Expr<'a>>,
  },
  FnCall {
    id:   ID,
    name: FunctionDesc_<'a>,
    args: Vec<Expr<'a>>,
  },
  Lambda {
    id:     ID,
    params: Cow<'a, [&'a str]>,
    body:   Box<Expr<'a>>,
  },
  BinOp {
    id:  ID,
    lhs: Box<Expr<'a>>,
    op:  FunctionDesc_<'a>,
    rhs: Box<Expr<'a>>,
  },
  If {
    id:        ID,
    cond:      Box<Expr<'a>>,
    then_body: Box<Expr<'a>>,
    else_body: Box<Expr<'a>>,
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

// pub type Expr<'a> = Arc<Expr_<'a>>;

use Expr::*;

pub fn elet<'a>(lhs: &'a str, rhs: Expr<'a>, body: Expr<'a>) -> Expr<'a> {
  Let { id: gid(),
                 lhs,
                 rhs: Box::new(rhs),
                 body: Box::new(body) }
}

pub fn estr<'a>(val: &'a str) -> Expr<'a> {
  StringLiteral { id:  gid(),
                           val: val, }
}
pub fn eint<'a>(val: i64) -> Expr<'a> {
  IntLiteral { id:  gid(),
                        val: ramp::Int::from(val), }
}

pub fn evar<'a>(name: &'a str) -> Expr<'a> {
  Variable { id:   gid(),
                      name: name, }
}

pub fn elambda<'a>(names: Cow<'a, [&'a str]>, body: Expr<'a>) -> Expr<'a> {
  Lambda { id: gid(),
                    params: names,
                    body: Box::new(body) }
}

pub fn eif<'a>(cond: Expr<'a>, then_body: Expr<'a>, else_body: Expr<'a>) -> Expr<'a> {
  If { id: gid(),
                cond: Box::new(cond),
                then_body: Box::new(then_body),
                else_body: Box::new(else_body) }
}

pub fn ebinop<'a>(lhs: Expr<'a>,
              module: &'a str,
              op: &'a str,
              version: u32,
              rhs: Expr<'a>)
              -> Expr<'a> {
  BinOp { id: gid(),
                   lhs: Box::new(lhs),
                   op:
                     FunctionDesc_::FunctionDesc("dark",
                                                 "stdlib",
                                                 module,
                                                 op,
                                                 version),
                   rhs: Box::new(rhs) }
}

pub fn eblank<'a>() -> Expr<'a> {
  Blank { id: gid() }
}

pub fn efn<'a>(owner: &'a str,
           package: &'a str,
           module: &'a str,
           name: &'a str,
           version: u32,
           args: Vec<Expr<'a>>)
           -> Expr<'a> {
  FnCall { id: gid(),
                    name:
                      FunctionDesc_::FunctionDesc(owner,
                                                  package,
                                                  module,
                                                  name,
                                                  version),
                    args }
}

// Stdlib function
pub fn esfn<'a>(module: &'a str,
            name: &'a str,
            version: u32,
            args: Vec<Expr<'a>>)
            -> Expr<'a> {
  efn("dark", "stdlib", module, name, version, args)
}
