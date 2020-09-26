use crate::expr;

pub enum Op {
  ReplaceBlankWithEmptyString,
}

pub fn process(_initial: expr::Expr, _ops: Vec<Op>) -> expr::Expr {
  expr::eblank()
}
