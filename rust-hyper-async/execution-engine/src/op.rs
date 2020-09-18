#[cfg(test)]
mod tests {
  #[test]
  fn can_create_hello_world() {
    // assert_eq!(process([Op::]), estr("hello_world!"));
  }
}

use crate::expr;

pub enum Op {
  ReplaceBlankWithEmptyString,
}

pub fn process(_initial: expr::Expr, _ops: Vec<Op>) -> expr::Expr {
  expr::eblank()
}
