pub mod evaluate;

use crate::expr::Expr;

#[derive(Debug, PartialEq)]
pub enum Stmt<'src> {
    Expression(Expr<'src>),
    Print(Expr<'src>),
}
