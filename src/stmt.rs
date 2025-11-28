use crate::{expr::Expr, token::Token};

#[derive(Debug, PartialEq)]
pub enum Stmt<'src> {
    Expression(Expr<'src>),
    Print(Expr<'src>),
    Var {
        name: Token<'src>,
        initializer: Expr<'src>,
    },
}
