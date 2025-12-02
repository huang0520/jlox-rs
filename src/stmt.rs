use crate::{expr::Expr, token::Token};

#[derive(Debug, PartialEq)]
pub enum Stmt<'src> {
    Expression(Expr<'src>),
    Print(Expr<'src>),
    Var {
        name: Token<'src>,
        initializer: Expr<'src>,
    },
    Block(Vec<Stmt<'src>>),
    If {
        condition: Expr<'src>,
        then_branch: Box<Stmt<'src>>,
        else_branch: Option<Box<Stmt<'src>>>,
    },
    While {
        condition: Expr<'src>,
        body: Box<Stmt<'src>>,
    },
}
