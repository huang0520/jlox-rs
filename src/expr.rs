mod evaluate;
mod print;

use crate::literal::Literal;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'src> {
    Assign {
        name: Token<'src>,
        value: Box<Expr<'src>>,
    },
    Binary {
        left: Box<Expr<'src>>,
        operator: Token<'src>,
        right: Box<Expr<'src>>,
    },
    Call {
        callee: Box<Expr<'src>>,
        paren: Token<'src>,
        arguments: Vec<Expr<'src>>,
    },
    Get {
        object: Box<Expr<'src>>,
        name: Token<'src>,
    },
    Grouping {
        expression: Box<Expr<'src>>,
    },
    Literal {
        value: Literal,
    },
    Logical {
        left: Box<Expr<'src>>,
        operator: Token<'src>,
        right: Box<Expr<'src>>,
    },
    Set {
        object: Box<Expr<'src>>,
        name: Token<'src>,
        value: Box<Expr<'src>>,
    },
    Super {
        keyword: Token<'src>,
        method: Token<'src>,
    },
    This {
        keyword: Token<'src>,
    },
    Unary {
        operator: Token<'src>,
        right: Box<Expr<'src>>,
    },
    Variable {
        name: Token<'src>,
    },
}
