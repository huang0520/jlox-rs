pub mod print;

use crate::literal::Literal;
use crate::token::{RuntimeToken, Token};

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

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeExpr {
    Assign {
        name: RuntimeToken,
        value: Box<RuntimeExpr>,
    },
    Binary {
        left: Box<RuntimeExpr>,
        operator: RuntimeToken,
        right: Box<RuntimeExpr>,
    },
    Call {
        callee: Box<RuntimeExpr>,
        paren: RuntimeToken,
        arguments: Vec<RuntimeExpr>,
    },
    Get {
        object: Box<RuntimeExpr>,
        name: RuntimeToken,
    },
    Grouping {
        expression: Box<RuntimeExpr>,
    },
    Literal {
        value: Literal,
    },
    Logical {
        left: Box<RuntimeExpr>,
        operator: RuntimeToken,
        right: Box<RuntimeExpr>,
    },
    Set {
        object: Box<RuntimeExpr>,
        name: RuntimeToken,
        value: Box<RuntimeExpr>,
    },
    Super {
        keyword: RuntimeToken,
        method: RuntimeToken,
    },
    This {
        keyword: RuntimeToken,
    },
    Unary {
        operator: RuntimeToken,
        right: Box<RuntimeExpr>,
    },
    Variable {
        name: RuntimeToken,
    },
}

impl<'src> From<Expr<'src>> for RuntimeExpr {
    fn from(expr: Expr<'src>) -> Self {
        match expr {
            Expr::Assign { name, value } => RuntimeExpr::Assign {
                name: name.into(),
                value: Box::new((*value).into()),
            },
            Expr::Binary {
                left,
                operator,
                right,
            } => RuntimeExpr::Binary {
                left: Box::new((*left).into()),
                operator: operator.into(),
                right: Box::new((*right).into()),
            },
            Expr::Call {
                callee,
                paren,
                arguments,
            } => RuntimeExpr::Call {
                callee: Box::new((*callee).into()),
                paren: paren.into(),
                arguments: arguments.into_iter().map(|arg| arg.into()).collect(),
            },
            Expr::Get { object, name } => RuntimeExpr::Get {
                object: Box::new((*object).into()),
                name: name.into(),
            },
            Expr::Grouping { expression } => RuntimeExpr::Grouping {
                expression: Box::new((*expression).into()),
            },
            Expr::Literal { value } => RuntimeExpr::Literal { value },
            Expr::Logical {
                left,
                operator,
                right,
            } => RuntimeExpr::Logical {
                left: Box::new((*left).into()),
                operator: operator.into(),
                right: Box::new((*right).into()),
            },
            Expr::Set {
                object,
                name,
                value,
            } => RuntimeExpr::Set {
                object: Box::new((*object).into()),
                name: name.into(),
                value: Box::new((*value).into()),
            },
            Expr::Super { keyword, method } => RuntimeExpr::Super {
                keyword: keyword.into(),
                method: method.into(),
            },
            Expr::This { keyword } => RuntimeExpr::This {
                keyword: keyword.into(),
            },
            Expr::Unary { operator, right } => RuntimeExpr::Unary {
                operator: operator.into(),
                right: Box::new((*right).into()),
            },
            Expr::Variable { name } => RuntimeExpr::Variable { name: name.into() },
        }
    }
}
