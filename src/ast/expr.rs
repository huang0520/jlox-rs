use crate::literal::Literal;
use crate::token::{BorrowedToken, OwnedToken, Token};

pub type BorrowedExpr<'src> = Expr<BorrowedToken<'src>>;
pub type OwnedExpr = Expr<OwnedToken>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<T: Token> {
    Assign {
        name: T,
        value: Box<Expr<T>>,
    },
    Binary {
        left: Box<Expr<T>>,
        operator: T,
        right: Box<Expr<T>>,
    },
    Call {
        callee: Box<Expr<T>>,
        paren: T,
        arguments: Vec<Expr<T>>,
    },
    Get {
        object: Box<Expr<T>>,
        name: T,
    },
    Grouping {
        expression: Box<Expr<T>>,
    },
    Literal {
        value: Literal,
    },
    Logical {
        left: Box<Expr<T>>,
        operator: T,
        right: Box<Expr<T>>,
    },
    Set {
        object: Box<Expr<T>>,
        name: T,
        value: Box<Expr<T>>,
    },
    Super {
        keyword: T,
        method: T,
    },
    This {
        keyword: T,
    },
    Unary {
        operator: T,
        right: Box<Expr<T>>,
    },
    Variable {
        name: T,
    },
}

impl<'src> From<BorrowedExpr<'src>> for OwnedExpr {
    fn from(expr: BorrowedExpr) -> Self {
        match expr {
            Expr::Assign { name, value } => OwnedExpr::Assign {
                name: name.into(),
                value: Box::new((*value).into()),
            },
            Expr::Binary {
                left,
                operator,
                right,
            } => OwnedExpr::Binary {
                left: Box::new((*left).into()),
                operator: operator.into(),
                right: Box::new((*right).into()),
            },
            Expr::Call {
                callee,
                paren,
                arguments,
            } => OwnedExpr::Call {
                callee: Box::new((*callee).into()),
                paren: paren.into(),
                arguments: arguments.into_iter().map(|arg| arg.into()).collect(),
            },
            Expr::Get { object, name } => OwnedExpr::Get {
                object: Box::new((*object).into()),
                name: name.into(),
            },
            Expr::Grouping { expression } => OwnedExpr::Grouping {
                expression: Box::new((*expression).into()),
            },
            Expr::Literal { value } => OwnedExpr::Literal { value },
            Expr::Logical {
                left,
                operator,
                right,
            } => OwnedExpr::Logical {
                left: Box::new((*left).into()),
                operator: operator.into(),
                right: Box::new((*right).into()),
            },
            Expr::Set {
                object,
                name,
                value,
            } => OwnedExpr::Set {
                object: Box::new((*object).into()),
                name: name.into(),
                value: Box::new((*value).into()),
            },
            Expr::Super { keyword, method } => OwnedExpr::Super {
                keyword: keyword.into(),
                method: method.into(),
            },
            Expr::This { keyword } => OwnedExpr::This {
                keyword: keyword.into(),
            },
            Expr::Unary { operator, right } => OwnedExpr::Unary {
                operator: operator.into(),
                right: Box::new((*right).into()),
            },
            Expr::Variable { name } => OwnedExpr::Variable { name: name.into() },
        }
    }
}
