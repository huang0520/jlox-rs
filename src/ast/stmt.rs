use crate::ast::expr::Expr;
use crate::token::{BorrowedToken, OwnedToken, Token};

pub type BorrowedStmt<'src> = Stmt<BorrowedToken<'src>>;
pub type OwnedStmt = Stmt<OwnedToken>;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<T: Token> {
    Block(Vec<Stmt<T>>),
    Break,
    Class {
        name: T,
        superclass: Expr<T>,
        methods: Vec<Stmt<T>>,
    },
    Expression(Expr<T>),
    Function {
        name: T,
        parameters: Vec<T>,
        body: Box<Stmt<T>>,
    },
    If {
        condition: Expr<T>,
        then_branch: Box<Stmt<T>>,
        else_branch: Option<Box<Stmt<T>>>,
    },
    Print(Expr<T>),
    Return {
        keyword: T,
        value: Expr<T>,
    },
    Var {
        name: T,
        initializer: Expr<T>,
    },
    While {
        condition: Expr<T>,
        body: Box<Stmt<T>>,
    },
}

impl From<BorrowedStmt<'_>> for OwnedStmt {
    fn from(stmt: BorrowedStmt<'_>) -> Self {
        match stmt {
            Stmt::Expression(expr) => OwnedStmt::Expression(expr.into()),
            Stmt::Print(expr) => OwnedStmt::Print(expr.into()),
            Stmt::Var { name, initializer } => OwnedStmt::Var {
                name: name.into(),
                initializer: initializer.into(),
            },
            Stmt::Block(stmts) => OwnedStmt::Block(stmts.into_iter().map(|s| s.into()).collect()),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => OwnedStmt::If {
                condition: condition.into(),
                then_branch: Box::new((*then_branch).into()),
                else_branch: else_branch.map(|b| Box::new((*b).into())),
            },
            Stmt::While { condition, body } => OwnedStmt::While {
                condition: condition.into(),
                body: Box::new((*body).into()),
            },
            Stmt::Break => OwnedStmt::Break,
            Stmt::Function {
                name,
                parameters,
                body,
            } => OwnedStmt::Function {
                name: name.into(),
                parameters: parameters.into_iter().map(|p| p.into()).collect(),
                body: Box::new((*body).into()),
            },
            Stmt::Class {
                name,
                superclass,
                methods,
            } => OwnedStmt::Class {
                name: name.into(),
                superclass: superclass.into(),
                methods: methods.into_iter().map(|m| m.into()).collect(),
            },
            Stmt::Return { keyword, value } => OwnedStmt::Return {
                keyword: keyword.into(),
                value: value.into(),
            },
        }
    }
}
