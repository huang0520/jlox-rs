use crate::{
    expr::{Expr, RuntimeExpr},
    token::{RuntimeToken, Token},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'src> {
    Block(Vec<Stmt<'src>>),
    Break,
    Class {
        name: Token<'src>,
        superclass: Expr<'src>,
        methods: Vec<Stmt<'src>>,
    },
    Expression(Expr<'src>),
    Function {
        name: Token<'src>,
        parameters: Vec<Token<'src>>,
        body: Box<Stmt<'src>>,
    },
    If {
        condition: Expr<'src>,
        then_branch: Box<Stmt<'src>>,
        else_branch: Option<Box<Stmt<'src>>>,
    },
    Print(Expr<'src>),
    Return {
        keyword: Token<'src>,
        value: Expr<'src>,
    },
    Var {
        name: Token<'src>,
        initializer: Expr<'src>,
    },
    While {
        condition: Expr<'src>,
        body: Box<Stmt<'src>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum RuntimeStmt {
    Expression(RuntimeExpr),
    Print(RuntimeExpr),
    Var {
        name: RuntimeToken,
        initializer: RuntimeExpr,
    },
    Block(Vec<RuntimeStmt>),
    If {
        condition: RuntimeExpr,
        then_branch: Box<RuntimeStmt>,
        else_branch: Option<Box<RuntimeStmt>>,
    },
    While {
        condition: RuntimeExpr,
        body: Box<RuntimeStmt>,
    },
    Break,
    Function {
        name: RuntimeToken,
        parameters: Vec<RuntimeToken>,
        body: Box<RuntimeStmt>,
    },
    Class {
        name: RuntimeToken,
        superclass: RuntimeExpr,
        methods: Vec<RuntimeStmt>,
    },
    Return {
        keyword: RuntimeToken,
        value: RuntimeExpr,
    },
}

impl From<Stmt<'_>> for RuntimeStmt {
    fn from(stmt: Stmt<'_>) -> Self {
        match stmt {
            Stmt::Expression(expr) => RuntimeStmt::Expression(expr.into()),
            Stmt::Print(expr) => RuntimeStmt::Print(expr.into()),
            Stmt::Var { name, initializer } => RuntimeStmt::Var {
                name: name.into(),
                initializer: initializer.into(),
            },
            Stmt::Block(stmts) => RuntimeStmt::Block(stmts.into_iter().map(|s| s.into()).collect()),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => RuntimeStmt::If {
                condition: condition.into(),
                then_branch: Box::new((*then_branch).into()),
                else_branch: else_branch.map(|b| Box::new((*b).into())),
            },
            Stmt::While { condition, body } => RuntimeStmt::While {
                condition: condition.into(),
                body: Box::new((*body).into()),
            },
            Stmt::Break => RuntimeStmt::Break,
            Stmt::Function {
                name,
                parameters,
                body,
            } => RuntimeStmt::Function {
                name: name.into(),
                parameters: parameters.into_iter().map(|p| p.into()).collect(),
                body: Box::new((*body).into()),
            },
            Stmt::Class {
                name,
                superclass,
                methods,
            } => RuntimeStmt::Class {
                name: name.into(),
                superclass: superclass.into(),
                methods: methods.into_iter().map(|m| m.into()).collect(),
            },
            Stmt::Return { keyword, value } => RuntimeStmt::Return {
                keyword: keyword.into(),
                value: value.into(),
            },
        }
    }
}
