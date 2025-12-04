use std::{fmt::Display, iter::zip};

use crate::{
    environment::Environment,
    interpreter::{Lox, RuntimeError, StmtResult},
    literal::Literal,
    stmt::RuntimeStmt,
};

#[derive(Debug, Clone)]
pub enum Callable {
    Native {
        name: &'static str,
        arity: usize,
        function: fn(&Lox, &[Literal]) -> Literal,
    },
    User(UserFunction),
}

#[derive(Debug, Clone)]
pub struct UserFunction {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<RuntimeStmt>,
}

impl Callable {
    pub fn call(&self, interpreter: &Lox, arguments: &[Literal]) -> Result<Literal, RuntimeError> {
        match self {
            Self::Native { function, .. } => Ok(function(interpreter, arguments)),
            Self::User(func) => {
                let local = Environment::new(Some(interpreter.global.clone()));

                for (param, arg) in zip(&func.parameters, arguments) {
                    local.define(&param, arg.clone());
                }

                match interpreter.evaluate_block(&func.body, &local)? {
                    StmtResult::Return(value) => Ok(value),
                    StmtResult::Normal => Ok(Literal::Nil),
                    StmtResult::Break => {
                        unreachable!("break should not exist outside of loop scope")
                    }
                }
            }
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Self::Native { arity, .. } => *arity,
            Self::User(func) => func.parameters.len(),
        }
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Callable::Native { name, .. } => write!(f, "<native fn {name}>"),
            Callable::User(func) => write!(f, "<fn {}>", func.name),
        }
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Callable::Native { name: n1, .. }, Callable::Native { name: n2, .. }) => n1 == n2,
            _ => false,
        }
    }
}
