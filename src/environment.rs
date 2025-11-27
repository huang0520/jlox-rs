use std::collections::HashMap;

use crate::{literal::Literal, token::Token};
use EnvironmentError as E;

#[derive(Debug, Default)]
pub struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn define(&mut self, name: &Token, value: Literal) {
        self.values.insert(name.lexeme.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Result<Literal, E> {
        self.values
            .get(name.lexeme.as_ref())
            .cloned()
            .ok_or(E::UndefinedVariable {
                name: name.lexeme.to_string(),
            })
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<(), E> {
        if let Some(v) = self.values.get_mut(name.lexeme.as_ref()) {
            *v = value;
            Ok(())
        } else {
            Err(E::UndefinedVariable {
                name: name.lexeme.to_string(),
            })
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum EnvironmentError {
    #[error("undefined variable '{name}'")]
    UndefinedVariable { name: String },
}
