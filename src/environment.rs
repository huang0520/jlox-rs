use std::collections::HashMap;

use crate::{literal::Literal, token::Token};
use EnvironmentError as E;

pub type EnvironmentIndex = usize;

#[derive(Debug)]
pub struct Environment {
    pub parent_index: Option<EnvironmentIndex>,
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new(parent_index: Option<EnvironmentIndex>) -> Self {
        Self {
            parent_index,
            values: HashMap::default(),
        }
    }

    pub fn define(&mut self, name: &Token, value: Literal) {
        self.values.insert(name.lexeme.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Result<Literal, E> {
        if let Some(value) = self.values.get(name.lexeme.as_ref()).cloned() {
            return Ok(value);
        }

        Err(E::UndefinedVariable {
            name: name.lexeme.to_string(),
        })
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<(), E> {
        if let Some(v) = self.values.get_mut(name.lexeme.as_ref()) {
            *v = value;
            return Ok(());
        };

        Err(E::UndefinedVariable {
            name: name.lexeme.to_string(),
        })
    }

    pub fn contains_name(&self, name: &Token) -> bool {
        self.values.contains_key(name.lexeme.as_ref())
    }
}

#[derive(Debug)]
pub struct Environments {
    environments: Vec<Environment>,
}

impl Environments {
    pub const GLOBAL_INDEX: EnvironmentIndex = 0;

    pub fn new() -> Self {
        let mut arena = Self {
            environments: Vec::new(),
        };
        arena.environments.push(Environment::new(None));
        arena
    }

    pub fn create_local(&mut self, enclosing_index: EnvironmentIndex) -> EnvironmentIndex {
        let new_index = self.environments.len();
        self.environments
            .push(Environment::new(Some(enclosing_index)));
        new_index
    }

    pub fn pop_local(&mut self) -> Option<Environment> {
        let total = self.environments.len();
        // Avoid pop global env
        self.environments.pop_if(|_| total > 1)
    }

    pub fn define_value(&mut self, name: &Token, value: Literal) {
        self.environments
            .last_mut()
            .expect("at least global exist")
            .define(name, value);
    }

    pub fn get_value(&self, name: &Token) -> Result<Literal, E> {
        let total = self.environments.len();
        let mut current = total.saturating_sub(1);
        loop {
            let env = self
                .environments
                .get(current)
                .expect("environment with index {current} not exist");
            if env.contains_name(name) {
                return env.get(name);
            }

            match env.parent_index {
                Some(enc) => current = enc,
                None => break,
            }
        }

        Err(E::UndefinedVariable {
            name: name.lexeme.to_string(),
        })
    }

    pub fn assign_value(&mut self, name: &Token, value: Literal) -> Result<(), E> {
        let total = self.environments.len();
        let mut current = total.saturating_sub(1);
        loop {
            let env = self
                .environments
                .get_mut(current)
                .expect("environment with index {current} not exist");
            if env.contains_name(name) {
                return env.assign(name, value);
            }

            match env.parent_index {
                Some(enc) => current = enc,
                None => break,
            }
        }

        Err(E::UndefinedVariable {
            name: name.lexeme.to_string(),
        })
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum EnvironmentError {
    #[error("undefined variable '{name}'")]
    UndefinedVariable { name: String },
}
