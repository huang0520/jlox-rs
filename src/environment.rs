use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use snafu::Snafu;

use crate::literal::Literal;

type Result<T> = std::result::Result<T, EnvironmentError>;

#[derive(Debug)]
struct Env {
    parent: Option<Environment>,
    values: HashMap<String, Literal>,
}

#[derive(Debug)]
pub struct Environment(Rc<RefCell<Env>>);

impl Environment {
    pub fn new(parent: Option<Self>) -> Self {
        Environment(Rc::new(RefCell::new(Env {
            parent,
            values: HashMap::new(),
        })))
    }

    pub fn define(&self, name: &str, value: Literal) {
        self.0.borrow_mut().values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Result<Literal> {
        let env = self.0.borrow();
        if let Some(value) = env.values.get(name) {
            return Ok(value.clone());
        }
        if let Some(parent) = &env.parent {
            return parent.get(name);
        }
        UndefinedVariableSnafu {
            name: name.to_string(),
        }
        .fail()
    }

    pub fn assign(&self, name: &str, value: Literal) -> Result<()> {
        let mut env = self.0.borrow_mut();
        if let Some(v) = env.values.get_mut(name) {
            *v = value;
            return Ok(());
        }
        if let Some(parent) = &env.parent {
            return parent.assign(name, value);
        }
        UndefinedVariableSnafu {
            name: name.to_string(),
        }
        .fail()
    }
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new(None)
    }
}

#[derive(Debug, Snafu)]
pub enum EnvironmentError {
    #[snafu(display("undefined variable '{name}'"))]
    UndefinedVariable { name: String },
}
