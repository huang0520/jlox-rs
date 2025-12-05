mod callable;

use std::fmt::Display;

use snafu::Snafu;

pub use callable::{Callable, UserFunction};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Callable(Callable),
    Nil,
}

impl Literal {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Literal::Boolean(false) | Literal::Nil)
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Literal::Nil)
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            Literal::Number(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            Literal::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Literal::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_callable(&self) -> Option<&Callable> {
        match self {
            Literal::Callable(c) => Some(c),
            _ => None,
        }
    }
}

macro_rules! impl_try_from {
    ($target_type:ty => $variant:ident($field_type:ty), $snafu:ident) => {
        impl TryFrom<Literal> for $target_type {
            type Error = TypeError;
            fn try_from(lit: Literal) -> Result<Self, Self::Error> {
                match lit {
                    Literal::$variant(v) => Ok(v),
                    _ => $snafu {
                        found: lit.to_string(),
                    }
                    .fail(),
                }
            }
        }
    };
}

impl_try_from!(f64 => Number(f64), NumberSnafu);
impl_try_from!(String => String(String), StringSnafu);
impl_try_from!(bool => Boolean(bool), BooleanSnafu);

#[derive(Debug, Snafu)]
pub enum TypeError {
    #[snafu(display("expected number, found {found}"))]
    Number { found: String },
    #[snafu(display("expected string, found {found}"))]
    String { found: String },
    #[snafu(display("expected boolean, found {found}"))]
    Boolean { found: String },
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => {
                let s = n.to_string();
                match s.strip_suffix(".0") {
                    Some(s) => write!(f, "{s}"),
                    None => write!(f, "{n}"),
                }
            }
            Literal::String(s) => write!(f, "{s:?}"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Callable(c) => write!(f, "{c}"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}
