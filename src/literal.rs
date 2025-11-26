use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Literal {
    pub fn into_truthy(self) -> bool {
        !matches!(self, Literal::Boolean(false) | Literal::Nil)
    }
}

impl TryFrom<Literal> for f64 {
    type Error = TypeError;
    fn try_from(lit: Literal) -> Result<Self, Self::Error> {
        match lit {
            Literal::Number(n) => Ok(n),
            _ => Err(TypeError::Number(lit)),
        }
    }
}

impl TryFrom<Literal> for String {
    type Error = TypeError;
    fn try_from(lit: Literal) -> Result<Self, Self::Error> {
        match lit {
            Literal::String(s) => Ok(s),
            _ => Err(TypeError::String(lit)),
        }
    }
}

impl TryFrom<Literal> for bool {
    type Error = TypeError;
    fn try_from(lit: Literal) -> Result<Self, Self::Error> {
        match lit {
            Literal::Boolean(b) => Ok(b),
            _ => Err(TypeError::Boolean(lit)),
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum TypeError {
    #[error("expected number, got {0}")]
    Number(Literal),
    #[error("expected string, got {0}")]
    String(Literal),
    #[error("expected boolean, got {0}")]
    Boolean(Literal),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{n}"),
            Literal::String(s) => write!(f, "{s}"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}
