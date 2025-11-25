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
        match self {
            Literal::Boolean(false) | Literal::Nil => false,
            _ => true,
        }
    }
}

impl TryFrom<Literal> for f64 {
    type Error = LiteralError;
    fn try_from(lit: Literal) -> Result<Self, Self::Error> {
        match lit {
            Literal::Number(n) => Ok(n),
            _ => Err(LiteralError::ExpectedNumber(lit)),
        }
    }
}

impl TryFrom<Literal> for String {
    type Error = LiteralError;
    fn try_from(lit: Literal) -> Result<Self, Self::Error> {
        match lit {
            Literal::String(s) => Ok(s),
            _ => Err(LiteralError::ExpectedString(lit)),
        }
    }
}

impl TryFrom<Literal> for bool {
    type Error = LiteralError;
    fn try_from(lit: Literal) -> Result<Self, Self::Error> {
        match lit {
            Literal::Boolean(b) => Ok(b),
            _ => Err(LiteralError::ExpectedBoolean(lit)),
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum LiteralError {
    #[error("expected number, got {0}")]
    ExpectedNumber(Literal),
    #[error("expected string, got {0}")]
    ExpectedString(Literal),
    #[error("expected boolean, got {0}")]
    ExpectedBoolean(Literal),
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
