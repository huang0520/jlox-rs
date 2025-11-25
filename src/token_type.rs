use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
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

impl From<Literal> for bool {
    fn from(lit: Literal) -> Self {
        !matches!(lit, Literal::Boolean(false) | Literal::Nil)
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum LiteralError {
    #[error("expected number, got {0}")]
    ExpectedNumber(Literal),
    #[error("expected string, got {0}")]
    ExpectedString(Literal),
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
