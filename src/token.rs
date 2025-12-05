mod token_type;

use std::fmt::Display;

use crate::literal::Literal;

pub use token_type::TokenType;

pub trait Token: Clone + PartialEq + Display {
    fn token_type(&self) -> &TokenType;
    fn lexeme(&self) -> &str;
    fn literal(&self) -> Option<&Literal>;
    fn line(&self) -> usize;
}

#[derive(Debug, PartialEq, Clone)]
pub struct BorrowedToken<'src> {
    token_type: TokenType,
    lexeme: &'src str,
    line: usize,
    literal: Option<Literal>,
}

impl<'src> BorrowedToken<'src> {
    pub fn new(
        token_type: TokenType,
        lexeme: &'src str,
        line: usize,
        literal: Option<Literal>,
    ) -> Self {
        BorrowedToken {
            token_type,
            lexeme,
            line,
            literal,
        }
    }

    pub fn new_simple(token_type: TokenType, lexeme: &'src str, line: usize) -> Self {
        BorrowedToken {
            token_type,
            lexeme,
            line,
            literal: None,
        }
    }

    pub fn new_number(value: f64, lexeme: &'src str, line: usize) -> Self {
        BorrowedToken {
            token_type: TokenType::Number,
            lexeme,
            line,
            literal: Some(Literal::Number(value)),
        }
    }

    pub fn new_string(value: &'src str, lexeme: &'src str, line: usize) -> Self {
        BorrowedToken {
            token_type: TokenType::String,
            lexeme,
            line,
            literal: Some(Literal::String(value.into())),
        }
    }

    pub fn new_eof(line: usize) -> Self {
        BorrowedToken {
            token_type: TokenType::Eof,
            lexeme: "",
            line,
            literal: None,
        }
    }
}

impl Token for BorrowedToken<'_> {
    fn token_type(&self) -> &TokenType {
        &self.token_type
    }
    fn lexeme(&self) -> &str {
        self.lexeme
    }
    fn literal(&self) -> Option<&Literal> {
        self.literal.as_ref()
    }
    fn line(&self) -> usize {
        self.line
    }
}

impl Display for BorrowedToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.token_type, self.lexeme)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct OwnedToken {
    token_type: TokenType,
    lexeme: String,
    line: usize,
    literal: Option<Literal>,
}

impl Token for OwnedToken {
    fn token_type(&self) -> &TokenType {
        &self.token_type
    }
    fn lexeme(&self) -> &str {
        &self.lexeme
    }
    fn literal(&self) -> Option<&Literal> {
        self.literal.as_ref()
    }
    fn line(&self) -> usize {
        self.line
    }
}

impl From<BorrowedToken<'_>> for OwnedToken {
    fn from(t: BorrowedToken<'_>) -> Self {
        Self {
            token_type: t.token_type,
            lexeme: t.lexeme.to_string(),
            line: t.line,
            literal: t.literal,
        }
    }
}

impl Display for OwnedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.token_type, self.lexeme)
    }
}
