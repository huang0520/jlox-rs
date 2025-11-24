use std::fmt::Display;

use crate::token_type::{Literal, TokenType};

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub literal: Option<Literal>,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: impl Into<String>,
        line: usize,
        literal: Option<Literal>,
    ) -> Self {
        Token {
            token_type,
            lexeme: lexeme.into(),
            line,
            literal,
        }
    }

    pub fn new_simple(token_type: TokenType, lexeme: impl Into<String>, line: usize) -> Self {
        Token {
            token_type,
            lexeme: lexeme.into(),
            line,
            literal: None,
        }
    }

    pub fn new_number(value: f64, lexeme: impl Into<String>, line: usize) -> Self {
        Token {
            token_type: TokenType::Number,
            lexeme: lexeme.into(),
            line,
            literal: Some(Literal::Number(value)),
        }
    }

    pub fn new_string(value: impl Into<String>, lexeme: impl Into<String>, line: usize) -> Self {
        Token {
            token_type: TokenType::String,
            lexeme: lexeme.into(),
            line,
            literal: Some(Literal::String(value.into())),
        }
    }

    pub fn new_eof(line: usize) -> Self {
        Token {
            token_type: TokenType::Eof,
            lexeme: String::new(),
            line,
            literal: None,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.token_type, self.lexeme)
    }
}
