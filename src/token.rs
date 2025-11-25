use std::borrow::Cow;
use std::fmt::Display;

use crate::literal::Literal;
use crate::token_type::TokenType;

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'src> {
    pub token_type: TokenType,
    pub lexeme: Cow<'src, str>,
    pub line: usize,
    pub literal: Option<Literal>,
}

impl<'src> Token<'src> {
    pub fn new(
        token_type: TokenType,
        lexeme: &'src str,
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

    pub fn new_simple(token_type: TokenType, lexeme: &'src str, line: usize) -> Self {
        Token {
            token_type,
            lexeme: lexeme.into(),
            line,
            literal: None,
        }
    }

    pub fn new_number(value: f64, lexeme: &'src str, line: usize) -> Self {
        Token {
            token_type: TokenType::Number,
            lexeme: lexeme.into(),
            line,
            literal: Some(Literal::Number(value)),
        }
    }

    pub fn new_string(value: &'src str, lexeme: &'src str, line: usize) -> Self {
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
            lexeme: Cow::Borrowed(""),
            line,
            literal: None,
        }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.token_type, self.lexeme)
    }
}
