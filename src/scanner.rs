use std::error::Error;
use std::fmt::Display;
use std::iter::Peekable;
use std::str::CharIndices;

use snafu::Snafu;

use crate::literal::Literal;
use crate::token::{BorrowedToken, TokenType};

#[derive(Debug)]
pub struct Scanner<'src> {
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
    line: usize,
    start: usize,
    current: usize,
}

impl<'src> Scanner<'src> {
    pub fn scan_tokens(source: &'src str) -> Result<Vec<BorrowedToken<'src>>, ScanErrors> {
        let mut tokens: Vec<BorrowedToken<'src>> = Vec::new();
        let mut errors = Vec::new();

        let mut scanner = Scanner {
            source,
            chars: source.char_indices().peekable(),
            line: 1,
            start: 0,
            current: 0,
        };

        loop {
            scanner.start = scanner.current;
            match scanner.advance() {
                Some(c) => match scanner.scan_token(c) {
                    Ok(Some(t)) => tokens.push(t),
                    Ok(None) => {}
                    Err(e) => errors.push(e),
                },
                None => break,
            }
        }

        if errors.is_empty() {
            tokens.push(BorrowedToken::new_eof(scanner.line));
            Ok(tokens)
        } else {
            Err(ScanErrors(errors))
        }
    }

    fn scan_token(&mut self, c: char) -> Result<Option<BorrowedToken<'src>>, ScanError> {
        match c {
            // Single-char token
            '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*' => {
                let token_type = match c {
                    '(' => TokenType::LeftParen,
                    ')' => TokenType::RightParen,
                    '{' => TokenType::LeftBrace,
                    '}' => TokenType::RightBrace,
                    ',' => TokenType::Comma,
                    '.' => TokenType::Dot,
                    '-' => TokenType::Minus,
                    '+' => TokenType::Plus,
                    ';' => TokenType::Semicolon,
                    '*' => TokenType::Star,
                    _ => unreachable!(),
                };
                Ok(Some(BorrowedToken::new_simple(
                    token_type,
                    self.current_lexeme(),
                    self.line,
                )))
            }

            // Double-char token
            '!' => Ok(Some(self.lookahead_token(
                '=',
                TokenType::BangEqual,
                TokenType::Bang,
            ))),
            '=' => Ok(Some(self.lookahead_token(
                '=',
                TokenType::EqualEqual,
                TokenType::Equal,
            ))),
            '>' => Ok(Some(self.lookahead_token(
                '=',
                TokenType::GreaterEqual,
                TokenType::Greater,
            ))),
            '<' => Ok(Some(self.lookahead_token(
                '=',
                TokenType::LessEqual,
                TokenType::Less,
            ))),
            '/' => {
                if let Some((_, nc)) = self.chars.next_if(|&(_, c)| matches!(c, '/' | '*')) {
                    self.advance();
                    if nc == '/' {
                        while self.chars.next_if(|&(_, c)| c != '\n').is_some() {}
                    } else {
                        self.scan_comment_block()?;
                    }
                    Ok(None)
                } else {
                    Ok(Some(BorrowedToken::new_simple(
                        TokenType::Slash,
                        self.current_lexeme(),
                        self.line,
                    )))
                }
            }

            // Whitespace
            ' ' | '\r' | '\t' => Ok(None),
            '\n' => {
                self.line += 1;
                Ok(None)
            }

            // State transitions
            '"' => Ok(Some(self.scan_string()?)),
            '0'..='9' => Ok(Some(self.scan_number())),
            'a'..='z' | 'A'..='Z' | '_' => Ok(Some(self.scan_identifier())),

            _ => Err(UnexpectedCharSnafu {
                found: c,
                line: self.line,
            }
            .build()),
        }
    }

    fn scan_comment_block(&mut self) -> Result<(), ScanError> {
        while let Some((_, c)) = self.chars.next() {
            if c == '*' && self.chars.peek().is_some_and(|&(_, c)| c == '/') {
                self.advance();
                return Ok(());
            }

            if c == '\n' {
                self.line += 1;
            }
        }
        Err(UnterminatedCommentBlockSnafu { line: self.line }.build())
    }

    fn scan_string(&mut self) -> Result<BorrowedToken<'src>, ScanError> {
        while self.peek().is_some() {
            let c = self.advance().expect("next char exist");
            match c {
                '"' => {
                    let lexeme = self.current_lexeme();
                    let value = &lexeme[1..lexeme.len() - 1];
                    return Ok(BorrowedToken::new_string(value, lexeme, self.line));
                }
                '\n' => self.line += 1,
                _ => {}
            }
        }
        Err(UnterminatedStringSnafu { line: self.line }.build())
    }

    fn scan_number(&mut self) -> BorrowedToken<'src> {
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Check decimal
        if self.peek() == Some('.') {
            // Check ahead
            let mut ahead_iter = self.chars.clone();
            ahead_iter.next();

            if ahead_iter.next().map(|(_, c)| c).is_some() {
                // Consume '.'
                self.advance();
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        let lexeme = self.current_lexeme();
        let value: f64 = lexeme.parse().expect("Should be able to parse");
        BorrowedToken::new_number(value, lexeme, self.line)
    }

    fn scan_identifier(&mut self) -> BorrowedToken<'src> {
        while self
            .peek()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            self.advance();
        }

        let lexeme = self.current_lexeme();
        let token_type = self.check_keyword(lexeme);
        match token_type {
            TokenType::True => {
                BorrowedToken::new(token_type, lexeme, self.line, Some(Literal::Boolean(true)))
            }
            TokenType::False => {
                BorrowedToken::new(token_type, lexeme, self.line, Some(Literal::Boolean(false)))
            }
            TokenType::Nil => BorrowedToken::new(token_type, lexeme, self.line, Some(Literal::Nil)),
            _ => BorrowedToken::new_simple(token_type, lexeme, self.line),
        }
    }

    // helper function
    fn advance(&mut self) -> Option<char> {
        let (idx, c) = self.chars.next()?;
        self.current = idx + c.len_utf8();
        Some(c)
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, c)| c)
    }

    fn lookahead_token(
        &mut self,
        expected: char,
        match_type: TokenType,
        default_type: TokenType,
    ) -> BorrowedToken<'src> {
        let token_type = if self.peek().is_some_and(|c| c == expected) {
            self.advance();
            match_type
        } else {
            default_type
        };
        BorrowedToken::new_simple(token_type, self.current_lexeme(), self.line)
    }

    fn current_lexeme(&self) -> &'src str {
        &self.source[self.start..self.current]
    }

    fn check_keyword(&self, lexeme: &str) -> TokenType {
        match lexeme {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            "break" => TokenType::Break,
            _ => TokenType::Identifier,
        }
    }
}

#[derive(Debug, Snafu)]
pub enum ScanError {
    #[snafu(display("line {line}: unexpected character '{found}'"))]
    UnexpectedChar { found: char, line: usize },

    #[snafu(display("line {line}: unterminated string"))]
    UnterminatedString { line: usize },

    #[snafu(display("line {line}: unterminated comment block"))]
    UnterminatedCommentBlock { line: usize },
}

/// Wrapper to aggregate mutiple parse error
#[derive(Debug)]
pub struct ScanErrors(Vec<ScanError>);

impl Error for ScanErrors {}

impl Display for ScanErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = self
            .0
            .iter()
            .map(|e| format!("  - {e}"))
            .collect::<Vec<_>>()
            .join("\n");
        write!(f, "{message}")
    }
}
