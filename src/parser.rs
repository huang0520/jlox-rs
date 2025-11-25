use std::iter::Peekable;

use crate::{expr::Expr, token::Token, token_type::TokenType};

pub struct Parser<'src, I: Iterator<Item = Token<'src>>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
}

impl<'src, I: Iterator<Item = Token<'src>>> Parser<'src, I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            errors: Vec::new(),
        }
    }
    pub fn parse(&mut self) -> Result<Expr<'src>, Vec<ParseError>> {
        while self
            .tokens
            .peek()
            .is_some_and(|t| t.token_type != TokenType::Eof)
        {
            match self.expression() {
                Ok(expr) => {
                    if self.errors.is_empty() {
                        return Ok(*expr);
                    }
                }
                Err(e) => {
                    self.errors.push(e);
                    self.sync();
                }
            }
        }
        if self.errors.is_empty() {
            Err(vec![ParseError::EmptyFile])
        } else {
            Err(self.errors.clone())
        }
    }

    fn expression(&mut self) -> Result<Box<Expr<'src>>, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Box<Expr<'src>>, ParseError> {
        self.parse_binary_op(
            |p| p.comparison(),
            &[TokenType::BangEqual, TokenType::EqualEqual],
        )
    }

    fn comparison(&mut self) -> Result<Box<Expr<'src>>, ParseError> {
        self.parse_binary_op(
            |p| p.term(),
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
        )
    }

    fn term(&mut self) -> Result<Box<Expr<'src>>, ParseError> {
        self.parse_binary_op(|p| p.factor(), &[TokenType::Minus, TokenType::Plus])
    }

    fn factor(&mut self) -> Result<Box<Expr<'src>>, ParseError> {
        self.parse_binary_op(|p| p.unary(), &[TokenType::Slash, TokenType::Star])
    }

    fn unary(&mut self) -> Result<Box<Expr<'src>>, ParseError> {
        if let Some(TokenType::Bang | TokenType::Minus) = self.tokens.peek().map(|t| t.token_type) {
            let operator = self.tokens.next().expect("operator exist").clone();
            let right = self.unary()?;
            Ok(Box::new(Expr::Unary { operator, right }))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Box<Expr<'src>>, ParseError> {
        let peek_token = self.tokens.peek();
        match peek_token.map(|t| t.token_type) {
            Some(
                TokenType::True
                | TokenType::False
                | TokenType::Nil
                | TokenType::Number
                | TokenType::String,
            ) => Ok(Box::new(Expr::Literal {
                value: self
                    .tokens
                    .next()
                    .expect("operator exist")
                    .literal
                    .expect("literal exist"),
            })),
            Some(TokenType::LeftParen) => {
                self.tokens.next();
                let expr = self.expression()?;
                match self.tokens.peek() {
                    Some(Token {
                        token_type: TokenType::RightParen,
                        ..
                    }) => {
                        self.tokens.next();
                        Ok(Box::new(Expr::Grouping { expression: expr }))
                    }
                    Some(t) => Err(ParseError::LackRightParan { line: t.line }),
                    None => unreachable!("shouldn't pass EOF token"),
                }
            }
            Some(_) => Err(ParseError::NotExpression {
                line: peek_token.expect("token exist").line,
            }),
            None => unreachable!("shouldn't pass EOF token"),
        }
    }

    fn sync(&mut self) {
        while let Some(t) = self.tokens.peek() {
            match t.token_type {
                TokenType::Semicolon => {
                    self.tokens.next();
                    break;
                }
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return
                | TokenType::Eof => {
                    break;
                }

                _ => {
                    self.tokens.next();
                }
            }
        }
    }

    // Helper function
    fn parse_binary_op<F>(
        &mut self,
        mut parse_operand: F,
        operators: &[TokenType],
    ) -> Result<Box<Expr<'src>>, ParseError>
    where
        F: FnMut(&mut Self) -> Result<Box<Expr<'src>>, ParseError>,
    {
        let mut expr = parse_operand(self)?;

        while let Some(token) = self.tokens.peek() {
            if !operators.contains(&token.token_type) {
                break;
            }

            let operator = self.tokens.next().expect("operator exist");
            let right = parse_operand(self)?;
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }

        Ok(expr)
    }
}

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ParseError {
    #[error("expect ')' after expression")]
    LackRightParan { line: usize },
    #[error("expect expression")]
    NotExpression { line: usize },
    #[error("empty file")]
    EmptyFile,
}
