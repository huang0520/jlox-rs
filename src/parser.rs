use std::fmt::Debug;
use std::iter::Peekable;

use crate::{expr::Expr, stmt::Stmt, token::Token, token_type::TokenType};

pub struct Parser<'src, I: Iterator<Item = Token<'src>>> {
    line: usize,
    tokens: Peekable<I>,
}

impl<'src, I: Iterator<Item = Token<'src>>> Parser<'src, I> {
    pub fn new(tokens: I) -> Self {
        Self {
            line: 1,
            tokens: tokens.peekable(),
        }
    }
    pub fn parse(&mut self) -> Result<Vec<Stmt<'src>>, Vec<ParseError>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while self.peek().is_some_and(|t| t.token_type != TokenType::Eof) {
            match self.statement() {
                Ok(stmt) => statements.push(*stmt),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    // Statement
    fn statement(&mut self) -> Result<Box<Stmt<'src>>, ParseError> {
        match self.peek().map(|t| t.token_type) {
            Some(TokenType::Print) => {
                self.advance();
                self.print_statement()
            }
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self) -> Result<Box<Stmt<'src>>, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, |line| ParseError::LackSemiColon {
            line,
        })?;
        Ok(Box::new(Stmt::Print(*expr)))
    }

    fn expression_statement(&mut self) -> Result<Box<Stmt<'src>>, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, |line| ParseError::LackSemiColon {
            line,
        })?;
        Ok(Box::new(Stmt::Expression(*expr)))
    }

    // Expression
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
        if let Some(TokenType::Bang | TokenType::Minus) = self.peek().map(|t| t.token_type) {
            let operator = self.advance().expect("operator exist").clone();
            let right = self.unary()?;
            Ok(Box::new(Expr::Unary { operator, right }))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Box<Expr<'src>>, ParseError> {
        let peek_token = self.peek();
        match peek_token.map(|t| t.token_type) {
            Some(
                TokenType::True
                | TokenType::False
                | TokenType::Nil
                | TokenType::Number
                | TokenType::String,
            ) => Ok(Box::new(Expr::Literal {
                value: self
                    .advance()
                    .expect("operator exist")
                    .literal
                    .expect("literal exist"),
            })),
            Some(TokenType::LeftParen) => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, |line| ParseError::LackRightParan {
                    line,
                })?;
                Ok(Box::new(Expr::Grouping { expression: expr }))
            }
            Some(_) => Err(ParseError::NotExpression {
                line: peek_token.expect("token exist").line,
            }),
            None => unreachable!("always a EOF token at the end"),
        }
    }

    fn sync(&mut self) {
        while let Some(t) = self.tokens.peek() {
            match t.token_type {
                TokenType::Semicolon => {
                    self.advance();
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
                    self.advance();
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

        while let Some(token) = self.peek() {
            if !operators.contains(&token.token_type) {
                break;
            }

            let operator = self.advance().expect("operator exist");
            let right = parse_operand(self)?;
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        Ok(expr)
    }

    fn peek(&mut self) -> Option<&Token<'src>> {
        self.tokens.peek()
    }

    fn advance(&mut self) -> Option<Token<'src>> {
        self.tokens.next().inspect(|t| self.line = t.line)
    }

    fn consume<F>(&mut self, match_type: TokenType, err_fn: F) -> Result<(), ParseError>
    where
        F: FnOnce(usize) -> ParseError,
    {
        match self.tokens.peek() {
            Some(t) if t.token_type == match_type => {
                self.advance();
                Ok(())
            }
            Some(_) => Err(err_fn(self.line)),
            None => unreachable!("always an EOF token"),
        }
    }
}

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ParseError {
    #[error("expect ';' after value")]
    LackSemiColon { line: usize },
    #[error("expect ')' after expression")]
    LackRightParan { line: usize },
    #[error("expect expression")]
    NotExpression { line: usize },
}
