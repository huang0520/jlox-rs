use std::fmt::Debug;
use std::iter::Peekable;

use crate::{expr::Expr, literal::Literal, stmt::Stmt, token::Token, token_type::TokenType};
use ParseError as E;

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub enum REPLResult<'src> {
    Stmt(Stmt<'src>),
    Expr(Expr<'src>),
}

#[derive(Debug)]
pub struct Parser<'src, I: Iterator<Item = Token<'src>>> {
    tokens: Peekable<I>,
    line: usize,
}

impl<'src, I: Iterator<Item = Token<'src>>> Parser<'src, I> {
    pub fn parse(tokens: I) -> (Vec<Stmt<'src>>, Vec<ParseError>) {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        let mut parser = Self {
            tokens: tokens.peekable(),
            line: 1,
        };

        while parser.peek().token_type != TokenType::Eof {
            match parser.declaration() {
                Ok(stmt) => statements.push(*stmt),
                Err(e) => {
                    errors.push(e);
                    parser.sync();
                }
            }
        }

        (statements, errors)
    }

    pub fn parse_repl(tokens: I) -> (Vec<REPLResult<'src>>, Vec<ParseError>) {
        let mut results = Vec::new();
        let mut errors = Vec::new();
        let mut parser = Self {
            tokens: tokens.peekable(),
            line: 1,
        };

        while parser.peek().token_type != TokenType::Eof {
            let result = match parser.peek().token_type {
                // Check is statement
                TokenType::Var | TokenType::Print | TokenType::LeftBrace => {
                    parser.declaration().map(|stmt| REPLResult::Stmt(*stmt))
                }
                _ => parser.expression().and_then(|expr| {
                    if parser.next_if(TokenType::Semicolon)?.is_some() {
                        Ok(REPLResult::Stmt(Stmt::Expression(*expr)))
                    } else if parser.peek().token_type == TokenType::Eof {
                        Ok(REPLResult::Expr(*expr))
                    } else {
                        Err(ParseError::LackSemiColon {
                            line: parser.peek().line,
                            after: "value",
                        })
                    }
                }),
            };

            match result {
                Ok(r) => results.push(r),
                Err(e) => {
                    errors.push(e);
                    parser.sync();
                }
            }
        }

        (results, errors)
    }

    // Declaration
    fn declaration(&mut self) -> Result<Box<Stmt<'src>>> {
        if self.next_if(TokenType::Var)?.is_some() {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Box<Stmt<'src>>> {
        let name = self.expect_next(TokenType::Identifier, E::NotIdentifier { line: self.line })?;
        let initializer = if self.next_if(TokenType::Equal)?.is_some() {
            self.expression()?
        } else {
            Box::new(Expr::Literal {
                value: Literal::Nil,
            })
        };
        let _ = self.expect_next(
            TokenType::Semicolon,
            E::LackSemiColon {
                line: self.line,
                after: "variable declaration",
            },
        )?;
        Ok(Box::new(Stmt::Var {
            name,
            initializer: *initializer,
        }))
    }

    // Statement
    fn statement(&mut self) -> Result<Box<Stmt<'src>>> {
        match self.peek().token_type {
            TokenType::Print => {
                self.next_token();
                self.print_statement()
            }
            TokenType::LeftBrace => {
                self.next_token();
                self.block_statement()
            }
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        let expr = self.expression()?;
        self.expect_next(
            TokenType::Semicolon,
            E::LackSemiColon {
                line: self.line,
                after: "value",
            },
        )?;
        Ok(Box::new(Stmt::Print(*expr)))
    }

    fn block_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        let mut statements: Vec<Stmt<'_>> = Vec::new();
        while !matches!(
            self.peek().token_type,
            TokenType::RightBrace | TokenType::Eof
        ) {
            statements.push(*self.declaration()?);
        }

        self.expect_next(TokenType::RightBrace, E::LackRightBrace { line: self.line })?;
        Ok(Box::new(Stmt::Block(statements)))
    }

    fn expression_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        let expr = self.expression()?;
        self.expect_next(
            TokenType::Semicolon,
            E::LackSemiColon {
                line: self.line,
                after: "value",
            },
        )?;
        Ok(Box::new(Stmt::Expression(*expr)))
    }

    // Expression
    fn expression(&mut self) -> Result<Box<Expr<'src>>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Box<Expr<'src>>> {
        let expr = self.equality()?;

        // If next token is '=' -> Assign value
        if self.next_if(TokenType::Equal)?.is_some() {
            let value = self.assignment()?;

            if let Expr::Variable { name } = expr.as_ref() {
                Ok(Box::new(Expr::Assign {
                    name: name.clone(),
                    value,
                }))
            } else {
                Err(E::InvalidAssignment { line: self.line })
            }
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self) -> Result<Box<Expr<'src>>> {
        self.parse_binary_op(
            |p| p.comparison(),
            &[TokenType::BangEqual, TokenType::EqualEqual],
        )
    }

    fn comparison(&mut self) -> Result<Box<Expr<'src>>> {
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

    fn term(&mut self) -> Result<Box<Expr<'src>>> {
        self.parse_binary_op(|p| p.factor(), &[TokenType::Minus, TokenType::Plus])
    }

    fn factor(&mut self) -> Result<Box<Expr<'src>>> {
        self.parse_binary_op(|p| p.unary(), &[TokenType::Slash, TokenType::Star])
    }

    fn unary(&mut self) -> Result<Box<Expr<'src>>> {
        if matches!(self.peek().token_type, TokenType::Bang | TokenType::Minus) {
            Ok(Box::new(Expr::Unary {
                operator: self.next_token().clone(),
                right: self.unary()?,
            }))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Box<Expr<'src>>> {
        match self.peek().token_type {
            TokenType::True
            | TokenType::False
            | TokenType::Nil
            | TokenType::Number
            | TokenType::String => Ok(Box::new(Expr::Literal {
                value: self.next_token().literal.expect("literal exist"),
            })),
            TokenType::LeftParen => {
                self.next_token();
                let expr = self.expression()?;
                self.expect_next(TokenType::RightParen, E::LackRightParan { line: self.line })?;
                Ok(Box::new(Expr::Grouping { expression: expr }))
            }
            TokenType::Identifier => Ok(Box::new(Expr::Variable {
                name: self.next_token(),
            })),
            _ => Err(E::NotExpression { line: self.line }),
        }
    }

    fn sync(&mut self) {
        loop {
            match self.peek().token_type {
                TokenType::Semicolon => {
                    self.next_token();
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
                    self.next_token();
                }
            }
        }
    }

    // Helper function
    fn parse_binary_op<F>(
        &mut self,
        parse_operand: F,
        operators: &[TokenType],
    ) -> Result<Box<Expr<'src>>>
    where
        F: Fn(&mut Self) -> Result<Box<Expr<'src>>>,
    {
        let mut expr = parse_operand(self)?;

        while operators.contains(&self.peek().token_type) {
            let operator = self.next_token();
            let right = parse_operand(self)?;
            expr = Box::new(Expr::Binary {
                left: expr,
                operator,
                right,
            });
        }
        Ok(expr)
    }

    fn peek(&mut self) -> &Token<'src> {
        self.tokens.peek().expect("always an EOF token")
    }

    fn next_token(&mut self) -> Token<'src> {
        self.tokens
            .next()
            .inspect(|t| self.line = t.line)
            .expect("always an EOF token")
    }

    /// Advance iterator when match and return matched token
    /// return None if not match
    fn next_if(&mut self, match_type: TokenType) -> Result<Option<Token<'src>>> {
        match self.tokens.peek() {
            Some(t) if t.token_type == match_type => Ok(Some(self.next_token())),
            Some(_) => Ok(None),
            None => unreachable!("always an EOF token"),
        }
    }

    /// Advance iterator when next token match expect type
    /// Return given Err if not match
    fn expect_next(&mut self, match_type: TokenType, err: ParseError) -> Result<Token<'src>> {
        match self.tokens.peek() {
            Some(t) if t.token_type == match_type => Ok(self.next_token()),
            Some(_) => Err(err),
            None => unreachable!("always an EOF token"),
        }
    }
}

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ParseError {
    #[error("expect ';' after {after}")]
    LackSemiColon { line: usize, after: &'static str },
    #[error("expect ')' after expression")]
    LackRightParan { line: usize },
    #[error("expect '}}' after block")]
    LackRightBrace { line: usize },
    #[error("expect variable name")]
    NotIdentifier { line: usize },
    #[error("expect expression")]
    NotExpression { line: usize },
    #[error("invalid assignment target")]
    InvalidAssignment { line: usize },
}
