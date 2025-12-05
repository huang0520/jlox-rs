use std::error::Error;
use std::fmt::{Debug, Display};
use std::iter::Peekable;

use crate::expr::RuntimeExpr;
use crate::stmt::RuntimeStmt;
use crate::{expr::Expr, literal::Literal, stmt::Stmt, token::Token, token_type::TokenType};
use snafu::Snafu;

type Result<T> = std::result::Result<T, ParseError>;

const MAX_ARITY: usize = 255;

#[derive(Debug)]
pub enum ASTNode<'src> {
    Stmt(Stmt<'src>),
    Expr(Expr<'src>),
}

#[derive(Debug)]
pub enum RuntimeASTNode {
    Stmt(RuntimeStmt),
    Expr(RuntimeExpr),
}

impl<'src> From<ASTNode<'src>> for RuntimeASTNode {
    fn from(node: ASTNode<'src>) -> Self {
        match node {
            ASTNode::Stmt(stmt) => RuntimeASTNode::Stmt(stmt.into()),
            ASTNode::Expr(expr) => RuntimeASTNode::Expr(expr.into()),
        }
    }
}

#[derive(Debug)]
pub enum ParserMode {
    File,
    Repl,
}

#[derive(Debug)]
pub struct Parser<'src, I: Iterator<Item = Token<'src>>> {
    tokens: Peekable<I>,
    errors: Vec<ParseError>,
    loop_depth: usize,
    mode: ParserMode,
}

impl<'src, I: Iterator<Item = Token<'src>>> Parser<'src, I> {
    pub fn parse(
        tokens: I,
        mode: ParserMode,
    ) -> std::result::Result<Vec<ASTNode<'src>>, ParseErrors> {
        let mut nodes = Vec::new();

        let mut parser = Self {
            tokens: tokens.peekable(),
            errors: Vec::new(),
            loop_depth: 0,
            mode,
        };

        while parser.peek().token_type != TokenType::Eof {
            match parser.declaration() {
                Ok(stmt) => match *stmt {
                    Stmt::Expression(expr) if parser.peek().token_type == TokenType::Eof => {
                        nodes.push(ASTNode::Expr(expr))
                    }
                    _ => nodes.push(ASTNode::Stmt(*stmt)),
                },
                Err(e) => {
                    parser.errors.push(e);
                    parser.sync();
                }
            }
        }

        if parser.errors.is_empty() {
            Ok(nodes)
        } else {
            Err(ParseErrors(parser.errors))
        }
    }

    // ========== Declaration Grammer ==========
    // declaration    → funDecl
    //                | varDecl
    //                | statement ;
    //
    fn declaration(&mut self) -> Result<Box<Stmt<'src>>> {
        match self.peek().token_type {
            TokenType::Var => self.var_declaration(),
            TokenType::Fun => self.fun_declaration("function"),
            _ => self.statement(),
        }
    }

    fn var_declaration(&mut self) -> Result<Box<Stmt<'src>>> {
        // Consume 'var'
        self.next_token();

        let name = self.expect_next(TokenType::Identifier, "'variable name'")?;
        let initializer = if self.next_if(TokenType::Equal).is_some() {
            self.expression()?
        } else {
            Box::new(Expr::Literal {
                value: Literal::Nil,
            })
        };
        let _ = self.expect_next(TokenType::Semicolon, "';' after variable declaration")?;
        Ok(Box::new(Stmt::Var {
            name,
            initializer: *initializer,
        }))
    }

    fn fun_declaration(&mut self, kind: &'static str) -> Result<Box<Stmt<'src>>> {
        // Consume 'fun'
        self.next_token();

        let name = self.expect_next(TokenType::Identifier, &format!("{kind} name"))?;
        self.expect_next(TokenType::LeftParen, &format!("'(' after {kind} name"))?;

        let mut parameters = Vec::new();
        if self.peek().token_type != TokenType::RightParen {
            loop {
                let line = self.peek().line;
                if parameters.len() >= MAX_ARITY {
                    self.errors.push(
                        TooMuchSnafu {
                            line,
                            what: "parameters",
                        }
                        .build(),
                    );
                }
                parameters.push(self.expect_next(TokenType::Identifier, "parameter name")?);
                if self.next_if(TokenType::Comma).is_none() {
                    break;
                }
            }
        }
        self.expect_next(TokenType::RightParen, "')' after arguments")?;

        let peeked = self.peek();
        if peeked.token_type != TokenType::LeftBrace {
            ExpectTokenSnafu {
                line: peeked.line,
                expect: format!("'{{' before {kind} body"),
                found: peeked.lexeme.to_string(),
            }
            .fail()?;
        }
        let body = self.block_statement()?;
        Ok(Box::new(Stmt::Function {
            name,
            parameters,
            body,
        }))
    }

    // ========== Statement Grammer ==========
    // statement      → exprStmt
    //                | forStmt
    //                | ifStmt
    //                | printStmt
    //                | returnStmt
    //                | whileStmt
    //                | block ;
    //
    // exprStmt       → expression ";" ;
    // forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
    //                            expression? ";"
    //                            expression? ")" statement ;
    // ifStmt         → "if" "(" expression ")" statement
    //                  ( "else" statement )? ;
    // printStmt      → "print" expression ";" ;
    // returnStmt     → "return" expression? ";" ;
    // whileStmt      → "while" "(" expression ")" statement ;
    // block          → "{" declaration* "}" ;
    //
    fn statement(&mut self) -> Result<Box<Stmt<'src>>> {
        match self.peek().token_type {
            TokenType::Print => self.print_statement(),
            TokenType::LeftBrace => self.block_statement(),
            TokenType::If => self.if_statement(),
            TokenType::While => self.while_statement(),
            TokenType::For => self.for_statement(),
            TokenType::Return => self.return_statement(),
            TokenType::Break => self.break_statement(),
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        // Consume 'print'
        self.next_token();

        let expr = self.expression()?;
        self.expect_next(TokenType::Semicolon, "';'")?;
        Ok(Box::new(Stmt::Print(*expr)))
    }

    fn block_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        // Consume '{'
        self.next_token();

        let mut statements: Vec<Stmt<'_>> = Vec::new();
        while !matches!(
            self.peek().token_type,
            TokenType::RightBrace | TokenType::Eof
        ) {
            statements.push(*self.declaration()?);
        }

        self.expect_next(TokenType::RightBrace, "'}}' after block")?;
        Ok(Box::new(Stmt::Block(statements)))
    }

    fn if_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        // Consume 'if'
        self.next_token();

        self.expect_next(TokenType::LeftParen, "'(' after 'if'")?;
        let condition = *self.expression()?;
        self.expect_next(TokenType::RightParen, "')' after if condition")?;

        let then_branch = self.statement()?;
        let else_branch = if self.next_if(TokenType::Else).is_some() {
            Some(self.statement()?)
        } else {
            None
        };

        Ok(Box::new(Stmt::If {
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn while_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        // Consume 'while'
        self.next_token();
        self.loop_depth += 1;

        self.expect_next(TokenType::LeftParen, "'(' after 'while'")?;
        let condition = *self.expression()?;
        self.expect_next(TokenType::RightParen, "')' after while condition")?;
        let body = self.statement()?;

        self.loop_depth -= 1;
        Ok(Box::new(Stmt::While { condition, body }))
    }

    fn for_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        // Consume 'for'
        self.next_token();
        self.loop_depth += 1;

        self.expect_next(TokenType::LeftParen, "'(' after 'for'")?;
        let initializer = match self.peek().token_type {
            TokenType::Semicolon => {
                self.next_token();
                None
            }
            TokenType::Var => Some(*self.var_declaration()?),
            _ => Some(*self.expression_statement()?),
        };

        let condition = if self.peek().token_type == TokenType::Semicolon {
            None
        } else {
            Some(*self.expression()?)
        };
        self.expect_next(TokenType::Semicolon, "';' after loop condition")?;

        let increment = if self.peek().token_type == TokenType::RightParen {
            None
        } else {
            Some(Stmt::Expression(*self.expression()?))
        };
        self.expect_next(TokenType::RightParen, "')' after for closure")?;
        let mut body = *self.statement()?;

        if increment.is_some() {
            body = Stmt::Block(vec![body, increment.expect("increment exist")]);
        }
        body = Stmt::While {
            condition: condition.unwrap_or(Expr::Literal {
                value: Literal::Boolean(true),
            }),
            body: Box::new(body),
        };
        if initializer.is_some() {
            body = Stmt::Block(vec![initializer.expect("initializer exist"), body]);
        }

        self.loop_depth -= 1;
        Ok(Box::new(body))
    }

    fn return_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        // Consume 'return'
        let token = self.next_token();

        let mut expr = Expr::Literal {
            value: Literal::Nil,
        };
        if self.peek().token_type != TokenType::Semicolon {
            expr = *self.expression()?;
        }
        self.expect_next(TokenType::Semicolon, "';' after return value")?;
        Ok(Box::new(Stmt::Return {
            keyword: token,
            value: expr,
        }))
    }

    fn break_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        // Consume 'break'
        let token = self.next_token();
        self.expect_next(TokenType::Semicolon, "';' after break")?;

        if self.loop_depth > 0 {
            Ok(Box::new(Stmt::Break))
        } else {
            NotInLoopSnafu { line: token.line }.fail()
        }
    }

    fn expression_statement(&mut self) -> Result<Box<Stmt<'src>>> {
        let expr = self.expression()?;
        // REPL Mode:
        // Accept lack of semicolon in expression statement if it is last
        if !matches!(self.mode, ParserMode::Repl) || self.peek().token_type != TokenType::Eof {
            self.expect_next(TokenType::Semicolon, "';'")?;
        }
        Ok(Box::new(Stmt::Expression(*expr)))
    }

    // ========== Expressions Grammer ==========
    // expression     → assignment ;
    //
    // assignment     → ( call "." )? IDENTIFIER "=" assignment
    //                | logic_or ;
    //
    // logic_or       → logic_and ( "or" logic_and )* ;
    // logic_and      → equality ( "and" equality )* ;
    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term           → factor ( ( "-" | "+" ) factor )* ;
    // factor         → unary ( ( "/" | "*" ) unary )* ;
    //
    // unary          → ( "!" | "-" ) unary | call ;
    // call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    // primary        → "true" | "false" | "nil" | "this"
    //                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
    //                | "super" "." IDENTIFIER ;
    //
    fn expression(&mut self) -> Result<Box<Expr<'src>>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Box<Expr<'src>>> {
        let expr = self.logic_or()?;

        // If next token is '=' -> Assign value
        let line = self.peek().line;
        if self.next_if(TokenType::Equal).is_some() {
            let value = self.assignment()?;

            if let Expr::Variable { name } = expr.as_ref() {
                Ok(Box::new(Expr::Assign {
                    name: name.clone(),
                    value,
                }))
            } else {
                InvalidAssignmentSnafu { line }.fail()
            }
        } else {
            Ok(expr)
        }
    }

    fn logic_or(&mut self) -> Result<Box<Expr<'src>>> {
        let mut expr = self.logic_and()?;
        while let Some(operator) = self.next_if(TokenType::Or) {
            let right = self.logic_and()?;
            expr = Box::new(Expr::Logical {
                left: expr,
                operator,
                right,
            });
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Box<Expr<'src>>> {
        let mut expr = self.equality()?;
        while let Some(operator) = self.next_if(TokenType::And) {
            let right = self.equality()?;
            expr = Box::new(Expr::Logical {
                left: expr,
                operator,
                right,
            });
        }
        Ok(expr)
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
            self.call()
        }
    }

    fn call(&mut self) -> Result<Box<Expr<'src>>> {
        let mut expr = self.primary()?;

        // Wait to complete
        while true {
            match self.peek().token_type {
                TokenType::LeftParen => {
                    expr = self.finish_call(expr)?;
                }
                _ => {
                    break;
                }
            }
        }

        Ok(expr)
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
                self.expect_next(TokenType::RightParen, ")")?;
                Ok(Box::new(Expr::Grouping { expression: expr }))
            }
            TokenType::Identifier => Ok(Box::new(Expr::Variable {
                name: self.next_token(),
            })),
            _ => NotExpressionSnafu {
                line: self.peek().line,
            }
            .fail(),
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
    fn parse_binary_op(
        &mut self,
        parse_operand: fn(&mut Self) -> Result<Box<Expr<'src>>>,
        operators: &[TokenType],
    ) -> Result<Box<Expr<'src>>> {
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

    fn finish_call(&mut self, callee: Box<Expr<'src>>) -> Result<Box<Expr<'src>>> {
        // Consume '('
        self.next_token();

        let mut arguments = Vec::new();
        if self.peek().token_type != TokenType::RightParen {
            loop {
                let line = self.peek().line;
                if arguments.len() >= MAX_ARITY {
                    self.errors.push(
                        TooMuchSnafu {
                            line,
                            what: "arguments",
                        }
                        .build(),
                    );
                }
                arguments.push(*self.expression()?);
                if self.next_if(TokenType::Comma).is_none() {
                    break;
                }
            }
        }
        let paren = self.expect_next(TokenType::RightParen, "')' after arguments")?;

        Ok(Box::new(Expr::Call {
            callee,
            paren,
            arguments,
        }))
    }

    fn peek(&mut self) -> &Token<'src> {
        self.tokens.peek().expect("always an EOF token")
    }

    fn next_token(&mut self) -> Token<'src> {
        self.tokens.next().expect("always an EOF token")
    }

    /// Advance iterator when match and return matched token
    /// return None if not match
    fn next_if(&mut self, match_type: TokenType) -> Option<Token<'src>> {
        match self.tokens.peek() {
            Some(t) if t.token_type == match_type => Some(self.next_token()),
            Some(_) => None,
            None => unreachable!("always an EOF token"),
        }
    }

    /// Advance iterator when next token match expect type
    /// Return given Err if not match
    fn expect_next(&mut self, match_type: TokenType, expect_desc: &str) -> Result<Token<'src>> {
        match self.tokens.peek() {
            Some(t) if t.token_type == match_type => Ok(self.next_token()),
            Some(found) => ExpectTokenSnafu {
                line: found.line,
                expect: expect_desc.to_string(),
                found: found.lexeme.to_string(),
            }
            .fail(),
            None => unreachable!("always an EOF token"),
        }
    }
}

#[derive(Debug, Snafu)]
pub enum ParseError {
    #[snafu(display("line {line}: expected {expect}, found {found}"))]
    ExpectToken {
        line: usize,
        expect: String,
        found: String,
    },
    #[snafu(display("line {line}: expected expression"))]
    NotExpression { line: usize },
    #[snafu(display("line {line}: invalid assignment target"))]
    InvalidAssignment { line: usize },
    #[snafu(display("line {line}: 'break' not in loop"))]
    NotInLoop { line: usize },
    #[snafu(display("line {line}: expected number of {what} less than {MAX_ARITY}"))]
    TooMuch { line: usize, what: &'static str },
}

/// Wrapper to aggregate mutiple parse error
#[derive(Debug)]
pub struct ParseErrors(Vec<ParseError>);

impl Error for ParseErrors {}

impl Display for ParseErrors {
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
