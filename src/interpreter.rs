use snafu::{ResultExt, Snafu};

use crate::callable::{Callable, UserFunction};
use crate::environment::{Environment, EnvironmentError};
use crate::expr::RuntimeExpr;
use crate::literal::{Literal, TypeError};
use crate::native_fn;
use crate::parser::{ParseErrors, Parser, REPLResult};
use crate::scanner::{ScanErrors, Scanner};
use crate::stmt::RuntimeStmt;
use crate::token_type::TokenType;
use std::fs;
use std::io::{self, Write};
use std::path::Path;

#[derive(Debug)]
pub struct Lox {
    pub global: Environment,
}

#[derive(Debug)]
pub enum StmtResult {
    Normal,
    Return(Literal),
    Break,
}

impl Lox {
    pub fn execute(&mut self, args: &[String]) -> Result<(), LoxError> {
        match args.len() {
            1 => self.run_prompt().context(RunPromptSnafu),
            2 => self
                .run_file(&args[1])
                .context(RunFileSnafu { path: &args[1] }),
            _ => InvalidArgumentsSnafu.fail(),
        }
    }

    fn run_file(&mut self, path: impl AsRef<Path>) -> Result<(), RunFileError> {
        let source = fs::read_to_string(&path).context(ReadFileSnafu)?;
        self.run(&source)?;
        Ok(())
    }

    fn run_prompt(&mut self) -> Result<(), RunPromptError> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let mut buffer = String::new();

        loop {
            print!("> ");
            stdout.flush().context(ReadPromptSnafu)?;
            stdin.read_line(&mut buffer).context(ReadPromptSnafu)?;

            if buffer.trim().is_empty() {
                break;
            };
            let _ = self
                .run_repl(buffer.trim())
                .inspect_err(|e| eprintln!("{e}"));
            buffer.clear();
        }
        Ok(())
    }

    fn run(&self, source: &str) -> Result<(), RunError> {
        let tokens = Scanner::scan_tokens(source).context(ScanSnafu)?;
        let statements = Parser::parse(tokens.into_iter()).context(ParseSnafu)?;

        for stmt in statements {
            self.evaluate_stmt(&stmt.into(), &self.global)
                .context(RuntimeSnafu)?;
        }
        Ok(())
    }

    fn run_repl(&self, source: &str) -> Result<(), RunError> {
        let tokens = Scanner::scan_tokens(source).context(ScanSnafu)?;
        let results = Parser::parse_repl(tokens.into_iter()).context(ParseSnafu)?;

        for rst in results {
            match rst {
                REPLResult::Stmt(stmt) => {
                    self.evaluate_stmt(&stmt.into(), &self.global)
                        .context(RuntimeSnafu)?;
                }
                REPLResult::Expr(expr) => {
                    println!(
                        "{}",
                        self.evaluate_expr(&expr.into(), &self.global)
                            .context(RuntimeSnafu)?
                    );
                }
            }
        }
        Ok(())
    }

    fn evaluate_stmt(
        &self,
        statement: &RuntimeStmt,
        environment: &Environment,
    ) -> Result<StmtResult, RuntimeError> {
        match statement {
            RuntimeStmt::Expression(expression) => {
                self.evaluate_expr(expression, environment)?;
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Print(expression) => {
                println!("{}", self.evaluate_expr(expression, environment)?);
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Var { name, initializer } => {
                let value = self.evaluate_expr(initializer, environment)?;
                environment.define(&name.lexeme, value);
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Block(statements) => self.evaluate_block(statements, environment),
            RuntimeStmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if self.evaluate_expr(condition, environment)?.is_truthy() {
                    self.evaluate_stmt(then_branch, environment)
                } else if let Some(stmt) = else_branch {
                    self.evaluate_stmt(stmt, environment)
                } else {
                    Ok(StmtResult::Normal)
                }
            }
            RuntimeStmt::While { condition, body } => {
                while self.evaluate_expr(condition, environment)?.is_truthy() {
                    match self.evaluate_stmt(body, environment)? {
                        StmtResult::Normal => {}
                        StmtResult::Break => break,
                        rst @ StmtResult::Return(..) => return Ok(rst),
                    }
                }
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Break => Ok(StmtResult::Break),
            RuntimeStmt::Function {
                name,
                parameters,
                body,
            } => {
                let function = UserFunction {
                    name: name.lexeme.clone(),
                    parameters: parameters.iter().map(|p| p.lexeme.clone()).collect(),
                    body: self.statements_of_block(*body.clone()),
                };
                environment.define(&name.lexeme, Literal::Callable(Callable::User(function)));
                Ok(StmtResult::Normal)
            }
            RuntimeStmt::Return { keyword, value } => {
                Ok(StmtResult::Return(self.evaluate_expr(value, environment)?))
            }
            _ => todo!(),
        }
    }

    pub fn evaluate_block(
        &self,
        statements: &[RuntimeStmt],
        parent_environment: &Environment,
    ) -> Result<StmtResult, RuntimeError> {
        let local = Environment::new(Some(parent_environment.clone()));

        for stmt in statements {
            match self.evaluate_stmt(stmt, &local)? {
                StmtResult::Normal => {}
                rst => return Ok(rst),
            }
        }
        Ok(StmtResult::Normal)
    }

    fn evaluate_expr(
        &self,
        expression: &RuntimeExpr,
        environment: &Environment,
    ) -> Result<Literal, RuntimeError> {
        match expression {
            RuntimeExpr::Variable { name } => {
                environment
                    .get(&name.lexeme)
                    .map_err(|e| RuntimeError::UndefinedVariable {
                        line: name.line,
                        source: e,
                    })
            }
            RuntimeExpr::Literal { value } => Ok(value.clone()),
            RuntimeExpr::Grouping { expression } => self.evaluate_expr(expression, environment),
            RuntimeExpr::Unary { operator, right } => {
                let right_lit = self.evaluate_expr(right, environment)?;

                match operator.token_type {
                    TokenType::Minus => Ok(Literal::Number(-right_lit.try_into().context(
                        TypeSnafu {
                            line: operator.line,
                        },
                    )?)),
                    TokenType::Bang => Ok(Literal::Boolean(!right_lit.is_truthy())),
                    _ => unreachable!("only minus and bang are unary operator"),
                }
            }
            RuntimeExpr::Binary {
                left,
                operator,
                right,
            } => {
                let left_lit = self.evaluate_expr(left, environment)?;
                let right_lit = self.evaluate_expr(right, environment)?;

                let line = operator.line;
                match operator.token_type {
                    TokenType::Minus => Ok(Literal::Number(
                        self.try_to_f64(left_lit, line)? - self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::Slash => Ok(Literal::Number(
                        self.try_to_f64(left_lit, line)? / self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::Star => Ok(Literal::Number(
                        self.try_to_f64(left_lit, line)? * self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::Plus => Ok(self.handle_plus(&left_lit, &right_lit)?),
                    TokenType::Greater => Ok(Literal::Boolean(
                        self.try_to_f64(left_lit, line)? > self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::GreaterEqual => Ok(Literal::Boolean(
                        self.try_to_f64(left_lit, line)? >= self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::Less => Ok(Literal::Boolean(
                        self.try_to_f64(left_lit, line)? < self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::LessEqual => Ok(Literal::Boolean(
                        self.try_to_f64(left_lit, line)? <= self.try_to_f64(right_lit, line)?,
                    )),
                    TokenType::EqualEqual => Ok(Literal::Boolean(left_lit == right_lit)),
                    TokenType::BangEqual => Ok(Literal::Boolean(left_lit != right_lit)),
                    _ => unreachable!(),
                }
            }
            RuntimeExpr::Assign { name, value } => {
                let value = self.evaluate_expr(value, environment)?;
                environment
                    .assign(&name.lexeme, value.clone())
                    .map_err(|e| RuntimeError::UndefinedVariable {
                        line: name.line,
                        source: e,
                    })?;
                Ok(value)
            }
            RuntimeExpr::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate_expr(left, environment)?;
                if operator.token_type == TokenType::Or {
                    if left.is_truthy() {
                        return Ok(left);
                    }
                } else if !left.is_truthy() {
                    return Ok(left);
                }
                self.evaluate_expr(right, environment)
            }
            RuntimeExpr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = self.evaluate_expr(callee, environment)?;

                let arguments: Result<Vec<Literal>, RuntimeError> = arguments
                    .iter()
                    .map(|expr| self.evaluate_expr(expr, environment))
                    .collect();
                let arguments = arguments?;

                match callee {
                    Literal::Callable(function) => {
                        if arguments.len() != function.arity() {
                            UnmatchedArgumentsSnafu {
                                line: paren.line,
                                expect: function.arity(),
                                found: arguments.len(),
                            }
                            .fail()
                        } else {
                            function.call(self, &arguments)
                        }
                    }
                    _ => NotCallableSnafu {
                        line: paren.line,
                        identifier: callee.to_string(),
                    }
                    .fail(),
                }
            }
            _ => todo!(),
        }
    }

    fn handle_plus(&self, left: &Literal, right: &Literal) -> Result<Literal, RuntimeError> {
        match (left, right) {
            (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
            (Literal::String(_), _) | (_, Literal::String(_)) => {
                Ok(Literal::String(format!("{}{}", left, right)))
            }
            _ => todo!(),
        }
    }

    fn statements_of_block(&self, block_statement: RuntimeStmt) -> Vec<RuntimeStmt> {
        match block_statement {
            RuntimeStmt::Block(statements) => statements,
            _ => unreachable!(),
        }
    }

    fn try_to_f64(&self, lit: Literal, line: usize) -> Result<f64, RuntimeError> {
        TryInto::<f64>::try_into(lit).context(TypeSnafu { line })
    }
}

impl Default for Lox {
    fn default() -> Self {
        let global = Environment::default();
        global.define(
            "clock",
            Literal::Callable(Callable::Native {
                name: "clock",
                arity: 0,
                function: native_fn::clock,
            }),
        );

        Self { global }
    }
}

#[derive(Debug, Snafu)]
pub enum LoxError {
    #[snafu(display("failed to execute file: {path}"))]
    RunFile { path: String, source: RunFileError },
    #[snafu(display("failed to execute prompt"))]
    RunPrompt { source: RunPromptError },
    #[snafu(display("invalid command line arguments"))]
    InvalidArguments,
}

#[derive(Debug, Snafu)]
pub enum RunFileError {
    #[snafu(display("failed to read file"))]
    ReadFile { source: std::io::Error },
    #[snafu(transparent)]
    Run { source: RunError },
}

#[derive(Debug, Snafu)]
pub enum RunPromptError {
    #[snafu(display("failed to read prompt"))]
    ReadPrompt { source: std::io::Error },
    #[snafu(transparent)]
    Run { source: RunError },
}

#[derive(Debug, Snafu)]
pub enum RunError {
    #[snafu(display("scan errors occurred"))]
    Scan { source: ScanErrors },
    #[snafu(display("parse errors occurred"))]
    Parse { source: ParseErrors },
    #[snafu(display("runtime errors occured"))]
    Runtime { source: RuntimeError },
}

#[derive(Debug, Snafu)]
pub enum RuntimeError {
    #[snafu(display("  - line: {line}: {source}"))]
    UndefinedVariable {
        line: usize,
        source: EnvironmentError,
    },
    #[snafu(display("  - line: {line}: {source}"))]
    Type { line: usize, source: TypeError },
    #[snafu(display("  - line: {line}: expected callable, found {identifier}"))]
    NotCallable { line: usize, identifier: String },
    #[snafu(display("  - line: {line}: expected {expect} arguments, found {found}"))]
    UnmatchedArguments {
        line: usize,
        expect: usize,
        found: usize,
    },
}
