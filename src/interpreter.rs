use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use crate::environment::{Environment, EnvironmentError};
use crate::expr::Expr;
use crate::literal::{Literal, TypeError};
use crate::parser::{ParseError, Parser, REPLResult};
use crate::scanner::{ScanError, Scanner};
use crate::stmt::Stmt;
use crate::token_type::TokenType;

#[derive(Debug)]
pub struct Lox {
    global: Environment,
}

#[derive(Debug)]
pub enum StmtResult {
    Normal,
    Return,
    Break,
}

impl Lox {
    pub fn new() -> Self {
        Self {
            global: Environment::new(None),
        }
    }

    pub fn execute(&mut self, args: &[String]) -> Result<(), LoxError> {
        match args.len() {
            1 => self
                .run_prompt()
                .map_err(|e| LoxErrorKind::RunPrompt(e).into_error(args)),
            2 => self
                .run_file(&args[1])
                .map_err(|e| LoxErrorKind::RunFile(e).into_error(args)),
            _ => Err(LoxErrorKind::InvalidArguments.into_error(args)),
        }
    }

    fn run_file(&mut self, path: impl AsRef<Path>) -> Result<(), RunFileError> {
        let source = fs::read_to_string(&path)
            .map_err(|e| RunFileErrorKind::ReadFile(e).into_error(path.as_ref()))?;
        self.run(&source)
            .map_err(|e| RunFileErrorKind::Run(e).into_error(path.as_ref()))
    }

    fn run_prompt(&mut self) -> Result<(), RunPromptError> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let mut buffer = String::new();

        loop {
            print!("> ");
            stdout.flush().map_err(RunPromptError::ReadPrompt)?;
            stdin
                .read_line(&mut buffer)
                .map_err(RunPromptError::ReadPrompt)?;

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
        let tokens = Scanner::scan_tokens(source).map_err(RunError::Scan)?;

        let (stmts, e) = Parser::parse(tokens.into_iter());
        if !e.is_empty() {
            return Err(RunError::Parse(e));
        }

        for stmt in stmts {
            self.evaluate_stmt(&stmt, &self.global)
                .map_err(RunError::Runtime)?;
        }
        Ok(())
    }

    fn run_repl(&self, source: &str) -> Result<(), RunError> {
        let tokens = Scanner::scan_tokens(source).map_err(RunError::Scan)?;

        let (results, e) = Parser::parse_repl(tokens.into_iter());
        if !e.is_empty() {
            return Err(RunError::Parse(e));
        }

        for rst in results {
            match rst {
                REPLResult::Stmt(stmt) => {
                    self.evaluate_stmt(&stmt, &self.global)
                        .map_err(RunError::Runtime)?;
                }
                REPLResult::Expr(expr) => {
                    println!(
                        "{}",
                        self.evaluate_expr(&expr, &self.global)
                            .map_err(RunError::Runtime)?
                    );
                }
            }
        }
        Ok(())
    }

    fn evaluate_stmt(
        &self,
        statement: &Stmt,
        environment: &Environment,
    ) -> Result<StmtResult, RuntimeError> {
        match statement {
            Stmt::Expression(expression) => {
                self.evaluate_expr(expression, environment)?;
                Ok(StmtResult::Normal)
            }
            Stmt::Print(expression) => {
                println!("{}", self.evaluate_expr(expression, environment)?);
                Ok(StmtResult::Normal)
            }
            Stmt::Var { name, initializer } => {
                let value = self.evaluate_expr(initializer, environment)?;
                environment.define(&name.lexeme, value);
                Ok(StmtResult::Normal)
            }
            Stmt::Block(statements) => {
                let local = Environment::new(Some(environment.clone()));
                for stmt in statements {
                    match self.evaluate_stmt(stmt, &local) {
                        Ok(rst) => match rst {
                            StmtResult::Normal => {}
                            StmtResult::Return => todo!(),
                            StmtResult::Break => return Ok(StmtResult::Break),
                        },
                        Err(e) => return Err(e),
                    }
                }
                Ok(StmtResult::Normal)
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if self.evaluate_expr(condition, environment)?.into_truthy() {
                    self.evaluate_stmt(then_branch, environment)
                } else if let Some(stmt) = else_branch {
                    self.evaluate_stmt(stmt, environment)
                } else {
                    Ok(StmtResult::Normal)
                }
            }
            Stmt::While { condition, body } => {
                while self.evaluate_expr(condition, environment)?.into_truthy() {
                    match self.evaluate_stmt(body, environment)? {
                        StmtResult::Normal => {}
                        StmtResult::Return => todo!(),
                        StmtResult::Break => break,
                    }
                }
                Ok(StmtResult::Normal)
            }
            Stmt::Break => Ok(StmtResult::Break),
        }
    }

    fn evaluate_expr(
        &self,
        expression: &Expr,
        environment: &Environment,
    ) -> Result<Literal, RuntimeError> {
        match expression {
            Expr::Variable { name } => {
                environment
                    .get(&name.lexeme)
                    .map_err(|e| RuntimeError::UndefinedVariable {
                        line: name.line,
                        source: e,
                    })
            }
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Grouping { expression } => self.evaluate_expr(expression, environment),
            Expr::Unary { operator, right } => {
                let right_lit = self.evaluate_expr(right, environment)?;

                match operator.token_type {
                    TokenType::Minus => Ok(Literal::Number(-right_lit.try_into()?)),
                    TokenType::Bang => Ok(Literal::Boolean(!right_lit.into_truthy())),
                    _ => unreachable!("only minus and bang are unary operator"),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_lit = self.evaluate_expr(left, environment)?;
                let right_lit = self.evaluate_expr(right, environment)?;

                match operator.token_type {
                    TokenType::Minus => Ok(Literal::Number(
                        TryInto::<f64>::try_into(left_lit)? - TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::Slash => Ok(Literal::Number(
                        TryInto::<f64>::try_into(left_lit)? / TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::Star => Ok(Literal::Number(
                        TryInto::<f64>::try_into(left_lit)? * TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::Plus => Ok(self.handle_plus(&left_lit, &right_lit)?),
                    TokenType::Greater => Ok(Literal::Boolean(
                        TryInto::<f64>::try_into(left_lit)? > TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::GreaterEqual => Ok(Literal::Boolean(
                        TryInto::<f64>::try_into(left_lit)? >= TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::Less => Ok(Literal::Boolean(
                        TryInto::<f64>::try_into(left_lit)? < TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::LessEqual => Ok(Literal::Boolean(
                        TryInto::<f64>::try_into(left_lit)? <= TryInto::<f64>::try_into(right_lit)?,
                    )),
                    TokenType::EqualEqual => Ok(Literal::Boolean(left_lit == right_lit)),
                    TokenType::BangEqual => Ok(Literal::Boolean(left_lit != right_lit)),
                    _ => unreachable!(),
                }
            }
            Expr::Assign { name, value } => {
                let value = self.evaluate_expr(value, environment)?;
                environment
                    .assign(&name.lexeme, value.clone())
                    .map_err(|e| RuntimeError::UndefinedVariable {
                        line: name.line,
                        source: e,
                    })?;
                Ok(value)
            }
            Expr::Logical {
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
}

#[derive(Debug, thiserror::Error)]
#[error("failed to execute commands: \"{}\"", args.join(" "))]
#[non_exhaustive]
pub struct LoxError {
    pub args: Vec<String>,
    pub source: LoxErrorKind,
}

#[derive(Debug, thiserror::Error)]
pub enum LoxErrorKind {
    #[error(transparent)]
    RunFile(#[from] RunFileError),
    #[error("error execute prompt")]
    RunPrompt(#[from] RunPromptError),
    #[error("invalid command line arguments")]
    InvalidArguments,
}

impl LoxErrorKind {
    pub fn into_error(self, args: &[String]) -> LoxError {
        LoxError {
            args: args.to_owned(),
            source: self,
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("error execute file: {path}")]
pub struct RunFileError {
    path: PathBuf,
    source: RunFileErrorKind,
}

#[derive(Debug, thiserror::Error)]
pub enum RunFileErrorKind {
    #[error("failed to read file")]
    ReadFile(#[source] std::io::Error),
    #[error(transparent)]
    Run(#[from] RunError),
}

impl RunFileErrorKind {
    pub fn into_error(self, path: &Path) -> RunFileError {
        RunFileError {
            path: path.to_path_buf(),
            source: self,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum RunPromptError {
    #[error("failed to read prompt")]
    ReadPrompt(#[source] std::io::Error),
    #[error(transparent)]
    Run(#[from] RunError),
}

#[derive(Debug, thiserror::Error)]
pub enum RunError {
    #[error("scan errors occurred:\n{}", format_errors(.0))]
    Scan(Vec<ScanError>),
    #[error("parse errors occurred:\n{}", format_errors(.0))]
    Parse(Vec<ParseError>),
    #[error("runtime errors occured:\n{0}")]
    Runtime(RuntimeError),
}

#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("  - line: {line}: {source}")]
    UndefinedVariable {
        line: usize,
        source: EnvironmentError,
    },
    #[error(transparent)]
    Type(#[from] TypeError),
}

fn format_errors<T: std::fmt::Display>(errors: &[T]) -> String {
    errors
        .iter()
        .map(|e| format!("  - {e}"))
        .collect::<Vec<_>>()
        .join("\n")
}
