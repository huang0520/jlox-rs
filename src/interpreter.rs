use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use crate::environment::{EnvironmentError, EnvironmentIndex, Environments};
use crate::expr::Expr;
use crate::literal::{Literal, TypeError};
use crate::parser::{ParseError, Parser};
use crate::scanner::{ScanError, Scanner};
use crate::stmt::Stmt;
use crate::token_type::TokenType;

#[derive(Debug)]
pub struct Lox {
    environments: Environments,
}

impl Lox {
    pub fn new() -> Self {
        Self {
            environments: Environments::new(),
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
            let _ = self.run(buffer.trim()).inspect_err(|e| eprintln!("{e}"));
            buffer.clear();
        }
        Ok(())
    }

    fn run(&mut self, source: &str) -> Result<(), RunError> {
        let tokens = Scanner::scan_tokens(source).map_err(RunError::Scan)?;
        let stmts = Parser::parse(tokens.into_iter()).map_err(RunError::Parse)?;
        for stmt in stmts {
            self.evaluate_stmt(&stmt, Environments::GLOBAL_INDEX)
                .map_err(RunError::Runtime)?;
        }
        Ok(())
    }

    fn evaluate_stmt(
        &mut self,
        statement: &Stmt,
        environment_idx: EnvironmentIndex,
    ) -> Result<(), RuntimeError> {
        match statement {
            Stmt::Expression(expr) => {
                self.evaluate_expr(expr)?;
                Ok(())
            }
            Stmt::Print(expr) => {
                println!("{}", self.evaluate_expr(expr)?);
                Ok(())
            }
            Stmt::Var { name, initializer } => {
                let value = self.evaluate_expr(initializer)?;
                self.environments.define_value(name, value);
                Ok(())
            }
            Stmt::Block(statements) => {
                let new_idx = self.environments.create_local(environment_idx);
                for stmt in statements {
                    if let Err(e) = self.evaluate_stmt(stmt, new_idx) {
                        self.environments.pop_local();
                        return Err(e);
                    }
                }
                self.environments.pop_local();
                Ok(())
            }
        }
    }

    fn evaluate_expr(&mut self, expression: &Expr) -> Result<Literal, RuntimeError> {
        match expression {
            Expr::Variable { name } => {
                self.environments
                    .get_value(name)
                    .map_err(|e| RuntimeError::UndefinedVariable {
                        line: name.line,
                        source: e,
                    })
            }
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Grouping { expression } => self.evaluate_expr(expression),
            Expr::Unary { operator, right } => {
                let right_lit = self.evaluate_expr(right)?;

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
                let left_lit = self.evaluate_expr(left)?;
                let right_lit = self.evaluate_expr(right)?;

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
                let value = self.evaluate_expr(value)?;
                self.environments
                    .assign_value(name, value.clone())
                    .map_err(|e| RuntimeError::UndefinedVariable {
                        line: name.line,
                        source: e,
                    })?;
                Ok(value)
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
    #[error("scan errors occurred:\n{}", format_scan_errors(.0))]
    Scan(Vec<ScanError>),
    #[error("parse errors occurred:\n{}", format_parse_errors(.0))]
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

fn format_scan_errors(errors: &[ScanError]) -> String {
    errors
        .iter()
        .map(|e| match e {
            ScanError::UnexpectedChar { line, .. } => format!("  - line: {line}: {e}"),
            ScanError::UnterminatedCommentBlock { line } => format!("  - line: {line}: {e}"),
            ScanError::UnterminatedString { line } => format!("  - line: {line}: {e}"),
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_parse_errors(errors: &[ParseError]) -> String {
    errors
        .iter()
        .map(|e| match e {
            ParseError::LackRightParan { line } => format!("  - line: {line}: {e}"),
            ParseError::NotExpression { line } => format!("  - line: {line}: {e}"),
            ParseError::LackSemiColon { line, .. } => format!("  - line: {line}: {e}"),
            ParseError::NotIdentifier { line } => format!("  - line: {line}: {e}"),
            ParseError::InvalidAssignment { line } => format!("  - line: {line}: {e}"),
            ParseError::LackRightBrace { line } => format!("  - line: {line}: {e}"),
        })
        .collect::<Vec<_>>()
        .join("\n")
}
