use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use crate::expr::evaluate::RuntimeError;
use crate::parser::{ParseError, Parser};
use crate::scanner::{ScanError, Scanner};

pub struct Lox {}

impl Lox {
    pub fn execute(&self, args: &[String]) -> Result<(), LoxError> {
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

    fn run_file(&self, path: impl AsRef<Path>) -> Result<(), RunFileError> {
        let source = fs::read_to_string(&path)
            .map_err(|e| RunFileErrorKind::ReadFile(e).into_error(path.as_ref()))?;
        self.run(&source)
            .map_err(|e| RunFileErrorKind::Run(e).into_error(path.as_ref()))
    }

    fn run_prompt(&self) -> Result<(), RunPromptError> {
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

    fn run(&self, source: &str) -> Result<(), RunError> {
        let tokens = Scanner::scan_tokens(source).map_err(RunError::Scan)?;
        let expr = Parser::new(tokens.into_iter().peekable())
            .parse()
            .map_err(RunError::Parse)?;
        println!("{}", expr.evaluate()?);
        Ok(())
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
    #[error(transparent)]
    Runtime(#[from] RuntimeError),
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
            _ => todo!(),
        })
        .collect::<Vec<_>>()
        .join("\n")
}
