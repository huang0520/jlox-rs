use std::error::Error;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::string::{FromUtf8Error, ParseError};

use crate::scanner::ScanError;

pub struct Lox {}

impl Lox {
    pub fn execute(&self, args: &[String]) -> Result<(), ExecutionError> {
        match args.len() {
            1 => self.run_prompt().map_err(|e| ExecutionError {
                args: args.to_vec(),
                kind: ExecutionErrorKind::from(e),
            })?,
            2 => self.run_file(&args[1]).map_err(|e| ExecutionError {
                args: args.to_vec(),
                kind: ExecutionErrorKind::from(e),
            })?,
            _ => {
                return Err(ExecutionError {
                    args: args.to_vec(),
                    kind: ExecutionErrorKind::InvalidArguments,
                });
            }
        };
        Ok(())
    }

    fn run_file(&self, path: impl AsRef<Path>) -> Result<(), RunFileError> {
        let bytes = fs::read(&path).map_err(|e| RunFileError {
            path: path.as_ref().to_path_buf(),
            kind: RunFileErrorKind::ReadFile(e),
        })?;
        self.run(&String::from_utf8(bytes).map_err(|e| RunFileError {
            path: path.as_ref().to_path_buf(),
            kind: RunFileErrorKind::NonUtf8File(e),
        })?)
        .map_err(|e| RunFileError {
            path: path.as_ref().to_path_buf(),
            kind: RunFileErrorKind::from(e),
        })?;
        Ok(())
    }

    fn run_prompt(&self) -> Result<(), RunPromptError> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let mut buffer = String::new();

        loop {
            print!("> ");
            stdout.flush().map_err(|e| RunPromptError {
                kind: RunPromptErrorKind::InvalidPrompt(e),
            })?;

            stdin.read_line(&mut buffer).map_err(|e| RunPromptError {
                kind: RunPromptErrorKind::InvalidPrompt(e),
            })?;
            if buffer.trim().is_empty() {
                break;
            };
            if let Err(e) = self.run(buffer.trim()) {
                eprintln!("{} | {}", e, e.kind);
            }
            buffer.clear();
        }
        Ok(())
    }

    fn run(&self, source: &str) -> Result<(), RunError> {
        todo!()
    }
}

#[derive(Debug, thiserror::Error)]
#[error("failed to execute commands: {:?}", args)]
#[non_exhaustive]
pub struct ExecutionError {
    pub args: Vec<String>,
    #[source]
    pub kind: ExecutionErrorKind,
}

#[derive(Debug, thiserror::Error)]
pub enum ExecutionErrorKind {
    #[error(transparent)]
    RunFile(#[from] RunFileError),
    #[error(transparent)]
    RunPrompt(#[from] RunPromptError),
    #[error("InvalidArguments")]
    InvalidArguments,
}

#[derive(Debug, thiserror::Error)]
#[error("error reading `{}`", path.display())]
#[non_exhaustive]
pub struct RunFileError {
    pub path: PathBuf,
    #[source]
    pub kind: RunFileErrorKind,
}

#[derive(Debug, thiserror::Error)]
pub enum RunFileErrorKind {
    #[error(transparent)]
    ReadFile(std::io::Error),
    #[error(transparent)]
    NonUtf8File(FromUtf8Error),
    #[error(transparent)]
    Parse(#[from] RunError),
}

#[derive(Debug, thiserror::Error)]
#[error("")]
#[non_exhaustive]
pub struct RunPromptError {
    #[source]
    pub kind: RunPromptErrorKind,
}

#[derive(Debug, thiserror::Error)]
pub enum RunPromptErrorKind {
    #[error(transparent)]
    InvalidPrompt(std::io::Error),
    #[error(transparent)]
    Paser(#[from] RunError),
}

#[derive(Debug, thiserror::Error)]
#[error("error: {line}: {kind}")]
#[non_exhaustive]
pub struct RunError {
    pub line: usize,
    #[source]
    pub kind: RunErrorKind,
}

#[derive(Debug, thiserror::Error)]
pub enum RunErrorKind {
    #[error(transparent)]
    ScanError(#[from] ScanError),
    #[error(transparent)]
    ParseError(#[from] ParseError),
}
