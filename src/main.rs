mod callable;
mod environment;
mod expr;
mod interpreter;
mod literal;
mod native_fn;
mod parser;
mod scanner;
mod stmt;
mod token;
mod token_type;

use std::env;
use std::process::{ExitCode, Termination};

use interpreter::Lox;
use snafu::{Report, Snafu};

use crate::interpreter::LoxError;

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::default();
    if let Err(e) = lox.execute(&args) {
        let exitcode = match e {
            LoxError::InvalidArguments => ExitCode::from(64),
            LoxError::RunFile { .. } => ExitCode::from(65),
            LoxError::RunPrompt { .. } => ExitCode::from(66),
        };
        eprintln!("{}", Report::capture(|| Err(Error { args, source: e })));
        Termination::report(exitcode);
    }
    Ok(())
}

#[derive(Debug, Snafu)]
#[snafu(display("failed to execute commands: \"{}\"", args.join(" ")))]
pub struct Error {
    pub args: Vec<String>,
    pub source: LoxError,
}
