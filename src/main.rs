mod expr;
mod interpreter;
mod parser;
mod scanner;
mod token;
mod token_type;

use std::env;
use std::process::{ExitCode, Termination};

use interpreter::Lox;

use crate::interpreter::ExecutionErrorKind;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
    let lox = Lox {};
    if let Err(e) = lox.execute(&args) {
        let exitcode = match e.kind {
            ExecutionErrorKind::InvalidArguments => ExitCode::from(64),
            ExecutionErrorKind::RunFile(_) => ExitCode::from(65),
            ExecutionErrorKind::RunPrompt(_) => ExitCode::from(66),
        };
        eprintln!("{:?}", anyhow::Error::from(e));
        Termination::report(exitcode);
    }
    Ok(())
}
