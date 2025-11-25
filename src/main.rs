mod expr;
mod interpreter;
mod literal;
mod parser;
mod scanner;
mod token;
mod token_type;

use std::env;
use std::process::{ExitCode, Termination};

use interpreter::Lox;

use crate::interpreter::LoxErrorKind;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
    let lox = Lox {};
    if let Err(e) = lox.execute(&args) {
        let exitcode = match e.source {
            LoxErrorKind::InvalidArguments => ExitCode::from(64),
            LoxErrorKind::RunFile(_) => ExitCode::from(65),
            LoxErrorKind::RunPrompt(_) => ExitCode::from(66),
        };
        eprintln!("{:?}", anyhow::Error::from(e));
        Termination::report(exitcode);
    }
    Ok(())
}
