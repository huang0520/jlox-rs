use snafu::{Report, ResultExt, Snafu};

use crate::callable::Callable;
use crate::environment::Environment;
use crate::evaluator::{Evaluator, RuntimeError};
use crate::literal::Literal;
use crate::native_fn;
use crate::parser::{ParseErrors, Parser, ParserMode, RuntimeASTNode};
use crate::scanner::{ScanErrors, Scanner};
use std::fs;
use std::io::{self, Write};
use std::path::Path;

#[derive(Debug)]
pub struct Lox {
    pub global: Environment,
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
        self.run(&source, ParserMode::File)?;
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
                .run(buffer.trim(), ParserMode::Repl)
                .inspect_err(|e| eprintln!("{}", Report::from_error(e)));

            buffer.clear();
        }
        Ok(())
    }

    fn run(&self, source: &str, mode: ParserMode) -> Result<(), RunError> {
        let tokens = Scanner::scan_tokens(source).context(ScanSnafu)?;
        let nodes = Parser::parse(tokens.into_iter(), mode).context(ParseSnafu)?;

        let runtime_nodes: Vec<RuntimeASTNode> = nodes.into_iter().map(|n| n.into()).collect();
        Evaluator::evaluate(&runtime_nodes, &self.global).context(RuntimeSnafu)
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
