#![forbid(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]

mod compiler;
mod interpreter;
mod ir;
mod lexer;
mod stack;
mod typ;

use anyhow::{bail, ensure, Context, Result};
use std::path::Path;

enum Command {
    Run,
    Compile,
}

fn main() -> Result<()> {
    let mut args = std::env::args_os().skip(1);
    ensure!(args.len() < 3, "too many command line arguments");

    let command = args.next().context("no command provided")?;
    let command = match command.to_str() {
        Some("run") => Command::Run,
        Some("compile") => Command::Compile,
        _ => bail!("command must be `run` or `compile`, not {command:?}"),
    };

    let source_path = args.next().context("no file provided")?;
    let source_code = std::fs::read_to_string(source_path)
        .context("failed to read source file")?;

    let program = ir::Program::parse(&source_code)?;
    let program = typ::check(program)?;

    match command {
        Command::Run => {
            interpreter::interpret(&program);
            Ok(())
        }
        Command::Compile => {
            let compilation_options = compiler::Options {
                target_triple: "x86_64-unknown-linux-gnu",
                out_path: Path::new("main.o"),
            };
            compiler::compile(&program, &compilation_options)
        }
    }
}
