#![forbid(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]

mod compiler;
mod diagnostics;
mod interpreter;
mod ir;
mod lexer;
mod stack;
mod typ;

use anyhow::{bail, ensure, Context, Result};
use codemap::CodeMap;
use std::{path::Path, process::ExitCode};

enum Command {
    Run,
    Compile,
}

fn main() -> Result<ExitCode> {
    let mut code_map = CodeMap::new();

    #[allow(clippy::option_if_let_else)] // `else` consumes `err`
    real_main(&mut code_map)
        .map(|()| ExitCode::SUCCESS)
        .or_else(|err| {
            if let Some(diagnostic) = err.downcast_ref::<diagnostics::Error>() {
                Ok(diagnostic.emit(&code_map))
            } else {
                Err(err)
            }
        })
}

fn real_main(code_map: &mut CodeMap) -> Result<()> {
    let mut args = std::env::args().skip(1);
    ensure!(args.len() < 3, "too many command line arguments");

    let command = args.next().context("no command provided")?;
    let command = match &*command {
        "run" => Command::Run,
        "compile" => Command::Compile,
        _ => bail!("command must be `run` or `compile`, not {command:?}"),
    };

    let source_path = args.next().context("no file provided")?;
    let source_code = std::fs::read_to_string(&source_path)
        .context("failed to read source file")?;
    let file = code_map.add_file(source_path, source_code);

    let program = ir::Program::parse(&file)?;
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
