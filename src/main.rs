#![forbid(unsafe_code)]
#![deny(
    clippy::allow_attributes,
    clippy::allow_attributes_without_reason,
    clippy::iter_over_hash_type
)]
#![warn(clippy::nursery, clippy::pedantic)]

mod call_graph;
mod compiler;
mod diagnostics;
mod formatter;
mod interpreter;
mod ir;
mod lexer;
mod parser;
mod ssa;
mod typ;
mod unicode;

use anyhow::{bail, ensure, Context, Result};
use codemap::CodeMap;
use std::{path::Path, process::ExitCode};

fn main() -> Result<ExitCode> {
    let mut code_map = CodeMap::new();

    real_main(&mut code_map)
        .map(|()| ExitCode::SUCCESS)
        .or_else(|err| {
            err.downcast::<diagnostics::Error>()
                .map(|diagnostic| diagnostic.emit(&code_map))
        })
}

fn real_main(code_map: &mut CodeMap) -> Result<()> {
    let mut args = std::env::args().skip(1);
    ensure!(args.len() < 3, "too many command line arguments");

    let command = args.next().context("no command provided")?;
    match &*command {
        "run" => {
            let source_path = args.next().context("no file provided")?;
            let source_code = std::fs::read_to_string(&source_path)
                .context("failed to read source file")?;
            let file = code_map.add_file(source_path, source_code);

            let program = parser::parse(&file)?;
            let program = typ::check(program)?;
            interpreter::interpret(&program);
            Ok(())
        }
        "compile" => {
            let source_path = args.next().context("no file provided")?;
            let source_code = std::fs::read_to_string(&source_path)
                .context("failed to read source file")?;
            let file = code_map.add_file(source_path, source_code);

            let program = parser::parse(&file)?;
            let program = typ::check(program)?;
            let mut var_generator = ssa::VarGenerator::default();
            let program = ssa::convert(program, &mut var_generator);
            let graph = call_graph::of(program.function_bodies);

            if std::env::var_os("SPACKEL_PRINT_SSA").is_some() {
                for function in graph.node_weights() {
                    eprintln!("{}: {:#?}", function.name, function.body);
                }
            }

            let target_triple = std::env::var("SPACKEL_TARGET");
            let compilation_options = compiler::Options {
                target_triple: target_triple
                    .as_deref()
                    .unwrap_or("x86_64-unknown-linux-gnu"),
                out_path: Path::new("main.o"),
            };
            compiler::compile(
                &graph,
                &program.function_signatures,
                &compilation_options,
            )
        }
        "format" => {
            ensure!(args.len() == 0, "too many command line arguments");
            let source_code = std::io::read_to_string(std::io::stdin().lock())
                .context("failed to read stdin")?;
            print!("{}", formatter::format(&source_code));
            Ok(())
        }
        _ => bail!(
            "command must be `run`, `compile` or `format`, not {command:?}"
        ),
    }
}
