#![forbid(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]

mod interpreter;

use anyhow::{ensure, Context, Result};

fn main() -> Result<()> {
    let mut args = std::env::args_os().skip(1);
    ensure!(args.len() < 2, "too many command line arguments");
    let source_path = args.next().context("no file provided")?;
    let source_code = std::fs::read_to_string(source_path)
        .context("failed to read source file")?;

    interpreter::interpret(&source_code)?;

    Ok(())
}
