#![forbid(unsafe_code)]
#![warn(clippy::nursery, clippy::pedantic)]

use std::process::ExitCode;

fn main() -> ExitCode {
    match real_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

fn real_main() -> Result<(), ()> {
    let mut args = std::env::args_os().skip(1);
    if args.len() > 1 {
        eprintln!("Error: too many command line arguments");
        return Err(());
    }
    let source_path = args
        .next()
        .ok_or_else(|| eprintln!("Error: no file provided"))?;
    let source_code = std::fs::read_to_string(source_path)
        .map_err(|err| eprintln!("Error: failed to read source file: {err}"))?;
    print!("{source_code}");

    Ok(())
}
