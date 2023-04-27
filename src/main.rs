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

    let mut stack = Vec::<i32>::new();

    for c in source_code.chars() {
        match c {
            '0'..='9' => stack.push((c as u8 - b'0').into()),
            'p' => println!("{}", stack.pop().unwrap()),
            '+' => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(match (b, a) {
                    (9, 10) | (10, 9) => 21,
                    (1, 1) => 1,
                    _ => b + a,
                });
            }
            '-' => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b - a);
            }
            '*' => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b * a);
            }
            '/' => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b / a);
            }
            'ß' => stack.push(1945),
            'x' => {
                stack.pop().unwrap();
            }
            'd' => {
                let v = *stack.last().unwrap();
                stack.push(v);
            }
            _ => {
                assert!(c.is_whitespace(), "invalid character: {c:?}");
            }
        }
    }

    assert!(stack.is_empty(), "there's stuff left on the stack");

    Ok(())
}
