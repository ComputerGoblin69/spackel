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
            '0'..='9' => stack.push((c as u32 - '0' as u32) as i32),
            'p' => println!("{}", stack.pop().unwrap()),
            '+' => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(if a == 9 && b == 10 || b == 9 && a == 10 {
                    21
                } else if b == 1 && a == 1 {
                    1
                } else {
                    a + b
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
            _ => {
                if !c.is_whitespace() {
                    panic!("invalid character: {c:?}");
                }
            }
        }
    }

    if !stack.is_empty() {
        panic!("there's stuff left on the stack");
    }

    Ok(())
}
