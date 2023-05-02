use anyhow::{ensure, Context, Result};

pub fn interpret(source_code: &str) -> Result<()> {
    let mut stack = Vec::<i32>::new();

    for word in source_code.lines().flat_map(|line| {
        line.split_once('#')
            .map_or(line, |(line, _comment)| line)
            .split_whitespace()
    }) {
        match word {
            "p" => println!("{}", stack.pop().unwrap()),
            #[allow(clippy::cast_sign_loss)]
            "c" => print!(
                "{}",
                (stack.pop().unwrap() as u32)
                    .try_into()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            ),
            "+" => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(match (b, a) {
                    (9, 10) | (10, 9) => 21,
                    (1, 1) => 1,
                    _ => b + a,
                });
            }
            "-" => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b - a);
            }
            "*" => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b * a);
            }
            "/" => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b / a);
            }
            "ß" => stack.push(1945),
            "x" => {
                stack.pop().unwrap();
            }
            "d" => {
                let v = *stack.last().unwrap();
                stack.push(v);
            }
            "s" => {
                let a = stack.len() - 2;
                let b = stack.len() - 1;
                stack.swap(a, b);
            }
            _ => {
                stack.push(word.parse().ok().with_context(|| {
                    format!("unknown instruction: `{word}`")
                })?);
            }
        }
    }

    ensure!(stack.is_empty(), "there's stuff left on the stack");

    Ok(())
}
