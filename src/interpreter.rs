#![forbid(clippy::unwrap_used)]

use anyhow::{ensure, Context, Result};

pub fn interpret(source_code: &str) -> Result<()> {
    Interpreter { stack: Vec::new() }.interpret(source_code)
}

struct Interpreter {
    stack: Vec<i32>,
}

impl Interpreter {
    fn pop(&mut self) -> Result<i32> {
        self.stack.pop().context("not enough arguments on stack")
    }

    fn push(&mut self, value: i32) {
        self.stack.push(value);
    }

    fn interpret(&mut self, source_code: &str) -> Result<()> {
        for word in source_code.lines().flat_map(|line| {
            line.split_once('#')
                .map_or(line, |(line, _comment)| line)
                .split_whitespace()
        }) {
            match word {
                "p" => println!("{}", self.pop()?),
                #[allow(clippy::cast_sign_loss)]
                "c" => print!(
                    "{}",
                    (self.pop()? as u32)
                        .try_into()
                        .unwrap_or(char::REPLACEMENT_CHARACTER)
                ),
                "+" => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.push(match (b, a) {
                        (9, 10) | (10, 9) => 21,
                        (1, 1) => 1,
                        _ => b + a,
                    });
                }
                "-" => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.push(b - a);
                }
                "*" => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.push(b * a);
                }
                "/" => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.push(b / a);
                }
                "ß" => self.push(1945),
                "x" => {
                    self.pop()?;
                }
                "d" => {
                    let v = self.pop()?;
                    self.push(v);
                    self.push(v);
                }
                "s" => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(b);
                    self.push(a);
                }
                _ => {
                    self.push(word.parse().ok().with_context(|| {
                        format!("unknown instruction: `{word}`")
                    })?);
                }
            }
        }

        ensure!(self.stack.is_empty(), "there's stuff left on the self");

        Ok(())
    }
}
