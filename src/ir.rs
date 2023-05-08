use anyhow::{ensure, Context, Result};
use std::collections::HashMap;

pub struct Program {
    pub instructions: Vec<Instruction>,
}

impl Program {
    pub fn parse(source_code: &str) -> Result<Self> {
        let mut tokens = source_code.lines().flat_map(|line| {
            line.split_once('#')
                .map_or(line, |(line, _comment)| line)
                .split_whitespace()
        });

        let mut macros = HashMap::<&str, Vec<Instruction>>::new();

        Ok(Self {
            instructions: crate::iter::try_from_fn(|| {
                let Some(token) = tokens.next() else {
                    return Ok(None);
                };

                ensure!(token != "end", "unexpected `end`");

                Ok(Some(if token == "macro" {
                    let name = tokens
                        .next()
                        .filter(|&name| !matches!(name, "macro" | "end"))
                        .context("macro definition has no name")?;
                    let body = tokens
                        .by_ref()
                        .take_while(|&token| token != "end")
                        .map(|token| {
                            ensure!(
                                token != "macro",
                                "nested macros are not supported"
                            );
                            Instruction::parse(token)
                        })
                        .collect::<Result<_>>()?;
                    ensure!(
                        macros.insert(name, body).is_none(),
                        "redefinition of macro `{name}`"
                    );
                    None
                } else {
                    Some(Instruction::parse(token)?)
                }))
            })
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?,
        })
    }
}

#[derive(Clone, Copy)]
pub enum Instruction {
    Push(i32),
    Println,
    PrintChar,
    BinMathOp(BinMathOp),
    Drop,
    Dup,
    Swap,
    Over,
    Nip,
    Tuck,
}

impl Instruction {
    fn parse(word: &str) -> Result<Self> {
        Ok(match word {
            "println" => Self::Println,
            "print-char" => Self::PrintChar,
            "+" => Self::BinMathOp(BinMathOp::Add),
            "-" => Self::BinMathOp(BinMathOp::Sub),
            "*" => Self::BinMathOp(BinMathOp::Mul),
            "/" => Self::BinMathOp(BinMathOp::Div),
            "%" => Self::BinMathOp(BinMathOp::Rem),
            "+🤡" => Self::BinMathOp(BinMathOp::SillyAdd),
            "ß" => Self::Push(1945),
            "drop" => Self::Drop,
            "dup" => Self::Dup,
            "swap" => Self::Swap,
            "over" => Self::Over,
            "nip" => Self::Nip,
            "tuck" => Self::Tuck,
            _ => {
                Self::Push(word.parse().ok().with_context(|| {
                    format!("unknown instruction: `{word}`")
                })?)
            }
        })
    }
}

#[derive(Clone, Copy)]
pub enum BinMathOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    SillyAdd,
}
