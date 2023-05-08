use anyhow::{ensure, Context, Result};
use itertools::Itertools;
use std::collections::HashMap;

pub struct Program {
    pub instructions: Vec<Instruction>,
}

impl Program {
    pub fn parse(source_code: &str) -> Result<Self> {
        let tokens = source_code
            .lines()
            .flat_map(|line| {
                line.split_once('#')
                    .map_or(line, |(line, _comment)| line)
                    .split_whitespace()
            })
            .peekable();

        Ok(Self {
            instructions: expand_macros(tokens)
                .map(|res| res.and_then(Instruction::parse))
                .collect::<Result<_>>()?,
        })
    }
}

fn expand_macros<'a>(
    tokens: impl Iterator<Item = &'a str>,
) -> impl Iterator<Item = Result<&'a str>> {
    #![allow(clippy::unused_peekable)]

    let mut tokens = tokens.peekable();
    let mut macros = HashMap::new();

    crate::iter::try_from_fn(move || {
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
                .peeking_take_while(|&token| token != "end")
                .map(|token| {
                    ensure!(
                        token != "macro",
                        "nested macros are not supported"
                    );
                    Ok(macros
                        .get(token)
                        .cloned()
                        .unwrap_or_else(|| vec![token]))
                })
                .flatten_ok()
                .collect::<Result<_>>()?;
            ensure!(
                tokens.next() == Some("end"),
                "unterminated macro definition"
            );
            ensure!(
                macros.insert(name, body).is_none(),
                "redefinition of macro `{name}`"
            );
            Vec::new()
        } else {
            macros.get(token).cloned().unwrap_or_else(|| vec![token])
        }))
    })
    .flatten_ok()
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
