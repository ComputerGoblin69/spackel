use anyhow::{anyhow, bail, ensure, Context, Result};
use itertools::{process_results, Itertools};
use std::{collections::HashMap, str::FromStr};

pub struct Program {
    pub instructions: Box<[Instruction]>,
}

impl Program {
    pub fn parse(source_code: &str) -> Result<Self> {
        let tokens = source_code.lines().flat_map(|line| {
            line.split_once('#')
                .map_or(line, |(line, _comment)| line)
                .split_whitespace()
        });
        let tokens = expand_macros(tokens);
        let (instructions, terminator) =
            process_results(tokens, |mut tokens| {
                instructions_until_terminator(&mut tokens)
            })??;
        if let Some(terminator) = terminator {
            bail!("unexpected `{terminator}`");
        }

        Ok(Self { instructions })
    }
}

fn expand_macros<'a>(
    tokens: impl Iterator<Item = &'a str>,
) -> impl Iterator<Item = Result<&'a str>> {
    let mut macros = HashMap::new();

    extra_iterators::batching_map(tokens, move |tokens, token| match token {
        "macro" => {
            let name = tokens
                .next()
                .filter(|&name| !matches!(name, "macro" | "end"))
                .context("macro definition has no name")?;
            let mut found_end = false;
            let body = tokens
                .by_ref()
                .map_while(|token| match token {
                    "end" => {
                        found_end = true;
                        None
                    }
                    "macro" => {
                        Some(Err(anyhow!("nested macros are not supported")))
                    }
                    _ => Some(Ok(macros
                        .get(token)
                        .cloned()
                        .unwrap_or_else(|| vec![token]))),
                })
                .flatten_ok()
                .collect::<Result<_>>()?;
            ensure!(found_end, "unterminated macro definition");
            ensure!(
                macros.insert(name, body).is_none(),
                "redefinition of macro `{name}`"
            );
            Ok(Vec::new())
        }
        _ => Ok(macros.get(token).cloned().unwrap_or_else(|| vec![token])),
    })
    .flatten_ok()
}

fn instructions_until_terminator<'a>(
    tokens: &mut impl Iterator<Item = &'a str>,
) -> Result<(Box<[Instruction]>, Option<&'a str>)> {
    let mut terminator = None;
    let instructions = extra_iterators::try_from_fn(|| {
        let Some(token) = tokens.next() else {
            return Ok(None);
        };
        Ok(match token {
            "end" => {
                terminator = Some("end");
                None
            }
            "then" => {
                let (body, terminator) = instructions_until_terminator(tokens)?;
                match terminator {
                    Some("end") => {}
                    None => bail!("unterminated `then` statement"),
                    _ => unreachable!(),
                }
                Some(Instruction::Then(body))
            }
            _ => Some(token.parse()?),
        })
    })
    .collect::<Result<_>>()?;

    Ok((instructions, terminator))
}

pub enum Instruction {
    Then(Box<[Instruction]>),
    Push(i32),
    True,
    False,
    Print,
    Println,
    PrintChar,
    BinMathOp(BinMathOp),
    Comparison(Comparison),
    Not,
    BinLogicOp(BinLogicOp),
    Drop,
    Dup,
    Swap,
    Over,
    Nip,
    Tuck,
}

impl FromStr for Instruction {
    type Err = anyhow::Error;

    fn from_str(word: &str) -> Result<Self> {
        Ok(match word {
            "true" => Self::True,
            "false" => Self::False,
            "print" => Self::Print,
            "println" => Self::Println,
            "print-char" => Self::PrintChar,
            "+" => Self::BinMathOp(BinMathOp::Add),
            "-" => Self::BinMathOp(BinMathOp::Sub),
            "*" => Self::BinMathOp(BinMathOp::Mul),
            "/" => Self::BinMathOp(BinMathOp::Div),
            "%" => Self::BinMathOp(BinMathOp::Rem),
            "+🤡" => Self::BinMathOp(BinMathOp::SillyAdd),
            "<" => Self::Comparison(Comparison::Lt),
            "<=" => Self::Comparison(Comparison::Le),
            "=" => Self::Comparison(Comparison::Eq),
            ">" => Self::Comparison(Comparison::Ge),
            ">=" => Self::Comparison(Comparison::Gt),
            "not" => Self::Not,
            "and" => Self::BinLogicOp(BinLogicOp::And),
            "or" => Self::BinLogicOp(BinLogicOp::Or),
            "xor" => Self::BinLogicOp(BinLogicOp::Xor),
            "nand" => Self::BinLogicOp(BinLogicOp::Nand),
            "nor" => Self::BinLogicOp(BinLogicOp::Nor),
            "xnor" => Self::BinLogicOp(BinLogicOp::Xnor),
            "ß" => Self::Push(1945),
            "drop" => Self::Drop,
            "dup" => Self::Dup,
            "swap" => Self::Swap,
            "over" => Self::Over,
            "nip" => Self::Nip,
            "tuck" => Self::Tuck,
            _ => Self::Push(
                word.parse()
                    .map_err(|_| anyhow!("unknown instruction: `{word}`"))?,
            ),
        })
    }
}

pub enum BinMathOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    SillyAdd,
}

pub enum Comparison {
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
}

pub enum BinLogicOp {
    And,
    Or,
    Xor,
    Nand,
    Nor,
    Xnor,
}
