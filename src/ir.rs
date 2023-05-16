use crate::{
    diagnostics,
    lexer::{lex, Token},
};
use anyhow::{anyhow, bail, ensure, Context, Result};
use codemap_diagnostic::Diagnostic;
use itertools::{process_results, Itertools};
use std::collections::HashMap;

pub struct Program {
    pub instructions: Box<[Instruction]>,
}

impl Program {
    pub fn parse(file: &codemap::File) -> Result<Self> {
        let tokens = expand_macros(lex(file));
        let (instructions, terminator) =
            process_results(tokens, |mut tokens| {
                instructions_until_terminator(&mut tokens)
            })??;
        if let Some(terminator) = terminator {
            bail!(unexpected_token(terminator));
        }

        Ok(Self { instructions })
    }
}

fn expand_macros<'a>(
    tokens: impl Iterator<Item = Token<'a>>,
) -> impl Iterator<Item = Result<Token<'a>>> {
    let mut macros = HashMap::new();

    extra_iterators::batching_map(tokens, move |tokens, token| match &*token {
        "macro" => {
            let name = tokens.next().context("macro definition has no name")?;
            ensure!(
                !is_keyword(&name),
                "keyword `{name}` cannot be used as a macro name"
            );
            let mut found_end = false;
            let mut layers = 0_usize;
            let body = tokens
                .by_ref()
                .map_while(|token| match &*token {
                    "end" => {
                        if layers == 0 {
                            found_end = true;
                            None
                        } else {
                            layers -= 1;
                            Some(Ok(vec![token]))
                        }
                    }
                    "macro" => {
                        Some(Err(anyhow!("nested macros are not supported")))
                    }
                    "then" => {
                        layers += 1;
                        Some(Ok(vec![token]))
                    }
                    _ => Some(Ok(macros
                        .get(&*token)
                        .cloned()
                        .unwrap_or_else(|| vec![token]))),
                })
                .flatten_ok()
                .collect::<Result<_>>()?;
            ensure!(found_end, unterminated("macro definition", token));
            ensure!(
                macros.insert(name.text, body).is_none(),
                "redefinition of macro `{name}`"
            );
            Ok(Vec::new())
        }
        _ => Ok(macros.get(&*token).cloned().unwrap_or_else(|| vec![token])),
    })
    .flatten_ok()
}

fn instructions_until_terminator<'a>(
    tokens: &mut impl Iterator<Item = Token<'a>>,
) -> Result<(Box<[Instruction]>, Option<Token<'a>>)> {
    let mut terminator = None;
    let instructions = extra_iterators::try_from_fn(|| {
        let Some(token) = tokens.next() else {
            return Ok(None);
        };
        Ok(Some(match &*token {
            "end" | "else" => {
                terminator = Some(token);
                return Ok(None);
            }
            "then" => {
                let (body, terminator) = instructions_until_terminator(tokens)?;
                match terminator.map(|t| t.text) {
                    Some("end") => Instruction::Then(body),
                    None => bail!(unterminated("`then` statement", token)),
                    Some("else") => {
                        let (else_, terminator) =
                            instructions_until_terminator(tokens)?;
                        match terminator {
                            Some(t) if &*t == "end" => {
                                Instruction::ThenElse(body, else_)
                            }
                            None => bail!(unterminated(
                                "`then else` statement",
                                token,
                            )),
                            Some(terminator) => {
                                bail!(unexpected_token(terminator))
                            }
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => token.try_into()?,
        }))
    })
    .collect::<Result<_>>()?;

    Ok((instructions, terminator))
}

fn is_keyword(token: &str) -> bool {
    matches!(token, "macro" | "then" | "else" | "end")
}

pub enum Instruction {
    Then(Box<[Instruction]>),
    ThenElse(Box<[Instruction]>, Box<[Instruction]>),
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

impl TryFrom<Token<'_>> for Instruction {
    type Error = anyhow::Error;

    fn try_from(token: Token<'_>) -> Result<Self> {
        use codemap_diagnostic::{Level, SpanLabel, SpanStyle};

        Ok(match &*token {
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
            _ => Self::Push(token.parse().map_err(|_| {
                diagnostics::Error(Diagnostic {
                    level: Level::Error,
                    message: format!("unknown instruction: `{token}`"),
                    code: None,
                    spans: vec![SpanLabel {
                        span: token.span,
                        label: None,
                        style: SpanStyle::Primary,
                    }],
                })
            })?),
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

fn unexpected_token(token: Token) -> diagnostics::Error {
    use codemap_diagnostic::{Level, SpanLabel, SpanStyle};

    diagnostics::Error(Diagnostic {
        level: Level::Error,
        message: format!("unexpected `{token}`"),
        code: None,
        spans: vec![SpanLabel {
            span: token.span,
            label: None,
            style: SpanStyle::Primary,
        }],
    })
}

fn unterminated(thing: &str, token: Token) -> diagnostics::Error {
    use codemap_diagnostic::{Level, SpanLabel, SpanStyle};

    diagnostics::Error(Diagnostic {
        level: Level::Error,
        message: format!("unterminated {thing}"),
        code: None,
        spans: vec![SpanLabel {
            span: token.span,
            label: None,
            style: SpanStyle::Primary,
        }],
    })
}
