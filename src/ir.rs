use crate::{
    diagnostics::{self, primary_label, secondary_label},
    lexer::{lex, Token},
};
use anyhow::{bail, ensure, Result};
use codemap::{Span, Spanned};
use itertools::{process_results, Itertools};
use std::collections::HashMap;

pub struct Program {
    pub instructions: Block,
}

impl Program {
    pub fn parse(file: &codemap::File) -> Result<Self> {
        let tokens = expand_macros(lex(file));
        let (instructions, terminator) =
            process_results(tokens, |mut tokens| {
                instructions_until_terminator(&mut tokens)
            })??;
        if let Some(terminator) = terminator {
            bail!(unexpected_token(terminator, "expected instruction"));
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
            let macro_token = token;
            let name = tokens.next().ok_or_else(|| {
                diagnostics::error(
                    "macro definition has no name".to_owned(),
                    vec![primary_label(token.span, "")],
                )
            })?;
            ensure!(
                !is_keyword(&name),
                diagnostics::error(
                    format!("keyword `{name}` cannot be used as a macro name"),
                    vec![primary_label(name.span, "")],
                ),
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
                    "macro" => Some(Err(diagnostics::error(
                        "nested macros are not supported".to_owned(),
                        vec![
                            primary_label(
                                token.span,
                                "inner macro starts here",
                            ),
                            secondary_label(
                                macro_token.span,
                                "outer macro starts here",
                            ),
                        ],
                    )
                    .into())),
                    "then" => {
                        layers += 1;
                        Some(Ok(vec![token]))
                    }
                    _ => Some(Ok(macros.get(&*token).map_or_else(
                        || vec![token],
                        |macro_: &Macro| macro_.body_with_span(token.span),
                    ))),
                })
                .flatten_ok()
                .collect::<Result<_>>()?;
            ensure!(found_end, unterminated("macro definition", token));
            let prev_definition = macros.insert(
                name.text,
                Macro {
                    declaration_span: macro_token.span.merge(name.span),
                    body,
                },
            );
            if let Some(prev_definition) = prev_definition {
                bail!(diagnostics::error(
                    format!("redefinition of macro `{name}`"),
                    vec![
                        primary_label(macro_token.span.merge(name.span), ""),
                        secondary_label(
                            prev_definition.declaration_span,
                            "previously defined here",
                        )
                    ],
                ));
            }
            Ok(Vec::new())
        }
        _ => Ok(macros.get(&*token).map_or_else(
            || vec![token],
            |macro_| macro_.body_with_span(token.span),
        )),
    })
    .flatten_ok()
}

struct Macro<'a> {
    declaration_span: Span,
    body: Vec<Token<'a>>,
}

impl<'a> Macro<'a> {
    fn body_with_span(&self, span: Span) -> Vec<Token<'a>> {
        self.body
            .iter()
            .map(|&token| Token { span, ..token })
            .collect()
    }
}

fn instructions_until_terminator<'a>(
    tokens: &mut impl Iterator<Item = Token<'a>>,
) -> Result<(Block, Option<Token<'a>>)> {
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
                let terminator = terminator
                    .ok_or_else(|| unterminated("`then` statement", token))?;
                match &*terminator {
                    "end" => Spanned {
                        span: token.span.merge(terminator.span),
                        node: Instruction::Then(body),
                    },
                    "else" => {
                        let (else_, terminator) =
                            instructions_until_terminator(tokens)?;
                        let terminator = terminator.ok_or_else(|| {
                            unterminated("`then else` statement", token)
                        })?;
                        match &*terminator {
                            "end" => Spanned {
                                span: token.span.merge(terminator.span),
                                node: Instruction::ThenElse(body, else_),
                            },
                            _ => bail!(unexpected_token(
                                terminator,
                                "expected `end`",
                            )),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => Spanned {
                span: token.span,
                node: token.try_into()?,
            },
        }))
    })
    .collect::<Result<_>>()?;

    Ok((instructions, terminator))
}

fn is_keyword(token: &str) -> bool {
    matches!(token, "macro" | "then" | "else" | "end")
}

type Block = Box<[Spanned<Instruction>]>;

pub enum Instruction {
    Then(Block),
    ThenElse(Block, Block),
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
                diagnostics::error(
                    format!("unknown instruction: `{token}`"),
                    vec![primary_label(token.span, "")],
                )
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

fn unexpected_token(
    token: Token,
    label: impl Into<String>,
) -> diagnostics::Error {
    diagnostics::error(
        format!("unexpected `{token}`"),
        vec![primary_label(token.span, label)],
    )
}

fn unterminated(thing: &str, token: Token) -> diagnostics::Error {
    diagnostics::error(
        format!("unterminated {thing}"),
        vec![primary_label(token.span, "")],
    )
}
