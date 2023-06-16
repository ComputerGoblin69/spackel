use crate::{
    diagnostics::{self, primary_label, secondary_label},
    lexer::{lex, Token},
    typ::Type,
};
use anyhow::{bail, ensure, Result};
use codemap::Span;
use itertools::{process_results, Itertools};
use std::collections::HashMap;

pub struct Program {
    pub functions: HashMap<String, Function>,
}

impl Program {
    pub fn parse(file: &codemap::File) -> Result<Self> {
        let tokens = expand_macros(lex(file));
        let functions = process_results(tokens, |tokens| {
            extra_iterators::batching_map(tokens, Function::parse)
                .collect::<Result<_>>()
        })??;

        Ok(Self { functions })
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
                    "then" | "repeat" => {
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
) -> Result<(Box<Block>, Option<Token<'a>>)> {
    let mut terminator = None;
    let instructions = extra_iterators::try_from_fn(|| {
        let Some(token) = tokens.next() else {
            return Ok(None);
        };
        Ok(Some(match &*token {
            "end" | "else" | "do" | ":" | "->" => {
                terminator = Some(token);
                return Ok(None);
            }
            "then" => {
                let (body, terminator) = instructions_until_terminator(tokens)?;
                let terminator = terminator
                    .ok_or_else(|| unterminated("`then` statement", token))?;
                match &*terminator {
                    "end" => (
                        Instruction::Then(body),
                        token.span.merge(terminator.span),
                    ),
                    "else" => {
                        let (else_, terminator) =
                            instructions_until_terminator(tokens)?;
                        let terminator = terminator.ok_or_else(|| {
                            unterminated("`then else` statement", token)
                        })?;
                        match &*terminator {
                            "end" => (
                                Instruction::ThenElse(body, else_),
                                token.span.merge(terminator.span),
                            ),
                            _ => bail!(unexpected_token(
                                terminator,
                                "expected `end`",
                            )),
                        }
                    }
                    _ => bail!(unexpected_token(
                        terminator,
                        "expected `else` or `end`",
                    )),
                }
            }
            "repeat" => {
                let (body, terminator) = instructions_until_terminator(tokens)?;
                let terminator = terminator
                    .ok_or_else(|| unterminated("`repeat` loop", token))?;
                match &*terminator {
                    "end" => (
                        Instruction::Repeat {
                            body,
                            end_span: terminator.span,
                        },
                        token.span.merge(terminator.span),
                    ),
                    _ => bail!(unexpected_token(terminator, "expected `end`",)),
                }
            }
            "unsafe" => {
                let (body, terminator) = instructions_until_terminator(tokens)?;
                let terminator = terminator
                    .ok_or_else(|| unterminated("`unsafe` block", token))?;
                match &*terminator {
                    "end" => (
                        Instruction::Unsafe(body),
                        token.span.merge(terminator.span),
                    ),
                    _ => bail!(unexpected_token(terminator, "expected `end`",)),
                }
            }
            _ => (token.try_into()?, token.span),
        }))
    })
    .collect::<Result<_>>()?;

    Ok((instructions, terminator))
}

pub struct Function {
    pub declaration_span: Span,
    pub parameters: Box<Block>,
    pub returns: Box<Block>,
    pub body: Box<Block>,
    pub end_span: Span,
}

impl Function {
    fn parse<'a>(
        mut tokens: &mut impl Iterator<Item = Token<'a>>,
        token: Token,
    ) -> Result<(String, Self)> {
        ensure!(
            *token == *"fn",
            unexpected_token(token, "expected function or macro definition")
        );

        let name = tokens.next().ok_or_else(|| {
            diagnostics::error(
                "function has no name".to_owned(),
                vec![primary_label(token.span, "")],
            )
        })?;
        ensure!(
            !is_keyword(&name),
            diagnostics::error(
                format!("keyword `{name}` cannot be used as a function name"),
                vec![primary_label(name.span, "")],
            ),
        );

        let colon = tokens
            .next()
            .ok_or_else(|| unterminated("function definition", token))?;
        ensure!(*colon == *":", unexpected_token(colon, "expected `:`"));

        let mut instructions_until_specific_terminator = |terminator| {
            let (instructions, Some(t)) =
                instructions_until_terminator(&mut tokens)?
            else {
                bail!(unterminated("function definition", token));
            };
            ensure!(
                &*t == terminator,
                unexpected_token(
                    t,
                    format!("expected instruction or `{terminator}`")
                )
            );
            Ok((instructions, t))
        };

        let parameters = instructions_until_specific_terminator("->")?.0;
        let returns = instructions_until_specific_terminator("do")?.0;
        let (body, end) = instructions_until_specific_terminator("end")?;

        Ok((
            name.text.to_owned(),
            Self {
                declaration_span: token.span.merge(name.span),
                parameters,
                returns,
                body,
                end_span: end.span,
            },
        ))
    }
}

fn is_keyword(token: &str) -> bool {
    matches!(
        token,
        "macro"
            | "then"
            | "else"
            | "repeat"
            | "end"
            | "do"
            | "fn"
            | ":"
            | "->"
            | "unsafe"
    )
}

pub type Block<T = Span> = [(Instruction<T>, T)];

#[derive(Clone, Debug)]
pub enum Instruction<T = Span> {
    Call(Box<str>),
    Then(Box<Block<T>>),
    ThenElse(Box<Block<T>>, Box<Block<T>>),
    Repeat { body: Box<Block<T>>, end_span: Span },
    Unsafe(Box<Block<T>>),
    PushI32(i32),
    PushF32(f32),
    PushBool(bool),
    PushType(Type),
    Ptr,
    TypeOf,
    Print,
    Println,
    PrintChar,
    BinMathOp(BinMathOp),
    Sqrt,
    Comparison(Comparison),
    Not,
    BinLogicOp(BinLogicOp),
    AddrOf,
    ReadPtr,
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
            "true" => Self::PushBool(true),
            "false" => Self::PushBool(false),
            "i32" => Self::PushType(Type::I32),
            "bool" => Self::PushType(Type::Bool),
            "type" => Self::PushType(Type::Type),
            "ptr" => Self::Ptr,
            "type-of" => Self::TypeOf,
            "print" => Self::Print,
            "println" => Self::Println,
            "print-char" => Self::PrintChar,
            "+" => Self::BinMathOp(BinMathOp::Add),
            "-" => Self::BinMathOp(BinMathOp::Sub),
            "*" => Self::BinMathOp(BinMathOp::Mul),
            "/" => Self::BinMathOp(BinMathOp::Div),
            "%" => Self::BinMathOp(BinMathOp::Rem),
            "+🤡" => Self::BinMathOp(BinMathOp::SillyAdd),
            "sqrt" => Self::Sqrt,
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
            "addr-of" => Self::AddrOf,
            "read-ptr" => Self::ReadPtr,
            "ß" => Self::PushI32(1945),
            "drop" => Self::Drop,
            "dup" => Self::Dup,
            "swap" => Self::Swap,
            "over" => Self::Over,
            "nip" => Self::Nip,
            "tuck" => Self::Tuck,
            _ =>
            {
                #[allow(clippy::option_if_let_else)]
                if let Ok(number) = token.parse::<i32>() {
                    Self::PushI32(number)
                } else if let Ok(number) = token.parse::<f32>() {
                    Self::PushF32(number)
                } else {
                    Self::Call(token.text.into())
                }
            }
        })
    }
}

impl<T> Instruction<T> {
    pub const fn is_unsafe(&self) -> bool {
        matches!(self, Self::ReadPtr)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinMathOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    SillyAdd,
}

#[derive(Clone, Copy, Debug)]
pub enum Comparison {
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
}

#[derive(Clone, Copy, Debug)]
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
