use crate::{
    diagnostics::{self, primary_label, secondary_label},
    ir::{Block, Function, Instruction, Program},
    lexer::{lex, Token},
};
use anyhow::{bail, ensure, Result};
use codemap::Span;
use itertools::{process_results, Itertools};
use std::collections::HashMap;

pub fn parse(file: &codemap::File) -> Result<Program> {
    let tokens = expand_macros(lex(file));
    let functions = process_results(tokens, |tokens| {
        extra_iterators::batching_map(tokens, parse_function)
            .collect::<Result<_>>()
    })??;

    Ok(Program { functions })
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
                    "then" | "repeat" | "unsafe" => {
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
            _ => (token.into(), token.span),
        }))
    })
    .collect::<Result<_>>()?;

    Ok((instructions, terminator))
}

fn parse_function<'a>(
    mut tokens: &mut impl Iterator<Item = Token<'a>>,
    token: Token,
) -> Result<(&'a str, Function)> {
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
        name.text,
        Function {
            declaration_span: token.span.merge(name.span),
            parameters,
            returns,
            body,
            end_span: end.span,
        },
    ))
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
