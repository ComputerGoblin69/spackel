use crate::{
    diagnostics::{self, primary_label},
    ir::{BinMathOp, Function, Instruction, Program},
};
use anyhow::{ensure, Context, Result};
use codemap::Span;
use itertools::Itertools;
use std::{
    collections::HashMap,
    fmt::{self, Write as _},
};

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    I32,
    F32,
    Type,
    Ptr(Box<Self>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            f.write_char('`')?;
        }
        match self {
            Self::Bool => f.write_str("bool"),
            Self::I32 => f.write_str("i32"),
            Self::F32 => f.write_str("f32"),
            Self::Type => f.write_str("type"),
            Self::Ptr(inner) => write!(f, "{inner} ptr"),
        }?;
        if f.alternate() {
            f.write_char('`')?;
        }
        Ok(())
    }
}

pub type Generics = Box<[Type]>;

pub struct CheckedProgram {
    pub functions: HashMap<String, CheckedFunction>,
}

pub struct CheckedFunction {
    declaration_span: Span,
    pub signature: FunctionSignature,
    pub body: Box<[(Instruction<Generics>, Generics)]>,
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub parameters: Box<[Type]>,
    pub returns: Box<[Type]>,
}

pub fn check(program: Program) -> Result<CheckedProgram> {
    let function_signatures = program
        .functions
        .iter()
        .map(|(name, function)| {
            let parameters = function
                .parameters
                .iter()
                .map(|(param, span)| match param {
                    Instruction::PushType(typ) => Ok(typ.clone()),
                    _ => Err(diagnostics::error(
                        "unsupported instruction in function signature"
                            .to_owned(),
                        vec![primary_label(*span, "")],
                    )),
                })
                .collect::<Result<Box<_>, _>>()?;
            let returns = function
                .returns
                .iter()
                .map(|(param, span)| match param {
                    Instruction::PushType(typ) => Ok(typ.clone()),
                    _ => Err(diagnostics::error(
                        "unsupported instruction in function signature"
                            .to_owned(),
                        vec![primary_label(*span, "")],
                    )),
                })
                .collect::<Result<Box<_>, _>>()?;
            Ok((
                name.clone(),
                FunctionSignature {
                    parameters,
                    returns,
                },
            ))
        })
        .collect::<Result<_>>()?;

    Checker {
        stack: Vec::new(),
        function_signatures,
    }
    .check(program)
}

struct Checker {
    stack: Vec<Type>,
    function_signatures: HashMap<String, FunctionSignature>,
}

impl Checker {
    fn check(&mut self, program: Program) -> Result<CheckedProgram> {
        let checked_program = CheckedProgram {
            functions: program
                .functions
                .into_iter()
                .map(|(name, function)| {
                    let checked_function =
                        self.check_function(&name, function)?;
                    Ok((name, checked_function))
                })
                .collect::<Result<_>>()?,
        };

        let main = checked_program
            .functions
            .get("main")
            .context("program has no `main` function")?;
        ensure!(
            main.signature.parameters.is_empty()
                && main.signature.returns.is_empty(),
            diagnostics::error(
                "`main` function has wrong signature".to_owned(),
                vec![primary_label(main.declaration_span, "defined here")]
            )
            .note("`main` must have no parameters and no return values")
        );

        Ok(checked_program)
    }

    fn check_function(
        &mut self,
        name: &str,
        function: Function,
    ) -> Result<CheckedFunction> {
        self.stack = self.function_signatures[name].parameters.to_vec();
        let body = function
            .body
            .into_vec()
            .into_iter()
            .map(|instruction| self.check_instruction(instruction))
            .collect::<Result<_>>()?;

        self.transform(
            &[],
            &self.function_signatures[name]
                .returns
                .iter()
                .cloned()
                .map(Pattern::Concrete)
                .collect::<Box<_>>(),
            &[],
            function.end_span,
        )?;
        ensure!(
            self.stack.is_empty(),
            diagnostics::error(
                format!(
                    "there are values left on the stack with the following types: `{}`",
                    self.stack.iter().format(" ")
                ),
                vec![primary_label(function.end_span, "")]
            )
        );

        Ok(CheckedFunction {
            declaration_span: function.declaration_span,
            signature: self.function_signatures[name].clone(),
            body,
        })
    }

    fn transform(
        &mut self,
        generics: &[Generic],
        parameters: &[Pattern],
        returns: &[Pattern],
        span: Span,
    ) -> Result<Generics> {
        Signature {
            generics,
            parameters,
            returns,
        }
        .apply(self)
        .map_err(|()| {
            let mut label = format!(
                "expected types `{}` ",
                parameters
                    .iter()
                    .map(|pattern| pattern.display(generics))
                    .format(" "),
            );
            if !generics.is_empty() {
                write!(label, "for some {:#} ", generics.iter().format(", "))
                    .unwrap();
            }
            write!(label, "but got `{}`", self.stack.iter().format(" "))
                .unwrap();

            diagnostics::error(
                "type mismatch".to_owned(),
                vec![primary_label(span, label)],
            )
            .into()
        })
    }

    fn check_instruction(
        &mut self,
        (instruction, span): (Instruction, Span),
    ) -> Result<(Instruction<Generics>, Generics)> {
        use Constraint::Any;
        use Generic as any;
        use Pattern::{Concrete as C, Generic as G};
        use Type::{Bool, F32, I32};

        let parameters;
        let returns;
        let (g, i, o): (&[_], &[Pattern], &[Pattern]) = match &instruction {
            Instruction::Call(name) => {
                ensure!(
                    **name != *"main",
                    diagnostics::error(
                        "`main` cannot be called".to_owned(),
                        vec![primary_label(span, "")]
                    ).note("`main` implicitly returns the program exit code, making its signature not match up with what the source code indicates")
                );

                let signature =
                    self.function_signatures.get(&**name).ok_or_else(|| {
                        diagnostics::error(
                            format!("unknown instruction: `{name}`"),
                            vec![primary_label(span, "")],
                        )
                    })?;
                parameters = signature
                    .parameters
                    .iter()
                    .cloned()
                    .map(C)
                    .collect::<Box<_>>();
                returns = signature
                    .returns
                    .iter()
                    .cloned()
                    .map(C)
                    .collect::<Box<_>>();
                (&[], &*parameters, &*returns)
            }
            Instruction::Then(_) | Instruction::ThenElse(..) => {
                (&[], &[C(Bool)], &[])
            }
            Instruction::Repeat { .. } => (&[], &[], &[]),
            Instruction::PushI32(_) => (&[], &[], &[C(I32)]),
            Instruction::PushF32(_) => (&[], &[], &[C(F32)]),
            Instruction::PushBool(_) => (&[], &[], &[C(Bool)]),
            Instruction::PushType(_) => (&[], &[], &[C(Type::Type)]),
            Instruction::TypeOf => {
                (&[any('T', Any)], &[G(0)], &[C(Type::Type)])
            }
            Instruction::BinMathOp(
                BinMathOp::Add
                | BinMathOp::Sub
                | BinMathOp::Mul
                | BinMathOp::Div,
            ) => (
                &[Generic('N', Constraint::OneOf(&[I32, F32]))],
                &[G(0), G(0)],
                &[G(0)],
            ),
            Instruction::BinMathOp(_) => (&[], &[C(I32), C(I32)], &[C(I32)]),
            Instruction::Sqrt => (&[], &[C(F32)], &[C(F32)]),
            Instruction::Comparison(_) => (&[], &[C(I32), C(I32)], &[C(Bool)]),
            Instruction::Print | Instruction::Println => (
                &[Generic('T', Constraint::OneOf(&[I32, F32]))],
                &[G(0)],
                &[],
            ),
            Instruction::PrintChar => (&[], &[C(I32)], &[]),
            Instruction::Not => (&[], &[C(Bool)], &[C(Bool)]),
            Instruction::BinLogicOp(_) => {
                (&[], &[C(Bool), C(Bool)], &[C(Bool)])
            }
            Instruction::Drop => (&[any('T', Any)], &[G(0)], &[]),
            Instruction::Dup => (&[any('T', Any)], &[G(0)], &[G(0), G(0)]),
            Instruction::Swap => (
                &[any('A', Any), any('B', Any)],
                &[G(0), G(1)],
                &[G(1), G(0)],
            ),
            Instruction::Over => (
                &[any('A', Any), any('B', Any)],
                &[G(0), G(1)],
                &[G(0), G(1), G(0)],
            ),
            Instruction::Nip => {
                (&[any('A', Any), any('B', Any)], &[G(0), G(1)], &[G(1)])
            }
            Instruction::Tuck => (
                &[any('A', Any), any('B', Any)],
                &[G(0), G(1)],
                &[G(1), G(0), G(1)],
            ),
        };
        let generics = self.transform(g, i, o, span)?;

        let instruction = match instruction {
            Instruction::Then(body) => {
                let before = self.stack.clone();
                let body = body
                    .into_vec()
                    .into_iter()
                    .map(|instruction| self.check_instruction(instruction))
                    .collect::<Result<_>>()?;
                ensure!(
                    before == self.stack,
                    diagnostics::error(
                        format!(
                            "`then` statement changes types from `{}` to `{}`",
                            before.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![primary_label(span, "")],
                    ),
                );
                Instruction::Then(body)
            }
            Instruction::ThenElse(then, else_) => {
                let before = self.stack.clone();
                let then = then
                    .into_vec()
                    .into_iter()
                    .map(|instruction| self.check_instruction(instruction))
                    .collect::<Result<_>>()?;
                let then_types = std::mem::replace(&mut self.stack, before);
                let else_ = else_
                    .into_vec()
                    .into_iter()
                    .map(|instruction| self.check_instruction(instruction))
                    .collect::<Result<_>>()?;
                ensure!(
                    then_types == self.stack,
                    diagnostics::error(
                        format!(
                            "`then else` statement diverges between types `{}` and `{}`",
                            then_types.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![primary_label(span, "")],
                    ),
                );
                Instruction::ThenElse(then, else_)
            }
            Instruction::Repeat { body, end_span } => {
                let before = self.stack.clone();
                let body = body
                    .into_vec()
                    .into_iter()
                    .map(|instruction| self.check_instruction(instruction))
                    .collect::<Result<_>>()?;
                self.transform(&[], &[C(Bool)], &[], end_span)?;
                ensure!(
                    before == self.stack,
                    diagnostics::error(
                        format!(
                            "`repeat` loop changes types from `{}` to `{}`",
                            before.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![primary_label(span, "")],
                    ),
                );
                Instruction::Repeat { body, end_span }
            }
            Instruction::Call(name) => Instruction::Call(name),
            Instruction::PushI32(n) => Instruction::PushI32(n),
            Instruction::PushF32(n) => Instruction::PushF32(n),
            Instruction::PushBool(b) => Instruction::PushBool(b),
            Instruction::PushType(typ) => Instruction::PushType(typ),
            Instruction::TypeOf => Instruction::TypeOf,
            Instruction::Print => Instruction::Print,
            Instruction::Println => Instruction::Println,
            Instruction::PrintChar => Instruction::PrintChar,
            Instruction::BinMathOp(op) => Instruction::BinMathOp(op),
            Instruction::Sqrt => Instruction::Sqrt,
            Instruction::Comparison(comparison) => {
                Instruction::Comparison(comparison)
            }
            Instruction::Not => Instruction::Not,
            Instruction::BinLogicOp(op) => Instruction::BinLogicOp(op),
            Instruction::Drop => Instruction::Drop,
            Instruction::Dup => Instruction::Dup,
            Instruction::Swap => Instruction::Swap,
            Instruction::Over => Instruction::Over,
            Instruction::Nip => Instruction::Nip,
            Instruction::Tuck => Instruction::Tuck,
        };
        Ok((instruction, generics))
    }
}

struct Signature<'a> {
    generics: &'a [Generic],
    parameters: &'a [Pattern],
    returns: &'a [Pattern],
}

impl Signature<'_> {
    fn apply(&self, checker: &mut Checker) -> Result<Generics, ()> {
        if checker.stack.len() < self.parameters.len() {
            return Err(());
        }
        let new_len = checker.stack.len() - self.parameters.len();
        let consumed = &checker.stack[new_len..];

        let mut generics = Vec::new();

        for (parameter, argument) in std::iter::zip(self.parameters, consumed) {
            match parameter {
                Pattern::Concrete(typ) => {
                    if *argument != *typ {
                        return Err(());
                    }
                }
                Pattern::Generic(i) => {
                    if let Constraint::OneOf(possibilities) =
                        self.generics[usize::from(*i)].1
                    {
                        if !possibilities.contains(argument) {
                            return Err(());
                        }
                    }
                    if let Some(generic) = generics.get(usize::from(*i)) {
                        if *argument != *generic {
                            return Err(());
                        }
                    } else {
                        generics.push(argument.clone());
                    }
                }
            }
        }

        let consumed = checker.stack.split_off(new_len);
        checker
            .stack
            .extend(self.returns.iter().map(|out| match out {
                Pattern::Concrete(typ) => typ.clone(),
                Pattern::Generic(i) => consumed[usize::from(*i)].clone(),
            }));

        Ok(generics.into())
    }
}

#[derive(Clone)]
enum Pattern {
    Concrete(Type),
    Generic(u8),
}

impl Pattern {
    const fn display<'a>(&'a self, generics: &'a [Generic]) -> DisplayPattern {
        DisplayPattern {
            pattern: self,
            generics,
        }
    }
}

struct DisplayPattern<'a> {
    pattern: &'a Pattern,
    generics: &'a [Generic],
}

impl fmt::Display for DisplayPattern<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pattern {
            Pattern::Concrete(typ) => typ.fmt(f),
            Pattern::Generic(i) => self.generics[usize::from(*i)].fmt(f),
        }
    }
}

struct Generic(char, Constraint);

impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            f.write_char('`')?;
        }
        write!(f, "<{}>", self.0)?;
        if f.alternate() {
            f.write_char('`')?;
            if let Constraint::OneOf(possibilities) = self.1 {
                write!(f, " in {{{:#}}}", possibilities.iter().format(", "))?;
            }
        }
        Ok(())
    }
}

enum Constraint {
    Any,
    OneOf(&'static [Type]),
}
