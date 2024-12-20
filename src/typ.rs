use crate::{
    diagnostics::{self, primary_label},
    ir::{BinMathOp, Block, Function, Instruction, Program},
};
use anyhow::{ensure, Result};
use codemap::Span;
use itertools::Itertools;
use std::{
    collections::BTreeMap,
    fmt::{self, Write as _},
};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Bool,
    I32,
    F32,
    #[expect(clippy::enum_variant_names, reason = "`Type` is a type")]
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

pub struct CheckedProgram<'src> {
    pub function_signatures: BTreeMap<&'src str, FunctionSignature>,
    pub function_bodies: BTreeMap<&'src str, Box<Block<Generics>>>,
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub parameters: Box<[Type]>,
    pub returns: Box<[Type]>,
}

pub fn check(program: Program) -> Result<CheckedProgram> {
    ensure!(
        program.functions.contains_key("main"),
        "program has no `main` function"
    );

    let function_signatures = program
        .functions
        .iter()
        .map(|(name, function)| {
            Ok((*name, check_function_signature(name, function)?))
        })
        .collect::<Result<_>>()?;

    Checker {
        stack: Vec::new(),
        function_signatures,
        unsafe_layers: 0,
    }
    .check(program)
}

fn check_function_signature(
    name: &str,
    function: &Function,
) -> Result<FunctionSignature> {
    let parameters = check_type_stack(&function.parameters)?;
    let returns = check_type_stack(&function.returns)?;

    if name == "main" {
        ensure!(
            parameters.is_empty() && returns.is_empty(),
            diagnostics::error(
                "`main` function has wrong signature".to_owned(),
                vec![primary_label(function.declaration_span, "defined here")]
            )
            .note("`main` must have no parameters and no return values")
        );
    }

    Ok(FunctionSignature {
        parameters,
        returns,
    })
}

fn check_type_stack(instructions: &Block<Span>) -> Result<Box<[Type]>> {
    instructions
        .iter()
        .map(|(param, span)| match param {
            Instruction::PushType(typ) => Ok(typ.clone()),
            _ => Err(diagnostics::error(
                "unsupported instruction in function signature".to_owned(),
                vec![primary_label(*span, "")],
            )
            .into()),
        })
        .collect::<Result<Box<_>>>()
}

struct Checker<'src> {
    stack: Vec<Type>,
    function_signatures: BTreeMap<&'src str, FunctionSignature>,
    unsafe_layers: usize,
}

impl<'src> Checker<'src> {
    fn check(mut self, program: Program<'src>) -> Result<CheckedProgram<'src>> {
        let function_bodies = program
            .functions
            .into_iter()
            .map(|(name, function)| {
                let body = self.check_function(name, function)?;
                Ok((name, body))
            })
            .collect::<Result<_>>()?;

        Ok(CheckedProgram {
            function_signatures: self.function_signatures,
            function_bodies,
        })
    }

    fn check_function(
        &mut self,
        name: &str,
        function: Function,
    ) -> Result<Box<Block<Generics>>> {
        self.stack = self.function_signatures[name].parameters.to_vec();
        let body = Box::into_iter(function.body)
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

        Ok(body)
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
        use Pattern::{Concrete as C, Generic as G, Ptr};
        use Type::{Bool, F32, I32};

        ensure!(
            !(instruction.is_unsafe() && self.unsafe_layers == 0),
            diagnostics::error(
                "unsafe instruction used in safe context".to_owned(),
                vec![primary_label(span, "")]
            )
        );

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
            Instruction::Repeat { .. } | Instruction::Unsafe(_) => {
                (&[], &[], &[])
            }
            Instruction::PushI32(_) => (&[], &[], &[C(I32)]),
            Instruction::PushF32(_) => (&[], &[], &[C(F32)]),
            Instruction::PushBool(_) => (&[], &[], &[C(Bool)]),
            Instruction::PushType(_) => (&[], &[], &[C(Type::Type)]),
            Instruction::Ptr => (&[], &[C(Type::Type)], &[C(Type::Type)]),
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
            Instruction::AddrOf => {
                (&[any('T', Any)], &[G(0)], &[G(0), Ptr(&G(0))])
            }
            Instruction::ReadPtr => (&[any('T', Any)], &[Ptr(&G(0))], &[G(0)]),
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
                let body = Box::into_iter(body)
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
                let then = Box::into_iter(then)
                    .map(|instruction| self.check_instruction(instruction))
                    .collect::<Result<_>>()?;
                let then_types = std::mem::replace(&mut self.stack, before);
                let else_ = Box::into_iter(else_)
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
                let body = Box::into_iter(body)
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
            Instruction::Unsafe(body) => {
                self.unsafe_layers += 1;
                let body = Box::into_iter(body)
                    .map(|instruction| self.check_instruction(instruction))
                    .collect::<Result<_>>()?;
                self.unsafe_layers -= 1;
                Instruction::Unsafe(body)
            }
            Instruction::Call(name) => Instruction::Call(name),
            Instruction::PushI32(n) => Instruction::PushI32(n),
            Instruction::PushF32(n) => Instruction::PushF32(n),
            Instruction::PushBool(b) => Instruction::PushBool(b),
            Instruction::PushType(typ) => Instruction::PushType(typ),
            Instruction::Ptr => Instruction::Ptr,
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
            Instruction::AddrOf => Instruction::AddrOf,
            Instruction::ReadPtr => Instruction::ReadPtr,
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

        for (mut parameter, mut argument) in
            std::iter::zip(self.parameters, consumed)
        {
            while let Pattern::Ptr(inner) = parameter {
                let Type::Ptr(argument_pointee) = argument else {
                    return Err(());
                };
                argument = argument_pointee;
                parameter = inner;
            }

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
                Pattern::Ptr(_) => unreachable!(),
            }
        }

        checker.stack.truncate(new_len);
        checker.stack.extend(
            self.returns.iter().map(|pattern| pattern.reify(&generics)),
        );

        Ok(generics.into())
    }
}

#[derive(Clone)]
enum Pattern {
    Concrete(Type),
    Generic(u8),
    Ptr(&'static Self),
}

impl Pattern {
    const fn display<'a>(
        &'a self,
        generics: &'a [Generic],
    ) -> DisplayPattern<'a> {
        DisplayPattern {
            pattern: self,
            generics,
        }
    }

    fn reify(&self, resolved_generics: &[Type]) -> Type {
        match self {
            Self::Concrete(typ) => typ.clone(),
            Self::Generic(i) => resolved_generics[usize::from(*i)].clone(),
            Self::Ptr(inner) => {
                Type::Ptr(Box::new(inner.reify(resolved_generics)))
            }
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
            Pattern::Ptr(inner) => {
                write!(f, "{} ptr", inner.display(self.generics))
            }
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
