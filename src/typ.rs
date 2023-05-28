use crate::{
    diagnostics::{self, primary_label},
    ir::{Function, Instruction, Program},
};
use anyhow::{ensure, Context, Result};
use codemap::{Span, Spanned};
use itertools::Itertools;
use std::{
    collections::HashMap,
    fmt::{self, Write as _},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Bool,
    I32,
    F32,
    Type,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bool => "bool",
            Self::I32 => "i32",
            Self::F32 => "f32",
            Self::Type => "type",
        })
    }
}

pub struct CheckedProgram {
    functions: HashMap<String, CheckedFunction>,
}

impl CheckedProgram {
    pub const fn functions(&self) -> &HashMap<String, CheckedFunction> {
        &self.functions
    }
}

pub struct CheckedFunction {
    declaration_span: Span,
    pub signature: FunctionSignature,
    pub body: Box<[Spanned<Instruction>]>,
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
                .map(|param| match &**param {
                    Instruction::PushType(typ) => Ok(*typ),
                    _ => Err(diagnostics::error(
                        "unsupported instruction in function signature"
                            .to_owned(),
                        vec![primary_label(param.span, "")],
                    )),
                })
                .collect::<Result<Box<_>, _>>()?;
            let returns = function
                .returns
                .iter()
                .map(|param| match &**param {
                    Instruction::PushType(typ) => Ok(*typ),
                    _ => Err(diagnostics::error(
                        "unsupported instruction in function signature"
                            .to_owned(),
                        vec![primary_label(param.span, "")],
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
        for instruction in &*function.body {
            self.check_instruction(instruction)?;
        }

        self.transform(
            &[],
            &self.function_signatures[name]
                .returns
                .iter()
                .copied()
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
            body: function.body,
        })
    }

    fn transform(
        &mut self,
        generics: &[Generic],
        parameters: &[Pattern],
        returns: &[Pattern],
        span: Span,
    ) -> Result<()> {
        Signature {
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
        instruction: &Spanned<Instruction>,
    ) -> Result<()> {
        use Generic as any;
        use Pattern::{Concrete as C, Generic as G};
        use Type::{Bool, F32, I32};

        let parameters;
        let returns;
        let (g, i, o): (&[_], &[Pattern], &[Pattern]) = match &**instruction {
            Instruction::Call(name) => {
                ensure!(
                    **name != *"main",
                    diagnostics::error(
                        "`main` cannot be called".to_owned(),
                        vec![primary_label(instruction.span, "")]
                    ).note("`main` implicitly returns the program exit code, making its signature not match up with what the source code indicates")
                );

                let signature =
                    self.function_signatures.get(&**name).ok_or_else(|| {
                        diagnostics::error(
                            format!("unknown instruction: `{name}`"),
                            vec![primary_label(instruction.span, "")],
                        )
                    })?;
                parameters = signature
                    .parameters
                    .iter()
                    .copied()
                    .map(C)
                    .collect::<Box<_>>();
                returns = signature
                    .returns
                    .iter()
                    .copied()
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
            Instruction::TypeOf => (&[any('T')], &[G(0)], &[C(Type::Type)]),
            Instruction::BinMathOp(_) => (&[], &[C(I32); 2], &[C(I32)]),
            Instruction::F32BinMathOp(_) => (&[], &[C(F32); 2], &[C(F32)]),
            Instruction::Sqrt => (&[], &[C(F32)], &[C(F32)]),
            Instruction::Comparison(_) => (&[], &[C(I32); 2], &[C(Bool)]),
            Instruction::Print
            | Instruction::Println
            | Instruction::PrintChar => (&[], &[C(I32)], &[]),
            Instruction::Not => (&[], &[C(Bool)], &[C(Bool)]),
            Instruction::BinLogicOp(_) => (&[], &[C(Bool); 2], &[C(Bool)]),
            Instruction::Drop => (&[any('T')], &[G(0)], &[]),
            Instruction::Dup => (&[any('T')], &[G(0)], &[G(0); 2]),
            Instruction::Swap => {
                (&[any('A'), any('B')], &[G(0), G(1)], &[G(1), G(0)])
            }
            Instruction::Over => {
                (&[any('A'), any('B')], &[G(0), G(1)], &[G(0), G(1), G(0)])
            }
            Instruction::Nip => (&[any('A'), any('B')], &[G(0), G(1)], &[G(1)]),
            Instruction::Tuck => {
                (&[any('A'), any('B')], &[G(0), G(1)], &[G(1), G(0), G(1)])
            }
        };
        self.transform(g, i, o, instruction.span)?;

        match &**instruction {
            Instruction::Then(body) => {
                let before = self.stack.clone();
                for instruction in &**body {
                    self.check_instruction(instruction)?;
                }
                ensure!(
                    before == self.stack,
                    diagnostics::error(
                        format!(
                            "`then` statement changes types from `{}` to `{}`",
                            before.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![primary_label(instruction.span, "")],
                    ),
                );
            }
            Instruction::ThenElse(then, else_) => {
                let before = self.stack.clone();
                for instruction in &**then {
                    self.check_instruction(instruction)?;
                }
                let then_types = std::mem::replace(&mut self.stack, before);
                for instruction in &**else_ {
                    self.check_instruction(instruction)?;
                }
                ensure!(
                    then_types == self.stack,
                    diagnostics::error(
                        format!(
                            "`then else` statement diverges between types `{}` and `{}`",
                            then_types.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![primary_label(instruction.span, "")],
                    ),
                );
            }
            Instruction::Repeat { body, end_span } => {
                let before = self.stack.clone();
                for instruction in &**body {
                    self.check_instruction(instruction)?;
                }
                self.transform(&[], &[C(Bool)], &[], *end_span)?;
                ensure!(
                    before == self.stack,
                    diagnostics::error(
                        format!(
                            "`repeat` loop changes types from `{}` to `{}`",
                            before.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![primary_label(instruction.span, "")],
                    ),
                );
            }
            _ => {}
        }

        Ok(())
    }
}

struct Signature<'a> {
    parameters: &'a [Pattern],
    returns: &'a [Pattern],
}

impl Signature<'_> {
    fn apply(&self, checker: &mut Checker) -> Result<(), ()> {
        if checker.stack.len() < self.parameters.len() {
            return Err(());
        }
        let new_len = checker.stack.len() - self.parameters.len();
        let consumed = &checker.stack[new_len..];

        let mut generics = Vec::new();

        for (parameter, &argument) in std::iter::zip(self.parameters, consumed)
        {
            match parameter {
                Pattern::Concrete(typ) => {
                    if argument != *typ {
                        return Err(());
                    }
                }
                Pattern::Generic(i) => {
                    if let Some(generic) = generics.get(usize::from(*i)) {
                        if argument != *generic {
                            return Err(());
                        }
                    } else {
                        generics.push(argument);
                    }
                }
            }
        }

        let consumed = checker.stack.split_off(new_len);
        checker
            .stack
            .extend(self.returns.iter().map(|out| match out {
                Pattern::Concrete(typ) => *typ,
                Pattern::Generic(i) => consumed[usize::from(*i)],
            }));

        Ok(())
    }
}

#[derive(Clone, Copy)]
enum Pattern {
    Concrete(Type),
    Generic(u8),
}

impl Pattern {
    const fn display(self, generics: &[Generic]) -> DisplayPattern {
        DisplayPattern {
            pattern: self,
            generics,
        }
    }
}

struct DisplayPattern<'a> {
    pattern: Pattern,
    generics: &'a [Generic],
}

impl fmt::Display for DisplayPattern<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pattern {
            Pattern::Concrete(typ) => typ.fmt(f),
            Pattern::Generic(i) => self.generics[usize::from(i)].fmt(f),
        }
    }
}

struct Generic(char);

impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            f.write_char('`')?;
        }
        write!(f, "<{}>", self.0)?;
        if f.alternate() {
            f.write_char('`')?;
        }
        Ok(())
    }
}
