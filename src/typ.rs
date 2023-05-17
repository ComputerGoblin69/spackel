use crate::{
    diagnostics,
    ir::{Instruction, Program},
};
use anyhow::{ensure, Result};
use codemap::Span;
use codemap_diagnostic::{SpanLabel, SpanStyle};
use itertools::Itertools;
use std::{fmt, ops::Deref};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Type {
    Bool,
    I32,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bool => "bool",
            Self::I32 => "i32",
        })
    }
}

pub struct Checked<T>(T);

impl<T> Deref for Checked<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn check(program: Program) -> Result<Checked<Program>> {
    Checker { stack: Vec::new() }.check(program)
}

struct Checker {
    stack: Vec<Type>,
}

impl Checker {
    fn check(&mut self, program: Program) -> Result<Checked<Program>> {
        for (span, instruction) in &*program.instructions {
            self.check_instruction(instruction, *span)?;
        }
        ensure!(
            self.stack.is_empty(),
            "there are values left on the stack with the following types: `{}`",
            self.stack.iter().format(" ")
        );
        Ok(Checked(program))
    }

    fn transform(
        &mut self,
        inputs: &[Parameter],
        outputs: &[Return],
        span: Span,
    ) -> Result<()> {
        ensure!(
            (self.stack.len() >= inputs.len()
                && self.stack[self.stack.len() - inputs.len()..] == *inputs),
            diagnostics::error(
                "type mismatch".to_owned(),
                vec![SpanLabel {
                    span,
                    label: Some(format!(
                        "expected types `{}` but got `{}`",
                        inputs.iter().format(" "),
                        self.stack.iter().format(" ")
                    )),
                    style: SpanStyle::Primary
                }]
            ),
        );
        let new_len = self.stack.len() - inputs.len();
        let consumed = self.stack.split_off(new_len);

        self.stack.extend(outputs.iter().map(|&out| match out {
            Return::Concrete(typ) => typ,
            Return::Get(i) => consumed[i],
        }));
        Ok(())
    }

    fn check_instruction(
        &mut self,
        instruction: &Instruction,
        span: Span,
    ) -> Result<()> {
        use Parameter::{Any, Concrete as P};
        use Return::{Concrete as R, Get};
        use Type::{Bool, I32};

        let (inputs, outputs): (&[Parameter], &[Return]) = match instruction {
            Instruction::Then(_) | Instruction::ThenElse(..) => {
                (&[P(Bool)], &[])
            }
            Instruction::Push(_) => (&[], &[R(I32)]),
            Instruction::True | Instruction::False => (&[], &[R(Bool)]),
            Instruction::BinMathOp(_) => (&[P(I32); 2], &[R(I32)]),
            Instruction::Comparison(_) => (&[P(I32); 2], &[R(Bool)]),
            Instruction::Print
            | Instruction::Println
            | Instruction::PrintChar => (&[P(I32)], &[]),
            Instruction::Not => (&[P(Bool)], &[R(Bool)]),
            Instruction::BinLogicOp(_) => (&[P(Bool); 2], &[R(Bool)]),
            Instruction::Drop => (&[Any], &[]),
            Instruction::Dup => (&[Any], &[Get(0); 2]),
            Instruction::Swap => (&[Any; 2], &[Get(1), Get(0)]),
            Instruction::Over => (&[Any; 2], &[Get(0), Get(1), Get(0)]),
            Instruction::Nip => (&[Any; 2], &[Get(1)]),
            Instruction::Tuck => (&[Any; 2], &[Get(1), Get(0), Get(1)]),
        };
        self.transform(inputs, outputs, span)?;

        match instruction {
            Instruction::Then(body) => {
                let before = self.stack.clone();
                for (span, instruction) in &**body {
                    self.check_instruction(instruction, *span)?;
                }
                ensure!(
                    before == self.stack,
                    diagnostics::error(
                        format!(
                            "`then` statement changes types from `{}` to `{}`",
                            before.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![SpanLabel {
                            span,
                            label: None,
                            style: SpanStyle::Primary
                        }]
                    ),
                );
            }
            Instruction::ThenElse(then, else_) => {
                let before = self.stack.clone();
                for (span, instruction) in &**then {
                    self.check_instruction(instruction, *span)?;
                }
                let then_types = std::mem::replace(&mut self.stack, before);
                for (span, instruction) in &**else_ {
                    self.check_instruction(instruction, *span)?;
                }
                ensure!(
                    then_types == self.stack,
                    diagnostics::error(
                        format!(
                            "`then else` statement diverges between types `{}` and `{}`",
                            then_types.iter().format(" "),
                            self.stack.iter().format(" "),
                        ),
                        vec![SpanLabel {
                            span,
                            label: None,
                            style: SpanStyle::Primary
                        }]
                    ),
                );
            }
            _ => {}
        }

        Ok(())
    }
}

#[derive(Clone, Copy)]
enum Parameter {
    Concrete(Type),
    Any,
}

impl PartialEq<Parameter> for Type {
    fn eq(&self, other: &Parameter) -> bool {
        match other {
            Parameter::Concrete(typ) => self == typ,
            Parameter::Any => true,
        }
    }
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Concrete(typ) => typ.fmt(f),
            Self::Any => f.write_str("<any>"),
        }
    }
}

#[derive(Clone, Copy)]
enum Return {
    Concrete(Type),
    Get(usize),
}
