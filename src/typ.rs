use crate::{
    diagnostics::{self, primary_label},
    ir::{Instruction, Program},
};
use anyhow::{ensure, Result};
use codemap::{Span, Spanned};
use itertools::Itertools;
use std::{fmt, ops::Deref};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Bool,
    I32,
    Type,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bool => "bool",
            Self::I32 => "i32",
            Self::Type => "type",
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
        for instruction in &*program.instructions {
            self.check_instruction(instruction)?;
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
                vec![primary_label(
                    span,
                    format!(
                        "expected types `{}` but got `{}`",
                        inputs.iter().format(" "),
                        self.stack.iter().format(" ")
                    )
                )],
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
        instruction: &Spanned<Instruction>,
    ) -> Result<()> {
        use Parameter::{Any, Concrete as P};
        use Return::{Concrete as R, Get};
        use Type::{Bool, I32};

        let (inputs, outputs): (&[Parameter], &[Return]) = match &**instruction
        {
            Instruction::Then(_) | Instruction::ThenElse(..) => {
                (&[P(Bool)], &[])
            }
            Instruction::PushI32(_) => (&[], &[R(I32)]),
            Instruction::PushBool(_) => (&[], &[R(Bool)]),
            Instruction::PushType(_) => (&[], &[R(Type::Type)]),
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
        self.transform(inputs, outputs, instruction.span)?;

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
