use crate::ir::{Instruction, Program};
use anyhow::{ensure, Result};
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
        for &instruction in &program.instructions {
            self.check_instruction(instruction)?;
        }
        ensure!(
            self.stack.is_empty(),
            "there are values left on the stack with the following types: `{}`",
            self.stack.iter().format(" ")
        );
        Ok(Checked(program))
    }

    fn take(&mut self, types: &[Type]) -> Result<()> {
        ensure!(
            self.stack.ends_with(types),
            "expected types `{}` but got `{}`",
            types.iter().format(" "),
            self.stack.iter().format(" ")
        );
        let new_len = self.stack.len() - types.len();
        self.stack.truncate(new_len);
        Ok(())
    }

    fn transform(&mut self, inputs: &[Type], outputs: &[Type]) -> Result<()> {
        self.take(inputs)?;
        self.stack.extend(outputs);
        Ok(())
    }

    fn check_instruction(&mut self, instruction: Instruction) -> Result<()> {
        use Type::{Bool, I32};
        let (inputs, outputs): (&[Type], &[Type]) = match instruction {
            Instruction::Push(_) => (&[], &[I32]),
            Instruction::True | Instruction::False => (&[], &[Bool]),
            Instruction::BinMathOp(_) | Instruction::Nip => (&[I32; 2], &[I32]),
            Instruction::Comparison(_) => (&[I32; 2], &[Bool]),
            Instruction::Println
            | Instruction::PrintChar
            | Instruction::Drop => (&[I32], &[]),
            Instruction::Dup => (&[I32], &[I32; 2]),
            Instruction::Swap => (&[I32; 2], &[I32; 2]),
            Instruction::Over | Instruction::Tuck => (&[I32; 2], &[I32; 3]),
        };
        self.transform(inputs, outputs)
    }
}
