use crate::ir::{Instruction, Program};
use anyhow::{ensure, Result};
use itertools::Itertools;
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Type {
    I32,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::I32 => "i32",
        })
    }
}

pub fn check(program: &Program) -> Result<()> {
    Checker { stack: Vec::new() }.check(program)
}

struct Checker {
    stack: Vec<Type>,
}

impl Checker {
    fn check(&mut self, program: &Program) -> Result<()> {
        for &instruction in &program.instructions {
            self.check_instruction(instruction)?;
        }
        ensure!(
            self.stack.is_empty(),
            "there are values left on the stack with the following types: `{}`",
            self.stack.iter().format(" ")
        );
        Ok(())
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
        use Type::I32;
        match instruction {
            Instruction::Push(_) => {
                self.stack.push(I32);
                Ok(())
            }
            Instruction::BinMathOp(_)
            | Instruction::Comparison(_)
            | Instruction::Nip => self.transform(&[I32; 2], &[I32]),
            Instruction::Println
            | Instruction::PrintChar
            | Instruction::Drop => self.take(&[I32]),
            Instruction::Dup => self.transform(&[I32], &[I32; 2]),
            Instruction::Swap => self.transform(&[I32; 2], &[I32; 2]),
            Instruction::Over | Instruction::Tuck => {
                self.transform(&[I32; 2], &[I32; 3])
            }
        }
    }
}
