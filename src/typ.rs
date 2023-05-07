use crate::{
    ir::{Instruction, Program},
    stack::Stack,
};
use anyhow::{ensure, Context, Result};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Type {
    I32,
}

pub fn check(program: &Program) -> Result<()> {
    Checker { stack: Vec::new() }.check(program)
}

struct Checker {
    stack: Vec<Type>,
}

impl Stack for Checker {
    type Item = Type;
    type Error = anyhow::Error;

    fn push(&mut self, element: Self::Item) {
        self.stack.push(element);
    }

    fn pop(&mut self) -> Result<Self::Item> {
        self.stack.pop().context("not enough arguments on stack")
    }
}

impl Checker {
    fn check(&mut self, program: &Program) -> Result<()> {
        for &instruction in &program.instructions {
            self.check_instruction(instruction)?;
        }
        ensure!(self.stack.is_empty(), "there's stuff left on the stack");
        Ok(())
    }

    fn take(&mut self, types: &[Type]) -> Result<()> {
        // TODO: Improve error message
        ensure!(self.stack.ends_with(types), "type error");
        let new_len = self.stack.len() - types.len();
        self.stack.truncate(new_len);
        Ok(())
    }

    fn check_instruction(&mut self, instruction: Instruction) -> Result<()> {
        use Type::I32;
        match instruction {
            Instruction::Push(_) => self.stack.push(I32),
            Instruction::BinMathOp(_) => {
                self.take(&[I32; 2])?;
                self.stack.push(I32);
            }
            Instruction::Println
            | Instruction::PrintChar
            | Instruction::Drop => self.take(&[I32])?,
            Instruction::Dup => self.dup()?,
            Instruction::Swap => self.swap()?,
            Instruction::Over => self.over()?,
            Instruction::Nip => self.nip()?,
            Instruction::Tuck => self.tuck()?,
        }
        Ok(())
    }
}
