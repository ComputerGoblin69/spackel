#![forbid(clippy::unwrap_used)]

use crate::{
    ir::{BinMathOp, Instruction, Program},
    stack::Stack,
};
use anyhow::{ensure, Context, Result};

pub fn interpret(program: &Program) -> Result<()> {
    Interpreter { stack: Vec::new() }.interpret(program)
}

struct Interpreter {
    stack: Vec<i32>,
}

impl Stack for Interpreter {
    type Item = i32;

    fn push(&mut self, element: Self::Item) {
        self.stack.push(element);
    }

    fn pop(&mut self) -> Result<Self::Item> {
        self.stack.pop().context("not enough arguments on stack")
    }
}

impl Interpreter {
    fn interpret(&mut self, program: &Program) -> Result<()> {
        for &instruction in &program.instructions {
            self.interpret_instruction(instruction)?;
        }
        ensure!(self.stack.is_empty(), "there's stuff left on the self");
        Ok(())
    }

    fn interpret_instruction(
        &mut self,
        instruction: Instruction,
    ) -> Result<(), anyhow::Error> {
        match instruction {
            Instruction::Push(number) => self.push(number),
            Instruction::Println => println!("{}", self.pop()?),
            #[allow(clippy::cast_sign_loss)]
            Instruction::PrintChar => print!(
                "{}",
                (self.pop()? as u32)
                    .try_into()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            ),
            Instruction::BinMathOp(op) => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.push(match op {
                    BinMathOp::Add => a + b,
                    BinMathOp::Sub => a - b,
                    BinMathOp::Mul => a * b,
                    BinMathOp::Div => a / b,
                    BinMathOp::Rem => a % b,
                    BinMathOp::SillyAdd => match (a, b) {
                        (9, 10) | (10, 9) => 21,
                        (1, 1) => 1,
                        _ => a + b,
                    },
                });
            }
            Instruction::Drop => {
                self.pop()?;
            }
            Instruction::Dup => self.dup()?,
            Instruction::Swap => self.swap()?,
            Instruction::Over => self.over()?,
            Instruction::Nip => self.nip()?,
            Instruction::Tuck => self.tuck()?,
        }
        Ok(())
    }
}
