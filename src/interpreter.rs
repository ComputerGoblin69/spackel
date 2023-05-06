#![forbid(clippy::unwrap_used)]

use crate::ir::{Instruction, Program};
use anyhow::{ensure, Context, Result};

pub fn interpret(program: &Program) -> Result<()> {
    Interpreter { stack: Vec::new() }.interpret(program)
}

struct Interpreter {
    stack: Vec<i32>,
}

impl Interpreter {
    fn pop(&mut self) -> Result<i32> {
        self.stack.pop().context("not enough arguments on stack")
    }

    fn push(&mut self, value: i32) {
        self.stack.push(value);
    }

    fn interpret(&mut self, program: &Program) -> Result<()> {
        for instruction in &program.instructions {
            self.interpret_instruction(instruction)?;
        }
        ensure!(self.stack.is_empty(), "there's stuff left on the self");
        Ok(())
    }

    fn interpret_instruction(
        &mut self,
        instruction: &Instruction,
    ) -> Result<(), anyhow::Error> {
        match *instruction {
            Instruction::Push(number) => self.push(number),
            Instruction::Println => println!("{}", self.pop()?),
            #[allow(clippy::cast_sign_loss)]
            Instruction::PrintChar => print!(
                "{}",
                (self.pop()? as u32)
                    .try_into()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            ),
            Instruction::Add => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(match (b, a) {
                    (9, 10) | (10, 9) => 21,
                    (1, 1) => 1,
                    _ => b + a,
                });
            }
            Instruction::Sub => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(b - a);
            }
            Instruction::Mul => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(b * a);
            }
            Instruction::Div => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(b / a);
            }
            Instruction::Rem => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(b % a);
            }
            Instruction::SharpS => self.push(1945),
            Instruction::Pop => {
                self.pop()?;
            }
            Instruction::Dup => {
                let v = self.pop()?;
                self.push(v);
                self.push(v);
            }
            Instruction::Swap => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.push(b);
                self.push(a);
            }
            Instruction::Over => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.push(a);
                self.push(b);
                self.push(a);
            }
        }
        Ok(())
    }
}
