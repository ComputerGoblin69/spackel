use crate::{
    ir::{BinLogicOp, BinMathOp, Comparison, Instruction, Program},
    stack::Stack,
};

pub fn interpret(program: &crate::typ::Checked<Program>) {
    Interpreter { stack: Vec::new() }.interpret(program);
}

#[derive(Clone, Copy)]
enum Value {
    Bool(bool),
    I32(i32),
}

struct Interpreter {
    stack: Vec<Value>,
}

impl Stack for Interpreter {
    type Item = Value;

    fn push(&mut self, element: Self::Item) {
        self.stack.push(element);
    }

    fn pop(&mut self) -> Self::Item {
        self.stack.pop().unwrap()
    }
}

impl Interpreter {
    fn interpret(&mut self, program: &Program) {
        for instruction in &*program.instructions {
            self.interpret_instruction(instruction);
        }
    }

    fn pop_i32(&mut self) -> i32 {
        match self.pop() {
            Value::I32(n) => n,
            _ => unreachable!(),
        }
    }

    fn pop_bool(&mut self) -> bool {
        match self.pop() {
            Value::Bool(b) => b,
            _ => unreachable!(),
        }
    }

    fn interpret_instruction(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Then(body) => {
                if self.pop_bool() {
                    for instruction in &**body {
                        self.interpret_instruction(instruction);
                    }
                }
            }
            Instruction::Push(number) => self.push(Value::I32(*number)),
            Instruction::True => self.push(Value::Bool(true)),
            Instruction::False => self.push(Value::Bool(false)),
            Instruction::Print => print!("{}", self.pop_i32()),
            Instruction::Println => println!("{}", self.pop_i32()),
            #[allow(clippy::cast_sign_loss)]
            Instruction::PrintChar => print!(
                "{}",
                (self.pop_i32() as u32)
                    .try_into()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            ),
            Instruction::BinMathOp(op) => {
                let b = self.pop_i32();
                let a = self.pop_i32();
                self.push(Value::I32(match op {
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
                }));
            }
            Instruction::Comparison(comparison) => {
                let b = self.pop_i32();
                let a = self.pop_i32();
                self.push(Value::Bool(match comparison {
                    Comparison::Lt => a < b,
                    Comparison::Le => a <= b,
                    Comparison::Eq => a == b,
                    Comparison::Ge => a >= b,
                    Comparison::Gt => a > b,
                }));
            }
            Instruction::Not => {
                let b = self.pop_bool();
                self.push(Value::Bool(!b));
            }
            Instruction::BinLogicOp(op) => {
                let b = self.pop_bool();
                let a = self.pop_bool();
                self.push(Value::Bool(match op {
                    BinLogicOp::And => a && b,
                    BinLogicOp::Or => a || b,
                    BinLogicOp::Xor => a ^ b,
                    BinLogicOp::Nand => !(a && b),
                    BinLogicOp::Nor => !(a || b),
                    BinLogicOp::Xnor => !(a ^ b),
                }));
            }
            Instruction::Drop => {
                self.pop();
            }
            Instruction::Dup => self.dup(),
            Instruction::Swap => self.swap(),
            Instruction::Over => self.over(),
            Instruction::Nip => self.nip(),
            Instruction::Tuck => self.tuck(),
        }
    }
}
