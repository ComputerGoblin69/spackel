use crate::{
    ir::{BinLogicOp, BinMathOp, Comparison, Instruction},
    typ::{Generics, Type},
};

pub fn interpret(program: &crate::typ::CheckedProgram) {
    Interpreter {
        stack: Vec::new(),
        program,
    }
    .interpret();
}

#[derive(Clone)]
enum Value {
    Bool(bool),
    I32(i32),
    F32(f32),
    Type(Type),
}

struct Interpreter<'a> {
    program: &'a crate::typ::CheckedProgram,
    stack: Vec<Value>,
}

impl Interpreter<'_> {
    fn push(&mut self, element: Value) {
        self.stack.push(element);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
}

impl Interpreter<'_> {
    fn interpret(&mut self) {
        for instruction in &*self.program.functions["main"].body {
            self.interpret_instruction(instruction);
        }
    }

    fn pop_i32(&mut self) -> i32 {
        match self.pop() {
            Value::I32(n) => n,
            _ => unreachable!(),
        }
    }

    fn pop_f32(&mut self) -> f32 {
        match self.pop() {
            Value::F32(n) => n,
            _ => unreachable!(),
        }
    }

    fn pop_bool(&mut self) -> bool {
        match self.pop() {
            Value::Bool(b) => b,
            _ => unreachable!(),
        }
    }

    fn pop_type(&mut self) -> Type {
        match self.pop() {
            Value::Type(typ) => typ,
            _ => unreachable!(),
        }
    }

    fn interpret_instruction(
        &mut self,
        (instruction, generics): &(Instruction<Generics>, Generics),
    ) {
        match instruction {
            Instruction::Call(name) => {
                let function = &self.program.functions[&**name];
                for instruction in &*function.body {
                    self.interpret_instruction(instruction);
                }
            }
            Instruction::Then(body) => {
                if self.pop_bool() {
                    for instruction in &**body {
                        self.interpret_instruction(instruction);
                    }
                }
            }
            Instruction::ThenElse(then, else_) => {
                for instruction in &**if self.pop_bool() { then } else { else_ }
                {
                    self.interpret_instruction(instruction);
                }
            }
            Instruction::Repeat { body, .. } => {
                while {
                    for instruction in &**body {
                        self.interpret_instruction(instruction);
                    }
                    self.pop_bool()
                } {}
            }
            Instruction::Unsafe(body) => {
                for instruction in &**body {
                    self.interpret_instruction(instruction);
                }
            }
            Instruction::PushI32(number) => self.push(Value::I32(*number)),
            Instruction::PushF32(number) => self.push(Value::F32(*number)),
            Instruction::PushBool(b) => self.push(Value::Bool(*b)),
            Instruction::PushType(typ) => self.push(Value::Type(typ.clone())),
            Instruction::Ptr => {
                let inner = self.pop_type();
                self.push(Value::Type(Type::Ptr(Box::new(inner))));
            }
            Instruction::TypeOf => {
                self.pop();
                self.push(Value::Type(generics[0].clone()));
            }
            Instruction::Print if generics[0] == Type::F32 => {
                print!("{}", self.pop_f32());
            }
            Instruction::Println if generics[0] == Type::F32 => {
                println!("{}", self.pop_f32());
            }
            Instruction::Print => print!("{}", self.pop_i32()),
            Instruction::Println => println!("{}", self.pop_i32()),
            #[allow(clippy::cast_sign_loss)]
            Instruction::PrintChar => print!(
                "{}",
                (self.pop_i32() as u32)
                    .try_into()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            ),
            Instruction::BinMathOp(op)
                if generics.first() == Some(&Type::F32) =>
            {
                let b = self.pop_f32();
                let a = self.pop_f32();
                self.push(Value::F32(match op {
                    BinMathOp::Add => a + b,
                    BinMathOp::Sub => a - b,
                    BinMathOp::Mul => a * b,
                    BinMathOp::Div => a / b,
                    BinMathOp::Rem | BinMathOp::SillyAdd => unreachable!(),
                }));
            }
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
            Instruction::Sqrt => {
                let n = self.pop_f32();
                self.push(Value::F32(n.sqrt()));
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
            Instruction::AddrOf | Instruction::ReadPtr => todo!(),
            Instruction::Drop => {
                self.pop();
            }
            Instruction::Dup => {
                let v = self.pop();
                self.push(v.clone());
                self.push(v);
            }
            Instruction::Swap => {
                let b = self.pop();
                let a = self.pop();
                self.push(b);
                self.push(a);
            }
            Instruction::Over => {
                let b = self.pop();
                let a = self.pop();
                self.push(a.clone());
                self.push(b);
                self.push(a);
            }
            Instruction::Nip => {
                let b = self.pop();
                self.pop();
                self.push(b);
            }
            Instruction::Tuck => {
                let b = self.pop();
                let a = self.pop();
                self.push(b.clone());
                self.push(a);
                self.push(b);
            }
        }
    }
}
