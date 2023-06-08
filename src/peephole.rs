use crate::{
    ir::{BinMathOp, Block, Comparison, Instruction},
    typ::{CheckedProgram, Generics},
};
use std::ops::{Deref, DerefMut};

pub fn optimize(program: &mut CheckedProgram) {
    for function in program.functions.values_mut() {
        optimize_block(&mut function.body);
    }
}

fn optimize_block(body: &mut Box<Block<Generics>>) {
    let mut optimizer = Optimizer {
        out: Vec::with_capacity(body.len()),
    };
    for instruction in std::mem::take(body).into_vec() {
        optimizer.push_instruction(instruction);
    }
    *body = optimizer.out.into_boxed_slice();
}

struct Optimizer {
    out: Vec<(Instruction<Generics>, Generics)>,
}

impl Deref for Optimizer {
    type Target = Vec<(Instruction<Generics>, Generics)>;

    fn deref(&self) -> &Self::Target {
        &self.out
    }
}

impl DerefMut for Optimizer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.out
    }
}

impl Optimizer {
    fn push_instruction(
        &mut self,
        (mut instruction, generics): (Instruction<Generics>, Generics),
    ) {
        match instruction {
            Instruction::Then(ref mut body) => {
                optimize_block(body);
                if let Some((Instruction::PushBool(condition), ..)) =
                    self.last()
                {
                    let condition = *condition;
                    self.pop();
                    if condition {
                        self.extend(std::mem::take(body).into_vec());
                    }
                    return;
                }
            }
            Instruction::Repeat { ref mut body, .. } => {
                optimize_block(body);
            }
            Instruction::ThenElse(ref mut then, ref mut else_) => {
                optimize_block(then);
                optimize_block(else_);
                if let Some((Instruction::PushBool(condition), ..)) =
                    self.last()
                {
                    let condition = *condition;
                    self.pop();
                    self.extend(
                        std::mem::take(if condition { then } else { else_ })
                            .into_vec(),
                    );
                    return;
                }
            }
            Instruction::BinMathOp(op) => {
                if let [.., (Instruction::PushI32(a), _), (Instruction::PushI32(b), _)] =
                    &mut ***self
                {
                    if let Some(res) = match op {
                        BinMathOp::Add => a.checked_add(*b),
                        BinMathOp::Sub => a.checked_sub(*b),
                        BinMathOp::Mul => a.checked_mul(*b),
                        BinMathOp::Div => a.checked_div(*b),
                        BinMathOp::Rem => a.checked_rem(*b),
                        BinMathOp::SillyAdd => match (*a, *b) {
                            (9, 10) | (10, 9) => Some(21),
                            (1, 1) => Some(1),
                            _ => a.checked_add(*b),
                        },
                    } {
                        *a = res;
                        self.pop();
                        return;
                    }
                } else if let [.., (Instruction::PushF32(a), _), (Instruction::PushF32(b), _)] =
                    &mut ***self
                {
                    match op {
                        BinMathOp::Add => *a += *b,
                        BinMathOp::Sub => *a -= *b,
                        BinMathOp::Mul => *a *= *b,
                        BinMathOp::Div => *a /= *b,
                        BinMathOp::Rem | BinMathOp::SillyAdd => {
                            unreachable!()
                        }
                    }
                    self.pop();
                    return;
                }
            }
            Instruction::Sqrt => {
                if let Some((Instruction::PushF32(n), ..)) = self.last_mut() {
                    *n = n.sqrt();
                    return;
                }
            }
            Instruction::Comparison(op) => {
                if let [.., (Instruction::PushI32(a), _), (Instruction::PushI32(b), _)] =
                    &***self
                {
                    let res = match op {
                        Comparison::Lt => *a < *b,
                        Comparison::Le => *a <= *b,
                        Comparison::Eq => *a == *b,
                        Comparison::Ge => *a >= *b,
                        Comparison::Gt => *a > *b,
                    };
                    self.pop();
                    self.pop();
                    self.push((Instruction::PushBool(res), [].into()));
                    return;
                }
            }
            Instruction::Dup => {
                if let Some(value) = self.last() {
                    if trivially_dupable(value) {
                        let value = value.clone();
                        self.push(value);
                        return;
                    }
                }
            }
            _ => {}
        }
        self.push((instruction, generics));
    }
}

const fn trivially_dupable(value: &(Instruction<Generics>, Generics)) -> bool {
    matches!(
        value.0,
        Instruction::PushI32(_)
            | Instruction::PushF32(_)
            | Instruction::PushBool(_)
    )
}
