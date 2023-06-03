use crate::{
    ir::{BinMathOp, Block, Comparison, Instruction},
    typ::{CheckedProgram, Generics},
};

pub fn optimize(program: &mut CheckedProgram) {
    for function in program.functions.values_mut() {
        optimize_block(&mut function.body);
    }
}

fn optimize_block(body: &mut Box<Block<Generics>>) {
    let mut out = Vec::<(_, _)>::with_capacity(body.len());

    for (mut instruction, generics) in std::mem::take(body).into_vec() {
        match instruction {
            Instruction::Then(ref mut body) => {
                optimize_block(body);
                if let Some((Instruction::PushBool(condition), ..)) = out.last()
                {
                    let condition = *condition;
                    out.pop();
                    if condition {
                        out.extend(std::mem::take(body).into_vec());
                    }
                    continue;
                }
            }
            Instruction::Repeat { ref mut body, .. } => {
                optimize_block(body);
            }
            Instruction::ThenElse(ref mut then, ref mut else_) => {
                optimize_block(then);
                optimize_block(else_);
                if let Some((Instruction::PushBool(condition), ..)) = out.last()
                {
                    let condition = *condition;
                    out.pop();
                    out.extend(
                        std::mem::take(if condition { then } else { else_ })
                            .into_vec(),
                    );
                    continue;
                }
            }
            Instruction::BinMathOp(op) => {
                if let [.., (Instruction::PushI32(a), _), (Instruction::PushI32(b), _)] =
                    &mut *out
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
                        out.pop();
                        continue;
                    }
                } else if let [.., (Instruction::PushF32(a), _), (Instruction::PushF32(b), _)] =
                    &mut *out
                {
                    match op {
                        BinMathOp::Add => *a += *b,
                        BinMathOp::Sub => *a -= *b,
                        BinMathOp::Mul => *a *= *b,
                        BinMathOp::Div => *a /= *b,
                        BinMathOp::Rem | BinMathOp::SillyAdd => unreachable!(),
                    }
                    out.pop();
                    continue;
                }
            }
            Instruction::Sqrt => {
                if let Some((Instruction::PushF32(n), ..)) = out.last_mut() {
                    *n = n.sqrt();
                    continue;
                }
            }
            Instruction::Comparison(op) => {
                if let [.., (Instruction::PushI32(a), _), (Instruction::PushI32(b), _)] =
                    &*out
                {
                    let res = match op {
                        Comparison::Lt => *a < *b,
                        Comparison::Le => *a <= *b,
                        Comparison::Eq => *a == *b,
                        Comparison::Ge => *a >= *b,
                        Comparison::Gt => *a > *b,
                    };
                    out.pop();
                    out.pop();
                    out.push((Instruction::PushBool(res), [].into()));
                    continue;
                }
            }
            Instruction::Dup => {
                if let Some(value) = out.last() {
                    if trivially_dupable(value) {
                        out.push(value.clone());
                        continue;
                    }
                }
            }
            _ => {}
        }
        out.push((instruction, generics));
    }

    *body = out.into_boxed_slice();
}

const fn trivially_dupable(value: &(Instruction<Generics>, Generics)) -> bool {
    matches!(
        value.0,
        Instruction::PushI32(_)
            | Instruction::PushF32(_)
            | Instruction::PushBool(_)
    )
}
