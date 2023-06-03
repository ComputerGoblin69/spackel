use crate::{
    ir::{BinMathOp, Block, Instruction},
    typ::{CheckedProgram, Generics},
};

pub fn optimize(program: &mut CheckedProgram) {
    for function in program.functions.values_mut() {
        optimize_block(&mut function.body);
    }
}

fn optimize_block(body: &mut Box<Block<Generics>>) {
    let mut out = Vec::<(_, _)>::with_capacity(body.len());

    for (instruction, generics) in std::mem::take(body).into_vec() {
        match instruction {
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
            Instruction::Dup => {
                if let Some(
                    value @ (
                        Instruction::PushI32(_) | Instruction::PushF32(_),
                        ..,
                    ),
                ) = out.last()
                {
                    out.push(value.clone());
                    continue;
                }
            }
            _ => {}
        }
        out.push((instruction, generics));
    }

    *body = out.into_boxed_slice();
}
