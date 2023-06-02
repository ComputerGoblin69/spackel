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
                if let [.., (Instruction::PushF32(a), _), (Instruction::PushF32(b), _)] =
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
            _ => {}
        }
        out.push((instruction, generics));
    }

    *body = out.into_boxed_slice();
}
