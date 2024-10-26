use crate::{
    ir::{BinLogicOp, BinMathOp, Block, Comparison, Instruction},
    typ::{FunctionSignature, Generics, Type},
};
use std::{collections::BTreeMap, fmt, ops::ControlFlow};

pub struct Program<'src> {
    pub function_signatures: BTreeMap<&'src str, FunctionSignature>,
    pub function_bodies: BTreeMap<&'src str, Graph>,
}

pub fn convert<'src>(
    program: crate::typ::CheckedProgram<'src>,
    var_generator: &mut VarGenerator,
) -> Program<'src> {
    let function_bodies = program
        .function_bodies
        .into_iter()
        .map(|(name, body)| {
            let input_count =
                program.function_signatures[&name].parameters.len();
            let body = Graph::from_block(
                body,
                input_count,
                &program.function_signatures,
                var_generator,
            );
            (name, body)
        })
        .collect();

    Program {
        function_signatures: program.function_signatures,
        function_bodies,
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var(u32);

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Default)]
pub struct VarGenerator(u32);

impl VarGenerator {
    fn new_var(&mut self) -> Var {
        let res = Var(self.0);
        self.0 += 1;
        res
    }
}

#[derive(Clone, Debug, Default)]
pub struct Graph {
    pub ops: Vec<Op>,
    pub defs: BTreeMap<Var, Def>,
    pub uses: BTreeMap<Var, Use>,
    output_count: usize,
}

impl Graph {
    pub fn from_block(
        block: Box<Block<Generics>>,
        input_count: usize,
        function_signatures: &BTreeMap<&str, FunctionSignature>,
        var_generator: &mut VarGenerator,
    ) -> Self {
        let mut graph = Self::default();

        let mut stack = (0..input_count)
            .map(|input_index| {
                let input = var_generator.new_var();
                graph.defs.insert(input, Def::Input { index: input_index });
                input
            })
            .collect();

        for instruction in block {
            graph.add_instruction(
                instruction,
                var_generator,
                function_signatures,
                &mut stack,
            );
        }

        graph.output_count = stack.len();
        graph.uses.extend(
            stack
                .into_iter()
                .enumerate()
                .map(|(index, var)| (var, Use::Output { index })),
        );

        graph
    }

    pub fn each_op<B>(
        &self,
        f: &mut impl FnMut(&Op) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        for op in &self.ops {
            f(op)?;
            match op {
                Op::ThenElse(then, else_) => {
                    then.each_op(f)?;
                    else_.each_op(f)?;
                }
                Op::Then(body) | Op::Repeat(body) => {
                    body.each_op(f)?;
                }
                _ => {}
            }
        }
        ControlFlow::Continue(())
    }

    fn add_instruction(
        &mut self,
        (instruction, generics): (Instruction<Generics>, Generics),
        var_generator: &mut VarGenerator,
        function_signatures: &BTreeMap<&str, FunctionSignature>,
        stack: &mut Vec<Var>,
    ) {
        let (result_count, arg_count, op) = match instruction {
            Instruction::Call(name) => {
                let signature = &function_signatures[&*name];
                (
                    signature.returns.len(),
                    signature.parameters.len(),
                    Op::Call(name),
                )
            }
            Instruction::Then(body) => {
                let body_graph = Self::from_block(
                    body,
                    stack.len() - 1,
                    function_signatures,
                    var_generator,
                );
                (stack.len() - 1, stack.len(), Op::Then(Box::new(body_graph)))
            }
            Instruction::ThenElse(then, else_) => {
                let then_graph = Self::from_block(
                    then,
                    stack.len() - 1,
                    function_signatures,
                    var_generator,
                );
                let else_graph = Self::from_block(
                    else_,
                    stack.len() - 1,
                    function_signatures,
                    var_generator,
                );
                (
                    then_graph.output_count,
                    stack.len(),
                    Op::ThenElse(Box::new(then_graph), Box::new(else_graph)),
                )
            }
            Instruction::Repeat { body, .. } => {
                let body_graph = Self::from_block(
                    body,
                    stack.len(),
                    function_signatures,
                    var_generator,
                );
                (stack.len(), stack.len(), Op::Repeat(Box::new(body_graph)))
            }
            Instruction::Unsafe(body) => {
                for instruction in body {
                    self.add_instruction(
                        instruction,
                        var_generator,
                        function_signatures,
                        stack,
                    );
                }
                return;
            }
            Instruction::Dup => (2, 1, Op::Dup),
            Instruction::Drop => (0, 1, Op::Drop),
            Instruction::PushI32(n) => (1, 0, Op::I32(n)),
            Instruction::PushF32(n) => (1, 0, Op::F32(n)),
            Instruction::PushBool(b) => (1, 0, Op::Bool(b)),
            Instruction::PushType(_) => (1, 0, Op::Type),
            Instruction::PrintChar => (0, 1, Op::PrintChar),
            Instruction::Print => (
                0,
                1,
                match generics[0] {
                    Type::I32 => Op::PrintI32,
                    Type::F32 => Op::PrintF32,
                    _ => unreachable!(),
                },
            ),
            Instruction::Println => (
                0,
                1,
                match generics[0] {
                    Type::I32 => Op::PrintlnI32,
                    Type::F32 => Op::PrintlnF32,
                    _ => unreachable!(),
                },
            ),
            Instruction::Not => (1, 1, Op::Not),
            Instruction::Sqrt => (1, 1, Op::Sqrt),
            Instruction::TypeOf => (1, 1, Op::TypeOf),
            Instruction::Ptr => (1, 1, Op::Ptr),
            Instruction::AddrOf => {
                (1, 1, Op::AddrOf(Box::into_iter(generics).next().unwrap()))
            }
            Instruction::ReadPtr => {
                (1, 1, Op::ReadPtr(Box::into_iter(generics).next().unwrap()))
            }
            Instruction::BinMathOp(operation) => (
                1,
                2,
                Op::BinMath {
                    operation,
                    typ: Box::into_iter(generics).next(),
                },
            ),
            Instruction::Comparison(comparison) => {
                (1, 2, Op::Compare(comparison))
            }
            Instruction::BinLogicOp(op) => (1, 2, Op::BinLogic(op)),
            Instruction::Swap => {
                let a = stack.len() - 2;
                let b = stack.len() - 1;
                stack.swap(a, b);
                return;
            }
            Instruction::Nip => {
                let a = stack.len() - 2;
                let b = stack.len() - 1;
                stack.swap(a, b);
                (0, 1, Op::Drop)
            }
            Instruction::Over => {
                let a = stack.len() - 2;
                let b = stack.len() - 1;
                stack.swap(a, b);
                self.add_instruction(
                    (
                        Instruction::Dup,
                        Box::new([Box::into_iter(generics).next().unwrap()]),
                    ),
                    var_generator,
                    function_signatures,
                    stack,
                );
                let a = stack.len() - 3;
                let b = stack.len() - 2;
                stack.swap(a, b);
                return;
            }
            Instruction::Tuck => {
                self.add_instruction(
                    (
                        Instruction::Dup,
                        Box::new([Box::into_iter(generics).nth(1).unwrap()]),
                    ),
                    var_generator,
                    function_signatures,
                    stack,
                );
                let len = stack.len();
                stack[len - 3..].rotate_right(1);
                return;
            }
        };

        let op_index = self.ops.len();
        self.ops.push(op);

        let remaining_len = stack.len() - arg_count;
        self.uses.extend(
            stack.split_off(remaining_len).into_iter().enumerate().map(
                |(arg_index, arg)| {
                    (
                        arg,
                        Use::Op {
                            index: op_index,
                            sub_index: arg_index,
                        },
                    )
                },
            ),
        );

        for sub_index in 0..result_count {
            let result = var_generator.new_var();
            self.defs.insert(
                result,
                Def::Op {
                    index: op_index,
                    sub_index,
                },
            );
            stack.push(result);
        }
    }
}

#[derive(Clone, Debug)]
pub enum Def {
    Input { index: usize },
    Op { index: usize, sub_index: usize },
}

#[derive(Clone, Debug)]
pub enum Use {
    Op { index: usize, sub_index: usize },
    Output { index: usize },
}

#[derive(Clone, Debug)]
pub enum Op {
    Dup,
    Drop,
    Then(Box<Graph>),
    ThenElse(Box<Graph>, Box<Graph>),
    Repeat(Box<Graph>),
    Call(Box<str>),
    I32(i32),
    F32(f32),
    Bool(bool),
    Type,
    PrintChar,
    PrintI32,
    PrintF32,
    PrintlnI32,
    PrintlnF32,
    Sqrt,
    TypeOf,
    Ptr,
    Not,
    BinMath {
        operation: BinMathOp,
        typ: Option<Type>,
    },
    BinLogic(BinLogicOp),
    Compare(Comparison),
    AddrOf(Type),
    ReadPtr(Type),
}
