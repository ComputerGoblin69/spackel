mod renaming;

use crate::{
    call_graph::Function,
    ir::{BinLogicOp, BinMathOp, Block, Comparison, Instruction},
    typ::{FunctionSignature, Generics, Type},
};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt, mem,
    ops::{ControlFlow, Range},
};

pub struct Program<'src> {
    pub function_signatures: HashMap<&'src str, FunctionSignature>,
    pub function_bodies: HashMap<&'src str, Graph>,
}

pub fn convert<'src>(
    program: crate::typ::CheckedProgram<'src>,
    value_generator: &mut ValueGenerator,
) -> Program<'src> {
    let function_bodies = program
        .function_bodies
        .into_iter()
        .map(|(name, body)| {
            let input_count = program.function_signatures[&name]
                .parameters
                .len()
                .try_into()
                .unwrap();
            let body = Graph::from_block(
                body,
                input_count,
                &program.function_signatures,
                value_generator,
            );
            (name, body)
        })
        .collect();

    Program {
        function_signatures: program.function_signatures,
        function_bodies,
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value(u32);

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Clone, Copy, Default)]
pub struct ValueSequence {
    start: u32,
    count: u8,
}

impl fmt::Debug for ValueSequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{:?}]",
            (self.start..self.start + u32::from(self.count))
                .map(Value)
                .format(", ")
        )
    }
}

impl From<Value> for ValueSequence {
    fn from(value: Value) -> Self {
        Self {
            start: value.0,
            count: 1,
        }
    }
}

impl IntoIterator for ValueSequence {
    type Item = Value;

    type IntoIter = ValueSequenceIter;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl std::ops::Add<u8> for ValueSequence {
    type Output = Value;

    fn add(self, rhs: u8) -> Self::Output {
        debug_assert!(rhs < self.count);
        Value(self.start + u32::from(rhs))
    }
}

impl ValueSequence {
    const fn iter(self) -> ValueSequenceIter {
        ValueSequenceIter(self)
    }

    fn range(self) -> Range<Value> {
        Value(self.start)..Value(self.start + u32::from(self.count))
    }

    const fn count(self) -> u8 {
        self.count
    }
}

pub struct ValueSequenceIter(ValueSequence);

impl Iterator for ValueSequenceIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.count == 0 {
            None
        } else {
            let res = Value(self.0.start);
            self.0.start += 1;
            self.0.count -= 1;
            Some(res)
        }
    }
}

#[derive(Default)]
pub struct ValueGenerator(u32);

impl ValueGenerator {
    pub fn new_value_sequence(&mut self, count: u8) -> ValueSequence {
        let start = self.0;
        self.0 += u32::from(count);
        ValueSequence { start, count }
    }
}

#[derive(Clone, Debug)]
pub struct Graph {
    pub inputs: ValueSequence,
    pub assignments: Vec<Assignment>,
    pub outputs: Vec<Value>,
}

impl Graph {
    pub fn from_block(
        block: Box<Block<Generics>>,
        input_count: u8,
        function_signatures: &HashMap<&str, FunctionSignature>,
        value_generator: &mut ValueGenerator,
    ) -> Self {
        let inputs = value_generator.new_value_sequence(input_count);
        let mut graph = Self {
            inputs,
            assignments: Vec::new(),
            outputs: Vec::new(),
        };
        let mut graph_builder = GraphBuilder {
            graph: &mut graph,
            function_signatures,
            value_generator,
            stack: inputs.iter().collect(),
            renames: renaming::Renames::default(),
        };
        for instruction in block.into_vec() {
            graph_builder.add_instruction(instruction);
        }
        graph_builder
            .renames
            .apply_to_slice(&mut graph_builder.stack);
        graph.outputs = graph_builder.stack;
        graph
    }

    fn source_op(&self, value: Value) -> Option<&Op> {
        // TODO: reduce time complexity
        self.assignments
            .iter()
            .find(|assignment| assignment.to.range().contains(&value))
            .map(|assignment| &assignment.op)
    }

    pub fn is_small_enough_to_inline(&self) -> bool {
        self.contains_at_most_n_ops(10)
    }

    fn contains_at_most_n_ops(&self, n: usize) -> bool {
        let mut op_count = 0;
        self.each_op(&mut |_| {
            if op_count < n {
                op_count += 1;
                ControlFlow::Continue(())
            } else {
                ControlFlow::Break(())
            }
        })
        .is_continue()
    }

    pub fn each_op<B>(
        &self,
        f: &mut impl FnMut(&Op) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        for assignment in &self.assignments {
            let op = &assignment.op;
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
}

#[derive(Clone)]
pub struct Assignment {
    pub to: ValueSequence,
    pub args: Box<[Value]>,
    pub op: Op,
}

impl fmt::Debug for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} <- {:?} {:#?}", self.to, self.args, self.op)
    }
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
    Type(Type),
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

impl Op {
    const fn trivially_dupable(&self) -> bool {
        matches!(self, Self::I32(_) | Self::F32(_) | Self::Bool(_))
    }

    const fn pure(&self) -> bool {
        // Most operations are pure, so it's more convenient to list the
        // *impure* ones.
        !matches!(
            self,
            Self::Then(_)
                | Self::ThenElse(..)
                | Self::Repeat(_)
                | Self::Call(_)
                | Self::PrintChar
                | Self::PrintI32
                | Self::PrintF32
                | Self::PrintlnI32
                | Self::PrintlnF32
                // Division by zero and maybe overflow?
                | Self::BinMath { typ: Some(Type::I32), .. }
        )
    }
}

struct GraphBuilder<'g> {
    graph: &'g mut Graph,
    function_signatures: &'g HashMap<&'g str, FunctionSignature>,
    value_generator: &'g mut ValueGenerator,
    stack: Vec<Value>,
    renames: renaming::Renames,
}

impl GraphBuilder<'_> {
    fn add_instruction(
        &mut self,
        (instruction, generics): (Instruction<Generics>, Generics),
    ) {
        let (to_count, arg_count, op) = match instruction {
            Instruction::Call(name) => {
                let signature = &self.function_signatures[&*name];
                (
                    signature.returns.len(),
                    signature.parameters.len(),
                    Op::Call(name),
                )
            }
            Instruction::Then(body) => {
                let body_graph = Graph::from_block(
                    body,
                    (self.stack.len() - 1).try_into().unwrap(),
                    self.function_signatures,
                    self.value_generator,
                );
                (
                    self.stack.len() - 1,
                    self.stack.len(),
                    Op::Then(Box::new(body_graph)),
                )
            }
            Instruction::ThenElse(then, else_) => {
                let then_graph = Graph::from_block(
                    then,
                    (self.stack.len() - 1).try_into().unwrap(),
                    self.function_signatures,
                    self.value_generator,
                );
                let else_graph = Graph::from_block(
                    else_,
                    (self.stack.len() - 1).try_into().unwrap(),
                    self.function_signatures,
                    self.value_generator,
                );
                (
                    then_graph.outputs.len(),
                    self.stack.len(),
                    Op::ThenElse(Box::new(then_graph), Box::new(else_graph)),
                )
            }
            Instruction::Repeat { body, .. } => {
                let body_graph = Graph::from_block(
                    body,
                    self.stack.len().try_into().unwrap(),
                    self.function_signatures,
                    self.value_generator,
                );
                (
                    self.stack.len(),
                    self.stack.len(),
                    Op::Repeat(Box::new(body_graph)),
                )
            }
            Instruction::Unsafe(body) => {
                for instruction in body.into_vec() {
                    self.add_instruction(instruction);
                }
                return;
            }
            Instruction::Dup => (2, 1, Op::Dup),
            Instruction::Drop => (0, 1, Op::Drop),
            Instruction::PushI32(n) => (1, 0, Op::I32(n)),
            Instruction::PushF32(n) => (1, 0, Op::F32(n)),
            Instruction::PushBool(b) => (1, 0, Op::Bool(b)),
            Instruction::PushType(typ) => (1, 0, Op::Type(typ)),
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
            Instruction::AddrOf => (
                1,
                1,
                Op::AddrOf(generics.into_vec().into_iter().next().unwrap()),
            ),
            Instruction::ReadPtr => (
                1,
                1,
                Op::ReadPtr(generics.into_vec().into_iter().next().unwrap()),
            ),
            Instruction::BinMathOp(operation) => (
                1,
                2,
                Op::BinMath {
                    operation,
                    typ: generics.into_vec().into_iter().next(),
                },
            ),
            Instruction::Comparison(comparison) => {
                (1, 2, Op::Compare(comparison))
            }
            Instruction::BinLogicOp(op) => (1, 2, Op::BinLogic(op)),
            Instruction::Swap => {
                let a = self.stack.len() - 2;
                let b = self.stack.len() - 1;
                self.stack.swap(a, b);
                return;
            }
            Instruction::Nip => {
                let dropped_type = generics[0].clone();
                self.add_instruction((Instruction::Swap, generics));
                self.add_instruction((
                    Instruction::Drop,
                    Box::new([dropped_type]),
                ));
                return;
            }
            Instruction::Over => {
                let a = self.stack.len() - 2;
                let b = self.stack.len() - 1;
                self.stack.swap(a, b);
                self.add_instruction((
                    Instruction::Dup,
                    Box::new([generics.into_vec().into_iter().next().unwrap()]),
                ));
                let a = self.stack.len() - 3;
                let b = self.stack.len() - 2;
                self.stack.swap(a, b);
                return;
            }
            Instruction::Tuck => {
                self.add_instruction((
                    Instruction::Dup,
                    Box::new([generics.into_vec().into_iter().nth(1).unwrap()]),
                ));
                let a = self.stack.len() - 2;
                let new_a = self.stack.len() - 1;
                self.stack.swap(a, new_a);
                let b = self.stack.len() - 3;
                let new_a = self.stack.len() - 2;
                self.stack.swap(b, new_a);
                return;
            }
        };
        let to = self
            .value_generator
            .new_value_sequence(to_count.try_into().unwrap());
        let remaining_len = self.stack.len() - arg_count;
        let args = self.stack[remaining_len..].into();
        self.stack.truncate(remaining_len);
        self.stack.extend(to);
        self.add(Assignment { to, args, op });
    }

    fn drop(&mut self, value: Value) {
        self.add(Assignment {
            to: ValueSequence::default(),
            args: [value].into(),
            op: Op::Drop,
        });
    }

    fn i32(&mut self, to: Value, n: i32) {
        self.add(Assignment {
            to: to.into(),
            args: [].into(),
            op: Op::I32(n),
        });
    }

    fn f32(&mut self, to: Value, n: f32) {
        self.add(Assignment {
            to: to.into(),
            args: [].into(),
            op: Op::F32(n),
        });
    }

    fn bool(&mut self, to: Value, b: bool) {
        self.add(Assignment {
            to: to.into(),
            args: [].into(),
            op: Op::Bool(b),
        });
    }

    fn add(
        &mut self,
        Assignment {
            to,
            mut args,
            mut op,
        }: Assignment,
    ) {
        self.renames.apply_to_slice(&mut args);

        match op {
            Op::Then(ref mut body) => {
                let (&condition_value, args) = args.split_last().unwrap();
                if let Some(Op::Bool(condition)) =
                    self.graph.source_op(condition_value)
                {
                    let condition = *condition;
                    self.drop(condition_value);
                    if condition {
                        self.renames.extend(
                            body.inputs.iter().zip(args.iter().copied()),
                        );
                        for assignment in mem::take(&mut body.assignments) {
                            self.add(assignment);
                        }
                        self.renames.apply_to_slice(&mut body.outputs);
                        self.renames.extend(
                            to.iter().zip(body.outputs.iter().copied()),
                        );
                    } else {
                        self.renames
                            .extend(to.iter().zip(args.iter().copied()));
                    }
                    return;
                }
            }
            Op::ThenElse(ref mut then, ref mut else_) => {
                let (&condition_value, args) = args.split_last().unwrap();
                if let Some(Op::Bool(condition)) =
                    self.graph.source_op(condition_value)
                {
                    let body = if *condition { then } else { else_ };
                    self.drop(condition_value);
                    self.renames
                        .extend(body.inputs.iter().zip(args.iter().copied()));
                    for assignment in mem::take(&mut body.assignments) {
                        self.add(assignment);
                    }
                    self.renames.apply_to_slice(&mut body.outputs);
                    self.renames
                        .extend(to.iter().zip(body.outputs.iter().copied()));
                    return;
                }
            }
            Op::Dup => {
                if let Some(source) = self
                    .graph
                    .source_op(args[0])
                    .filter(|op| op.trivially_dupable())
                {
                    let source = source.clone();
                    self.drop(args[0]);
                    self.add(Assignment {
                        to: (to + 0).into(),
                        args: [].into(),
                        op: source.clone(),
                    });
                    self.add(Assignment {
                        to: (to + 1).into(),
                        args: [].into(),
                        op: source,
                    });
                    return;
                }
            }
            Op::BinMath { operation, .. } => {
                let a = self.graph.source_op(args[0]);
                let b = self.graph.source_op(args[1]);
                if let (Some(Op::I32(a)), Some(Op::I32(b))) = (a, b) {
                    if let Some(res) = match operation {
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
                        self.drop(args[0]);
                        self.drop(args[1]);
                        self.i32(to + 0, res);
                        return;
                    }
                } else if let (Some(Op::F32(a)), Some(Op::F32(b))) = (a, b) {
                    let res = match operation {
                        BinMathOp::Add => *a + *b,
                        BinMathOp::Sub => *a - *b,
                        BinMathOp::Mul => *a * *b,
                        BinMathOp::Div => *a / *b,
                        _ => unreachable!(),
                    };
                    self.drop(args[0]);
                    self.drop(args[1]);
                    self.add(Assignment {
                        to,
                        args: [].into(),
                        op: Op::F32(res),
                    });
                    return;
                }
            }
            Op::Compare(comparison) => {
                let a = self.graph.source_op(args[0]);
                let b = self.graph.source_op(args[1]);
                if let (Some(Op::I32(a)), Some(Op::I32(b))) = (a, b) {
                    let res = match comparison {
                        Comparison::Lt => *a < *b,
                        Comparison::Le => *a <= *b,
                        Comparison::Eq => *a == *b,
                        Comparison::Ge => *a >= *b,
                        Comparison::Gt => *a > *b,
                    };
                    self.drop(args[0]);
                    self.drop(args[1]);
                    self.bool(to + 0, res);
                    return;
                }
            }
            Op::Sqrt => {
                if let Some(Op::F32(num)) = self.graph.source_op(args[0]) {
                    let num = *num;
                    self.drop(args[0]);
                    self.f32(to + 0, num.sqrt());
                    return;
                }
            }
            _ => {}
        }
        self.graph.assignments.push(Assignment { to, args, op });
    }
}

pub fn rebuild_graph_inlining(
    graph: &mut Graph,
    function: &Function,
    value_generator: &mut ValueGenerator,
) {
    let mut builder = GraphBuilder {
        graph,
        function_signatures: &HashMap::new(),
        value_generator,
        stack: Vec::new(),
        renames: crate::ssa::renaming::Renames::default(),
    };
    for assignment in mem::take(&mut builder.graph.assignments) {
        if matches!(&assignment.op, Op::Call(name) if **name == *function.name)
        {
            builder.renames.extend(
                function
                    .body
                    .inputs
                    .iter()
                    .zip(assignment.args.iter().copied()),
            );
            for mut assignment in function.body.assignments.iter().cloned() {
                let new_out = builder
                    .value_generator
                    .new_value_sequence(assignment.to.count());
                builder.renames.extend(assignment.to.iter().zip(new_out));
                assignment.to = new_out;
                builder.add(assignment);
            }
            let outputs = function
                .body
                .outputs
                .iter()
                .map(|&out| builder.renames.take(out))
                .collect::<Vec<_>>();
            builder.renames.extend(assignment.to.iter().zip(outputs));
        } else {
            builder.add(assignment);
        }
    }
    builder.renames.apply_to_slice(&mut builder.graph.outputs);
}

pub fn propagate_drops(graph: &mut Graph) -> bool {
    let mut did_something = false;

    // Recurse.
    for assignment in &mut graph.assignments {
        did_something |= match &mut assignment.op {
            Op::Then(body) | Op::Repeat(body) => propagate_drops(body),
            Op::ThenElse(then, else_) => {
                propagate_drops(then) || propagate_drops(else_)
            }
            _ => false,
        }
    }

    let mut useless_values = HashSet::new();
    let mut out = Vec::new();
    for assignment in mem::take(&mut graph.assignments).into_iter().rev() {
        if assignment.op.pure()
            && assignment
                .to
                .iter()
                .all(|res| useless_values.contains(&res))
        {
            did_something |= !matches!(assignment.op, Op::Drop);
            // If the values produced by a pure operation are all useless then the
            // arguments are also useless.
            useless_values.extend(assignment.args.iter().copied());
            // The operation is pure and useless, so remove it and just drop
            // its arguments.
            out.extend(assignment.args.iter().map(|&arg| Assignment {
                to: ValueSequence::default(),
                args: [arg].into(),
                op: Op::Drop,
            }));
        } else {
            out.push(assignment);
        }
    }
    out.reverse();

    // Remove drops for values created by useless operations.
    let mut produced = graph.inputs.iter().collect::<HashSet<_>>();
    out.retain(|assignment| {
        produced.extend(assignment.to);
        assignment.args.iter().all(|arg| produced.contains(arg))
    });

    graph.assignments = out;

    did_something
}
