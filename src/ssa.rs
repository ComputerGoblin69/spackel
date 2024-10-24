mod renaming;

use crate::{
    call_graph::Function,
    ir::{BinLogicOp, BinMathOp, Block, Comparison, Instruction},
    typ::{FunctionSignature, Generics, Type},
};
use itertools::Itertools;
use renaming::Renames;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt, mem,
    ops::ControlFlow,
};

pub struct Program<'src> {
    pub function_signatures: BTreeMap<&'src str, FunctionSignature>,
    pub function_bodies: BTreeMap<&'src str, Graph>,
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
        function_signatures: &BTreeMap<&str, FunctionSignature>,
        value_generator: &mut ValueGenerator,
    ) -> Self {
        let inputs = value_generator.new_value_sequence(input_count);
        let mut graph = Self {
            inputs,
            assignments: Vec::new(),
            outputs: Vec::new(),
        };
        let mut stack = inputs.iter().collect();
        let mut renames = Renames::default();
        for instruction in block {
            graph.add_instruction(
                instruction,
                &mut renames,
                value_generator,
                function_signatures,
                &mut stack,
            );
        }
        renames.apply_to_slice(&mut stack);
        graph.outputs = stack;
        graph
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

    fn add_instruction(
        &mut self,
        (instruction, generics): (Instruction<Generics>, Generics),
        renames: &mut Renames,
        value_generator: &mut ValueGenerator,
        function_signatures: &BTreeMap<&str, FunctionSignature>,
        stack: &mut Vec<Value>,
    ) {
        let (to_count, arg_count, op) = match instruction {
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
                    (stack.len() - 1).try_into().unwrap(),
                    function_signatures,
                    value_generator,
                );
                (stack.len() - 1, stack.len(), Op::Then(Box::new(body_graph)))
            }
            Instruction::ThenElse(then, else_) => {
                let then_graph = Self::from_block(
                    then,
                    (stack.len() - 1).try_into().unwrap(),
                    function_signatures,
                    value_generator,
                );
                let else_graph = Self::from_block(
                    else_,
                    (stack.len() - 1).try_into().unwrap(),
                    function_signatures,
                    value_generator,
                );
                (
                    then_graph.outputs.len(),
                    stack.len(),
                    Op::ThenElse(Box::new(then_graph), Box::new(else_graph)),
                )
            }
            Instruction::Repeat { body, .. } => {
                let body_graph = Self::from_block(
                    body,
                    stack.len().try_into().unwrap(),
                    function_signatures,
                    value_generator,
                );
                (stack.len(), stack.len(), Op::Repeat(Box::new(body_graph)))
            }
            Instruction::Unsafe(body) => {
                for instruction in body {
                    self.add_instruction(
                        instruction,
                        renames,
                        value_generator,
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
                    renames,
                    value_generator,
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
                    renames,
                    value_generator,
                    function_signatures,
                    stack,
                );
                let len = stack.len();
                stack[len - 3..].rotate_right(1);
                return;
            }
        };
        let to =
            value_generator.new_value_sequence(to_count.try_into().unwrap());
        let remaining_len = stack.len() - arg_count;
        let args = stack[remaining_len..].into();
        stack.truncate(remaining_len);
        stack.extend(to);
        self.add(Assignment { to, args, op }, renames);
    }

    fn add(&mut self, mut assignment: Assignment, renames: &mut Renames) {
        renames.apply_to_slice(&mut assignment.args);
        self.assignments.push(assignment);
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

impl Op {
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

pub fn rebuild_graph_inlining(
    graph: &mut Graph,
    function: &Function,
    value_generator: &mut ValueGenerator,
) {
    let mut renames = Renames::default();
    for assignment in mem::take(&mut graph.assignments) {
        if matches!(&assignment.op, Op::Call(name) if **name == *function.name)
        {
            let mut function = function.body.clone();
            refresh_graph(&mut function, value_generator, false);

            renames.extend(
                function.inputs.iter().zip(assignment.args.iter().copied()),
            );
            for assignment in &mut function.assignments {
                renames.apply_to_slice(&mut assignment.args);
            }
            renames.apply_to_slice(&mut function.outputs);
            renames.extend(assignment.to.iter().zip(function.outputs));

            for assignment in function.assignments {
                graph.add(assignment, &mut renames);
            }
        } else {
            graph.add(assignment, &mut renames);
        }
    }
    renames.apply_to_slice(&mut graph.outputs);
}

fn refresh_graph(
    graph: &mut Graph,
    value_generator: &mut ValueGenerator,
    including_inputs: bool,
) {
    let mut renames = renaming::Renames::default();

    if including_inputs {
        let inputs = value_generator.new_value_sequence(graph.inputs.count());
        renames.extend(std::iter::zip(graph.inputs, inputs));
        graph.inputs = inputs;
    }

    for assignment in &mut graph.assignments {
        renames.apply_to_slice(&mut assignment.args);
        let to = value_generator.new_value_sequence(assignment.to.count());
        renames.extend(std::iter::zip(assignment.to, to));
        assignment.to = to;

        match &mut assignment.op {
            Op::Then(body) | Op::Repeat(body) => {
                refresh_graph(body, value_generator, true);
            }
            Op::ThenElse(then, else_) => {
                refresh_graph(then, value_generator, true);
                refresh_graph(else_, value_generator, true);
            }
            _ => {}
        }
    }

    renames.apply_to_slice(&mut graph.outputs);
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

    let mut useless_values = BTreeSet::new();
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
    let mut produced = graph.inputs.iter().collect::<BTreeSet<_>>();
    out.retain(|assignment| {
        produced.extend(assignment.to);
        assignment.args.iter().all(|arg| produced.contains(arg))
    });

    graph.assignments = out;

    did_something
}
