use crate::{
    ir::{BinMathOp, Comparison, Instruction},
    typ::{FunctionSignature, Generics, Type},
};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt, mem,
    ops::Range,
};

type GInstruction = (Instruction<Generics>, Generics);

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

impl IntoIterator for &ValueSequence {
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
    pub fn new_value(&mut self) -> Value {
        let value = self.0;
        self.0 += 1;
        Value(value)
    }

    pub fn new_value_sequence(&mut self, count: u8) -> ValueSequence {
        let start = self.0;
        self.0 += u32::from(count);
        ValueSequence { start, count }
    }
}

#[derive(Clone, Debug)]
pub struct Graph {
    pub inputs: Vec<Value>,
    pub assignments: Vec<Assignment>,
    pub outputs: Vec<Value>,
}

impl Graph {
    pub fn from_block(
        block: Box<[GInstruction]>,
        input_count: u32,
        function_signatures: &HashMap<String, FunctionSignature>,
        value_generator: &mut ValueGenerator,
    ) -> Self {
        let inputs = std::iter::repeat_with(|| value_generator.new_value())
            .take(input_count.try_into().unwrap())
            .collect::<Vec<_>>();
        let mut graph = Self {
            inputs: inputs.clone(),
            assignments: Vec::new(),
            outputs: Vec::new(),
        };
        let mut graph_builder = GraphBuilder {
            graph: &mut graph,
            function_signatures,
            value_generator,
            stack: inputs,
            renames: HashMap::new(),
        };
        for instruction in block.into_vec() {
            graph_builder.add_instruction(instruction);
        }
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
}

#[derive(Clone)]
pub struct Assignment {
    pub to: ValueSequence,
    pub args: Vec<Value>,
    pub op: Op,
}

impl fmt::Debug for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} <- {:?} {:#?}", self.to, self.args, self.op)
    }
}

#[derive(Clone, Debug)]
pub enum Op {
    Ins(GInstruction),
    Dup,
    Drop,
    Then(Box<Graph>),
    ThenElse(Box<Graph>, Box<Graph>),
    Repeat(Box<Graph>),
}

impl Op {
    const fn trivially_dupable(&self) -> bool {
        matches!(
            self,
            Self::Ins((
                Instruction::PushI32(_)
                    | Instruction::PushF32(_)
                    | Instruction::PushBool(_),
                _
            ))
        )
    }

    const fn pure(&self) -> bool {
        // Most operations are pure, so it's more convenient to list the
        // *impure* ones.
        !matches!(
            self,
            Self::Then(_)
                | Self::ThenElse(..)
                | Self::Repeat(_)
                | Self::Ins((
                    Instruction::Call(_)
                        | Instruction::PrintChar
                        | Instruction::Print
                        | Instruction::Println,
                    _
                ))
        ) && !matches!(
            self,
            // Division by zero and maybe overflow?
            Self::Ins((Instruction::BinMathOp(_), generics)) if matches!(generics[0], Type::I32)
        )
    }
}

struct GraphBuilder<'g> {
    graph: &'g mut Graph,
    function_signatures: &'g HashMap<String, FunctionSignature>,
    value_generator: &'g mut ValueGenerator,
    stack: Vec<Value>,
    renames: HashMap<Value, Value>,
}

impl GraphBuilder<'_> {
    fn add_instruction(&mut self, instruction: GInstruction) {
        let (to_count, arg_count, op) = match instruction.0 {
            Instruction::Call(ref name) => {
                let signature = &self.function_signatures[&**name];
                (
                    signature.returns.len(),
                    signature.parameters.len(),
                    Op::Ins(instruction),
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
            Instruction::PushI32(_)
            | Instruction::PushF32(_)
            | Instruction::PushBool(_)
            | Instruction::PushType(_) => (1, 0, Op::Ins(instruction)),
            Instruction::Print
            | Instruction::Println
            | Instruction::PrintChar => (0, 1, Op::Ins(instruction)),
            Instruction::TypeOf
            | Instruction::Sqrt
            | Instruction::Not
            | Instruction::AddrOf
            | Instruction::ReadPtr => (1, 1, Op::Ins(instruction)),
            Instruction::BinMathOp(_)
            | Instruction::Comparison(_)
            | Instruction::BinLogicOp(_) => (1, 2, Op::Ins(instruction)),
            Instruction::Swap => {
                let a = self.stack.len() - 2;
                let b = self.stack.len() - 1;
                self.stack.swap(a, b);
                return;
            }
            Instruction::Nip => {
                let dropped_type = instruction.1[0].clone();
                self.add_instruction((Instruction::Swap, instruction.1));
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
                    Box::new([instruction.1[0].clone()]),
                ));
                let a = self.stack.len() - 3;
                let b = self.stack.len() - 2;
                self.stack.swap(a, b);
                return;
            }
            Instruction::Tuck => {
                self.add_instruction((
                    Instruction::Dup,
                    Box::new([instruction.1[1].clone()]),
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
        let args = self.stack.split_off(self.stack.len() - arg_count);
        self.stack.extend(&to);
        self.add(Assignment { to, args, op });
    }

    fn drop(&mut self, value: Value) {
        self.add(Assignment {
            to: ValueSequence::default(),
            args: vec![value],
            op: Op::Drop,
        });
    }

    fn i32(&mut self, to: Value, n: i32) {
        self.add(Assignment {
            to: to.into(),
            args: Vec::new(),
            op: Op::Ins((Instruction::PushI32(n), [].into())),
        });
    }

    fn f32(&mut self, to: Value, n: f32) {
        self.add(Assignment {
            to: to.into(),
            args: Vec::new(),
            op: Op::Ins((Instruction::PushF32(n), [].into())),
        });
    }

    fn bool(&mut self, to: Value, b: bool) {
        self.add(Assignment {
            to: to.into(),
            args: Vec::new(),
            op: Op::Ins((Instruction::PushBool(b), [].into())),
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
        for arg in &mut args {
            *arg = self.renames.remove(arg).unwrap_or(*arg);
        }

        match op {
            Op::Then(ref mut body) => {
                let (&condition_value, args) = args.split_last().unwrap();
                if let Some(Op::Ins((Instruction::PushBool(condition), _))) =
                    self.graph.source_op(condition_value)
                {
                    let condition = *condition;
                    self.drop(condition_value);
                    if condition {
                        self.renames.extend(
                            body.inputs
                                .iter()
                                .copied()
                                .zip(args.iter().copied()),
                        );
                        for assignment in mem::take(&mut body.assignments) {
                            self.add(assignment);
                        }
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
                if let Some(Op::Ins((Instruction::PushBool(condition), _))) =
                    self.graph.source_op(condition_value)
                {
                    let body = if *condition { then } else { else_ };
                    self.drop(condition_value);
                    self.renames.extend(
                        body.inputs.iter().copied().zip(args.iter().copied()),
                    );
                    for assignment in mem::take(&mut body.assignments) {
                        self.add(assignment);
                    }
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
                    self.renames.insert(to + 0, args[0]);
                    self.add(Assignment {
                        to: (to + 1).into(),
                        args: Vec::new(),
                        op: source.clone(),
                    });
                    return;
                }
            }
            Op::Ins((Instruction::BinMathOp(op), _)) => {
                let a = self.graph.source_op(args[0]);
                let b = self.graph.source_op(args[1]);
                if let (
                    Some(Op::Ins((Instruction::PushI32(a), _))),
                    Some(Op::Ins((Instruction::PushI32(b), _))),
                ) = (a, b)
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
                        self.drop(args[0]);
                        self.drop(args[1]);
                        self.i32(to + 0, res);
                        return;
                    }
                } else if let (
                    Some(Op::Ins((Instruction::PushF32(a), _))),
                    Some(Op::Ins((Instruction::PushF32(b), _))),
                ) = (a, b)
                {
                    let res = match op {
                        BinMathOp::Add => *a + *b,
                        BinMathOp::Sub => *a - *b,
                        BinMathOp::Mul => *a * *b,
                        BinMathOp::Div => *a / *b,
                        BinMathOp::Rem | BinMathOp::SillyAdd => {
                            unreachable!()
                        }
                    };
                    self.drop(args[0]);
                    self.drop(args[1]);
                    self.add(Assignment {
                        to,
                        args: Vec::new(),
                        op: Op::Ins((Instruction::PushF32(res), [].into())),
                    });
                    return;
                }
            }
            Op::Ins((Instruction::Comparison(op), _)) => {
                let a = self.graph.source_op(args[0]);
                let b = self.graph.source_op(args[1]);
                if let (
                    Some(Op::Ins((Instruction::PushI32(a), _))),
                    Some(Op::Ins((Instruction::PushI32(b), _))),
                ) = (a, b)
                {
                    let res = match op {
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
            Op::Ins((Instruction::Sqrt, _)) => {
                if let Some(Op::Ins((Instruction::PushF32(num), _))) =
                    self.graph.source_op(args[0])
                {
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

pub fn propagate_drops(graph: &mut Graph) {
    // Recurse.
    for assignment in &mut graph.assignments {
        match &mut assignment.op {
            Op::Then(body) | Op::Repeat(body) => propagate_drops(body),
            Op::ThenElse(then, else_) => {
                propagate_drops(then);
                propagate_drops(else_);
            }
            _ => {}
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
            // If the values produced by a pure operation are all useless then the
            // arguments are also useless.
            useless_values.extend(assignment.args.iter().copied());
            // The operation is pure and useless, so remove it and just drop
            // its arguments.
            out.extend(assignment.args.iter().map(|&arg| Assignment {
                to: ValueSequence::default(),
                args: vec![arg],
                op: Op::Drop,
            }));
        } else {
            out.push(assignment);
        }
    }
    out.reverse();

    // Remove drops for values created by useless operations.
    let mut produced = graph.inputs.iter().copied().collect::<HashSet<_>>();
    out.retain(|assignment| {
        produced.extend(&assignment.to);
        assignment.args.iter().all(|arg| produced.contains(arg))
    });

    graph.assignments = out;
}
