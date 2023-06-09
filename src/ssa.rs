use crate::{
    ir::Instruction,
    typ::{FunctionSignature, Generics},
};
use std::{collections::HashMap, fmt};

type GInstruction = (Instruction<Generics>, Generics);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(u32);

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

pub struct ValueGenerator(Value);

impl Default for ValueGenerator {
    fn default() -> Self {
        Self(Value(0))
    }
}

impl ValueGenerator {
    pub fn new_value(&mut self) -> Value {
        let value = self.0;
        self.0 .0 += 1;
        value
    }
}

#[derive(Debug)]
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
        };
        for instruction in block.into_vec() {
            graph_builder.add_instruction(instruction);
        }
        graph.outputs = graph_builder.stack;
        graph
    }
}

pub struct Assignment {
    pub to: Vec<Value>,
    pub args: Vec<Value>,
    pub op: Op,
}

impl fmt::Debug for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} <- {:?} {:#?}", self.to, self.args, self.op)
    }
}

#[derive(Debug)]
pub enum Op {
    Ins(GInstruction),
    Then(Box<Graph>),
    ThenElse(Box<Graph>, Box<Graph>),
    Repeat(Box<Graph>),
}

struct GraphBuilder<'g> {
    graph: &'g mut Graph,
    function_signatures: &'g HashMap<String, FunctionSignature>,
    value_generator: &'g mut ValueGenerator,
    stack: Vec<Value>,
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
            Instruction::PushI32(_)
            | Instruction::PushF32(_)
            | Instruction::PushBool(_)
            | Instruction::PushType(_) => (1, 0, Op::Ins(instruction)),
            Instruction::Print
            | Instruction::Println
            | Instruction::PrintChar
            | Instruction::Drop => (0, 1, Op::Ins(instruction)),
            Instruction::TypeOf
            | Instruction::Sqrt
            | Instruction::Not
            | Instruction::AddrOf
            | Instruction::ReadPtr => (1, 1, Op::Ins(instruction)),
            Instruction::BinMathOp(_)
            | Instruction::Comparison(_)
            | Instruction::BinLogicOp(_) => (1, 2, Op::Ins(instruction)),
            Instruction::Dup => (2, 1, Op::Ins(instruction)),
            Instruction::Swap => {
                self.stack.swap(self.stack.len() - 1, self.stack.len() - 2);
                return;
            }
            Instruction::Nip => {
                self.add_instruction((Instruction::Swap, instruction.1));
                self.add_instruction((
                    Instruction::Drop,
                    Box::new([instruction.1[0]]),
                ));
                return;
            }
            Instruction::Over | Instruction::Tuck => {
                (3, 2, Op::Ins(instruction))
            }
        };
        let to = std::iter::repeat_with(|| self.value_generator.new_value())
            .take(to_count)
            .collect::<Vec<_>>();
        let args = self.stack.split_off(self.stack.len() - arg_count);
        self.stack.extend(to.iter().copied());
        self.graph.assignments.push(Assignment { to, args, op });
    }
}
