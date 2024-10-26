use crate::ssa::Op;
use petgraph::prelude::DiGraph;
use std::{collections::BTreeMap, convert::Infallible, ops::ControlFlow};

pub type CallGraph<'src> = DiGraph<Function<'src>, ()>;

#[derive(Debug)]
pub struct Function<'src> {
    pub name: &'src str,
    pub body: crate::ssa::Graph,
}

pub fn of(mut function_bodies: BTreeMap<&str, crate::ssa::Graph>) -> CallGraph {
    let mut graph = DiGraph::new();

    let nodes = function_bodies
        .keys()
        .map(|&name| (name, graph.add_node(name)))
        .collect::<BTreeMap<_, _>>();

    for (caller, body) in &function_bodies {
        let start = nodes[&**caller];
        body.each_op(&mut |op| {
            if let Op::Call(called_function) = op {
                let end = nodes[&**called_function];
                graph.update_edge(start, end, ());
            }
            ControlFlow::<Infallible>::Continue(())
        });
    }

    graph.map(
        |_, &name| Function {
            name,
            body: function_bodies.remove(name).unwrap(),
        },
        |_, ()| (),
    )
}
