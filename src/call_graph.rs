use crate::ssa::{GraphBuilder, Op, ValueGenerator};
use petgraph::{prelude::DiGraph, Direction};
use std::{collections::HashMap, convert::Infallible, ops::ControlFlow};

pub type CallGraph = DiGraph<Function, ()>;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: crate::ssa::Graph,
}

pub fn of(
    mut function_bodies: HashMap<String, crate::ssa::Graph>,
) -> CallGraph {
    let mut graph = DiGraph::new();

    let nodes = function_bodies
        .keys()
        .map(|name| (&**name, graph.add_node(name.clone())))
        .collect::<HashMap<_, _>>();

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
        |_, name| Function {
            name: name.clone(),
            body: function_bodies.remove(name).unwrap(),
        },
        |_, ()| (),
    )
}

pub fn inline(graph: &mut CallGraph, value_generator: &mut ValueGenerator) {
    // Find a function to inline.
    while let Some(node) = graph
        // Only inline leaf functions.
        .externals(Direction::Outgoing)
        .find(|&node| {
            let function = &graph[node];
            // Don't inline `main`; how would that even work?
            function.name != "main"
            // Don't inline functions that are too large.
            && (function.body.is_small_enough_to_inline()
            // ...unless they are called in at most one place, meaning that
            // there will be no code size increase.
            || graph.edges(node).nth(1).is_none())
        })
    {
        let mut callers =
            graph.neighbors_directed(node, Direction::Incoming).detach();
        while let Some(caller) = callers.next_node(graph) {
            let (function, caller) = graph.index_twice_mut(node, caller);
            GraphBuilder {
                graph: &mut caller.body,
                function_signatures: &HashMap::new(),
                value_generator,
                stack: Vec::new(),
                renames: crate::ssa::renaming::Renames::default(),
            }
            .rebuild_inlining(function);
        }
        // After inlining, the original function definition ends up unused, so
        // remove it.
        graph.remove_node(node);
    }
}
