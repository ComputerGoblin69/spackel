use crate::ssa::{Op, ValueGenerator};
use petgraph::{prelude::DiGraph, Direction};
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

pub fn optimize(graph: &mut CallGraph, value_generator: &mut ValueGenerator) {
    while graph
        .node_weights_mut()
        .any(|function| crate::ssa::propagate_drops(&mut function.body))
        | inline(graph, value_generator)
    {}
}

fn inline(graph: &mut CallGraph, value_generator: &mut ValueGenerator) -> bool {
    let mut did_something = false;

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
        did_something = true;

        let mut callers =
            graph.neighbors_directed(node, Direction::Incoming).detach();
        while let Some(caller) = callers.next_node(graph) {
            let (function, caller) = graph.index_twice_mut(node, caller);
            crate::ssa::rebuild_graph_inlining(
                &mut caller.body,
                function,
                value_generator,
            );
        }
        // After inlining, the original function definition ends up unused, so
        // remove it.
        graph.remove_node(node);
    }

    did_something
}
