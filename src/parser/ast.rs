pub mod node;

use std::fmt::{Display, Formatter, Write as _};
pub use node::{
    Node, NodeData, Visibility, StructMember, StructDefinition, ClassMember, Method,
    ConditionalNode, Constructor, ClassDefinition, Operator, OperatorType, FunctionDefinition,
    OperationExpression,
};

use crate::lexer::CodePosition;

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    nodes: Vec<Node>,
}

impl AST {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn get_pos(&self) -> CodePosition {
        if self.nodes.is_empty() {
            CodePosition::EMPTY
        }else {
            self.nodes.first().unwrap().pos().combine(&self.nodes.last().unwrap().pos())
        }
    }

    pub fn add_child(&mut self, node: Node) {
        self.nodes.push(node);
    }

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    pub(in crate::parser) fn nodes_mut(&mut self) -> &mut Vec<Node> {
        &mut self.nodes
    }

    pub fn into_nodes(self) -> Vec<Node> {
        self.nodes
    }

    pub fn into_node(self) -> Node {
        if self.nodes.len() == 1 {
            self.nodes.into_iter().next().unwrap()
        }else {
            Node::new_list_node(self.nodes)
        }
    }

    pub fn optimize_ast(&mut self) {
        Node::optimize_nodes(&mut self.nodes);
    }
}

impl Default for AST {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = String::new();
        builder += "AST: Children: {\n";
        for node in self.nodes.iter() {
            for token in node.to_string().split("\n") {
                let _ = writeln!(builder, "\t{token}");
            }
        }
        builder += "}";

        f.write_str(builder.as_str())
    }
}

impl From<&[Node]> for AST {
    fn from(value: &[Node]) -> Self {
        Self {
            nodes: Vec::from(value),
        }
    }
}

impl From<&mut [Node]> for AST {
    fn from(value: &mut [Node]) -> Self {
        Self {
            nodes: Vec::from(value),
        }
    }
}

impl <const N: usize> From<&[Node; N]> for AST {
    fn from(value: &[Node; N]) -> Self {
        Self {
            nodes: Vec::from(value),
        }
    }
}

impl <const N: usize> From<&mut [Node; N]> for AST {
    fn from(value: &mut [Node; N]) -> Self {
        Self {
            nodes: Vec::from(value),
        }
    }
}

impl <const N: usize> From<[Node; N]> for AST {
    fn from(value: [Node; N]) -> Self {
        Self {
            nodes: Vec::from(value),
        }
    }
}
