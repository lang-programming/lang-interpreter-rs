mod node;

pub use node::{Node, NodeData, Visibility, ClassDefinition, Operator, OperatorType, FunctionDefinition};

use crate::lexer::CodePosition;

#[derive(Debug, Clone)]
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

    pub fn convert_to_node(self) -> Node {
        if self.nodes.len() == 1 {
            self.nodes.into_iter().next().unwrap()
        }else {
            Node::new_list_node(self.nodes)
        }
    }
}

impl Default for AST {
    fn default() -> Self {
        Self::new()
    }
}
