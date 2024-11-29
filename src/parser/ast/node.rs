use crate::lexer::CodePosition;
use crate::parser::ast::AST;
use crate::parser::ParsingError;

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum OperatorType {
    All,
    General,
    Math,
    Condition,
}

impl OperatorType {
    pub fn is_compatible_with(&self, operator_type: OperatorType) -> bool {
        *self == OperatorType::All || operator_type == OperatorType::General || *self == operator_type
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Operator {
    //General
    Non,
    Len,
    DeepCopy,
    Concat,
    Spaceship,
    Elvis,
    NullCoalescing,
    InlineIf,

    //Math
    MathNon,
    Pow,
    Pos,
    Inv,
    BitwiseNot,
    Inc,
    Dec,
    Mul,
    Div,
    TruncDiv,
    FloorDiv,
    CeilDiv,
    Mod,
    Add,
    Sub,
    Lshift,
    Rshift,
    Rzshift,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,

    //Condition
    ConditionalNon,
    Not,
    InstanceOf,
    Equals,
    NotEquals,
    Matches,
    NotMatches,
    StrictEquals,
    StrictNotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,
    And,
    Or,

    //ALL
    /**
     * COMMA is a parser-o
     */
    Comma,
    GetItem,
    OptionalGetItem,
    Slice,
    OptionalSlice,
    MemberAccess,
    /**
     * MEMBER_ACCESS opera
     */
    MemberAccessThis,
    OptionalMemberAccess,
    MemberAccessPointer,
}

impl Operator {
    pub fn symbol(&self) -> &'static str {
        match self {
            //General
            Operator::Non => "",
            Operator::Len => "@",
            Operator::DeepCopy => "^",
            Operator::Concat => "|||",
            Operator::Spaceship => "<=>",
            Operator::Elvis => "?:",
            Operator::NullCoalescing => "??",
            Operator::InlineIf => "?...:",

            //Math
            Operator::MathNon => "",
            Operator::Pow => "**",
            Operator::Pos => "+",
            Operator::Inv => "-",
            Operator::BitwiseNot => "~",
            Operator::Inc => "+|",
            Operator::Dec => "-|",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::TruncDiv => "~/",
            Operator::FloorDiv => "//",
            Operator::CeilDiv => "^/",
            Operator::Mod => "%",
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Lshift => "<<",
            Operator::Rshift => ">>",
            Operator::Rzshift => ">>>",
            Operator::BitwiseAnd => "&",
            Operator::BitwiseXor => "^",
            Operator::BitwiseOr => "|",

            //Condition
            Operator::ConditionalNon => "",
            Operator::Not => "!",
            Operator::InstanceOf => "~~",
            Operator::Equals => "==",
            Operator::NotEquals => "!=",
            Operator::Matches => "=~",
            Operator::NotMatches => "!=~",
            Operator::StrictEquals => "===",
            Operator::StrictNotEquals => "!==",
            Operator::LessThan => "<",
            Operator::GreaterThan => ">",
            Operator::LessThanOrEquals => "<=",
            Operator::GreaterThanOrEquals => ">=",
            Operator::And => "&&",
            Operator::Or => "||",

            //ALL
            Operator::Comma => ",",
            Operator::GetItem => "[...]",
            Operator::OptionalGetItem => "?.[...]",
            Operator::Slice => "[...:...]",
            Operator::OptionalSlice => "?.[...:...]",
            Operator::MemberAccess => "::",
            Operator::MemberAccessThis => "::",
            Operator::OptionalMemberAccess => "?::",
            Operator::MemberAccessPointer => "->",
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            //General
            Operator::Non => 1,
            Operator::Len => 1,
            Operator::DeepCopy => 1,
            Operator::Concat => 2,
            Operator::Spaceship => 2,
            Operator::Elvis => 2,
            Operator::NullCoalescing => 2,
            Operator::InlineIf => 3,

            //Math
            Operator::MathNon => 1,
            Operator::Pow => 2,
            Operator::Pos => 1,
            Operator::Inv => 1,
            Operator::BitwiseNot => 1,
            Operator::Inc => 1,
            Operator::Dec => 1,
            Operator::Mul => 2,
            Operator::Div => 2,
            Operator::TruncDiv => 2,
            Operator::FloorDiv => 2,
            Operator::CeilDiv => 2,
            Operator::Mod => 2,
            Operator::Add => 2,
            Operator::Sub => 2,
            Operator::Lshift => 2,
            Operator::Rshift => 2,
            Operator::Rzshift => 2,
            Operator::BitwiseAnd => 2,
            Operator::BitwiseXor => 2,
            Operator::BitwiseOr => 2,

            //Condition
            Operator::ConditionalNon => 1,
            Operator::Not => 1,
            Operator::InstanceOf => 2,
            Operator::Equals => 2,
            Operator::NotEquals => 2,
            Operator::Matches => 2,
            Operator::NotMatches => 2,
            Operator::StrictEquals => 2,
            Operator::StrictNotEquals => 2,
            Operator::LessThan => 2,
            Operator::GreaterThan => 2,
            Operator::LessThanOrEquals => 2,
            Operator::GreaterThanOrEquals => 2,
            Operator::And => 2,
            Operator::Or => 2,

            //ALL
            Operator::Comma => 2,
            Operator::GetItem => 2,
            Operator::OptionalGetItem => 2,
            Operator::Slice => 3,
            Operator::OptionalSlice => 3,
            Operator::MemberAccess => 2,
            Operator::MemberAccessThis => 1,
            Operator::OptionalMemberAccess => 2,
            Operator::MemberAccessPointer => 2,
        }
    }

    pub fn precedence(&self) -> isize {
        match self {
            //General
            Operator::Non => -1,
            Operator::Len => 1,
            Operator::DeepCopy => 1,
            Operator::Concat => 5,
            Operator::Spaceship => 10,
            Operator::Elvis => 13,
            Operator::NullCoalescing => 13,
            Operator::InlineIf => 14,

            //Math
            Operator::MathNon => -1,
            Operator::Pow => 2,
            Operator::Pos => 3,
            Operator::Inv => 3,
            Operator::BitwiseNot => 3,
            Operator::Inc => 3,
            Operator::Dec => 3,
            Operator::Mul => 4,
            Operator::Div => 4,
            Operator::TruncDiv => 4,
            Operator::FloorDiv => 4,
            Operator::CeilDiv => 4,
            Operator::Mod => 4,
            Operator::Add => 5,
            Operator::Sub => 5,
            Operator::Lshift => 6,
            Operator::Rshift => 6,
            Operator::Rzshift => 6,
            Operator::BitwiseAnd => 7,
            Operator::BitwiseXor => 8,
            Operator::BitwiseOr => 9,

            //Condition
            Operator::ConditionalNon => -1,
            Operator::Not => 3,
            Operator::InstanceOf => 10,
            Operator::Equals => 10,
            Operator::NotEquals => 10,
            Operator::Matches => 10,
            Operator::NotMatches => 10,
            Operator::StrictEquals => 10,
            Operator::StrictNotEquals => 10,
            Operator::LessThan => 10,
            Operator::GreaterThan => 10,
            Operator::LessThanOrEquals => 10,
            Operator::GreaterThanOrEquals => 10,
            Operator::And => 11,
            Operator::Or => 12,

            //ALL
            Operator::Comma => 15,
            Operator::GetItem => 0,
            Operator::OptionalGetItem => 0,
            Operator::Slice => 0,
            Operator::OptionalSlice => 0,
            Operator::MemberAccess => 0,
            Operator::MemberAccessThis => 0,
            Operator::OptionalMemberAccess => 0,
            Operator::MemberAccessPointer => 0,
        }
    }

    pub fn lazy_evaluation(&self) -> bool {
        matches!(
            self,

            //General
            Operator::Elvis |
            Operator::NullCoalescing |
            Operator::InlineIf |

            //Condition
            Operator::And |
            Operator::Or |

            //ALL
            Operator::MemberAccess |
            Operator::MemberAccessThis |
            Operator::OptionalMemberAccess |
            Operator::MemberAccessPointer
        )
    }

    pub fn operator_type(&self) -> OperatorType {
        match self {
            //General
            Operator::Non |
            Operator::Len |
            Operator::DeepCopy |
            Operator::Concat |
            Operator::Spaceship |
            Operator::Elvis |
            Operator::NullCoalescing |
            Operator::InlineIf => OperatorType::General,

            //Math
            Operator::MathNon |
            Operator::Pow |
            Operator::Pos |
            Operator::Inv |
            Operator::BitwiseNot |
            Operator::Inc |
            Operator::Dec |
            Operator::Mul |
            Operator::Div |
            Operator::TruncDiv |
            Operator::FloorDiv |
            Operator::CeilDiv |
            Operator::Mod |
            Operator::Add |
            Operator::Sub |
            Operator::Lshift |
            Operator::Rshift |
            Operator::Rzshift |
            Operator::BitwiseAnd |
            Operator::BitwiseXor |
            Operator::BitwiseOr => OperatorType::Math,

            //Condition
            Operator::ConditionalNon |
            Operator::Not |
            Operator::InstanceOf |
            Operator::Equals |
            Operator::NotEquals |
            Operator::Matches |
            Operator::NotMatches |
            Operator::StrictEquals |
            Operator::StrictNotEquals |
            Operator::LessThan |
            Operator::GreaterThan |
            Operator::LessThanOrEquals |
            Operator::GreaterThanOrEquals |
            Operator::And |
            Operator::Or => OperatorType::Condition,

            //ALL
            Operator::Comma |
            Operator::GetItem |
            Operator::OptionalGetItem |
            Operator::Slice |
            Operator::OptionalSlice |
            Operator::MemberAccess |
            Operator::MemberAccessThis |
            Operator::OptionalMemberAccess |
            Operator::MemberAccessPointer => OperatorType::All,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    function_name: String,
    overloaded: bool,
    combinator: bool,
    doc_comment: Option<String>,
    return_value_type_constraint: Option<String>,
    function_body: AST,
}

impl FunctionDefinition {
    pub fn new(function_name: String, overloaded: bool, combinator: bool, doc_comment: Option<String>, return_value_type_constraint: Option<String>, function_body: AST) -> Self {
        Self { function_name, overloaded, combinator, doc_comment, return_value_type_constraint, function_body }
    }

    pub fn function_name(&self) -> &str {
        &self.function_name
    }

    pub fn overloaded(&self) -> bool {
        self.overloaded
    }

    pub fn combinator(&self) -> bool {
        self.combinator
    }

    pub fn doc_comment(&self) -> Option<&str> {
        self.doc_comment.as_deref()
    }

    pub fn return_value_type_constraint(&self) -> Option<&str> {
        self.return_value_type_constraint.as_deref()
    }

    pub fn function_body(&self) -> &AST {
        &self.function_body
    }
}

#[derive(Debug, Clone)]
pub struct StructMember {
    name: String,
    type_constraint: Option<String>,
}

impl StructMember {
    pub fn new(name: String, type_constraint: Option<String>) -> Self {
        Self { name, type_constraint }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_constraint(&self) -> Option<&str> {
        self.type_constraint.as_deref()
    }
}

#[derive(Debug, Clone)]
pub struct StructDefinition {
    struct_name: String,

    members: Vec<StructMember>,
}

impl StructDefinition {
    pub fn new(struct_name: String, members: Vec<StructMember>) -> Self {
        Self { struct_name, members }
    }

    pub fn struct_name(&self) -> &str {
        &self.struct_name
    }

    pub fn members(&self) -> &[StructMember] {
        &self.members
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Visibility {
    Private,
    Protected,
    Public,
}

#[derive(Debug, Clone)]
pub struct ClassMember {
    name: String,
    type_constraint: Option<String>,
    value: Option<Node>,
    final_flag: bool,
    visibility: Visibility,
}

impl ClassMember {
    pub fn new(name: String, type_constraint: Option<String>, value: Option<Node>, final_flag: bool, visibility: Visibility) -> Self {
        Self { name, type_constraint, value, final_flag, visibility }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_constraint(&self) -> Option<&str> {
        self.type_constraint.as_deref()
    }

    pub fn value(&self) -> Option<&Node> {
        self.value.as_ref()
    }

    pub fn final_flag(&self) -> bool {
        self.final_flag
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    name: String,
    body: AST,
    override_flag: bool,
    visibility: Visibility,
}

impl Method {
    pub fn new(name: String, body: AST, override_flag: bool, visibility: Visibility) -> Self {
        Self { name, body, override_flag, visibility }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn body(&self) -> &AST {
        &self.body
    }

    pub fn override_flag(&self) -> bool {
        self.override_flag
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }
}

#[derive(Debug, Clone)]
pub struct Constructor {
    body: AST,
    visibility: Visibility,
}

impl Constructor {
    pub fn new(body: AST, visibility: Visibility) -> Self {
        Self { body, visibility }
    }

    pub fn body(&self) -> &AST {
        &self.body
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }
}

#[derive(Debug, Clone)]
pub struct ClassDefinition {
    class_name: String,

    static_members: Vec<ClassMember>,

    members: Vec<ClassMember>,

    /**
     * If multiple methods have the same name, they are overloaded
     */
    methods: Vec<Method>,

    constructors: Vec<Constructor>,

    /**
     * List of parent nodes separated by ArgumentSeparator nodes
     */
    parent_classes: Vec<Node>,
}

impl ClassDefinition {
    pub fn new(
        class_name: String,
        static_members: Vec<ClassMember>,
        members: Vec<ClassMember>,
        methods: Vec<Method>,
        constructors: Vec<Constructor>,
        parent_classes: Vec<Node>,
    ) -> Self {
        if members.iter().any(|member| member.value.is_some()) {
            panic!("Non-static class members can not have a default values");
        }

        Self {
            class_name,
            static_members,
            members,
            methods,
            constructors,
            parent_classes,
        }
    }

    pub fn class_name(&self) -> &str {
        &self.class_name
    }

    pub fn static_members(&self) -> &[ClassMember] {
        &self.static_members
    }

    pub fn members(&self) -> &[ClassMember] {
        &self.members
    }

    pub fn methods(&self) -> &[Method] {
        &self.methods
    }

    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    pub fn parent_classes(&self) -> &[Node] {
        &self.parent_classes
    }
}

#[derive(Debug, Clone)]
pub struct ConditionalNode {
    node: Node,
}

impl ConditionalNode {
    pub fn new(node: Node) -> Self {
        if !matches!(node.node_data, NodeData::Condition{..}) {
            panic!("Node type is not compatible with this node");
        }

        Self { node }
    }

    pub fn node(&self) -> &Node {
        &self.node
    }
}

#[derive(Debug, Clone)]
pub enum NodeData {
    List,

    ParsingError {
        error: ParsingError,
        message: String,
    },

    Assignment,

    EscapeSequence(char),
    UnicodeEscapeSequence(String),

    UnprocessedVariableName(String),
    VariableName {
        variable_name: String,
        type_constraint: String,
    },

    ArgumentSeparator(String),

    FunctionCall(String),
    FunctionCallPreviousNodeValue {
        leading_whitespace: String,
        trailing_whitespace: String,
    },

    FunctionDefinition(Box<FunctionDefinition>),

    IfStatementPartIf {
        if_body: AST,
        condition: Box<ConditionalNode>,
    },
    IfStatementPartElse(AST),
    IfStatement,

    LoopStatementPartLoop(AST),
    LoopStatementPartWhile {
        loop_body: AST,
        condition: Box<ConditionalNode>,
    },
    LoopStatementPartUntil {
        loop_body: AST,
        condition: Box<ConditionalNode>,
    },
    LoopStatementPartRepeat {
        loop_body: AST,
        var_pointer_node: Box<Node>,
        repeat_count_node: Box<Node>,
    },
    LoopStatementPartForEach {
        loop_body: AST,
        var_pointer_node: Box<Node>,
        composite_or_text_node: Box<Node>,
    },
    LoopStatementPartElse(AST),
    LoopStatement,

    TryStatementPartTry(AST),
    TryStatementPartSoftTry(AST),
    TryStatementPartNonTry(AST),
    TryStatementPartCatch {
        try_body: AST,
        errors: Option<Vec<Node>>,
    },
    TryStatementPartElse(AST),
    TryStatementPartFinally(AST),
    TryStatement,

    ContinueBreakStatement {
        number_node: Box<Node>,
        continue_node: bool,
    },

    Operation {
        operator: Operator,
        left_side_operand: Option<Box<Node>>,
        middle_operand: Option<Box<Node>>,
        right_side_operand: Option<Box<Node>>,
    },
    Math {
        operator: Operator,
        left_side_operand: Option<Box<Node>>,
        middle_operand: Option<Box<Node>>,
        right_side_operand: Option<Box<Node>>,
    },
    Condition {
        operator: Operator,
        left_side_operand: Option<Box<Node>>,
        middle_operand: Option<Box<Node>>,
        right_side_operand: Option<Box<Node>>,
    },

    Return,
    Throw,

    IntValue(i32),
    LongValue(i64),
    FloatValue(f32),
    DoubleValue(f64),
    CharValue(char),
    TextValue(String),
    NullValue,
    VoidValue,
    ArrayValue,

    StructDefinition(Box<StructDefinition>),

    ClassDefinition(Box<ClassDefinition>),
}

#[derive(Debug, Clone)]
pub struct Node {
    pos: CodePosition,
    child_nodes: Vec<Node>,
    node_data: NodeData,
}

impl Node {
    pub fn new_list_node(child_nodes: Vec<Node>) -> Node {
        let pos = if child_nodes.is_empty() {
            CodePosition::EMPTY
        }else {
            child_nodes.first().unwrap().pos().combine(&child_nodes.last().unwrap().pos())
        };

        Self {
            pos,
            child_nodes,
            node_data: NodeData::List,
        }
    }

    pub fn new_parsing_error_node(pos: CodePosition, error: ParsingError, message: impl Into<String>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::ParsingError {
                error,
                message: message.into(),
            },
        }
    }

    pub fn new_assignment_node(lvalue: Node, rvalue: Node) -> Node {
        let pos = lvalue.pos.combine(&rvalue.pos);

        let child_nodes = vec![lvalue, rvalue];

        Self {
            pos,
            child_nodes,
            node_data: NodeData::Assignment,
        }
    }

    pub fn new_escape_sequence_node(pos: CodePosition, char: char) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::EscapeSequence(char),
        }
    }

    pub fn new_unicode_escape_sequence_node(pos: CodePosition, hex_codepoint: impl Into<String>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::UnicodeEscapeSequence(hex_codepoint.into()),
        }
    }

    pub fn new_unprocessed_variable_name_node(pos: CodePosition, variable_name: impl Into<String>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::UnprocessedVariableName(variable_name.into()),
        }
    }

    pub fn new_variable_name_node(pos: CodePosition, variable_name: impl Into<String>, type_constraint: impl Into<String>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::VariableName {
                variable_name: variable_name.into(),
                type_constraint: type_constraint.into(),
            },
        }
    }

    pub fn new_argument_separator_node(pos: CodePosition, original_text: impl Into<String>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::ArgumentSeparator(original_text.into()),
        }
    }

    pub fn new_function_call_node(pos: CodePosition, argument_list: Vec<Node>, function_name: impl Into<String>) -> Node {
        Self {
            pos,
            child_nodes: argument_list,
            node_data: NodeData::FunctionCall(function_name.into()),
        }
    }

    pub fn new_function_call_previous_node_value_node(
        pos: CodePosition,
        argument_list: Vec<Node>,
        leading_whitespace: impl Into<String>,
        trailing_whitespace: impl Into<String>,
    ) -> Node {
        Self {
            pos,
            child_nodes: argument_list,
            node_data: NodeData::FunctionCallPreviousNodeValue {
                leading_whitespace: leading_whitespace.into(),
                trailing_whitespace: trailing_whitespace.into(),
            },
        }
    }

    pub fn new_function_definition_node(pos: CodePosition, function_definition: FunctionDefinition, parameter_list: Vec<Node>) -> Node {
        Self {
            pos,
            child_nodes: parameter_list,
            node_data: NodeData::FunctionDefinition(Box::new(function_definition)),
        }
    }

    pub fn new_if_statement_part_if_node(pos: CodePosition, if_body: AST, condition: ConditionalNode) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::IfStatementPartIf {
                if_body,
                condition: Box::new(condition),
            },
        }
    }

    pub fn new_if_statement_part_else_node(pos: CodePosition, if_body: AST) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::IfStatementPartElse(if_body),
        }
    }

    pub fn new_if_statement_node(pos: CodePosition, nodes: Vec<Node>) -> Node {
        if nodes.iter().any(|node| {
            matches!(node.node_data, NodeData::IfStatementPartIf{..}) ||
                    matches!(node.node_data, NodeData::IfStatementPartElse(..))
        }) {
            panic!("Node type is not compatible with this node");
        }

        Self {
            pos,
            child_nodes: nodes,
            node_data: NodeData::IfStatement,
        }
    }

    pub fn new_loop_statement_part_loop_node(pos: CodePosition, loop_body: AST) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::LoopStatementPartLoop(loop_body),
        }
    }

    pub fn new_loop_statement_part_while_node(pos: CodePosition, loop_body: AST, condition: ConditionalNode) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::LoopStatementPartWhile {
                loop_body,
                condition: Box::new(condition),
            },
        }
    }

    pub fn new_loop_statement_part_until_node(pos: CodePosition, loop_body: AST, condition: ConditionalNode) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::LoopStatementPartUntil {
                loop_body,
                condition: Box::new(condition),
            },
        }
    }

    pub fn new_loop_statement_part_repeat_node(pos: CodePosition, loop_body: AST, var_pointer_node: Node, repeat_count_node: Node) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::LoopStatementPartRepeat {
                loop_body,
                var_pointer_node: Box::new(var_pointer_node),
                repeat_count_node: Box::new(repeat_count_node),
            },
        }
    }

    pub fn new_loop_statement_part_for_each_node(pos: CodePosition, loop_body: AST, var_pointer_node: Node, composite_or_text_node: Node) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::LoopStatementPartForEach {
                loop_body,
                var_pointer_node: Box::new(var_pointer_node),
                composite_or_text_node: Box::new(composite_or_text_node),
            },
        }
    }

    pub fn new_loop_statement_part_else_node(pos: CodePosition, loop_body: AST) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::LoopStatementPartElse(loop_body),
        }
    }

    pub fn new_loop_statement_node(pos: CodePosition, nodes: Vec<Node>) -> Node {
        if nodes.iter().any(|node| {
            matches!(node.node_data, NodeData::LoopStatementPartLoop(..)) ||
                    matches!(node.node_data, NodeData::LoopStatementPartWhile{..}) ||
                    matches!(node.node_data, NodeData::LoopStatementPartUntil{..}) ||
                    matches!(node.node_data, NodeData::LoopStatementPartRepeat{..}) ||
                    matches!(node.node_data, NodeData::LoopStatementPartForEach{..}) ||
                    matches!(node.node_data, NodeData::LoopStatementPartElse(..))
        }) {
            panic!("Node type is not compatible with this node");
        }

        Self {
            pos,
            child_nodes: nodes,
            node_data: NodeData::LoopStatement,
        }
    }

    pub fn new_continue_break_statement_node(pos: CodePosition, number_node: Box<Node>, continue_node: bool) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::ContinueBreakStatement {
                number_node,
                continue_node,
            },
        }
    }

    pub fn new_try_statement_part_try_node(pos: CodePosition, try_body: AST) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::TryStatementPartTry(try_body),
        }
    }

    pub fn new_try_statement_part_soft_try_node(pos: CodePosition, try_body: AST) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::TryStatementPartSoftTry(try_body),
        }
    }

    pub fn new_try_statement_part_non_try_node(pos: CodePosition, try_body: AST) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::TryStatementPartNonTry(try_body),
        }
    }

    pub fn new_try_statement_part_catch_node(pos: CodePosition, try_body: AST, errors: Option<Vec<Node>>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::TryStatementPartCatch {
                try_body,
                errors,
            },
        }
    }

    pub fn new_try_statement_part_else_node(pos: CodePosition, try_body: AST) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::TryStatementPartElse(try_body),
        }
    }

    pub fn new_try_statement_part_finally_node(pos: CodePosition, try_body: AST) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::TryStatementPartFinally(try_body),
        }
    }

    pub fn new_try_statement_node(pos: CodePosition, nodes: Vec<Node>) -> Node {
        if nodes.iter().any(|node| {
            matches!(node.node_data, NodeData::TryStatementPartTry(..)) ||
                    matches!(node.node_data, NodeData::TryStatementPartSoftTry{..}) ||
                    matches!(node.node_data, NodeData::TryStatementPartNonTry{..}) ||
                    matches!(node.node_data, NodeData::TryStatementPartCatch{..}) ||
                    matches!(node.node_data, NodeData::TryStatementPartElse(..)) ||
                    matches!(node.node_data, NodeData::TryStatementPartFinally(..))
        }) {
            panic!("Node type is not compatible with this node");
        }

        Self {
            pos,
            child_nodes: nodes,
            node_data: NodeData::TryStatement,
        }
    }

    pub fn new_operation_statement_node(
        pos: CodePosition,
        left_side_operand: Option<Box<Node>>,
        middle_operand: Option<Box<Node>>,
        right_side_operand: Option<Box<Node>>,
        operator: Operator,
        operator_type: OperatorType,
    ) -> Node {
        let arity = operator.arity();
        if matches!(
            arity,

            1 if left_side_operand.is_none() || middle_operand.is_some() || right_side_operand.is_some(),
        ) {
            panic!("Invalid operand count for unary operator");
        }

        if matches!(
            arity,

            2 if left_side_operand.is_none() || middle_operand.is_some() || right_side_operand.is_none(),
        ) {
            panic!("Invalid operand count for binary operator");
        }

        if matches!(
            arity,

            3 if left_side_operand.is_none() || middle_operand.is_none() || right_side_operand.is_none(),
        ) {
            panic!("Invalid operand count for ternary operator");
        }

        if !operator.operator_type().is_compatible_with(operator_type) {
            panic!("Node type is not compatible with the operator");
        }

        Self {
            pos,
            child_nodes: Default::default(),
            node_data: match operator_type {
                OperatorType::All | OperatorType::General => NodeData::Operation {
                    left_side_operand,
                    middle_operand,
                    right_side_operand,
                    operator,
                },
                OperatorType::Math => NodeData::Math {
                    left_side_operand,
                    middle_operand,
                    right_side_operand,
                    operator,
                },
                OperatorType::Condition => NodeData::Condition {
                    left_side_operand,
                    middle_operand,
                    right_side_operand,
                    operator,
                },
            },
        }
    }

    pub fn new_return_statement_node(pos: CodePosition, return_value: Option<Node>) -> Node {
        Self {
            pos,
            child_nodes: match return_value {
                Some(return_value) => vec![return_value],
                None => vec![],
            },
            node_data: NodeData::Return,
        }
    }

    pub fn new_throw_statement_node(pos: CodePosition, throw_value: Node, message_value: Option<Node>) -> Node {
        Self {
            pos,
            child_nodes: match message_value {
                Some(message_value) => vec![throw_value, message_value],
                None => vec![throw_value],
            },
            node_data: NodeData::Throw,
        }
    }

    pub fn new_int_value_node(pos: CodePosition, value: i32) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::IntValue(value),
        }
    }

    pub fn new_long_value_node(pos: CodePosition, value: i64) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::LongValue(value),
        }
    }

    pub fn new_float_value_node(pos: CodePosition, value: f32) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::FloatValue(value),
        }
    }

    pub fn new_double_value_node(pos: CodePosition, value: f64) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::DoubleValue(value),
        }
    }

    pub fn new_char_value_node(pos: CodePosition, value: char) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::CharValue(value),
        }
    }

    pub fn new_text_value_node(pos: CodePosition, value: String) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::TextValue(value),
        }
    }

    pub fn new_null_value_node(pos: CodePosition) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::NullValue,
        }
    }

    pub fn new_void_value_node(pos: CodePosition) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::VoidValue,
        }
    }

    pub fn new_array_value_node(pos: CodePosition, child_nodes: Vec<Node>) -> Node {
        Self {
            pos,
            child_nodes,
            node_data: NodeData::ArrayValue,
        }
    }

    pub fn new_struct_definition_node(pos: CodePosition, struct_definition: StructDefinition) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::StructDefinition(Box::new(struct_definition)),
        }
    }

    pub fn new_class_definition_node(pos: CodePosition, class_definition: ClassDefinition) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::ClassDefinition(Box::new(class_definition)),
        }
    }

    pub fn pos(&self) -> CodePosition {
        self.pos
    }

    pub fn child_nodes(&self) -> &[Node] {
        &self.child_nodes
    }

    pub fn node_data(&self) -> &NodeData {
        &self.node_data
    }
}
