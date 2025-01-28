use std::fmt::{Display, Formatter, Write as _};
use std::mem;
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

impl Display for OperatorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            OperatorType::All => "ALL",
            OperatorType::General => "GENERAL",
            OperatorType::Math => "MATH",
            OperatorType::Condition => "CONDITION",
        })
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
    /// Parser-only operator (This operator can only be used during parsing)
    Comma,
    GetItem,
    OptionalGetItem,
    Slice,
    OptionalSlice,
    MemberAccess,
    /// [MemberAccess](Operator::MemberAccess) operator where composite type is `&this`
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

    pub fn is_unary(&self) -> bool {
        self.arity() == 1
    }

    pub fn is_binary(&self) -> bool {
        self.arity() == 2
    }

    pub fn is_ternary(&self) -> bool {
        self.arity() == 3
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

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            //General
            Operator::Non => "NON",
            Operator::Len => "LEN",
            Operator::DeepCopy => "DEEP_COPY",
            Operator::Concat => "CONCAT",
            Operator::Spaceship => "SPACESHIP",
            Operator::Elvis => "ELVIS",
            Operator::NullCoalescing => "NULL_COALESCING",
            Operator::InlineIf => "INLINE_IF",

            //Math
            Operator::MathNon => "MATH_NON",
            Operator::Pow => "POW",
            Operator::Pos => "POS",
            Operator::Inv => "INV",
            Operator::BitwiseNot => "BITWISE_NOT",
            Operator::Inc => "INC",
            Operator::Dec => "DEC",
            Operator::Mul => "MUL",
            Operator::Div => "DIV",
            Operator::TruncDiv => "TRUNC_DIV",
            Operator::FloorDiv => "FLOOR_DIV",
            Operator::CeilDiv => "CEIL_DIV",
            Operator::Mod => "MOD",
            Operator::Add => "ADD",
            Operator::Sub => "SUB",
            Operator::Lshift => "LSHIFT",
            Operator::Rshift => "RSHIFT",
            Operator::Rzshift => "RZSHIFT",
            Operator::BitwiseAnd => "BITWISE_AND",
            Operator::BitwiseXor => "BITWISE_XOR",
            Operator::BitwiseOr => "BITWISE_OR",

            //Condition
            Operator::ConditionalNon => "CONDITIONAL_NON",
            Operator::Not => "NOT",
            Operator::InstanceOf => "INSTANCE_OF",
            Operator::Equals => "EQUALS",
            Operator::NotEquals => "NOT_EQUALS",
            Operator::Matches => "MATCHES",
            Operator::NotMatches => "NOT_MATCHES",
            Operator::StrictEquals => "STRICT_EQUALS",
            Operator::StrictNotEquals => "STRICT_NOT_EQUALS",
            Operator::LessThan => "LESS_THAN",
            Operator::GreaterThan => "GREATER_THAN",
            Operator::LessThanOrEquals => "LESS_THAN_OR_EQUALS",
            Operator::GreaterThanOrEquals => "GREATER_THAN_OR_EQUALS",
            Operator::And => "AND",
            Operator::Or => "OR",

            //ALL
            Operator::Comma => "COMMA",
            Operator::GetItem => "GET_ITEM",
            Operator::OptionalGetItem => "OPTIONAL_GET_ITEM",
            Operator::Slice => "SLICE",
            Operator::OptionalSlice => "OPTIONAL_SLICE",
            Operator::MemberAccess => "MEMBER_ACCESS",
            Operator::MemberAccessThis => "MEMBER_ACCESS_THIS",
            Operator::OptionalMemberAccess => "OPTIONAL_MEMBER_ACCESS",
            Operator::MemberAccessPointer => "MEMBER_ACCESS_POINTER",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    function_name: Option<Box<str>>,
    overloaded: bool,
    combinator: bool,
    doc_comment: Option<Box<str>>,
    return_value_type_constraint: Option<Box<str>>,
    function_body: AST,
}

impl FunctionDefinition {
    pub fn new(function_name: Option<Box<str>>, overloaded: bool, combinator: bool, doc_comment: Option<Box<str>>, return_value_type_constraint: Option<Box<str>>, function_body: AST) -> Self {
        Self { function_name, overloaded, combinator, doc_comment, return_value_type_constraint, function_body }
    }

    pub fn function_name(&self) -> Option<&str> {
        self.function_name.as_deref()
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

#[derive(Debug, Clone, PartialEq)]
pub struct StructMember {
    name: Box<str>,
    type_constraint: Option<Box<str>>,
}

impl StructMember {
    pub fn new(name: Box<str>, type_constraint: Option<Box<str>>) -> Self {
        Self { name, type_constraint }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_constraint(&self) -> Option<&str> {
        self.type_constraint.as_deref()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    struct_name: Option<Box<str>>,

    members: Vec<StructMember>,
}

impl StructDefinition {
    pub fn new(struct_name: Option<Box<str>>, members: Vec<StructMember>) -> Self {
        Self { struct_name, members }
    }

    pub fn struct_name(&self) -> Option<&str> {
        self.struct_name.as_deref()
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

impl Visibility {
    pub fn from_symbol(symbol: u8) -> Option<Visibility> {
        match symbol {
            b'-' => Some(Visibility::Private),
            b'~' => Some(Visibility::Protected),
            b'+' => Some(Visibility::Public),

            _ => None,
        }
    }

    pub fn from_keyword(keyword: &str) -> Option<Visibility> {
        match keyword {
            "private" => Some(Visibility::Private),
            "protected" => Some(Visibility::Protected),
            "public" => Some(Visibility::Public),

            _ => None,
        }
    }

    pub fn symbol(&self) -> &'static str {
        match self {
            Visibility::Private => "-",
            Visibility::Protected => "~",
            Visibility::Public => "+",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassMember {
    name: Box<str>,
    type_constraint: Option<Box<str>>,
    value: Option<Node>,
    final_flag: bool,
    visibility: Visibility,
}

impl ClassMember {
    pub fn new(name: Box<str>, type_constraint: Option<Box<str>>, value: Option<Node>, final_flag: bool, visibility: Visibility) -> Self {
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

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    name: Box<str>,
    body: Node,
    override_flag: bool,
    visibility: Visibility,
}

impl Method {
    pub fn new(name: Box<str>, definition: Node, override_flag: bool, visibility: Visibility) -> Self {
        Self { name, body: definition, override_flag, visibility }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn body(&self) -> &Node {
        &self.body
    }

    pub fn override_flag(&self) -> bool {
        self.override_flag
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    body: Node,
    visibility: Visibility,
}

impl Constructor {
    pub fn new(definition: Node, visibility: Visibility) -> Self {
        Self { body: definition, visibility }
    }

    pub fn body(&self) -> &Node {
        &self.body
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    class_name: Option<Box<str>>,

    static_members: Vec<ClassMember>,

    members: Vec<ClassMember>,

    /// If multiple methods have the same name, they are overloaded
    methods: Vec<Method>,

    constructors: Vec<Constructor>,

    /// List of parent nodes separated by [ArgumentSeparator](NodeData::ArgumentSeparator) nodes
    parent_classes: Vec<Node>,
}

impl ClassDefinition {
    pub fn new(
        class_name: Option<Box<str>>,
        static_members: Vec<ClassMember>,
        members: Vec<ClassMember>,
        methods: Vec<Method>,
        constructors: Vec<Constructor>,
        parent_classes: Vec<Node>,
    ) -> Self {
        if members.iter().any(|member| member.value.is_some()) {
            panic!("Non-static class members can not have a default value");
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

    pub fn class_name(&self) -> Option<&str> {
        self.class_name.as_deref()
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

#[derive(Debug, Clone, PartialEq)]
pub struct OperationExpression {
    left_side_operand: Option<Box<Node>>,
    middle_operand: Option<Box<Node>>,
    right_side_operand: Option<Box<Node>>,
    operator: Operator,
    operator_type: OperatorType,
}

impl OperationExpression {
    pub fn new(
        left_side_operand: Option<Box<Node>>,
        middle_operand: Option<Box<Node>>,
        right_side_operand: Option<Box<Node>>,
        operator: Operator,
        operator_type: OperatorType,
    ) -> Self {
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
            left_side_operand,
            middle_operand,
            right_side_operand,
            operator,
            operator_type,
        }
    }

    pub fn left_side_operand(&self) -> Option<&Node> {
        self.left_side_operand.as_deref()
    }

    pub fn middle_operand(&self) -> Option<&Node> {
        self.middle_operand.as_deref()
    }

    pub fn right_side_operand(&self) -> Option<&Node> {
        self.right_side_operand.as_deref()
    }

    pub fn operator(&self) -> Operator {
        self.operator
    }

    pub fn operator_type(&self) -> OperatorType {
        self.operator.operator_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum NodeData {
    List,

    ParsingError {
        error: ParsingError,
        message: Box<str>,
    },

    Assignment,

    EscapeSequence(char),
    UnicodeEscapeSequence(Box<str>),

    UnprocessedVariableName(Box<str>),
    VariableName {
        variable_name: Box<str>,
        type_constraint: Option<Box<str>>,
    },

    ArgumentSeparator(Box<str>),

    FunctionCall(Box<str>),
    FunctionCallPreviousNodeValue {
        leading_whitespace: Box<str>,
        trailing_whitespace: Box<str>,
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
        number_node: Option<Box<Node>>,
        continue_node: bool,
    },

    Operation(OperationExpression),
    Math(OperationExpression),
    Condition(OperationExpression),

    Return,
    Throw,

    IntValue(i32),
    LongValue(i64),
    FloatValue(f32),
    DoubleValue(f64),
    CharValue(char),
    TextValue(Box<str>),
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

    pub fn new_parsing_error_node(pos: CodePosition, error: ParsingError, message: impl Into<Box<str>>) -> Node {
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

    pub fn new_unicode_escape_sequence_node(pos: CodePosition, hex_codepoint: impl Into<Box<str>>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::UnicodeEscapeSequence(hex_codepoint.into()),
        }
    }

    pub fn new_unprocessed_variable_name_node(pos: CodePosition, variable_name: impl Into<Box<str>>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::UnprocessedVariableName(variable_name.into()),
        }
    }

    pub fn new_variable_name_node(pos: CodePosition, variable_name: impl Into<Box<str>>, type_constraint: Option<Box<str>>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::VariableName {
                variable_name: variable_name.into(),
                type_constraint,
            },
        }
    }

    pub fn new_argument_separator_node(pos: CodePosition, original_text: impl Into<Box<str>>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::ArgumentSeparator(original_text.into()),
        }
    }

    pub fn new_function_call_node(pos: CodePosition, argument_list: Vec<Node>, function_name: impl Into<Box<str>>) -> Node {
        Self {
            pos,
            child_nodes: argument_list,
            node_data: NodeData::FunctionCall(function_name.into()),
        }
    }

    pub fn new_function_call_previous_node_value_node(
        pos: CodePosition,
        leading_whitespace: impl Into<Box<str>>,
        trailing_whitespace: impl Into<Box<str>>,
        argument_list: Vec<Node>,
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
            !matches!(node.node_data, NodeData::IfStatementPartIf{..}) &&
                    !matches!(node.node_data, NodeData::IfStatementPartElse(..))
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
            !matches!(node.node_data, NodeData::LoopStatementPartLoop(..)) &&
                    !matches!(node.node_data, NodeData::LoopStatementPartWhile{..}) &&
                    !matches!(node.node_data, NodeData::LoopStatementPartUntil{..}) &&
                    !matches!(node.node_data, NodeData::LoopStatementPartRepeat{..}) &&
                    !matches!(node.node_data, NodeData::LoopStatementPartForEach{..}) &&
                    !matches!(node.node_data, NodeData::LoopStatementPartElse(..))
        }) {
            panic!("Node type is not compatible with this node");
        }

        Self {
            pos,
            child_nodes: nodes,
            node_data: NodeData::LoopStatement,
        }
    }

    pub fn new_continue_break_statement_node(pos: CodePosition, number_node: Option<Box<Node>>, continue_node: bool) -> Node {
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
            !matches!(node.node_data, NodeData::TryStatementPartTry(..)) &&
                    !matches!(node.node_data, NodeData::TryStatementPartSoftTry{..}) &&
                    !matches!(node.node_data, NodeData::TryStatementPartNonTry{..}) &&
                    !matches!(node.node_data, NodeData::TryStatementPartCatch{..}) &&
                    !matches!(node.node_data, NodeData::TryStatementPartElse(..)) &&
                    !matches!(node.node_data, NodeData::TryStatementPartFinally(..))
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
        operation_expression: OperationExpression,
    ) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: match operation_expression.operator_type {
                OperatorType::All | OperatorType::General => NodeData::Operation(operation_expression),
                OperatorType::Math => NodeData::Math(operation_expression),
                OperatorType::Condition => NodeData::Condition(operation_expression),
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

    pub fn new_text_value_node(pos: CodePosition, value: impl Into<Box<str>>) -> Node {
        Self {
            pos,
            child_nodes: Default::default(),
            node_data: NodeData::TextValue(value.into()),
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

    pub fn operator(&self) -> Option<Operator> {
        match &self.node_data {
            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) =>
                Some(operation_expression.operator),

            _ => None,
        }
    }

    pub fn left_side_operand(&self) -> Option<&Node> {
        match &self.node_data {
            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) =>
                operation_expression.left_side_operand.as_deref(),

            _ => None,
        }
    }

    pub fn into_left_side_operand(self) -> Option<Node> {
        match self.node_data {
            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) =>
                operation_expression.left_side_operand.map(|node| *node),

            _ => None,
        }
    }

    pub fn middle_operand(&self) -> Option<&Node> {
        match &self.node_data {
            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) =>
                operation_expression.middle_operand.as_deref(),

            _ => None,
        }
    }

    pub fn into_middle_operand(self) -> Option<Node> {
        match self.node_data {
            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) =>
                operation_expression.middle_operand.map(|node| *node),

            _ => None,
        }
    }

    pub fn right_side_operand(&self) -> Option<&Node> {
        match &self.node_data {
            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) =>
                operation_expression.right_side_operand.as_deref(),

            _ => None,
        }
    }

    pub fn into_right_side_operand(self) -> Option<Node> {
        match self.node_data {
            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) =>
                operation_expression.right_side_operand.map(|node| *node),

            _ => None,
        }
    }

    pub fn operands(&self) -> (Option<&Node>, Option<&Node>, Option<&Node>) {
        match &self.node_data {
            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) =>
                (
                    operation_expression.left_side_operand.as_deref(),
                    operation_expression.middle_operand.as_deref(),
                    operation_expression.right_side_operand.as_deref(),
                ),

            _ => (None, None, None),
        }
    }

    pub fn into_operands(self) -> (Option<Node>, Option<Node>, Option<Node>) {
        match self.node_data {
            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) =>
                (
                    operation_expression.left_side_operand.map(|node| *node),
                    operation_expression.middle_operand.map(|node| *node),
                    operation_expression.right_side_operand.map(|node| *node),
                ),

            _ => (None, None, None),
        }
    }

    fn try_evaluate_escape_sequence(char: char) -> Option<String> {
        match char {
            '0' => Some("\0".to_string()),
            'n' => Some("\n".to_string()),
            'r' => Some("\r".to_string()),
            'f' => Some("\u{000C}".to_string()),
            's' => Some(" ".to_string()),
            'e' => Some(String::new()),
            'E' => Some("\u{001B}".to_string()),
            'b' => Some("\u{0008}".to_string()),
            't' => Some("\t".to_string()),

            '$' | '&' | '#' | ',' | '.' | '(' | ')' | '[' | ']' | '{' | '}' | '=' | '<' | '>' |
            '+' | '-' | '/' | '*' | '%' | '|' | '~' | '^' | '?' | ':' | '@' | '\u{25b2}' |
            '\u{25bc}' | '\"' | '\\' => Some(char.to_string()),

            _ => None,
        }
    }

    pub(super) fn optimize_nodes(nodes: &mut Vec<Node>) {
        let mut i = 0;
        while i < nodes.len() {
            nodes[i].optimize_node();

            let mut builder = None;
            if let NodeData::EscapeSequence(char) = nodes[i].node_data() {
                builder = Self::try_evaluate_escape_sequence(*char);
            }else if let NodeData::TextValue(str) = nodes[i].node_data() {
                builder = Some(str.to_string());
            }else if let NodeData::CharValue(char) = nodes[i].node_data() {
                builder = Some(char.to_string());
            }

            if let Some(mut builder) = builder {
                let mut pos = nodes[i].pos();

                let mut changed = false;
                let index = i + 1;
                while index < nodes.len() {
                    match nodes[index].node_data() {
                        NodeData::EscapeSequence(char) => {
                            if let Some(str) = Self::try_evaluate_escape_sequence(*char) {
                                changed = true;

                                builder += &str;
                                pos = pos.combine(&nodes[index].pos());

                                nodes.remove(index);
                                continue;
                            }

                            break;
                        },

                        NodeData::TextValue(str) => {
                            changed = true;

                            builder += str;
                            pos = pos.combine(&nodes[index].pos());

                            nodes.remove(index);
                            continue;
                        },

                        NodeData::CharValue(char) => {
                            changed = true;

                            builder += &char.to_string();
                            pos = pos.combine(&nodes[index].pos());

                            nodes.remove(index);
                            continue;
                        },

                        _ => break,
                    }
                }

                if changed {
                    nodes[i] = Node::new_text_value_node(pos, builder);
                }
            }

            i += 1;
        }
    }

    pub(super) fn optimize_node(&mut self) {
        if matches!(self.node_data, NodeData::List | NodeData::FunctionCall(..) |
            NodeData::FunctionCallPreviousNodeValue { .. } | NodeData::IfStatement |
            NodeData::LoopStatement | NodeData::TryStatement) {
            Self::optimize_nodes(&mut self.child_nodes);
        }else {
            for node in &mut self.child_nodes {
                node.optimize_node();
            }
        }

        match &mut self.node_data {
            NodeData::List if self.child_nodes.len() == 1 => {
                let _ = mem::replace(self, self.child_nodes[0].clone());
            },

            NodeData::FunctionDefinition(function_definition) => {
                function_definition.function_body.optimize_ast();
            },

            NodeData::IfStatementPartIf { if_body, condition } => {
                if_body.optimize_ast();

                Self::optimize_node(&mut condition.node);
            },

            NodeData::IfStatementPartElse(if_body) => {
                if_body.optimize_ast();
            },

            NodeData::LoopStatementPartLoop(loop_body) |
            NodeData::LoopStatementPartElse(loop_body) => {
                loop_body.optimize_ast();
            },

            NodeData::LoopStatementPartWhile { loop_body, condition } |
            NodeData::LoopStatementPartUntil { loop_body, condition } => {
                loop_body.optimize_ast();

                Self::optimize_node(&mut condition.node);
            },

            NodeData::LoopStatementPartRepeat {
                loop_body,
                var_pointer_node,
                repeat_count_node,
            } => {
                loop_body.optimize_ast();

                Self::optimize_node(var_pointer_node);
                Self::optimize_node(repeat_count_node);
            },

            NodeData::LoopStatementPartForEach {
                loop_body,
                var_pointer_node,
                composite_or_text_node,
            } => {
                loop_body.optimize_ast();

                Self::optimize_node(var_pointer_node);
                Self::optimize_node(composite_or_text_node);
            },

            NodeData::TryStatementPartTry(try_body) |
            NodeData::TryStatementPartSoftTry(try_body) |
            NodeData::TryStatementPartNonTry(try_body) |
            NodeData::TryStatementPartElse(try_body) |
            NodeData::TryStatementPartFinally(try_body) => {
                try_body.optimize_ast();
            },

            NodeData::TryStatementPartCatch { try_body, errors } => {
                try_body.optimize_ast();

                if let Some(errors) = errors {
                    Self::optimize_nodes(errors);
                }
            },

            NodeData::ContinueBreakStatement { number_node: Some(number_node), .. } => {
                Self::optimize_node(number_node);
            },

            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) => {
                if let Some(node) = &mut operation_expression.left_side_operand {
                    Self::optimize_node(node);
                }

                if let Some(node) = &mut operation_expression.middle_operand {
                    Self::optimize_node(node);
                }

                if let Some(node) = &mut operation_expression.right_side_operand {
                    Self::optimize_node(node);
                }

                if matches!(operation_expression.operator(), Operator::Non | Operator::MathNon |
                    Operator::ConditionalNon) {
                    let mut inner_non_value = None;

                    if let Some(node) = operation_expression.left_side_operand.as_deref() {
                        if let
                                NodeData::Operation(inner_operation_expression) |
                                NodeData::Math(inner_operation_expression) |
                                NodeData::Condition(inner_operation_expression) = &node.node_data {
                            if matches!(inner_operation_expression.operator(), Operator::Non) ||
                                    mem::discriminant(&operation_expression.operator) ==
                                            mem::discriminant(&inner_operation_expression.operator) {
                                inner_non_value.clone_from(&inner_operation_expression.left_side_operand);
                            }
                        }
                    }

                    if inner_non_value.is_some() {
                        operation_expression.left_side_operand = inner_non_value;
                    }
                }
            },

            NodeData::ClassDefinition(class_definition) => {
                for member in &mut class_definition.static_members {
                    if let Some(value) = &mut member.value {
                        Self::optimize_node(value);
                    }
                }

                for member in &mut class_definition.members {
                    if let Some(value) = &mut member.value {
                        Self::optimize_node(value);
                    }
                }

                for method in &mut class_definition.methods {
                    Self::optimize_node(&mut method.body);
                }

                for constructor in &mut class_definition.constructors {
                    Self::optimize_node(&mut constructor.body);
                }
            },

            _ => {},
        }
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.child_nodes == other.child_nodes && self.node_data == other.node_data
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = String::new();

        match &self.node_data {
            NodeData::List => {
                let _ = write!(builder, "ListNode: Position: {}", self.pos.to_compact_string());
                builder += ", Children: {\n";
                for node in self.child_nodes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                builder += "}";
            },

            NodeData::ParsingError { error, message } => {
                let _ = write!(builder, "ParsingErrorNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Error: \"{error}\"");
                let _ = write!(builder, ", Message: \"{message}\"");
            },

            NodeData::Assignment => {
                let _ = write!(builder, "AssignmentNode: Position: {}", self.pos.to_compact_string());
                builder += ", lvalue: {\n";
                for token in format!("{}", self.child_nodes[0]).split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}, rvalue: {\n";
                for token in format!("{}", self.child_nodes[1]).split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::EscapeSequence(char) => {
                let _ = write!(builder, "EscapeSequenceNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Char: \"{char}\"");
            },

            NodeData::UnicodeEscapeSequence(hex_code_point) => {
                let _ = write!(builder, "UnicodeEscapeSequenceNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", hexCodePoint: \"{hex_code_point}\"");
            },

            NodeData::UnprocessedVariableName(variable_name) => {
                let _ = write!(builder, "UnprocessedVariableNameNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Variable Name: \"{variable_name}\"");
            },

            NodeData::VariableName { variable_name, type_constraint } => {
                let _ = write!(builder, "VariableNameNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Variable Name: \"{variable_name}\"");
                let _ = write!(builder, ", TypeConstraint: \"{}\"", type_constraint.as_deref().unwrap_or("null"));
            },

            NodeData::ArgumentSeparator(original_text) => {
                let _ = write!(builder, "ArgumentSeparatorNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", OriginalText: \"{original_text}\"");
            },

            NodeData::FunctionCall(function_name) => {
                let _ = write!(builder, "FunctionCallNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", FunctionName: \"{function_name}\"");
                builder += ", ParameterList: {\n";
                for node in self.child_nodes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                builder += "}";
            },

            NodeData::FunctionCallPreviousNodeValue { leading_whitespace, trailing_whitespace } => {
                let _ = write!(builder, "FunctionCallNode: Position: {}", self.pos.to_compact_string());
                builder += ", ArgumentList: {\n";
                for node in self.child_nodes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                let _ = write!(builder, "}}, LeadingWhitespace: \"{leading_whitespace}\"");
                let _ = write!(builder, ", TrailingWhitespace: \"{trailing_whitespace}\"");
            },

            NodeData::FunctionDefinition(function_definition) => {
                let _ = write!(builder, "FunctionDefinitionNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", FunctionName: \"{}\"", function_definition.function_name.as_deref().unwrap_or("null"));
                let _ = write!(builder, ", Overloaded: \"{}\"", function_definition.overloaded);
                let _ = write!(builder, ", Combinator: \"{}\"", function_definition.combinator);
                let _ = write!(builder, ", Doc Comment: \"{}\"", function_definition.doc_comment.as_deref().unwrap_or("null"));
                builder += ", ParameterList: {\n";
                for node in self.child_nodes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                let _ = write!(builder, 
                    "}}, ReturnValueTypeConstraint: \"{}\"",
                    function_definition.return_value_type_constraint.as_deref().unwrap_or("null"),
                );
                builder += ", FunctionBody: {\n";
                for token in function_definition.function_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::IfStatementPartIf { if_body, condition } => {
                let _ = write!(builder, "IfStatementPartIfNode: Position: {}", self.pos.to_compact_string());
                builder += ", Condition: {\n";
                for token in condition.node.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}, IfBody: {\n";
                for token in if_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::IfStatementPartElse(if_body) => {
                let _ = write!(builder, "IfStatementPartElseNode: Position: {}", self.pos.to_compact_string());
                builder += ", IfBody: {\n";
                for token in if_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::IfStatement => {
                let _ = write!(builder, "IfStatementNode: Position: {}", self.pos.to_compact_string());
                builder += ", Children: {\n";
                for node in self.child_nodes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                builder += "}";
            },

            NodeData::LoopStatementPartLoop(loop_body) => {
                let _ = write!(builder, "LoopStatementPartLoopNode: Position: {}", self.pos.to_compact_string());
                builder += ", LoopBody: {\n";
                for token in loop_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::LoopStatementPartWhile { loop_body, condition } => {
                let _ = write!(builder, "LoopStatementPartWhileNode: Position: {}", self.pos.to_compact_string());
                builder += ", Condition: {\n";
                for token in condition.node.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}, LoopBody: {\n";
                for token in loop_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::LoopStatementPartUntil { loop_body, condition } => {
                let _ = write!(builder, "LoopStatementPartUntilNode: Position: {}", self.pos.to_compact_string());
                builder += ", Condition: {\n";
                for token in condition.node.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}, LoopBody: {\n";
                for token in loop_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::LoopStatementPartRepeat {
                loop_body,
                var_pointer_node,
                repeat_count_node,
            } => {
                let _ = write!(builder, "LoopStatementPartRepeatNode: Position: {}", self.pos.to_compact_string());
                builder += ", varPointer: {\n";
                for token in var_pointer_node.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}, repeatCount: {\n";
                for token in repeat_count_node.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}, LoopBody: {\n";
                for token in loop_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::LoopStatementPartForEach {
                loop_body,
                var_pointer_node,
                composite_or_text_node,
            } => {
                let _ = write!(builder, "LoopStatementPartForEachNode: Position: {}", self.pos.to_compact_string());
                builder += ", Condition: {\n";
                for token in var_pointer_node.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}, compositeOrTextNode: {\n";
                for token in composite_or_text_node.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}, LoopBody: {\n";
                for token in loop_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::LoopStatementPartElse(loop_body) => {
                let _ = write!(builder, "LoopStatementPartElseNode: Position: {}", self.pos.to_compact_string());
                builder += ", LoopBody: {\n";
                for token in loop_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::LoopStatement => {
                let _ = write!(builder, "LoopStatementNode: Position: {}", self.pos.to_compact_string());
                builder += ", Children: {\n";
                for node in self.child_nodes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                builder += "}";
            },

            NodeData::TryStatementPartTry(try_body) => {
                let _ = write!(builder, "TryStatementPartTryNode: Position: {}", self.pos.to_compact_string());
                builder += ", TryBody: {\n";
                for token in try_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::TryStatementPartSoftTry(try_body) => {
                let _ = write!(builder, "TryStatementPartSoftTryNode: Position: {}", self.pos.to_compact_string());
                builder += ", TryBody: {\n";
                for token in try_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::TryStatementPartNonTry(try_body) => {
                let _ = write!(builder, "TryStatementPartNonTryNode: Position: {}", self.pos.to_compact_string());
                builder += ", TryBody: {\n";
                for token in try_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::TryStatementPartCatch { try_body, errors } => {
                let _ = write!(builder, "TryStatementPartCatchNode: Position: {}", self.pos.to_compact_string());
                builder += ", Errors: {\n";
                if let Some(errors) = errors {
                    for node in errors.iter() {
                        for token in node.to_string().split("\n") {
                            let _ = writeln!(builder, "\t{token}");
                        }
                    }
                }else {
                    builder += "\tnull\n";
                }
                builder += "}, TryBody: {\n";
                for token in try_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::TryStatementPartElse(try_body) => {
                let _ = write!(builder, "TryStatementPartElseNode: Position: {}", self.pos.to_compact_string());
                builder += ", TryBody: {\n";
                for token in try_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::TryStatementPartFinally(try_body) => {
                let _ = write!(builder, "TryStatementPartFinallyNode: Position: {}", self.pos.to_compact_string());
                builder += ", TryBody: {\n";
                for token in try_body.to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}";
            },

            NodeData::TryStatement => {
                let _ = write!(builder, "TryStatementNode: Position: {}", self.pos.to_compact_string());
                builder += ", Children: {\n";
                for node in self.child_nodes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                builder += "}";
            },

            NodeData::ContinueBreakStatement { number_node, continue_node } => {
                let _ = write!(builder, "LoopStatementContinueBreakStatementNode: Position: {}", self.pos.to_compact_string());
                builder += ", numberNode: {\n";
                if let Some(number_node) = number_node {
                    for token in number_node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }else {
                    builder += "\tnull\n";
                }
                let _ = write!(builder, "}}, continueNode: \"{}\"", continue_node);
            },

            NodeData::Operation(operation_expression) |
            NodeData::Math(operation_expression) |
            NodeData::Condition(operation_expression) => {
                let _ = write!(builder, "OperationNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", NodeType: \"{}\"", operation_expression.operator_type);
                let _ = write!(builder, ", Operator: \"{}\"", operation_expression.operator);
                let _ = write!(builder, ", OperatorType: \"{}\"", operation_expression.operator.operator_type());
                builder += ", Operands: {\n";
                for node in operation_expression.left_side_operand.iter().
                        chain(operation_expression.middle_operand.iter()).
                        chain(operation_expression.right_side_operand.iter()) {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                builder += "}";
            },

            NodeData::Return => {
                let _ = write!(builder, "ReturnNode: Position: {}", self.pos.to_compact_string());
                builder += ", Children: {\n";
                for node in self.child_nodes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                builder += "}";
            },

            NodeData::Throw => {
                let _ = write!(builder, "ThrowNode: Position: {}", self.pos.to_compact_string());
                builder += ", ThrowValue: {\n";
                for token in self.child_nodes[0].to_string().split("\n") {
                    let _ = writeln!(builder, "\t{token}");
                }
                builder += "}, Message: ";
                if let Some(message) = self.child_nodes.get(1) {
                    builder += "{\n";
                    for token in message.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                    builder += "}";
                }else {
                    builder += "null";
                }
            },

            NodeData::IntValue(value) => {
                let _ = write!(builder, "IntValueNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Value: \"{value}\"");
            },

            NodeData::LongValue(value) => {
                let _ = write!(builder, "LongValueNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Value: \"{value}\"");
            },

            NodeData::FloatValue(value) => {
                let _ = write!(builder, "FloatValueNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Value: \"{value}\"");
            },

            NodeData::DoubleValue(value) => {
                let _ = write!(builder, "DoubleValueNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Value: \"{value}\"");
            },

            NodeData::CharValue(value) => {
                let _ = write!(builder, "CharValueNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Value: \"{value}\"");
            },

            NodeData::TextValue(value) => {
                let _ = write!(builder, "TextValueNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", Value: \"{value}\"");
            },

            NodeData::NullValue => {
                let _ = write!(builder, "NullValueNode: Position: {}", self.pos.to_compact_string());
            },

            NodeData::VoidValue => {
                let _ = write!(builder, "VoidValueNode: Position: {}", self.pos.to_compact_string());
            },

            NodeData::ArrayValue => {
                let _ = write!(builder, "ArrayNode: Position: {}", self.pos.to_compact_string());
                builder += ", Elements: {\n";
                for node in self.child_nodes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                builder += "}";
            },

            NodeData::StructDefinition(struct_definition) => {
                let _ = write!(builder, "StructDefinitionNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", StructName: \"{}\"", struct_definition.struct_name.as_deref().unwrap_or("null"));
                builder += ", Members{TypeConstraints}: {\n";
                for member in struct_definition.members.iter() {
                    let _ = write!(builder, "\t{}", member.name);
                    if let Some(type_constraint) = &member.type_constraint {
                        let _ = write!(builder, "{{{type_constraint}}}");
                    }
                    builder += "\n";
                }
                builder += "}";
            },

            NodeData::ClassDefinition(class_definition) => {
                let _ = write!(builder, "ClassDefinitionNode: Position: {}", self.pos.to_compact_string());
                let _ = write!(builder, ", ClassName: \"{}\"", class_definition.class_name.as_deref().unwrap_or("null"));
                builder += ", StaticMembers{TypeConstraints} = <value>: {\n";
                for member in class_definition.static_members.iter() {
                    let _ = write!(builder, "\t{}{}{}", member.visibility.symbol(), if member.final_flag {
                        "final:"
                    } else {
                        ""
                    }, member.name);
                    if let Some(type_constraint) = &member.type_constraint {
                        let _ = write!(builder, "{{{type_constraint}}}");
                    }
                    if let Some(value) = &member.value {
                        builder += "= {\n";
                        for token in value.to_string().split("\n") {
                            let _ = writeln!(builder, "\t\t{token}");
                        }
                        builder += "\t}";
                    }
                    builder += "\n";
                }
                builder += "}, Members{TypeConstraints}: {\n";
                for member in class_definition.members.iter() {
                    let _ = write!(builder, "\t{}{}{}", member.visibility.symbol(), if member.final_flag {
                        "final:"
                    } else {
                        ""
                    }, member.name);
                    if let Some(type_constraint) = &member.type_constraint {
                        let _ = write!(builder, "{{{type_constraint}}}");
                    }
                    builder += "\n";
                }
                builder += "}, MethodName = <definition>: {\n";
                for method in class_definition.methods.iter() {
                    let _ = writeln!(builder, "\t{}{}{} = {{", method.visibility.symbol(), if method.override_flag {
                        "override:"
                    } else {
                        ""
                    }, method.name);
                    for token in method.body.to_string().split("\n") {
                        let _ = writeln!(builder, "\t\t{token}");
                    }
                    builder += "\t}\n";
                }
                builder += "}, Constructors: {\n";
                for constructor in class_definition.constructors.iter() {
                    let _ = writeln!(builder, "\t{}construct = {{", constructor.visibility.symbol());
                    for token in constructor.body.to_string().split("\n") {
                        let _ = writeln!(builder, "\t\t{token}");
                    }
                    builder += "\t}\n";
                }
                builder += "}, ParentClasses: {\n";
                for node in class_definition.parent_classes.iter() {
                    for token in node.to_string().split("\n") {
                        let _ = writeln!(builder, "\t{token}");
                    }
                }
                builder += "}";
            },
        }

        f.write_str(builder.as_str())
    }
}
