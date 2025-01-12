pub mod ast;

#[cfg(test)]
mod tests;

use crate::lexer::{CodePosition, Lexer, Token, TokenType};
use crate::parser::ast::{
    ClassDefinition, ClassMember, ConditionalNode, Constructor, FunctionDefinition,
    Method, Node, NodeData, OperationExpression, Operator, OperatorType,
    StructDefinition, StructMember, Visibility, AST
};
use crate::{regex_patterns, utils};

use std::collections::VecDeque;
use std::mem;
use std::str::FromStr;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParsingError {
    BracketMismatch,
    ContFlowArgMissing,
    Eof,
    InvalidConPart,
    InvalidAssignment,
    InvalidParameter,
    LexerError,
}

impl ParsingError {
    pub fn error_code(&self) -> i32 {
        match self {
            ParsingError::BracketMismatch => -1,
            ParsingError::ContFlowArgMissing => -2,
            ParsingError::Eof => -3,
            ParsingError::InvalidConPart => -4,
            ParsingError::InvalidAssignment => -5,
            ParsingError::InvalidParameter => -6,
            ParsingError::LexerError => -7,
        }
    }

    pub fn error_text(&self) -> &'static str {
        match self {
            ParsingError::BracketMismatch => "Bracket mismatch",
            ParsingError::ContFlowArgMissing => "Control flow statement condition(s) or argument(s) is/are missing",
            ParsingError::Eof => "End of file was reached early",
            ParsingError::InvalidConPart => "Invalid statement part in control flow statement",
            ParsingError::InvalidAssignment => "Invalid assignment operation",
            ParsingError::InvalidParameter => "Invalid function parameter",
            ParsingError::LexerError => "Error during lexical parsing",
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    lang_doc_comment: Option<String>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            lexer: Lexer::new(),
            lang_doc_comment: None,
        }
    }

    pub fn reset_position_vars(&mut self) {
        self.lexer.reset_position_vars();
        self.lang_doc_comment = None;
    }

    pub fn line_number(&self) -> usize {
        self.lexer.line_number()
    }

    pub fn set_line_number(&mut self, line_number: usize) {
        self.lexer.set_line_number(line_number);
        self.lexer.set_column(1);
    }

    pub fn parse_lines(&mut self, lines: impl Into<String>) -> Option<AST> {
        let tokens = VecDeque::from(self.lexer.read_tokens(lines));

        self.parse_tokens(tokens)
    }

    pub fn parse_tokens(&mut self, mut tokens: VecDeque<Token>) -> Option<AST> {
        Self::remove_line_continuation_and_single_line_text_quotes_tokens(&mut tokens);

        self.parse_tokens_internal(&mut tokens).map(|mut ast| {
            ast.optimize_ast();

            ast
        })
    }

    fn parse_tokens_internal(&mut self, tokens: &mut VecDeque<Token>) -> Option<AST> {
        if tokens.is_empty() {
            return None;
        }

        let mut ast = AST::new();
        let mut block_pos = 0;

        let mut error_nodes = Vec::new();

        while !tokens.is_empty() {
            Self::trim_first_line(tokens);

            self.parse_comment_tokens(tokens, &mut error_nodes);
            if !error_nodes.is_empty() {
                if !error_nodes.is_empty() {
                    error_nodes.into_iter().for_each(|token| ast.add_child(token));
                }

                break;
            }

            Self::trim_first_line(tokens);

            if tokens.is_empty() {
                break;
            }

            if matches!(tokens[0].token_type(), TokenType::Eol) {
                tokens.pop_front();

                continue;
            }

            if matches!(tokens[0].token_type(), TokenType::Eof) {
                let token = tokens.pop_front().unwrap();

                if !tokens.is_empty() {
                    ast.add_child(Node::new_parsing_error_node(
                        token.pos(),
                        ParsingError::LexerError,
                        "Tokens after EOF are not allowed",
                    ));
                }

                break;
            }

            let current_token = &tokens[0];

            //Blocks
            if matches!(current_token.token_type(), TokenType::OpeningBlockBracket) {
                tokens.pop_front();

                block_pos += 1;

                continue;
            }else if matches!(current_token.token_type(), TokenType::ClosingBlockBracket) {
                tokens.pop_front();

                if block_pos == 0 {
                    break;
                }

                block_pos -= 1;

                continue;
            }

            //Assignments
            if !matches!(current_token.token_type(), TokenType::Other) || !(
                    current_token.value() == "return" || current_token.value() == "throw") {
                let returned_node = self.parse_assignment(tokens, false);
                if let Some(returned_node) = returned_node {
                    ast.add_child(returned_node);

                    continue;
                }
            }

            //Non assignments
            let returned_ast = self.parse_line(tokens);
            if let Some(returned_ast) = returned_ast {
                ast.add_child(returned_ast.into_node());
            }else {
                //End of if
                return Some(ast);
            }
        }

        Some(ast)
    }

    #[inline(always)]
    fn parse_condition_expr(&mut self, tokens: &mut VecDeque<Token>) -> Option<Node> {
        self.parse_operator_expr(tokens, OperatorType::Condition)
    }

    #[inline(always)]
    fn parse_math_expr(&mut self, tokens: &mut VecDeque<Token>) -> Option<Node> {
        self.parse_operator_expr(tokens, OperatorType::Math)
    }

    #[inline(always)]
    fn parse_operation_expr(&mut self, tokens: &mut VecDeque<Token>) -> Option<Node> {
        self.parse_operator_expr(tokens, OperatorType::General)
    }

    #[inline(always)]
    fn parse_operator_expr(&mut self, tokens: &mut VecDeque<Token>, operator_type: OperatorType) -> Option<Node> {
        self.parse_operator_expression(tokens, &mut None, &mut None, 0, operator_type)
    }

    fn parse_operator_expression(
        &mut self,
        tokens: &mut VecDeque<Token>,
        tokens_left: &mut Option<&mut VecDeque<Token>>,
        tokens_left_behind_middle_part_end: &mut Option<&mut VecDeque<Token>>,
        current_operator_precedence: isize,
        operator_type: OperatorType,
    ) -> Option<Node> {
        let non_operator = match operator_type {
            OperatorType::Math => Operator::MathNon,
            OperatorType::Condition => Operator::ConditionalNon,
            OperatorType::General => Operator::Non,
            OperatorType::All => {
                return None;
            },
        };

        Self::trim_first_line(tokens);

        let mut operator = None;
        let mut left_nodes = Vec::new();
        let mut middle_node = None;
        let mut right_node = None;

        let mut whitespaces = VecDeque::new();

        let mut other_tokens = VecDeque::new();

        'tokenProcessing:
        while !tokens.is_empty() {
            let t = tokens[0].clone();

            match t.token_type() {
                TokenType::Eol | TokenType::Eof => {
                    break 'tokenProcessing;
                },

                TokenType::StartComment | TokenType::StartDocComment => {
                    self.parse_comment_tokens(tokens, &mut left_nodes);
                },

                TokenType::LiteralNull | TokenType::LiteralText | TokenType::LiteralNumber |
                TokenType::EscapeSequence | TokenType::Assignment | TokenType::ClosingBracket |
                TokenType::LexerError =>  {
                    if !whitespaces.is_empty() {
                        other_tokens.append(&mut whitespaces);
                    }

                    if !other_tokens.is_empty() {
                        self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                        other_tokens.clear();
                    }

                    tokens.pop_front();

                    match t.token_type() {
                        TokenType::LiteralNull => {
                            left_nodes.push(Node::new_null_value_node(t.pos()));
                        },

                        TokenType::LiteralText | TokenType::Assignment | TokenType::ClosingBracket => {
                            left_nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                        },

                        TokenType::LiteralNumber => {
                            self.parse_number_token(t, &mut left_nodes);
                        },

                        TokenType::EscapeSequence => {
                            self.parse_escape_sequence_token(t, &mut left_nodes);
                        },

                        TokenType::LexerError => {
                            self.parse_lexer_error_token(t, &mut left_nodes);
                        },

                        _ => {},
                    }
                },

                TokenType::StartMultilineText => {
                    if !whitespaces.is_empty() {
                        other_tokens.append(&mut whitespaces);
                    }

                    if !other_tokens.is_empty() {
                        self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                        other_tokens.clear();
                    }

                    tokens.pop_front();

                    loop {
                        if let Some(t) = tokens.pop_front() {
                            if matches!(t.token_type(), TokenType::EndMultilineText) {
                                break;
                            }

                            if matches!(t.token_type(), TokenType::LiteralText | TokenType::Eol) {
                                left_nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                            }else if matches!(t.token_type(), TokenType::EscapeSequence) {
                                self.parse_escape_sequence_token(t, &mut left_nodes);
                            }else if matches!(t.token_type(), TokenType::LexerError) {
                                left_nodes.push(Node::new_parsing_error_node(
                                    t.pos(),
                                    ParsingError::LexerError,
                                    t.value()
                                ));
                            }else {
                                left_nodes.push(Node::new_parsing_error_node(
                                    CodePosition::EMPTY,
                                    ParsingError::Eof,
                                    format!(
                                        "Invalid token type ({:?}) in multiline text during operator parsing",
                                        t.token_type(),
                                    ),
                                ));
                            }
                        }else {
                            left_nodes.push(Node::new_parsing_error_node(
                                CodePosition::EMPTY,
                                ParsingError::Eof,
                                "Missing multiline text end token during operator parsing",
                            ));

                            break 'tokenProcessing;
                        }
                    }
                },

                TokenType::Whitespace => {
                    tokens.pop_front();

                    whitespaces.push_back(t);
                },

                TokenType::Identifier | TokenType::ParserFunctionIdentifier => {
                    self.parse_operator_expression_variable_name_and_function_call(
                        tokens, operator_type, &mut other_tokens, &mut left_nodes, t, &mut whitespaces,
                    );
                },

                TokenType::OpeningBracket | TokenType::Operator | TokenType::ArgumentSeparator => {
                    let mut t = t;
                    let mut value = t.value();

                    //Convert argument separator token to operator token with additional whitespace
                    if matches!(t.token_type(), TokenType::ArgumentSeparator) {
                        let byte_index = value.find(",").unwrap();

                        if byte_index > 0 {
                            whitespaces.push_back(Token::new(t.pos(), &value[..byte_index], TokenType::Whitespace));
                        }

                        if byte_index < value.len() - 1 {
                            tokens.insert(1, Token::new(t.pos(), &value[byte_index+1..], TokenType::Whitespace));
                        }

                        t = Token::new(t.pos(), ",", TokenType::Operator);
                        value = t.value();
                        tokens[0] = t.clone();
                    }

                    //Grouping
                    if matches!(t.token_type(), TokenType::OpeningBracket) && value == "(" {
                        let end_index = utils::get_index_of_matching_bracket_tok(
                            tokens.make_contiguous(), 0, usize::MAX, "(", ")", true,
                        );
                        let Some(end_index) = end_index else {
                            left_nodes.push(Node::new_parsing_error_node(
                                CodePosition::EMPTY,
                                ParsingError::BracketMismatch,
                                "Bracket in operator expression is missing",
                            ));

                            break 'tokenProcessing;
                        };

                        //Parse "()" as function call previous value if something was before
                        if other_tokens.is_empty() && left_nodes.is_empty() {
                            if !whitespaces.is_empty() {
                                whitespaces.clear();
                            }

                            let mut parameter_tokens = utils::split_off_arguments(tokens, end_index);

                            left_nodes.push(self.parse_operator_expr(&mut parameter_tokens, operator_type).unwrap());
                        }else {
                            if !whitespaces.is_empty() {
                                whitespaces.clear();
                            }

                            if !other_tokens.is_empty() {
                                self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                                other_tokens.clear();
                            }

                            let opening_bracket_token = &tokens[0];
                            let closing_bracket_token = &tokens[end_index];
                            let pos = opening_bracket_token.pos().combine(&closing_bracket_token.pos());

                            let mut function_call_tokens = utils::split_off_arguments(tokens, end_index);

                            let node = self.parse_operator_expr(&mut function_call_tokens, operator_type).unwrap();
                            left_nodes.push(Node::new_function_call_previous_node_value_node(
                                pos, "", "",
                                Self::convert_comma_operators_to_argument_separators(node),
                            ));
                        }

                        continue 'tokenProcessing;
                    }

                    //(Optional) Get Item / (Optional) Slice / Array Creation
                    if (matches!(t.token_type(), TokenType::OpeningBracket) && value == "[") ||
                            (matches!(t.token_type(), TokenType::Operator) && value == "?." &&
                                    tokens.len() > 2 && matches!(tokens[1].token_type(), TokenType::OpeningBracket) &&
                                    tokens[1].value() == "[") {
                        let starts_with_optional_marker = matches!(t.token_type(), TokenType::Operator);
                        let end_index = utils::get_index_of_matching_bracket_tok(
                            tokens.make_contiguous(),
                            if starts_with_optional_marker { 1 } else { 0 }, usize::MAX,
                            "[", "]", true,
                        );
                        let Some(end_index) = end_index else {
                            left_nodes.push(Node::new_parsing_error_node(
                                CodePosition::EMPTY,
                                ParsingError::BracketMismatch,
                                "Bracket in operator expression is missing",
                            ));

                            break 'tokenProcessing;
                        };

                        //Binary operator if something was before else unary operator
                        if OperatorType::All.is_compatible_with(operator_type) &&
                                (!other_tokens.is_empty() || !left_nodes.is_empty()) {
                            let old_operator = operator.replace(
                                if starts_with_optional_marker {
                                    Operator::OptionalGetItem
                                }else {
                                    Operator::GetItem
                                }
                            );

                            if current_operator_precedence <= operator.unwrap().precedence() {
                                if let Some(tokens_left) = tokens_left {
                                    tokens_left.append(tokens);

                                    if !whitespaces.is_empty() {
                                        whitespaces.clear();
                                    }

                                    operator = old_operator;

                                    break 'tokenProcessing;
                                }
                            }

                            if !whitespaces.is_empty() {
                                whitespaces.clear();
                            }

                            if !other_tokens.is_empty() {
                                self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                                other_tokens.clear();
                            }

                            //Parse middle part (":") for potential slice operator
                            let mut inner_tokens_left_behind_middle_part_end = VecDeque::new();

                            //Add dummy whitespace token to allow empty end index in slice operator
                            inner_tokens_left_behind_middle_part_end.push_back(Token::new(
                                CodePosition::EMPTY,
                                "DUMMY-A",
                                TokenType::Whitespace,
                            ));

                            let start_index = if starts_with_optional_marker { 2 } else { 1 };
                            let mut tokens_list = VecDeque::from_iter(
                                tokens.make_contiguous()[start_index..end_index].iter().cloned(),
                            );

                            let inner_middle_node_ret = self.parse_operator_expression(
                                &mut tokens_list,
                                &mut None,
                                &mut Some(&mut inner_tokens_left_behind_middle_part_end),
                                0,
                                operator_type,
                            );
                            if let Some(inner_middle_node_ret) = inner_middle_node_ret {
                                //Contains matching ":" -> Slice

                                operator.replace(
                                    if starts_with_optional_marker {
                                        Operator::OptionalSlice
                                    }else {
                                        Operator::Slice
                                    }
                                );

                                //Remove dummy whitespace token
                                inner_tokens_left_behind_middle_part_end.pop_front();

                                tokens.drain(..=end_index);

                                let mut tokens_list = inner_tokens_left_behind_middle_part_end;

                                let inner_right_node_ret = self.parse_operator_expr(
                                    &mut tokens_list, operator_type,
                                ).unwrap();
                                if tokens.is_empty() {
                                    //Add middle node directly if node has NON operator
                                    if inner_middle_node_ret.operator() == Some(non_operator) {
                                        middle_node = inner_middle_node_ret.into_left_side_operand();
                                    }else {
                                        middle_node = Some(inner_middle_node_ret);
                                    }

                                    //Add right node directly if node has NON operator
                                    if inner_right_node_ret.operator() == Some(non_operator) {
                                        right_node = inner_right_node_ret.into_left_side_operand();
                                    }else {
                                        right_node = Some(inner_right_node_ret);
                                    }

                                    break 'tokenProcessing;
                                }else {
                                    //Add middle node directly if node has NON operator
                                    let middle_node = if inner_middle_node_ret.operator() == Some(non_operator) {
                                        inner_middle_node_ret.into_left_side_operand().unwrap()
                                    }else {
                                        inner_middle_node_ret
                                    };

                                    //Add node directly if node has NON operator
                                    let right_node = if inner_right_node_ret.operator() == Some(non_operator) {
                                        inner_right_node_ret.into_left_side_operand().unwrap()
                                    }else {
                                        inner_right_node_ret
                                    };

                                    let left_node = if left_nodes.len() == 1 {
                                        left_nodes.pop().unwrap()
                                    }else {
                                        Node::new_list_node(Vec::from_iter(left_nodes.drain(..)))
                                    };

                                    left_nodes.push(Node::new_operation_statement_node(
                                        left_node.pos().combine(&right_node.pos()),
                                        OperationExpression::new(
                                            Some(Box::new(left_node)),
                                            Some(Box::new(middle_node)),
                                            Some(Box::new(right_node)),
                                            operator.take().unwrap(), operator_type,
                                        ),
                                    ));
                                }

                                continue 'tokenProcessing;
                            }

                            let mut tokens_list = utils::split_off_arguments(tokens, end_index);
                            if starts_with_optional_marker {
                                //Remove "[" because "?." was at index 0 and "[" was at index 1
                                tokens_list.pop_front();
                            }

                            let node = self.parse_operator_expr(&mut tokens_list, operator_type).unwrap();
                            if tokens.is_empty() {
                                //Add node directly if node has NON operator
                                if node.operator() == Some(non_operator) {
                                    right_node = node.into_left_side_operand();
                                }else {
                                    right_node = Some(node);
                                }

                                break 'tokenProcessing;
                            }else {
                                //Add node directly if node has NON operator
                                let right_node = if node.operator() == Some(non_operator) {
                                    node.into_left_side_operand().unwrap()
                                }else {
                                    node
                                };

                                let left_node = if left_nodes.len() == 1 {
                                    left_nodes.pop().unwrap()
                                }else {
                                    Node::new_list_node(Vec::from_iter(left_nodes.drain(..)))
                                };

                                left_nodes.push(Node::new_operation_statement_node(
                                    left_node.pos().combine(&right_node.pos()),
                                    OperationExpression::new(
                                        Some(Box::new(left_node)),
                                        None,
                                        Some(Box::new(right_node)),
                                        operator.take().unwrap(), operator_type,
                                    ),
                                ));

                                continue 'tokenProcessing;
                            }
                        }else if OperatorType::All.is_compatible_with(operator_type) && !starts_with_optional_marker {
                            if !whitespaces.is_empty() {
                                whitespaces.clear();
                            }

                            if !other_tokens.is_empty() {
                                self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                                other_tokens.clear();
                            }

                            //Array creation
                            let pos = t.pos().combine(&tokens[end_index].pos());

                            let mut tokens_list = utils::split_off_arguments(tokens, end_index);

                            let node = self.parse_operator_expr(&mut tokens_list, operator_type).unwrap();
                            left_nodes.push(Node::new_array_value_node(
                                pos,
                                Self::convert_comma_operators_to_argument_separators(node),
                            ));

                            if tokens.is_empty() {
                                operator = None;

                                break 'tokenProcessing;
                            }

                            continue 'tokenProcessing;
                        }else {
                            //Incompatible type
                            if !whitespaces.is_empty() {
                                other_tokens.append(&mut whitespaces);
                            }
                        }
                    }

                    if value == "**" {
                        let old_operator = operator.replace(Operator::Pow);

                        //If something is before operator and operator type is compatible with type
                        if operator.unwrap().operator_type().is_compatible_with(operator_type) &&
                                (!other_tokens.is_empty() || !left_nodes.is_empty()) {
                            //No "<=" because it should be parsed right-to-left
                            if current_operator_precedence < operator.unwrap().precedence() {
                                if let Some(tokens_left) = tokens_left {
                                    tokens_left.append(tokens);

                                    if !whitespaces.is_empty() {
                                        whitespaces.clear();
                                    }

                                    operator = old_operator;

                                    break 'tokenProcessing;
                                }
                            }

                            //Add as value if nothing is behind operator
                            if tokens.len() == 1 {
                                if !whitespaces.is_empty() {
                                    other_tokens.append(&mut whitespaces);
                                }

                                operator = None;
                                other_tokens.push_back(t);
                                tokens.pop_front();

                                break 'tokenProcessing;
                            }

                            if !whitespaces.is_empty() {
                                whitespaces.clear();
                            }

                            if !other_tokens.is_empty() {
                                self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                                other_tokens.clear();
                            }

                            let mut inner_tokens_left = VecDeque::new();
                            let mut tokens_list = VecDeque::from_iter(
                                tokens.make_contiguous()[1..].iter().cloned(),
                            );

                            //"?": if None end was reached inside middle part of a ternary operator
                            let node = self.parse_operator_expression(
                                &mut tokens_list,
                                &mut Some(&mut inner_tokens_left),
                                tokens_left_behind_middle_part_end,
                                operator.unwrap().precedence(),
                                operator_type,
                            )?;

                            *tokens = inner_tokens_left;

                            if tokens.is_empty() {
                                //Add node directly if node has NON operator
                                if node.operator() == Some(non_operator) {
                                    right_node = node.into_left_side_operand();
                                }else {
                                    right_node = Some(node);
                                }

                                break 'tokenProcessing;
                            }else {
                                //Add node directly if node has NON operator
                                let right_node = if node.operator() == Some(non_operator) {
                                    node.into_left_side_operand().unwrap()
                                }else {
                                    node
                                };

                                let left_node = if left_nodes.len() == 1 {
                                    left_nodes.pop().unwrap()
                                }else {
                                    Node::new_list_node(Vec::from_iter(left_nodes.drain(..)))
                                };

                                left_nodes.push(Node::new_operation_statement_node(
                                    left_node.pos().combine(&right_node.pos()),
                                    OperationExpression::new(
                                        Some(Box::new(left_node)),
                                        None,
                                        Some(Box::new(right_node)),
                                        operator.take().unwrap(), operator_type,
                                    ),
                                ));

                                continue 'tokenProcessing;
                            }
                        }else {
                            operator = old_operator;

                            //Ignore operator: nothing was before for binary operator or operator type is not compatible with type
                            if !whitespaces.is_empty() {
                                other_tokens.append(&mut whitespaces);
                            }
                        }
                    }

                    if matches!(
                        value,
                        "!==" | "!=~" | "!=" | "===" | "=~" | "==" | "<=>" | "<=" | ">=" | "<" |
                        ">" | "|||" | "&&" | "||" | "!" | "&" | "~~" | "~/" | "~" | "\u{25b2}" |
                        "\u{25bc}" | "*" | "//" | "^/" | "/" | "%" | "^" | "|" | "<<" |  ">>>" |
                        ">>" | "+|" | "-|" | "+" | "->" | "-" | "@" | "?:" | "??" | "," | "?::" |
                        "::"
                    ) {
                        let something_before_operator = !other_tokens.is_empty() || !left_nodes.is_empty();

                        let old_operator = operator.take();

                        if operator.is_none() && OperatorType::All.is_compatible_with(operator_type) {
                            match value {
                                "?::" => {
                                    operator = Some(Operator::OptionalMemberAccess);
                                },
                                "::" => {
                                    if something_before_operator {
                                        operator = Some(Operator::MemberAccess);
                                    }else {
                                        operator = Some(Operator::MemberAccessThis);
                                    }
                                },
                                "->" => {
                                    operator = Some(Operator::MemberAccessPointer);
                                },
                                "," => {
                                    operator = Some(Operator::Comma);
                                },
                                
                                _ => {},
                            }
                        }

                        if operator.is_none() && OperatorType::General.is_compatible_with(operator_type) {
                            match value {
                                "|||" => {
                                    operator = Some(Operator::Concat);
                                },
                                "@" => {
                                    operator = Some(Operator::Len);
                                },
                                "?:" => {
                                    operator = Some(Operator::Elvis);
                                },
                                "??" => {
                                    operator = Some(Operator::NullCoalescing);
                                },
                                "^" => {
                                    if !something_before_operator {
                                        operator = Some(Operator::DeepCopy);
                                    }
                                },
                                
                                _ => {},
                            }
                        }

                        if operator.is_none() && OperatorType::Math.is_compatible_with(operator_type) {
                            match value {
                                "<<" => {
                                    operator = Some(Operator::Lshift);
                                },
                                ">>>" => {
                                    operator = Some(Operator::Rzshift);
                                },
                                ">>" => {
                                    operator = Some(Operator::Rshift);
                                },
                                "<=>" => {
                                    operator = Some(Operator::Spaceship);
                                },
                                "&" => {
                                    operator = Some(Operator::BitwiseAnd);
                                },
                                "~/" => {
                                    operator = Some(Operator::TruncDiv);
                                },
                                "~" => {
                                    operator = Some(Operator::BitwiseNot);
                                },
                                "+|" | "\u{25b2}" => {
                                    operator = Some(Operator::Inc);
                                },
                                "-|" | "\u{25bc}" => {
                                    operator = Some(Operator::Dec);
                                },
                                "*" => {
                                    operator = Some(Operator::Mul);
                                },
                                "^/" => {
                                    operator = Some(Operator::CeilDiv);
                                },
                                "//" => {
                                    operator = Some(Operator::FloorDiv);
                                },
                                "/" => {
                                    operator = Some(Operator::Div);
                                },
                                "%" => {
                                    operator = Some(Operator::Mod);
                                },
                                "|" => {
                                    operator = Some(Operator::BitwiseOr);
                                },
                                "+" => {
                                    if something_before_operator {
                                        operator = Some(Operator::Add);
                                    }else {
                                        operator = Some(Operator::Pos);
                                    }
                                },
                                "-" => {
                                    if something_before_operator {
                                        operator = Some(Operator::Sub);
                                    }else {
                                        operator = Some(Operator::Inv);
                                    }
                                },
                                "^" => {
                                    if something_before_operator {
                                        operator = Some(Operator::BitwiseXor);
                                    }
                                },

                                _ => {},
                            }
                        }

                        if operator.is_none() && OperatorType::Condition.is_compatible_with(operator_type) {
                            match value {
                                "!==" => {
                                    operator = Some(Operator::StrictNotEquals);
                                },
                                "!=~" => {
                                    operator = Some(Operator::NotMatches);
                                },
                                "!=" => {
                                    operator = Some(Operator::NotEquals);
                                },
                                "===" => {
                                    operator = Some(Operator::StrictEquals);
                                },
                                "=~" => {
                                    operator = Some(Operator::Matches);
                                },
                                "==" => {
                                    operator = Some(Operator::Equals);
                                },
                                "<=" => {
                                    operator = Some(Operator::LessThanOrEquals);
                                },
                                ">=" => {
                                    operator = Some(Operator::GreaterThanOrEquals);
                                },
                                "<" => {
                                    operator = Some(Operator::LessThan);
                                },
                                ">" => {
                                    operator = Some(Operator::GreaterThan);
                                },
                                "&&" => {
                                    operator = Some(Operator::And);
                                },
                                "||" => {
                                    operator = Some(Operator::Or);
                                },
                                "!" => {
                                    operator = Some(Operator::Not);
                                },
                                "~~" => {
                                    operator = Some(Operator::InstanceOf);
                                },

                                _ => {},
                            }
                        }

                        match operator {
                            Some(op) if op.is_binary() && something_before_operator => {
                                if current_operator_precedence <= op.precedence() {
                                    if let Some(tokens_left) = tokens_left {
                                        tokens_left.append(tokens);

                                        if !whitespaces.is_empty() {
                                            whitespaces.clear();
                                        }

                                        operator = old_operator;

                                        break 'tokenProcessing;
                                    }
                                }

                                //Add as value if nothing is behind "operator"
                                if tokens.len() == 1 {
                                    other_tokens.append(&mut whitespaces);

                                    operator = None;
                                    other_tokens.push_back(t);
                                    tokens.pop_front();

                                    break 'tokenProcessing;
                                }

                                if !whitespaces.is_empty() {
                                    whitespaces.clear();
                                }

                                if !other_tokens.is_empty() {
                                    self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                                    other_tokens.clear();
                                }

                                let mut inner_tokens_left = VecDeque::new();
                                let mut tokens_list = VecDeque::from_iter(
                                    tokens.make_contiguous()[1..].iter().cloned(),
                                );

                                //"?": if None end was reached inside middle part of a ternary operator
                                let node = self.parse_operator_expression(
                                    &mut tokens_list,
                                    &mut Some(&mut inner_tokens_left),
                                    tokens_left_behind_middle_part_end,
                                    operator.unwrap().precedence(),
                                    operator_type,
                                )?;

                                *tokens = inner_tokens_left;

                                if tokens.is_empty() {
                                    //Add node directly if node has NON operator
                                    if node.operator() == Some(non_operator) {
                                        right_node = node.into_left_side_operand();
                                    }else {
                                        right_node = Some(node);
                                    }

                                    break 'tokenProcessing;
                                }else {
                                    //Add node directly if node has NON operator
                                    let right_node = if node.operator() == Some(non_operator) {
                                        node.into_left_side_operand().unwrap()
                                    }else {
                                        node
                                    };

                                    let left_node = if left_nodes.len() == 1 {
                                        left_nodes.pop().unwrap()
                                    }else {
                                        Node::new_list_node(Vec::from_iter(left_nodes.drain(..)))
                                    };

                                    left_nodes.push(Node::new_operation_statement_node(
                                        left_node.pos().combine(&right_node.pos()),
                                        OperationExpression::new(
                                            Some(Box::new(left_node)),
                                            None,
                                            Some(Box::new(right_node)),
                                            operator.take().unwrap(), operator_type,
                                        ),
                                    ));

                                    continue 'tokenProcessing;
                                }
                            },

                            Some(op) if op.is_unary() && !something_before_operator => {
                                if !whitespaces.is_empty() {
                                    whitespaces.clear();
                                }

                                let pos_start = t.pos();

                                let mut inner_tokens_left = VecDeque::new();
                                let mut tokens_list = VecDeque::from_iter(
                                    tokens.make_contiguous()[1..].iter().cloned(),
                                );

                                //"?": if None end was reached inside middle part of a ternary operator
                                let node = self.parse_operator_expression(
                                    &mut tokens_list,
                                    &mut Some(&mut inner_tokens_left),
                                    tokens_left_behind_middle_part_end,
                                    operator.unwrap().precedence(),
                                    operator_type,
                                )?;

                                *tokens = inner_tokens_left;

                                //Add node directly if node has NON operator
                                let left_node = if node.operator() == Some(non_operator) {
                                    node.into_left_side_operand().unwrap()
                                }else {
                                    node
                                };

                                left_nodes.push(Node::new_operation_statement_node(
                                    pos_start.combine(&left_node.pos()),
                                    OperationExpression::new(
                                        Some(Box::new(left_node)),
                                        None,
                                        None,
                                        operator.take().unwrap(), operator_type,
                                    ),
                                ));

                                if tokens.is_empty() {
                                    break 'tokenProcessing;
                                }else {
                                    continue 'tokenProcessing;
                                }
                            },

                            _ => {
                                operator = old_operator;

                                //Ignore operator: something was before for unary operator or nothing was before for binary operator or operator type is not compatible with type
                                if !whitespaces.is_empty() {
                                    other_tokens.append(&mut whitespaces);
                                }
                            },
                        }
                    }

                    if value == "?" {
                        let old_operator = operator.replace(Operator::InlineIf);

                        //Inline if -> Only parse if something is before and ":" was found -> else "?" will be parsed as text

                        if operator.unwrap().operator_type().is_compatible_with(operator_type) &&
                                (!other_tokens.is_empty() || !left_nodes.is_empty()) {
                            //No "<=" because it should be parsed right-to-left
                            if current_operator_precedence < operator.unwrap().precedence() {
                                if let Some(tokens_left) = tokens_left {
                                    tokens_left.append(tokens);

                                    if !whitespaces.is_empty() {
                                        whitespaces.clear();
                                    }

                                    operator = old_operator;

                                    break 'tokenProcessing;
                                }
                            }

                            //Parse middle part
                            let mut inner_tokens_left_behind_middle_part_end = VecDeque::new();
                            let mut tokens_list = VecDeque::from_iter(
                                tokens.make_contiguous()[1..].iter().cloned(),
                            );

                            let inner_middle_node_ret = self.parse_operator_expression(
                                &mut tokens_list,
                                &mut None,
                                &mut Some(&mut inner_tokens_left_behind_middle_part_end),
                                0,
                                operator_type,
                            );
                            if let Some(inner_middle_node_ret) = inner_middle_node_ret {
                                //Only parse as operator if matching ":" was found

                                //Add as value if nothing is behind "operator"
                                if inner_tokens_left_behind_middle_part_end.is_empty() {
                                    if !whitespaces.is_empty() {
                                        other_tokens.append(&mut whitespaces);
                                    }

                                    operator = None;
                                    other_tokens.push_back(t);

                                    break 'tokenProcessing;
                                }

                                *tokens = inner_tokens_left_behind_middle_part_end;

                                if !whitespaces.is_empty() {
                                    whitespaces.clear();
                                }

                                if !other_tokens.is_empty() {
                                    self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                                    other_tokens.clear();
                                }

                                let mut inner_tokens_left = VecDeque::new();

                                let inner_right_node_ret = self.parse_operator_expression(
                                    tokens,
                                    &mut Some(&mut inner_tokens_left),
                                    tokens_left_behind_middle_part_end,
                                    operator.unwrap().precedence(),
                                    operator_type,
                                ).unwrap();

                                *tokens = inner_tokens_left;

                                if tokens.is_empty() {
                                    //Add middle node directly if node has NON operator
                                    if inner_middle_node_ret.operator() == Some(non_operator) {
                                        middle_node = inner_middle_node_ret.into_left_side_operand();
                                    }else {
                                        middle_node = Some(inner_middle_node_ret);
                                    }

                                    //Add right node directly if node has NON operator
                                    if inner_right_node_ret.operator() == Some(non_operator) {
                                        right_node = inner_right_node_ret.into_left_side_operand();
                                    }else {
                                        right_node = Some(inner_right_node_ret);
                                    }

                                    break 'tokenProcessing;
                                }else {
                                    //Add middle node directly if node has NON operator
                                    let middle_node = if inner_middle_node_ret.operator() == Some(non_operator) {
                                        inner_middle_node_ret.into_left_side_operand().unwrap()
                                    }else {
                                        inner_middle_node_ret
                                    };

                                    //Add node directly if node has NON operator
                                    let right_node = if inner_right_node_ret.operator() == Some(non_operator) {
                                        inner_right_node_ret.into_left_side_operand().unwrap()
                                    }else {
                                        inner_right_node_ret
                                    };

                                    let left_node = if left_nodes.len() == 1 {
                                        left_nodes.pop().unwrap()
                                    }else {
                                        Node::new_list_node(Vec::from_iter(left_nodes.drain(..)))
                                    };

                                    left_nodes.push(Node::new_operation_statement_node(
                                        left_node.pos().combine(&right_node.pos()),
                                        OperationExpression::new(
                                            Some(Box::new(left_node)),
                                            Some(Box::new(middle_node)),
                                            Some(Box::new(right_node)),
                                            operator.take().unwrap(), operator_type,
                                        ),
                                    ));

                                    continue 'tokenProcessing;
                                }
                            }else {
                                operator = old_operator;

                                //Ignore operator: nothing was before for ternary operator or operator type is not compatible with type
                                if !whitespaces.is_empty() {
                                    other_tokens.append(&mut whitespaces);
                                }
                            }
                        }else {
                            operator = old_operator;

                            //Ignore operator: nothing was before for ternary operator or operator type is not compatible with type
                            if !whitespaces.is_empty() {
                                other_tokens.append(&mut whitespaces);
                            }
                        }
                    }

                    if value == ":" {
                        if let Some(tokens_left_behind_middle_part_end) = tokens_left_behind_middle_part_end {
                            //End of inline if or slice

                            //Replace DUMMY slice operator
                            if tokens_left_behind_middle_part_end.front().is_some_and(|token|
                                    matches!(token.token_type(), TokenType::Whitespace) && token.value() == "DUMMY-A") {
                                tokens_left_behind_middle_part_end[0] = Token::new(CodePosition::EMPTY, "DUMMY-B", TokenType::Whitespace);
                            }

                            if !whitespaces.is_empty() {
                                whitespaces.clear();
                            }

                            tokens.pop_front();
                            tokens_left_behind_middle_part_end.append(tokens);

                            //Reset (Simulated end)
                            if let Some(tokens_left) = tokens_left {
                                if !tokens_left.is_empty() {
                                    tokens_left.clear();
                                }
                            }

                            break 'tokenProcessing;
                        }
                    }

                    tokens.pop_front();

                    if !whitespaces.is_empty() {
                        other_tokens.append(&mut whitespaces);
                    }

                    if !other_tokens.is_empty() {
                        self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                        other_tokens.clear();
                    }

                    //Allow "+<LITERAL_NUMBER>" and "-<LITERAL_NUMBER>" in conditional parsing (if nothing is before)
                    if other_tokens.is_empty() && left_nodes.is_empty() && matches!(t.value(), "+" | "-") &&
                            !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::LiteralNumber) {
                        let number_token = tokens.pop_front().unwrap();

                        let combined_number_token = Token::new(
                            t.pos().combine(&number_token.pos()),
                            &(t.value().to_string() + number_token.value()),
                            TokenType::LiteralNumber,
                        );

                        self.parse_number_token(combined_number_token, &mut left_nodes);
                    }else {
                        left_nodes.push(Node::new_text_value_node(t.pos(), value));
                    }
                },

                TokenType::Other => {
                    if !whitespaces.is_empty() {
                        whitespaces.clear();
                    }

                    if !other_tokens.is_empty() {
                        self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
                        other_tokens.clear();
                    }

                    let ret = self.parse_function_call_without_prefix(tokens, Some(operator_type));
                    if let Some(ret) = ret {
                        left_nodes.push(ret);
                    }else {
                        tokens.pop_front();
                        other_tokens.push_back(t);
                    }
                },

                TokenType::OpeningBlockBracket | TokenType::ClosingBlockBracket |
                TokenType::LineContinuation | TokenType::EndComment | TokenType::EndMultilineText |
                TokenType::SingleLineTextQuotes => {
                    left_nodes.push(Node::new_parsing_error_node(
                        CodePosition::EMPTY,
                        ParsingError::LexerError,
                        format!(
                            "Invalid token type in operator expression: \"{:?}\"",
                            t.token_type(),
                        ),
                    ));

                    break 'tokenProcessing;
                },
            }
        }

        //Remove DUMMY token from slice operator
        if let Some(tokens_left_behind_middle_part_end) = tokens_left_behind_middle_part_end {
            if tokens_left_behind_middle_part_end.front().is_some_and(|token|
                    matches!(token.token_type(), TokenType::Whitespace) && token.value() == "DUMMY-A") {
                tokens_left_behind_middle_part_end.pop_front();
            }

            //End of middle part was not found for ternary operator -> ignore ternary operator
            if tokens_left_behind_middle_part_end.is_empty() {
                return None;
            }
        }

        if !whitespaces.is_empty() {
            other_tokens.append(&mut whitespaces);
        }

        if !other_tokens.is_empty() {
            self.parse_text_and_char_value(&mut other_tokens, &mut left_nodes);
            other_tokens.clear();
        }

        let operator = operator.unwrap_or(non_operator);

        let left_node = if left_nodes.len() == 1 {
            left_nodes.pop().unwrap()
        }else {
            Node::new_list_node(Vec::from_iter(left_nodes.drain(..)))
        };

        if let Some(tokens_left) = tokens_left {
            if !tokens.is_empty() {
                tokens_left.append(tokens);
            }
        }

        let pos = left_node.pos().combine(&right_node.as_ref().map(Node::pos).unwrap_or(left_node.pos()));

        Some(Node::new_operation_statement_node(
            pos,
            OperationExpression::new(
                Some(Box::new(left_node)),
                middle_node.map(Box::new),
                right_node.map(Box::new),
                operator, operator_type,
            ),
        ))
    }

    fn parse_operator_expression_variable_name_and_function_call(
        &mut self,
        tokens: &mut VecDeque<Token>,
        operator_type: OperatorType,
        other_tokens: &mut VecDeque<Token>,
        left_nodes: &mut Vec<Node>,
        t: Token,
        whitespaces: &mut VecDeque<Token>,
    ) {
        //TODO: Improve
        //Parse "&<name>" if something is before "&<name>" as "&" operator with new other value lexical analysis of "<name>"
        if (!other_tokens.is_empty() || !left_nodes.is_empty()) && t.value().starts_with("&") {
            tokens[0] = Token::new(t.pos(), "&", TokenType::Operator);
            tokens.insert(1, self.lexer.tokenize_other_value(&t.value()[1..], t.pos()));

            return;
        }

        if !whitespaces.is_empty() {
            other_tokens.append(whitespaces);
        }

        if !other_tokens.is_empty() {
            self.parse_text_and_char_value(other_tokens, left_nodes);
            other_tokens.clear();
        }

        let is_identifier = matches!(t.token_type(), TokenType::Identifier);
        let ret = if is_identifier {
            self.parse_variable_name_and_function_call(tokens, Some(operator_type))
        }else {
            self.parse_parser_function_call(tokens)
        };

        if let Some(ret) = ret {
            if let NodeData::UnprocessedVariableName(variable_name) = ret.node_data() {
                if is_identifier && tokens.front().is_some_and(|token|
                        matches!(token.token_type(), TokenType::Operator) && token.value() == "...") {
                    let array_unpacking_operator_token = tokens.pop_front().unwrap();
                    left_nodes.push(Node::new_unprocessed_variable_name_node(
                        ret.pos().combine(&array_unpacking_operator_token.pos()),
                        variable_name.to_string() + array_unpacking_operator_token.value(),
                    ));
                }else {
                    left_nodes.push(ret);
                }
            }else {
                left_nodes.push(ret);
            }
        }
    }

    fn convert_comma_operators_to_argument_separators(operator_node: Node) -> Vec<Node> {
        let mut nodes = Vec::new();

        if let Some(operator) = operator_node.operator() {
            match operator {
                Operator::Non | Operator::MathNon | Operator::ConditionalNon => {
                    //Ignore NON operators
                    let operand = operator_node.into_left_side_operand().unwrap();

                    if matches!(
                        operand.node_data(),
                        NodeData::Operation {..} | NodeData::Math {..} | NodeData::Condition {..},
                    ) {
                        nodes.append(&mut Self::convert_comma_operators_to_argument_separators(operand));
                    }else {
                        nodes.push(operand);
                    }
                },

                Operator::Comma => {
                    //Only parse COMMA operators and COMMA operators inside COMMA operators but only if they are the left node
                    let argument_separator_pos = operator_node.pos();
                    let (left_side_operand, _, right_side_operand) = operator_node.into_operands();
                    let left_side_operand = left_side_operand.unwrap();
                    let right_side_operand = right_side_operand.unwrap();

                    //Add left side operand
                    if matches!(
                        left_side_operand.node_data(),
                        NodeData::Operation {..} | NodeData::Math {..} | NodeData::Condition {..},
                    ) {
                        nodes.append(&mut Self::convert_comma_operators_to_argument_separators(left_side_operand));
                    }else {
                        nodes.push(left_side_operand);
                    }

                    //Add argument separator
                    nodes.push(Node::new_argument_separator_node(argument_separator_pos, ", "));

                    //Add right side operand
                    nodes.push(right_side_operand);
                },

                _ => {
                    nodes.push(operator_node);
                }
            }
        }

        nodes
    }

    fn parse_assignment(&mut self, tokens: &mut VecDeque<Token>, inner_assignment: bool) -> Option<Node> {
        if tokens.is_empty() {
            return None;
        }

        Self::trim_first_line(tokens);

        let mut assignment_index = None;
        let mut token_count_first_line = None;
        for (i, token) in tokens.iter().
                enumerate() {
            if matches!(token.token_type(), TokenType::Eol | TokenType::Eof) {
                token_count_first_line = Some(i);

                break;
            }

            if assignment_index.is_none() {
                //Do not parse assignments in function body definition
                if i + 2 < tokens.len() && matches!(token.token_type(), TokenType::ClosingBracket) &&
                        token.value() == ")" && matches!(tokens[i + 1].token_type(), TokenType::Whitespace) &&
                        matches!(tokens[i + 2].token_type(), TokenType::Operator) && tokens[i + 2].value() == "->" {
                    return None;
                }

                if matches!(token.token_type(), TokenType::Assignment) {
                    assignment_index = Some(i);
                }
            }
        }

        let token_count_first_line = token_count_first_line.unwrap_or(tokens.len());

        let Some(assignment_index) = assignment_index else {
            if inner_assignment || token_count_first_line != 1 || !matches!(tokens[0].token_type(), TokenType::Identifier) {
                return None;
            }

            if regex_patterns::VAR_NAME_FULL.is_match(tokens[0].value()) {
                let variable_name_token = tokens.pop_front().unwrap();

                return Some(Node::new_assignment_node(
                    Node::new_unprocessed_variable_name_node(variable_name_token.pos(), variable_name_token.value()),
                    Node::new_null_value_node(variable_name_token.pos()),
                ));
            }

            return None;
        };

        let mut lvalue_tokens = VecDeque::from_iter(
            tokens.make_contiguous()[..assignment_index].iter().cloned(),
        );

        Self::trim_first_line(&mut lvalue_tokens);

        if lvalue_tokens.is_empty() {
            return None;
        }

        let assignment_token = &tokens[assignment_index];

        let is_simple_assignment = assignment_token.value() == "=";
        if is_simple_assignment || assignment_token.value() == " = " {
            let pos = lvalue_tokens[0].pos().combine(&lvalue_tokens[assignment_index - 1].pos());

            let regex = if is_simple_assignment {
                &regex_patterns::PARSING_SIMPLE_ASSIGNMENT_VARIABLE_NAME_LVALUE
            }else {
                &regex_patterns::VAR_NAME_FULL
            };

            if lvalue_tokens.len() == 1 && matches!(lvalue_tokens[0].token_type(), TokenType::Identifier) &&
                    regex.is_match(lvalue_tokens[0].value()) {
                tokens.drain(..=assignment_index);
                Self::trim_first_line(tokens);

                if is_simple_assignment {
                    //The assignment value for empty simple assignments will be set to empty text ""
                    return Some(Node::new_assignment_node(
                        Node::new_unprocessed_variable_name_node(pos, lvalue_tokens[0].value()),
                        self.parse_simple_assignment_value(tokens).into_node(),
                    ));
                }

                let returned_node = self.parse_assignment(tokens, true);
                let rvalue_node = returned_node.
                        unwrap_or_else(|| self.parse_lrvalue(tokens, true).into_node());
                return Some(Node::new_assignment_node(
                    Node::new_unprocessed_variable_name_node(pos, lvalue_tokens[0].value()),
                    rvalue_node,
                ));
            }

            let lvalue = lvalue_tokens.iter().
                    map(|token| token.to_raw_string().to_string()).
                    collect::<Vec<String>>().
                    join("");
            if regex_patterns::PARSING_PARSER_FLAG.is_match(&lvalue) {
                let mut rvalue_tokens = VecDeque::from_iter(
                    tokens.make_contiguous()[assignment_index + 1..token_count_first_line].iter().cloned(),
                );

                Self::trim_first_line(&mut rvalue_tokens);

                let ast = if is_simple_assignment {
                    self.parse_simple_assignment_value(&mut rvalue_tokens)
                }else {
                    self.parse_lrvalue(&mut rvalue_tokens, true)
                };

                self.parse_parser_flags(lvalue, ast.into_node());

                tokens.drain(..token_count_first_line);

                return None;
            }

            if regex_patterns::PARSING_SIMPLE_TRANSLATION_KEY.is_match(&lvalue) {
                tokens.drain(..=assignment_index);

                //The translation value for empty simple translation will be set to empty text ""
                let ast = if is_simple_assignment {
                    self.parse_simple_assignment_value(tokens)
                }else {
                    self.parse_lrvalue(tokens, true)
                };

                return Some(Node::new_assignment_node(
                    Node::new_text_value_node(pos, lvalue),
                    ast.into_node(),
                ));
            }
        }

        let is_variable_assignment = lvalue_tokens.len() == 1 &&
                matches!(lvalue_tokens[0].token_type(), TokenType::Identifier) &&
                regex_patterns::VAR_NAME_FULL.is_match(lvalue_tokens[0].value());

        if assignment_token.value() == " =" {
            let pos = assignment_token.pos();

            tokens.drain(..token_count_first_line);

            let ast = if is_variable_assignment {
                self.parse_lrvalue(&mut lvalue_tokens, false)
            }else {
                self.parse_translation_key(&mut lvalue_tokens)
            };

            return Some(Node::new_assignment_node(
                ast.into_node(),
                Node::new_null_value_node(pos),
            ));
        }

        if regex_patterns::PARSING_ASSIGNMENT_OPERATOR.is_match(assignment_token.value()) {
            let assignment_token_pos = assignment_token.pos();
            let assignment_operator = assignment_token.value();
            let assignment_operator = assignment_operator[1..assignment_operator.len()-2].to_string();

            tokens.drain(..=assignment_index);

            let mut operator = None;
            if !assignment_operator.is_empty() {
                match assignment_operator.as_str() {
                    "**" => {
                        operator = Some(Operator::Pow);
                    },
                    "*" => {
                        operator = Some(Operator::Mul);
                    },
                    "/" => {
                        operator = Some(Operator::Div);
                    },
                    "~/" => {
                        operator = Some(Operator::TruncDiv);
                    },
                    "//" => {
                        operator = Some(Operator::FloorDiv);
                    },
                    "^/" => {
                        operator = Some(Operator::CeilDiv);
                    },
                    "%" => {
                        operator = Some(Operator::Mod);
                    },
                    "+" => {
                        operator = Some(Operator::Add);
                    },
                    "-" => {
                        operator = Some(Operator::Sub);
                    },
                    "<<" => {
                        operator = Some(Operator::Lshift);
                    },
                    ">>" => {
                        operator = Some(Operator::Rshift);
                    },
                    ">>>" => {
                        operator = Some(Operator::Rzshift);
                    },
                    "&" => {
                        operator = Some(Operator::BitwiseAnd);
                    },
                    "^" => {
                        operator = Some(Operator::BitwiseXor);
                    },
                    "|" => {
                        operator = Some(Operator::BitwiseOr);
                    },
                    "|||" => {
                        operator = Some(Operator::Concat);
                    },
                    "?:" => {
                        operator = Some(Operator::Elvis);
                    },
                    "??" => {
                        operator = Some(Operator::NullCoalescing);
                    },
                    "?" => {
                        operator = Some(Operator::ConditionalNon);
                    },
                    ":" => {
                        operator = Some(Operator::MathNon);
                    },
                    "$" => {
                        operator = Some(Operator::Non);
                    },

                    _ => {}
                }
            }

            let lvalue_node = match operator {
                _ if is_variable_assignment => {
                    self.parse_lrvalue(&mut lvalue_tokens, false).into_node()
                },

                Some(Operator::ConditionalNon) => {
                    self.parse_condition_expr(&mut lvalue_tokens).unwrap()
                },

                Some(Operator::MathNon) => {
                    self.parse_math_expr(&mut lvalue_tokens).unwrap()
                },

                _ => {
                    self.parse_operation_expr(&mut lvalue_tokens).unwrap()
                },
            };

            let rvalue_node = match operator {
                _ if assignment_operator == "::" => {
                    let returned_node = self.parse_assignment(tokens, true);

                    returned_node.unwrap_or_else(|| self.parse_lrvalue(tokens, true).into_node())
                },

                None => {
                    Node::new_parsing_error_node(
                        assignment_token_pos,
                        ParsingError::InvalidAssignment,
                        format!("Invalid assignment operator: \" {}= \"", assignment_operator),
                    )
                },

                Some(Operator::ConditionalNon) => {
                    self.parse_condition_expr(tokens).unwrap()
                },

                Some(Operator::MathNon) => {
                    self.parse_math_expr(tokens).unwrap()
                },

                Some(Operator::Non) => {
                    self.parse_operation_expr(tokens).unwrap()
                },

                Some(operator) => {
                    let left_side_operand = lvalue_node.clone();
                    let right_side_operand = self.parse_operation_expr(tokens).unwrap();

                    Node::new_operation_statement_node(
                        left_side_operand.pos().combine(&right_side_operand.pos()),
                        OperationExpression::new(
                            Some(Box::new(left_side_operand)),
                            None,
                            Some(Box::new(right_side_operand)),
                            operator, operator.operator_type(),
                        ),
                    )
                },
            };

            return Some(Node::new_assignment_node(lvalue_node, rvalue_node));
        }

        //Translation with " = "
        if assignment_token.value() == " = " {
            tokens.drain(..=assignment_index);

            //The translation value for empty simple translation will be set to empty text ""
            return Some(Node::new_assignment_node(
                self.parse_translation_key(&mut lvalue_tokens).into_node(),
                self.parse_lrvalue(tokens, true).into_node(),
            ));
        }

        None
    }

    fn parse_line(&mut self, tokens: &mut VecDeque<Token>) -> Option<AST> {
        let mut ast = AST::new();
        let nodes = ast.nodes_mut();

        Self::trim_first_line(tokens);

        let mut token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());

        //Control flow statements
        let starts_with_con_expression = tokens.front().is_some_and(|token|
                matches!(token.token_type(), TokenType::Other) && token.value().starts_with("con."));
        let ends_with_opening_bracket = tokens.get(token_count_first_line - 1).is_some_and(|token|
                matches!(token.token_type(), TokenType::OpeningBlockBracket));
        if starts_with_con_expression || ends_with_opening_bracket {
            let mut con_expression = tokens[0].value().to_string();
            let original_con_expression = con_expression.clone();

            //"con." is optional if the curly brackets syntax is used
            if ends_with_opening_bracket && !starts_with_con_expression {
                con_expression = "con.".to_string() + &con_expression;
            }

            match con_expression.as_str() {
                "con.continue" | "con.break" if !ends_with_opening_bracket => {
                    let con_expression_token = tokens.pop_front().unwrap();
                    let mut pos_last_token = con_expression_token.pos();

                    let number_node = if token_count_first_line > 1 && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                            tokens[0].value() == "(" {
                        let arguments_end_index = utils::get_index_of_matching_bracket_tok(
                            tokens.make_contiguous(),
                            0, usize::MAX,
                            "(", ")", true,
                        );
                        let Some(arguments_end_index) = arguments_end_index else {
                            nodes.push(Node::new_parsing_error_node(
                                tokens[0].pos(),
                                ParsingError::BracketMismatch,
                                "Bracket for con.break or con.continue is missing",
                            ));

                            return Some(ast);
                        };

                        pos_last_token = tokens[arguments_end_index].pos();

                        let mut argument_tokens = utils::split_off_arguments(tokens, arguments_end_index);

                        Some(self.parse_function_parameter_list(&mut argument_tokens, false).into_node())
                    }else {
                        None
                    };

                    let pos = con_expression_token.pos().combine(&pos_last_token);
                    nodes.push(Node::new_continue_break_statement_node(
                        pos,
                        number_node.map(Box::new),
                        con_expression_token.value() == "con.continue",
                    ));

                    return Some(ast);
                },

                "con.try" | "con.softtry" | "con.nontry" => {
                    let mut try_statement_parts = Vec::new();

                    let block_bracket_flag = ends_with_opening_bracket;
                    while !tokens.is_empty() {
                        Self::trim_first_line(tokens);

                        let mut token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());
                        if token_count_first_line == 0 {
                            break;
                        }

                        let ends_with_opening_bracket = matches!(tokens[token_count_first_line - 1].token_type(), TokenType::OpeningBlockBracket);

                        con_expression = tokens[0].value().to_string();

                        //"con." is optional if the curly brackets syntax is used
                        if ends_with_opening_bracket && !starts_with_con_expression {
                            con_expression = "con.".to_string() + &con_expression;
                        }

                        if block_bracket_flag {
                            //Remove "{" and "}" for the curly brackets if statement syntax
                            let pos = tokens[token_count_first_line - 1].pos();

                            if !ends_with_opening_bracket {
                                nodes.push(Node::new_parsing_error_node(
                                    pos,
                                    ParsingError::InvalidConPart,
                                    "Missing \"{\" token after con statement",
                                ));
                            }

                            tokens.remove(token_count_first_line - 1);

                            token_count_first_line -= 1;

                            if token_count_first_line == 0 || !matches!(tokens[0].token_type(), TokenType::Other) {
                                nodes.push(Node::new_parsing_error_node(
                                    pos,
                                    ParsingError::InvalidConPart,
                                    "Missing con statement",
                                ));
                            }

                            con_expression = tokens[0].value().to_string();

                            //"con." is optional if the curly brackets syntax is used
                            if ends_with_opening_bracket && !starts_with_con_expression {
                                con_expression = "con.".to_string() + &con_expression;
                            }

                            Self::trim_first_line(tokens);

                            token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());
                        }

                        let mut try_arguments;
                        match con_expression.as_str() {
                            "con.try" | "con.softtry" | "con.nontry" | "con.else" | "con.finally" => {
                                let try_statement_token = tokens.pop_front().unwrap();
                                token_count_first_line -= 1;

                                if token_count_first_line >= 1 && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                                        tokens[0].value() == "(" {
                                    nodes.push(Node::new_parsing_error_node(
                                        try_statement_token.pos(),
                                        ParsingError::InvalidConPart,
                                        "Try/Softtry/Nontry/Finally/Else part with arguments",
                                    ));

                                    return Some(ast);
                                }

                                try_arguments = None;
                            },

                            "con.catch" => {
                                if token_count_first_line == 1 {
                                    try_arguments = None;
                                }else {
                                    tokens.pop_front().unwrap();
                                    token_count_first_line -= 1;

                                    if token_count_first_line > 1 && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                                            tokens[0].value() == "(" {
                                        let arguments_end_index = utils::get_index_of_matching_bracket_tok(
                                            tokens.make_contiguous(),
                                            0, usize::MAX,
                                            "(", ")", true,
                                        );
                                        let Some(arguments_end_index) = arguments_end_index else {
                                            nodes.push(Node::new_parsing_error_node(
                                                tokens[0].pos(),
                                                ParsingError::BracketMismatch,
                                                "Missing catch statement arguments",
                                            ));

                                            return Some(ast);
                                        };

                                        try_arguments = Some(utils::split_off_arguments(tokens, arguments_end_index));
                                        token_count_first_line -= arguments_end_index + 1;
                                    }else {
                                        try_arguments = None;
                                    }

                                    if token_count_first_line != 0 {
                                        nodes.push(Node::new_parsing_error_node(
                                            tokens[0].pos(),
                                            ParsingError::InvalidConPart,
                                            "Trailing stuff behind arguments",
                                        ));

                                        return Some(ast);
                                    }
                                }
                            },

                            "con.endtry" if !block_bracket_flag => {
                                tokens.pop_front();

                                break;
                            },

                            _ => {
                                //TODO lineNumber
                                nodes.push(Node::new_parsing_error_node(
                                    CodePosition::EMPTY,
                                    ParsingError::InvalidConPart,
                                    format!("Try statement part is invalid: \"{}\"", con_expression),
                                ));

                                return Some(ast);
                            },
                        };

                        let try_body = self.parse_tokens_internal(tokens);
                        let Some(try_body) = try_body else {
                            //TODO line numbers
                            nodes.push(Node::new_try_statement_node(CodePosition::EMPTY, try_statement_parts));
                            nodes.push(Node::new_parsing_error_node(
                                CodePosition::EMPTY,
                                ParsingError::Eof,
                                "In try body",
                            ));

                            return Some(ast);
                        };

                        //TODO line numbers
                        match con_expression.as_str() {
                            "con.try" => {
                                try_statement_parts.push(Node::new_try_statement_part_try_node(
                                    CodePosition::EMPTY,
                                    try_body,
                                ));
                            },

                            "con.softtry" => {
                                try_statement_parts.push(Node::new_try_statement_part_soft_try_node(
                                    CodePosition::EMPTY,
                                    try_body,
                                ));
                            },

                            "con.nontry" => {
                                try_statement_parts.push(Node::new_try_statement_part_non_try_node(
                                    CodePosition::EMPTY,
                                    try_body,
                                ));
                            },

                            "con.catch" => {
                                try_statement_parts.push(Node::new_try_statement_part_catch_node(
                                    CodePosition::EMPTY,
                                    try_body,
                                    try_arguments.as_mut().map(|tokens|
                                            self.parse_function_parameter_list(tokens, false).
                                                    into_nodes()),
                                ));
                            },

                            "con.else" => {
                                try_statement_parts.push(Node::new_try_statement_part_else_node(
                                    CodePosition::EMPTY,
                                    try_body,
                                ));
                            },

                            "con.finally" => {
                                try_statement_parts.push(Node::new_try_statement_part_finally_node(
                                    CodePosition::EMPTY,
                                    try_body,
                                ));
                            },

                            _ => {},
                        }
                    }

                    //TODO line numbers
                    nodes.push(Node::new_try_statement_node(CodePosition::EMPTY, try_statement_parts));
                    return Some(ast);
                },

                "con.loop" | "con.while" | "con.until" | "con.repeat" | "con.foreach" => {
                    let mut loop_statement_parts = Vec::new();

                    let block_bracket_flag = ends_with_opening_bracket;
                    while !tokens.is_empty() {
                        Self::trim_first_line(tokens);

                        let mut token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());
                        if token_count_first_line == 0 {
                            break;
                        }

                        let ends_with_opening_bracket = matches!(tokens[token_count_first_line - 1].token_type(), TokenType::OpeningBlockBracket);

                        con_expression = tokens[0].value().to_string();

                        //"con." is optional if the curly brackets syntax is used
                        if ends_with_opening_bracket && !starts_with_con_expression {
                            con_expression = "con.".to_string() + &con_expression;
                        }

                        if block_bracket_flag {
                            //Remove "{" and "}" for the curly brackets if statement syntax
                            let pos = tokens[token_count_first_line - 1].pos();

                            if !ends_with_opening_bracket {
                                nodes.push(Node::new_parsing_error_node(
                                    pos,
                                    ParsingError::InvalidConPart,
                                    "Missing \"{\" token after con statement",
                                ));
                            }

                            tokens.remove(token_count_first_line - 1);

                            token_count_first_line -= 1;

                            if token_count_first_line == 0 || !matches!(tokens[0].token_type(), TokenType::Other) {
                                nodes.push(Node::new_parsing_error_node(
                                    pos,
                                    ParsingError::InvalidConPart,
                                    "Missing con statement",
                                ));
                            }

                            con_expression = tokens[0].value().to_string();

                            //"con." is optional if the curly brackets syntax is used
                            if ends_with_opening_bracket && !starts_with_con_expression {
                                con_expression = "con.".to_string() + &con_expression;
                            }

                            Self::trim_first_line(tokens);

                            token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());
                        }

                        let loop_condition;
                        match con_expression.as_str() {
                            "con.else" | "con.loop" => {
                                let try_statement_token = tokens.pop_front().unwrap();
                                token_count_first_line -= 1;

                                if token_count_first_line >= 1 && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                                        tokens[0].value() == "(" {
                                    nodes.push(Node::new_parsing_error_node(
                                        try_statement_token.pos(),
                                        ParsingError::InvalidConPart,
                                        "Loop/Else part with arguments",
                                    ));

                                    return Some(ast);
                                }

                                loop_condition = None;
                            },

                            "con.while" | "con.until" | "con.repeat" | "con.foreach" => {
                                tokens.pop_front().unwrap();
                                token_count_first_line -= 1;

                                if token_count_first_line > 1 && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                                        tokens[0].value() == "(" {
                                    let arguments_end_index = utils::get_index_of_matching_bracket_tok(
                                        tokens.make_contiguous(),
                                        0, usize::MAX,
                                        "(", ")", true,
                                    );
                                    let Some(arguments_end_index) = arguments_end_index else {
                                        nodes.push(Node::new_parsing_error_node(
                                            tokens[0].pos(),
                                            ParsingError::BracketMismatch,
                                            "Missing loop statement arguments",
                                        ));

                                        return Some(ast);
                                    };

                                    loop_condition = Some(utils::split_off_arguments(tokens, arguments_end_index));
                                    token_count_first_line -= arguments_end_index + 1;
                                }else {
                                    nodes.push(Node::new_parsing_error_node(
                                        tokens[0].pos(),
                                        ParsingError::BracketMismatch,
                                        "Bracket for loop statement missing",
                                    ));

                                    return Some(ast);
                                }

                                if token_count_first_line != 0 {
                                    nodes.push(Node::new_parsing_error_node(
                                        tokens[0].pos(),
                                        ParsingError::InvalidConPart,
                                        "Trailing stuff behind arguments",
                                    ));

                                    return Some(ast);
                                }
                            },

                            "con.endloop" if !block_bracket_flag => {
                                tokens.pop_front();

                                break;
                            },

                            _ => {
                                //TODO lineNumber
                                nodes.push(Node::new_parsing_error_node(
                                    CodePosition::EMPTY,
                                    ParsingError::InvalidConPart,
                                    format!("Loop statement part is invalid: \"{}\"", con_expression),
                                ));

                                return Some(ast);
                            },
                        }

                        let loop_body = self.parse_tokens_internal(tokens);
                        let Some(loop_body) = loop_body else {
                            //TODO line numbers
                            nodes.push(Node::new_loop_statement_node(CodePosition::EMPTY, loop_statement_parts));
                            nodes.push(Node::new_parsing_error_node(
                                CodePosition::EMPTY,
                                ParsingError::Eof,
                                "In loop body",
                            ));

                            return Some(ast);
                        };

                        //TODO line numbers
                        match con_expression.as_str() {
                            "con.else" => {
                                loop_statement_parts.push(Node::new_loop_statement_part_else_node(
                                    CodePosition::EMPTY,
                                    loop_body,
                                ));
                            },

                            "con.loop" => {
                                loop_statement_parts.push(Node::new_loop_statement_part_loop_node(
                                    CodePosition::EMPTY,
                                    loop_body,
                                ));
                            },

                            "con.while" => {
                                let operand = self.parse_operation_expr(&mut loop_condition.unwrap()).
                                        unwrap();

                                let conditional_non_node = ConditionalNode::new(Node::new_operation_statement_node(
                                    operand.pos(),
                                    OperationExpression::new(
                                        Some(Box::new(operand)),
                                        None,
                                        None,
                                        Operator::ConditionalNon, OperatorType::Condition,
                                    ),
                                ));

                                loop_statement_parts.push(Node::new_loop_statement_part_while_node(
                                    CodePosition::EMPTY,
                                    loop_body,
                                    conditional_non_node,
                                ));
                            },

                            "con.until" => {
                                let operand = self.parse_operation_expr(&mut loop_condition.unwrap()).
                                        unwrap();

                                let conditional_non_node = ConditionalNode::new(Node::new_operation_statement_node(
                                    operand.pos(),
                                    OperationExpression::new(
                                        Some(Box::new(operand)),
                                        None,
                                        None,
                                        Operator::ConditionalNon, OperatorType::Condition,
                                    ),
                                ));

                                loop_statement_parts.push(Node::new_loop_statement_part_until_node(
                                    CodePosition::EMPTY,
                                    loop_body,
                                    conditional_non_node,
                                ));
                            },

                            "con.repeat" | "con.foreach" => {
                                let arguments = self.parse_operation_expr(&mut loop_condition.unwrap()).unwrap();
                                let arguments = Self::convert_comma_operators_to_argument_separators(arguments);

                                let mut argument_iter = arguments.into_iter();

                                let mut var_pointer_node = None;
                                let mut flag = false;
                                for node in argument_iter.by_ref() {
                                    if matches!(node.node_data(), NodeData::ArgumentSeparator(_)) ||
                                            var_pointer_node.is_some() {
                                        flag = true;
                                        break;
                                    }

                                    var_pointer_node = Some(node);
                                }
                                if !flag {
                                    nodes.push(Node::new_parsing_error_node(
                                        CodePosition::EMPTY,
                                        ParsingError::InvalidConPart,
                                        "con.repeat or con.foreach arguments are invalid",
                                    ));

                                    return Some(ast);
                                }

                                let mut repeat_count_argument = Vec::new();
                                for node in argument_iter {
                                    if matches!(node.node_data(), NodeData::ArgumentSeparator(_)) {
                                        nodes.push(Node::new_parsing_error_node(
                                            CodePosition::EMPTY,
                                            ParsingError::InvalidConPart,
                                            "con.repeat or con.foreach arguments are invalid",
                                        ));

                                        return Some(ast);
                                    }

                                    repeat_count_argument.push(node);
                                }

                                let repeat_count_or_array_or_text_node = if repeat_count_argument.len() == 1 {
                                    repeat_count_argument.into_iter().next().unwrap()
                                }else {
                                    Node::new_list_node(repeat_count_argument)
                                };

                                if con_expression == "con.repeat" {
                                    loop_statement_parts.push(Node::new_loop_statement_part_repeat_node(
                                        CodePosition::EMPTY,
                                        loop_body,
                                        var_pointer_node.unwrap(),
                                        repeat_count_or_array_or_text_node,
                                    ));
                                }else {
                                    loop_statement_parts.push(Node::new_loop_statement_part_for_each_node(
                                        CodePosition::EMPTY,
                                        loop_body,
                                        var_pointer_node.unwrap(),
                                        repeat_count_or_array_or_text_node,
                                    ));
                                }
                            },

                            _ => {},
                        }
                    }

                    //TODO line numbers
                    nodes.push(Node::new_loop_statement_node(CodePosition::EMPTY, loop_statement_parts));
                    return Some(ast);
                },

                "con.if" => {
                    let mut if_statement_parts = Vec::new();

                    let block_bracket_flag = ends_with_opening_bracket;
                    while !tokens.is_empty() {
                        Self::trim_first_line(tokens);

                        let mut token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());
                        if token_count_first_line == 0 {
                            break;
                        }

                        let ends_with_opening_bracket = matches!(tokens[token_count_first_line - 1].token_type(), TokenType::OpeningBlockBracket);

                        con_expression = tokens[0].value().to_string();

                        //"con." is optional if the curly brackets syntax is used
                        if ends_with_opening_bracket && !starts_with_con_expression {
                            con_expression = "con.".to_string() + &con_expression;
                        }

                        if block_bracket_flag {
                            //Remove "{" and "}" for the curly brackets if statement syntax
                            let pos = tokens[token_count_first_line - 1].pos();

                            if !ends_with_opening_bracket {
                                nodes.push(Node::new_parsing_error_node(
                                    pos,
                                    ParsingError::InvalidConPart,
                                    "Missing \"{\" token after con statement",
                                ));
                            }

                            tokens.remove(token_count_first_line - 1);

                            token_count_first_line -= 1;

                            if token_count_first_line == 0 || !matches!(tokens[0].token_type(), TokenType::Other) {
                                nodes.push(Node::new_parsing_error_node(
                                    pos,
                                    ParsingError::InvalidConPart,
                                    "Missing con statement",
                                ));
                            }

                            con_expression = tokens[0].value().to_string();

                            //"con." is optional if the curly brackets syntax is used
                            if ends_with_opening_bracket && !starts_with_con_expression {
                                con_expression = "con.".to_string() + &con_expression;
                            }

                            Self::trim_first_line(tokens);

                            token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());
                        }

                        let mut if_condition;
                        match con_expression.as_str() {
                            "con.else" => {
                                let try_statement_token = tokens.pop_front().unwrap();
                                token_count_first_line -= 1;

                                if token_count_first_line >= 1 && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                                        tokens[0].value() == "(" {
                                    nodes.push(Node::new_parsing_error_node(
                                        try_statement_token.pos(),
                                        ParsingError::InvalidConPart,
                                        "Else part with arguments",
                                    ));

                                    return Some(ast);
                                }

                                if_condition = None;
                            },

                            "con.if" | "con.elif" => {
                                tokens.pop_front().unwrap();
                                token_count_first_line -= 1;

                                if token_count_first_line > 1 && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                                        tokens[0].value() == "(" {
                                    let arguments_end_index = utils::get_index_of_matching_bracket_tok(
                                        tokens.make_contiguous(),
                                        0, usize::MAX,
                                        "(", ")", true,
                                    );
                                    let Some(arguments_end_index) = arguments_end_index else {
                                        nodes.push(Node::new_parsing_error_node(
                                            tokens[0].pos(),
                                            ParsingError::BracketMismatch,
                                            "Missing if statement arguments",
                                        ));

                                        return Some(ast);
                                    };

                                    if_condition = Some(utils::split_off_arguments(tokens, arguments_end_index));
                                    token_count_first_line -= arguments_end_index + 1;
                                }else {
                                    nodes.push(Node::new_parsing_error_node(
                                        tokens[0].pos(),
                                        ParsingError::BracketMismatch,
                                        "Bracket for if statement missing",
                                    ));

                                    return Some(ast);
                                }

                                if token_count_first_line != 0 {
                                    nodes.push(Node::new_parsing_error_node(
                                        tokens[0].pos(),
                                        ParsingError::InvalidConPart,
                                        "Trailing stuff behind arguments",
                                    ));

                                    return Some(ast);
                                }
                            },

                            "con.endif" if !block_bracket_flag => {
                                tokens.pop_front();

                                break;
                            },

                            _ => {
                                //TODO lineNumber
                                nodes.push(Node::new_parsing_error_node(
                                    CodePosition::EMPTY,
                                    ParsingError::InvalidConPart,
                                    format!("If statement part is invalid: \"{}\"", con_expression),
                                ));

                                return Some(ast);
                            },
                        }

                        let if_body = self.parse_tokens_internal(tokens);
                        let Some(if_body) = if_body else {
                            //TODO line numbers
                            nodes.push(Node::new_if_statement_node(CodePosition::EMPTY, if_statement_parts));
                            nodes.push(Node::new_parsing_error_node(
                                CodePosition::EMPTY,
                                ParsingError::Eof,
                                "In if body",
                            ));

                            return Some(ast);
                        };

                        //TODO line numbers
                        if let Some(ref mut if_condition) = if_condition {
                            let operand = self.parse_operation_expr(if_condition).
                                    unwrap();

                            let conditional_non_node = ConditionalNode::new(Node::new_operation_statement_node(
                                operand.pos(),
                                OperationExpression::new(
                                    Some(Box::new(operand)),
                                    None,
                                    None,
                                    Operator::ConditionalNon, OperatorType::Condition,
                                ),
                            ));

                            if_statement_parts.push(Node::new_if_statement_part_if_node(
                                CodePosition::EMPTY,
                                if_body,
                                conditional_non_node,
                            ));
                        }else {
                            if_statement_parts.push(Node::new_if_statement_part_else_node(
                                CodePosition::EMPTY,
                                if_body,
                            ));
                        }
                    }

                    //TODO line numbers
                    nodes.push(Node::new_if_statement_node(CodePosition::EMPTY, if_statement_parts));
                    return Some(ast);
                }

                _ if original_con_expression.starts_with("con.") => {
                    return None;
                },

                _ => {},
            }
        }

        //Return values
        if token_count_first_line >= 1 && matches!(tokens[0].token_type(), TokenType::Other) &&
                tokens[0].value() == "return" {
            let return_statement_token_pos = tokens.front().unwrap().pos();

            //Return without value
            if token_count_first_line == 1 {
                nodes.push(Node::new_return_statement_node(return_statement_token_pos, None));
                tokens.pop_front();

                return Some(ast);
            }

            //Return with value
            tokens.pop_front();

            if matches!(tokens[0].token_type(), TokenType::Whitespace) {
                let node = self.parse_assignment(tokens, true).
                        unwrap_or_else(|| self.parse_lrvalue(tokens, true).into_node());
                nodes.push(Node::new_return_statement_node(
                    return_statement_token_pos.combine(&node.pos()),
                    Some(node),
                ));

                return Some(ast);
            }
        }

        //Throw values
        if token_count_first_line > 1 && matches!(tokens[0].token_type(), TokenType::Other) &&
                tokens[0].value() == "throw" {
            let throw_statement_token_pos = tokens.pop_front().unwrap().pos();

            let arguments = self.parse_operation_expr(tokens).unwrap();
            let arguments = Self::convert_comma_operators_to_argument_separators(arguments);

            let mut argument_iter = arguments.into_iter();

            let mut error_nodes = Vec::new();
            let mut flag = false;
            for node in argument_iter.by_ref() {
                if matches!(node.node_data(), NodeData::ArgumentSeparator(_)) {
                    flag = true;
                    break;
                }

                error_nodes.push(node);
            }
            if !flag && error_nodes.is_empty() {
                nodes.push(Node::new_parsing_error_node(
                    throw_statement_token_pos,
                    ParsingError::LexerError,
                    "throw arguments are invalid",
                ));

                return Some(ast);
            }

            let mut message_nodes = Vec::new();
            for node in argument_iter {
                if matches!(node.node_data(), NodeData::ArgumentSeparator(_)) {
                    nodes.push(Node::new_parsing_error_node(
                        CodePosition::EMPTY,
                        ParsingError::LexerError,
                        "throw arguments are invalid",
                    ));

                    return Some(ast);
                }

                message_nodes.push(node);
            }

            let error_node = if error_nodes.len() == 1 {
                error_nodes.into_iter().next().unwrap()
            }else {
                Node::new_list_node(error_nodes)
            };

            let message_node = if message_nodes.is_empty() {
                None
            }else if message_nodes.len() == 1 {
                message_nodes.into_iter().next()
            }else {
                Some(Node::new_list_node(message_nodes))
            };

            let pos = throw_statement_token_pos.combine(&message_node.as_ref().unwrap_or(&error_node).pos());

            nodes.push(Node::new_throw_statement_node(pos, error_node, message_node));

            return Some(ast);
        }

        //Function definition
        if token_count_first_line > 3 && matches!(tokens[0].token_type(), TokenType::Other) &&
                tokens[0].value() == "function" && ends_with_opening_bracket {
            let function_definition_start_token = tokens.pop_front().unwrap();
            token_count_first_line -= 1;

            if !matches!(tokens[0].token_type(), TokenType::Whitespace) {
                nodes.push(Node::new_parsing_error_node(
                    function_definition_start_token.pos(),
                    ParsingError::LexerError,
                    "Invalid function definition: Whitespace is missing after \"function\"",
                ));

                return Some(ast);
            }

            tokens.pop_front();
            token_count_first_line -= 1;

            let overloaded = matches!(tokens[0].token_type(), TokenType::Other) &&
                    tokens[0].value() == "overload" && matches!(tokens[1].token_type(), TokenType::Whitespace);
            if overloaded {
                tokens.pop_front();
                tokens.pop_front();
                token_count_first_line -= 2;
            }

            let combinator = matches!(tokens[0].token_type(), TokenType::Other) &&
                    tokens[0].value() == "combinator" && matches!(tokens[1].token_type(), TokenType::Whitespace);
            if combinator {
                tokens.pop_front();
                tokens.pop_front();
                token_count_first_line -= 2;
            }

            #[expect(clippy::nonminimal_bool)]
            if !(matches!(tokens[0].token_type(), TokenType::Identifier) &&
                    regex_patterns::VAR_NAME_NORMAL_FUNCTION_WITHOUT_PREFIX.is_match(tokens[0].value())) &&
                    !(matches!(tokens[0].token_type(), TokenType::Other) &&
                            regex_patterns::WORD.is_match(tokens[0].value())) {
                nodes.push(Node::new_parsing_error_node(
                    function_definition_start_token.pos(),
                    ParsingError::LexerError,
                    format!(
                        "Invalid function definition: Invalid function identifier: {}",
                        tokens[0].value(),
                    ),
                ));

                return Some(ast);
            }

            let function_name_token = tokens.pop_front().unwrap();
            token_count_first_line -= 1;

            let mut function_name = function_name_token.value().to_string();
            if !function_name.starts_with("fp.") && !function_name.starts_with("$") {
                function_name = "fp.".to_string() + &function_name;
            }

            if matches!(tokens[0].token_type(), TokenType::Whitespace) {
                tokens.pop_front();
                token_count_first_line -= 1;
            }

            if !matches!(tokens[0].token_type(), TokenType::OpeningBracket) ||
                    tokens[0].value() != "(" {
                nodes.push(Node::new_parsing_error_node(
                    function_definition_start_token.pos(),
                    ParsingError::BracketMismatch,
                    "Bracket is missing in parameter list in function definition",
                ));

                return Some(ast);
            }

            let bracket_end_index = utils::get_index_of_matching_bracket_tok(
                tokens.make_contiguous(),
                0, usize::MAX,
                "(", ")", true,
            );
            let Some(bracket_end_index) = bracket_end_index else {
                nodes.push(Node::new_parsing_error_node(
                    function_definition_start_token.pos(),
                    ParsingError::BracketMismatch,
                    "Bracket is missing in parameter list in function definition",
                ));

                return Some(ast);
            };

            let mut parameter_list = utils::split_off_arguments(tokens, bracket_end_index);
            token_count_first_line -= bracket_end_index + 1;

            let type_constraint = if token_count_first_line > 2 &&
                    matches!(tokens[0].token_type(), TokenType::Operator) && tokens[0].value() == ":" &&
                    matches!(tokens[1].token_type(), TokenType::OpeningBracket) && tokens[1].value() == "{" {
                tokens.pop_front();
                token_count_first_line -= 1;

                let bracket_end_index = utils::get_index_of_matching_bracket_tok(
                    tokens.make_contiguous(),
                    0, usize::MAX,
                    "{", "}", true,
                );
                let Some(bracket_end_index) = bracket_end_index else {
                    nodes.push(Node::new_parsing_error_node(
                        function_definition_start_token.pos(),
                        ParsingError::BracketMismatch,
                        "Bracket is missing in return type constraint in function definition",
                    ));

                    return Some(ast);
                };

                let mut type_constraint_tokens = tokens.split_off(bracket_end_index + 1);
                mem::swap(tokens, &mut type_constraint_tokens);
                token_count_first_line -= bracket_end_index + 1;

                self.parse_type_constraint(&mut type_constraint_tokens, false, nodes)
            }else {
                None
            };

            if matches!(tokens[0].token_type(), TokenType::Whitespace) {
                tokens.pop_front();
                token_count_first_line -= 1;
            }

            if token_count_first_line != 1 {
                nodes.push(Node::new_parsing_error_node(
                    function_definition_start_token.pos(),
                    ParsingError::LexerError,
                    "Invalid tokens after function return type constraint",
                ));

                return Some(ast);
            }

            tokens.pop_front();

            if !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::Eol) {
                tokens.pop_front();
            }

            nodes.append(&mut self.parse_function_definition(
                Some(function_name),
                overloaded,
                combinator,
                &mut parameter_list,
                type_constraint,
                tokens,
            ).into_nodes());

            return Some(ast);
        }

        //Struct definition
        if token_count_first_line > 1 && matches!(tokens[0].token_type(), TokenType::Other) &&
                tokens[0].value() == "struct" && ends_with_opening_bracket {
            let struct_definition_start_token = tokens.pop_front().unwrap();
            token_count_first_line -= 1;

            if !matches!(tokens[0].token_type(), TokenType::Whitespace) {
                nodes.push(Node::new_parsing_error_node(
                    struct_definition_start_token.pos(),
                    ParsingError::LexerError,
                    "Invalid struct definition: Whitespace is missing after \"struct\"",
                ));

                return Some(ast);
            }

            tokens.pop_front();
            token_count_first_line -= 1;

            if !matches!(tokens[0].token_type(), TokenType::Identifier) ||
                    !regex_patterns::VAR_NAME_NORMAL_ARRAY_WITHOUT_PREFIX.is_match(tokens[0].value()) {
                nodes.push(Node::new_parsing_error_node(
                    struct_definition_start_token.pos(),
                    ParsingError::LexerError,
                    format!("Invalid struct definition: Invalid struct identifier: \"{}\"", tokens[0]),
                ));

                return Some(ast);
            }

            let struct_name_token = tokens.pop_front().unwrap();
            token_count_first_line -= 1;

            let struct_name = struct_name_token.value().to_string();

            if matches!(tokens[0].token_type(), TokenType::Whitespace) {
                tokens.pop_front();
                token_count_first_line -= 1;
            }

            if token_count_first_line != 1 {
                nodes.push(Node::new_parsing_error_node(
                    struct_definition_start_token.pos(),
                    ParsingError::LexerError,
                    "Invalid tokens after struct identifier",
                ));

                return Some(ast);
            }

            tokens.pop_front();

            if !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::Eol) {
                tokens.pop_front();
            }

            nodes.append(&mut self.parse_struct_definition(
                struct_definition_start_token.pos(),
                Some(struct_name),
                tokens,
            ).into_nodes());

            return Some(ast);
        }

        //Class definition
        if token_count_first_line > 1 && matches!(tokens[0].token_type(), TokenType::Other) &&
                tokens[0].value() == "class" && ends_with_opening_bracket {
            let class_definition_start_token = tokens.pop_front().unwrap();
            token_count_first_line -= 1;

            if !matches!(tokens[0].token_type(), TokenType::Whitespace) {
                nodes.push(Node::new_parsing_error_node(
                    class_definition_start_token.pos(),
                    ParsingError::LexerError,
                    "Invalid class definition: Whitespace is missing after \"class\"",
                ));

                return Some(ast);
            }

            tokens.pop_front();
            token_count_first_line -= 1;

            if !matches!(tokens[0].token_type(), TokenType::Identifier) ||
                    !regex_patterns::VAR_NAME_NORMAL_ARRAY_WITHOUT_PREFIX.is_match(tokens[0].value()) {
                nodes.push(Node::new_parsing_error_node(
                    class_definition_start_token.pos(),
                    ParsingError::LexerError,
                    format!("Invalid class definition: Invalid class identifier: \"{}\"", tokens[0]),
                ));

                return Some(ast);
            }

            let class_name_token = tokens.pop_front().unwrap();
            token_count_first_line -= 1;

            let class_name = class_name_token.value().to_string();

            if matches!(tokens[0].token_type(), TokenType::Whitespace) {
                tokens.pop_front();
                token_count_first_line -= 1;
            }

            let mut parent_class_tokens = if matches!(tokens[0].token_type(), TokenType::Operator) &&
                    tokens[0].value() == "<" && ends_with_opening_bracket {
                //TODO check for matching brackets ("<" and ">")

                let mut parent_class_end_index = None;
                for i in (0..token_count_first_line).rev() {
                    if matches!(tokens[i].token_type(), TokenType::Operator) &&
                            tokens[i].value() == ">" && ends_with_opening_bracket {
                        parent_class_end_index = Some(i);

                        break;
                    }
                }

                let Some(parent_class_end_index) = parent_class_end_index else {
                    nodes.push(Node::new_parsing_error_node(
                        class_definition_start_token.pos(),
                        ParsingError::BracketMismatch,
                        "Bracket is missing in class definition",
                    ));

                    return Some(ast);
                };

                let parent_class_tokens = utils::split_off_arguments(tokens, parent_class_end_index);
                token_count_first_line -= parent_class_end_index + 1;

                if matches!(tokens[0].token_type(), TokenType::Whitespace) {
                    tokens.pop_front();
                    token_count_first_line -= 1;
                }

                parent_class_tokens
            }else {
                VecDeque::new()
            };

            if token_count_first_line != 1 {
                nodes.push(Node::new_parsing_error_node(
                    class_definition_start_token.pos(),
                    ParsingError::LexerError,
                    "Invalid tokens after class definition",
                ));

                return Some(ast);
            }

            tokens.pop_front();

            if !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::Eol) {
                tokens.pop_front();
            }

            nodes.append(&mut self.parse_class_definition(
                class_definition_start_token.pos(),
                Some(class_name),
                &mut parent_class_tokens,
                tokens,
            ).into_nodes());

            return Some(ast);
        }

        nodes.append(&mut self.parse_token(tokens).into_nodes());

        Some(ast)
    }

    /**
     * Returns true if the parser flag was valid else false
     */
    fn parse_parser_flags(&mut self, _parse_flag: impl Into<String>, _value: Node) -> bool {
        //TODO

        false
    }

    fn parse_translation_key(&mut self, tokens: &mut VecDeque<Token>) -> AST {
        let mut ast = AST::new();
        let nodes = ast.nodes_mut();

        let mut pos = tokens.front().map(|token| token.pos()).
                unwrap_or(CodePosition::EMPTY);

        Self::trim_first_line(tokens);

        if pos == CodePosition::EMPTY {
            pos = tokens.front().map(|token| token.pos()).unwrap_or(CodePosition::EMPTY);
        }

        nodes.push(Node::new_text_value_node(pos, ""));

        if tokens.len() >= 2 && matches!(tokens[0].token_type(), TokenType::Operator) &&
                tokens[0].value() == "%" && matches!(tokens[1].token_type(), TokenType::Identifier) &&
                tokens[1].value().starts_with("$") {
            //Prepare "%$" for translation key
            tokens.pop_front();
        }

        'tokenProcessing:
        while !tokens.is_empty() {
            let t = tokens[0].clone();

            match t.token_type() {
                TokenType::Eol | TokenType::Eof => {
                    break 'tokenProcessing;
                },

                TokenType::StartComment | TokenType::StartDocComment => {
                    self.parse_comment_tokens(tokens, nodes);
                },

                TokenType::LiteralNull | TokenType::LiteralText | TokenType::LiteralNumber |
                TokenType::ArgumentSeparator | TokenType::Assignment | TokenType::Operator |
                TokenType::OpeningBracket | TokenType::ClosingBracket | TokenType::OpeningBlockBracket |
                TokenType::ClosingBlockBracket | TokenType::Whitespace | TokenType::Other =>  {
                    tokens.pop_front();

                    nodes.push(Node::new_text_value_node(pos, t.value()));
                },

                TokenType::EscapeSequence => {
                    tokens.pop_front();

                    self.parse_escape_sequence_token(t, nodes);
                },

                TokenType::LexerError => {
                    tokens.pop_front();

                    self.parse_lexer_error_token(t, nodes);
                },

                TokenType::StartMultilineText => {
                    tokens.pop_front();

                    loop {
                        if let Some(t) = tokens.pop_front() {
                            if matches!(t.token_type(), TokenType::EndMultilineText) {
                                break;
                            }

                            if matches!(t.token_type(), TokenType::LiteralText | TokenType::Eol) {
                                nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                            }else if matches!(t.token_type(), TokenType::EscapeSequence) {
                                self.parse_escape_sequence_token(t, nodes);
                            }else if matches!(t.token_type(), TokenType::LexerError) {
                                nodes.push(Node::new_parsing_error_node(
                                    t.pos(),
                                    ParsingError::LexerError,
                                    t.value()
                                ));
                            }else {
                                nodes.push(Node::new_parsing_error_node(
                                    CodePosition::EMPTY,
                                    ParsingError::Eof,
                                    format!(
                                        "Invalid token type ({:?}) in multiline text during translation key parsing",
                                        t.token_type(),
                                    ),
                                ));
                            }
                        }else {
                            nodes.push(Node::new_parsing_error_node(
                                CodePosition::EMPTY,
                                ParsingError::Eof,
                                "Missing multiline text end token during translation key parsing",
                            ));

                            break 'tokenProcessing;
                        }
                    }
                },

                TokenType::Identifier | TokenType::ParserFunctionIdentifier => {
                    if matches!(t.token_type(), TokenType::Identifier) &&
                            !regex_patterns::VAR_NAME_FULL.is_match(t.value()) {
                        tokens.pop_front();

                        nodes.push(Node::new_text_value_node(pos, t.value()));
                    }

                    let ret = if matches!(t.token_type(), TokenType::Identifier) {
                        self.parse_variable_name_and_function_call(tokens, None)
                    }else {
                        self.parse_parser_function_call(tokens)
                    };

                    if let Some(ret) = ret {
                        nodes.push(ret);
                    }
                },

                TokenType::LineContinuation | TokenType::EndComment | TokenType::EndMultilineText |
                TokenType::SingleLineTextQuotes => {
                    nodes.push(Node::new_parsing_error_node(
                        CodePosition::EMPTY,
                        ParsingError::LexerError,
                        format!(
                            "Invalid token type in translation key expression: \"{:?}\"",
                            t.token_type(),
                        ),
                    ));

                    break 'tokenProcessing;
                },
            }
        }

        ast
    }

    fn parse_lrvalue(&mut self, tokens: &mut VecDeque<Token>, is_rvalue: bool) -> AST {
        let mut ast = AST::new();
        let nodes = ast.nodes_mut();

        Self::trim_first_line(tokens);

        if is_rvalue {
            let mut token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());

            if token_count_first_line >= 1 && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                    tokens[0].value() == "(" {
                //Possible function definition

                let parameter_end_index = utils::get_index_of_matching_bracket_tok(
                    tokens.make_contiguous(), 0, usize::MAX, "(", ")", true,
                );
                let Some(parameter_end_index) = parameter_end_index else {
                    nodes.push(Node::new_parsing_error_node(
                        CodePosition::EMPTY,
                        ParsingError::BracketMismatch,
                        "Bracket is missing in function definition",
                    ));

                    return ast;
                };

                let mut parameter_list_tokens = utils::split_off_arguments(tokens, parameter_end_index);
                let parameter_list = self.parse_function_parameter_list(&mut parameter_list_tokens, true).into_nodes();

                token_count_first_line -= parameter_end_index + 1;

                let mut return_type_constraint = None;
                if token_count_first_line >= 2 && matches!(tokens[0].token_type(), TokenType::Operator) &&
                        tokens[0].value() == ":" && matches!(tokens[1].token_type(), TokenType::OpeningBracket) &&
                        tokens[1].value() == "{" {
                    tokens.pop_front();
                    token_count_first_line -= 1;

                    let return_type_constraint_end_index = utils::get_index_of_matching_bracket_tok(
                        tokens.make_contiguous(), 0, usize::MAX, "{", "}", true,
                    );
                    let Some(return_type_constraint_end_index) = return_type_constraint_end_index else {
                        nodes.push(Node::new_parsing_error_node(
                            CodePosition::EMPTY,
                            ParsingError::BracketMismatch,
                            "Bracket is missing in return type constraint of function definition",
                        ));

                        return ast;
                    };

                    let mut type_constraint_tokens = tokens.split_off(return_type_constraint_end_index + 1);
                    mem::swap(tokens, &mut type_constraint_tokens);

                    token_count_first_line -= return_type_constraint_end_index + 1;

                    return_type_constraint = self.parse_type_constraint(
                        &mut type_constraint_tokens,
                        false,
                        nodes,
                    );
                }

                if token_count_first_line >= 3 && matches!(tokens[0].token_type(), TokenType::Whitespace) &&
                        matches!(tokens[1].token_type(), TokenType::Operator) && tokens[1].value() == "->" &&
                        matches!(tokens[2].token_type(), TokenType::Whitespace) {
                    tokens.pop_front();
                    tokens.pop_front();
                    tokens.pop_front();
                    token_count_first_line -= 3;

                    //TODO line numbers
                    if token_count_first_line >= 1 && matches!(tokens[0].token_type(), TokenType::OpeningBlockBracket) {
                        tokens.pop_front();

                        nodes.push(Node::new_function_definition_node(
                            CodePosition::EMPTY,
                            FunctionDefinition::new(
                                None, false, false,
                                self.lang_doc_comment.take().map(Box::from),
                                return_type_constraint.map(Box::from),
                                self.parse_tokens_internal(tokens).unwrap(),
                            ),
                            parameter_list,
                        ));
                    }else {
                        let mut function_body = tokens.split_off(token_count_first_line);
                        mem::swap(tokens, &mut function_body);

                        nodes.push(Node::new_function_definition_node(
                            CodePosition::EMPTY,
                            FunctionDefinition::new(
                                None, false, false,
                                self.lang_doc_comment.take().map(Box::from),
                                return_type_constraint.map(Box::from),
                                self.parse_tokens_internal(&mut function_body).unwrap(),
                            ),
                            parameter_list,
                        ));
                    }

                    return ast;
                }
            }

            if token_count_first_line == 1 && matches!(tokens[0].token_type(), TokenType::Identifier) &&
                    regex_patterns::VAR_NAME_FUNC_PTR_WITH_FUNCS.is_match(tokens[0].value()) {
                //Function pointer copying

                let t = tokens.pop_front().unwrap();

                nodes.push(Node::new_unprocessed_variable_name_node(t.pos(), t.value()));

                return ast;
            }else if token_count_first_line == 1 && tokens.len() > token_count_first_line &&
                    matches!(tokens[0].token_type(), TokenType::OpeningBlockBracket) {
                //Struct definition

                let start_pos = tokens[0].pos();

                tokens.pop_front();
                tokens.pop_front();

                nodes.append(&mut self.parse_struct_definition(
                    start_pos,
                    None,
                    tokens
                ).into_nodes());

                return ast;
            }else if token_count_first_line > 3 && tokens.len() > token_count_first_line &&
                    matches!(tokens[0].token_type(), TokenType::Operator) && tokens[0].value() == "<" &&
                    matches!(tokens[token_count_first_line - 2].token_type(), TokenType::Operator) &&
                    tokens[token_count_first_line - 2].value() == ">" &&
                    matches!(tokens[token_count_first_line - 1].token_type(), TokenType::OpeningBlockBracket) {
                //Class definition

                let start_pos = tokens[0].pos();

                //TODO check for matching brackets ("<" and ">")
                let mut parent_class_tokens = utils::split_off_arguments(tokens, token_count_first_line - 2);
                //Remove Opening Block bracket and EOL tokens after parent class tokens
                tokens.pop_front();
                tokens.pop_front();

                nodes.append(&mut self.parse_class_definition(
                    start_pos,
                    None,
                    &mut parent_class_tokens,
                    tokens,
                ).into_nodes());

                return ast;
            }
        }

        nodes.append(&mut self.parse_token(tokens).into_nodes());

        ast
    }

    fn parse_function_definition(
        &mut self,
        function_name: Option<String>,
        overloaded: bool,
        combinator: bool,
        parameter_list_tokens: &mut VecDeque<Token>,
        return_value_type_constraint: Option<String>,
        tokens: &mut VecDeque<Token>,
    ) -> AST {
        let mut ast = AST::new();
        let nodes = ast.nodes_mut();

        Self::trim_first_line(tokens);

        let parameter_list_nodes = self.parse_function_parameter_list(
            parameter_list_tokens,
            true,
        ).into_nodes();

        //TODO line numbers
        nodes.push(Node::new_function_definition_node(
            CodePosition::EMPTY,
            FunctionDefinition::new(
                function_name.map(Box::from),
                overloaded,
                combinator,
                self.lang_doc_comment.take().map(Box::from),
                return_value_type_constraint.map(Box::from),
                self.parse_tokens_internal(tokens).unwrap(),
            ),
            parameter_list_nodes,
        ));

        ast
    }

    fn parse_struct_definition(
        &mut self,
        start_pos: CodePosition,
        struct_name: Option<String>,
        tokens: &mut VecDeque<Token>,
    ) -> AST {
        let mut ast = AST::new();
        let nodes = ast.nodes_mut();

        Self::trim_first_line(tokens);

        let mut has_end_brace = false;

        let mut members = Vec::new();

        let mut end_pos = CodePosition::EMPTY;

        'tokenProcessing:
        while !tokens.is_empty() {
            let t = tokens[0].clone();
            end_pos = t.pos();

            match t.token_type() {
                TokenType::Eof => {
                    break 'tokenProcessing;
                },

                TokenType::Eol => {
                    tokens.pop_front();

                    Self::trim_first_line(tokens);
                },

                TokenType::Whitespace => {
                    tokens.pop_front();
                },

                TokenType::StartComment | TokenType::StartDocComment => {
                    self.parse_comment_tokens(tokens, nodes);
                },

                TokenType::ClosingBlockBracket =>  {
                    tokens.pop_front();

                    has_end_brace = true;

                    break 'tokenProcessing;
                },

                TokenType::Identifier => {
                    if !regex_patterns::VAR_NAME_WITHOUT_PREFIX.is_match(t.value()) {
                        nodes.push(Node::new_parsing_error_node(
                            t.pos(),
                            ParsingError::InvalidAssignment,
                            format!(
                                "Invalid struct member name: \"{}\"",
                                t.value(),
                            ),
                        ));

                        return ast;
                    }

                    let identifier_token = tokens.pop_front().unwrap();

                    let mut type_constraint = None;
                    if !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                            tokens[0].value() == "{" {
                        let bracket_end_index = utils::get_index_of_matching_bracket_tok(
                            tokens.make_contiguous(),
                            0, usize::MAX,
                            "{", "}", true,
                        );
                        let Some(bracket_end_index) = bracket_end_index else {
                            nodes.push(Node::new_parsing_error_node(
                                identifier_token.pos(),
                                ParsingError::BracketMismatch,
                                format!(
                                    "Bracket is missing in type constraint in struct definition for member: \"{}\"",
                                    identifier_token.value(),
                                ),
                            ));

                            return ast;
                        };

                        let mut type_constraint_tokens = tokens.split_off(bracket_end_index + 1);
                        mem::swap(tokens, &mut type_constraint_tokens);

                        type_constraint = self.parse_type_constraint(
                            &mut type_constraint_tokens,
                            false,
                            nodes,
                        );
                    }

                    if members.iter().any(|member: &StructMember| member.name() == identifier_token.value()) {
                        nodes.push(Node::new_parsing_error_node(
                            identifier_token.pos(),
                            ParsingError::InvalidAssignment,
                            format!(
                                "Duplicated struct member name: \"{}\"", identifier_token.value(),
                            ),
                        ));

                        return ast;
                    };

                    members.push(StructMember::new(Box::from(identifier_token.value()), type_constraint.map(Box::from)));
                },

                TokenType::LexerError => {
                    tokens.pop_front();

                    self.parse_lexer_error_token(t, nodes);

                    break 'tokenProcessing;
                },

                TokenType::ParserFunctionIdentifier | TokenType::LiteralNull | TokenType::LiteralNumber |
                TokenType::LiteralText | TokenType::ArgumentSeparator | TokenType::Assignment |
                TokenType::Other | TokenType::Operator | TokenType::OpeningBracket | TokenType::ClosingBracket |
                TokenType::OpeningBlockBracket | TokenType::EscapeSequence | TokenType::StartMultilineText |
                TokenType::LineContinuation | TokenType::EndComment | TokenType::EndMultilineText |
                TokenType::SingleLineTextQuotes => {
                    nodes.push(Node::new_parsing_error_node(
                        CodePosition::EMPTY,
                        ParsingError::LexerError,
                        format!(
                            "Invalid token type for struct definition expression: \"{:?}\"",
                            t.token_type(),
                        ),
                    ));

                    return ast;
                },
            }
        }

        let pos = start_pos.combine(&end_pos);

        if !has_end_brace {
            nodes.push(Node::new_parsing_error_node(
                pos,
                ParsingError::Eof,
                "\"}\" is missing in struct definition",
            ));

            return ast;
        }

        nodes.push(Node::new_struct_definition_node(pos, StructDefinition::new(
            struct_name.map(Box::from),
            members,
        )));

        ast
    }

    fn parse_class_definition(
        &mut self,
        start_pos: CodePosition,
        class_name: Option<String>,
        parent_class_tokens: &mut VecDeque<Token>,
        tokens: &mut VecDeque<Token>,
    ) -> AST {
        let mut ast = AST::new();
        let nodes = ast.nodes_mut();

        Self::trim_first_line(tokens);

        let parent_classes = self.parse_function_parameter_list(parent_class_tokens, false).into_nodes();

        let mut has_end_brace = false;

        let mut static_members: Vec<ClassMember> = Vec::new();
        let mut members = Vec::new();
        let mut methods = Vec::new();
        let mut constructors = Vec::new();

        let mut end_pos = CodePosition::EMPTY;

        'tokenProcessing:
        while !tokens.is_empty() {
            let t = tokens[0].clone();
            end_pos = t.pos();

            match t.token_type() {
                TokenType::Eof => {
                    break 'tokenProcessing;
                },

                TokenType::Eol => {
                    tokens.pop_front();

                    Self::trim_first_line(tokens);
                },

                TokenType::Whitespace => {
                    tokens.pop_front();
                },

                TokenType::StartComment | TokenType::StartDocComment => {
                    self.parse_comment_tokens(tokens, nodes);
                },

                TokenType::ClosingBlockBracket =>  {
                    tokens.pop_front();

                    has_end_brace = true;

                    break 'tokenProcessing;
                },

                TokenType::Other | TokenType::Operator => {
                    let visibility = if t.value().len() == 1 {
                        let visibility_symbol = t.value().as_bytes()[0];
                        let visibility = Visibility::from_symbol(visibility_symbol);

                        if visibility.is_none() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::LexerError,
                                "Invalid visibility symbol (One of [\"-\", \"~\", or \"+\"] must be used)"
                            ));

                            return ast;
                        }

                        tokens.pop_front();

                        visibility.unwrap()
                    }else {
                        let visibility_keyword = t.value();
                        let visibility = Visibility::from_keyword(visibility_keyword);

                        if visibility.is_none() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::LexerError,
                                "Invalid visibility keyword (One of [\"private\", \"protected\", or \"public\"] must be used)"
                            ));

                            return ast;
                        }

                        tokens.pop_front();

                        if tokens.front().is_none_or(|token|
                                !matches!(token.token_type(), TokenType::Whitespace)) {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::Eof,
                                "Missing whitespace after visibility keyword specifier"
                            ));

                            return ast;
                        }

                        tokens.pop_front();

                        visibility.unwrap()
                    };

                    let Some(mut t) = tokens.front().cloned() else {
                        nodes.push(Node::new_parsing_error_node(
                            t.pos(),
                            ParsingError::Eof,
                            "Missing value after visibility specifier"
                        ));

                        return ast;
                    };

                    //Constructor methods
                    if matches!(t.token_type(), TokenType::Other) && t.value() == "construct" {
                        tokens.pop_front();

                        let Some(t) = tokens.front() else {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::Eof,
                                "Missing value after construct method"
                            ));

                            return ast;
                        };

                        if !matches!(t.token_type(), TokenType::Assignment) || t.value() != " = " {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidAssignment,
                                "Invalid assignment for constructor (only \" = \" is allowed)"
                            ));

                            return ast;
                        }

                        tokens.pop_front();

                        constructors.push(Constructor::new(
                            self.parse_lrvalue(tokens, true).into_node(),
                            visibility,
                        ));

                        continue 'tokenProcessing;
                    }

                    //Methods
                    let is_override_method = tokens.len() >= 2 && matches!(t.token_type(), TokenType::Other) &&
                            t.value() == "override" && matches!(tokens[1].token_type(), TokenType::Operator) &&
                            tokens[1].value() == ":";
                    if is_override_method {
                        tokens.pop_front();
                        tokens.pop_front();

                        if tokens.is_empty() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::Eof,
                                "Missing identifier after override keyword"
                            ));

                            return ast;
                        }

                        t = tokens[0].clone();
                    }

                    if matches!(t.token_type(), TokenType::Identifier) && t.value().starts_with("op:") {
                        if !matches!(visibility, Visibility::Public) {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidAssignment,
                                "Operator method must be public"
                            ));

                            return ast;
                        }

                        let method_name_token = tokens.pop_front().unwrap();
                        let method_name = method_name_token.value();
                        if !regex_patterns::OPERATOR_METHOD_NAME.is_match(method_name) {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidAssignment,
                                format!(
                                    "Invalid operator method name: \"{}\"",
                                    method_name,
                                ),
                            ));

                            return ast;
                        }

                        if tokens.is_empty() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::Eof,
                                "Missing value after operator method",
                            ));

                            return ast;
                        }

                        let t = tokens[0].clone();
                        if !matches!(t.token_type(), TokenType::Assignment) || t.value() != " = " {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidAssignment,
                                "Invalid assignment for operator method (only \" = \" is allowed)",
                            ));

                            return ast;
                        }

                        tokens.pop_front();

                        methods.push(Method::new(
                            Box::from(method_name),
                            self.parse_lrvalue(tokens, true).into_node(),
                            is_override_method,
                            visibility,
                        ));

                        continue 'tokenProcessing;
                    }

                    if matches!(t.token_type(), TokenType::Identifier) && t.value().starts_with("to:") {
                        if !matches!(visibility, Visibility::Public) {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidAssignment,
                                "Conversion method must be public"
                            ));

                            return ast;
                        }

                        let method_name_token = tokens.pop_front().unwrap();
                        let method_name = method_name_token.value();
                        if !regex_patterns::CONVERSION_METHOD_NAME.is_match(method_name) {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidAssignment,
                                format!(
                                    "Invalid conversion method name: \"{}\"",
                                    method_name,
                                ),
                            ));

                            return ast;
                        }

                        if tokens.is_empty() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::Eof,
                                "Missing value after conversion method",
                            ));

                            return ast;
                        }

                        let t = tokens[0].clone();
                        if !matches!(t.token_type(), TokenType::Assignment) || t.value() != " = " {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidAssignment,
                                "Invalid assignment for conversion method (only \" = \" is allowed)",
                            ));

                            return ast;
                        }

                        tokens.pop_front();

                        methods.push(Method::new(
                            Box::from(method_name),
                            self.parse_lrvalue(tokens, true).into_node(),
                            is_override_method,
                            visibility,
                        ));

                        continue 'tokenProcessing;
                    }

                    if matches!(t.token_type(), TokenType::Identifier) &&
                            regex_patterns::METHOD_NAME.is_match(t.value()) {
                        let method_name_token = tokens.pop_front().unwrap();
                        let method_name = method_name_token.value();

                        if tokens.is_empty() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::Eof,
                                "Missing value after normal method",
                            ));

                            return ast;
                        }

                        let t = tokens[0].clone();
                        if !matches!(t.token_type(), TokenType::Assignment) || t.value() != " = " {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidAssignment,
                                "Invalid assignment for conversion method (only \" = \" is allowed)",
                            ));

                            return ast;
                        }

                        tokens.pop_front();

                        methods.push(Method::new(
                            Box::from(method_name),
                            self.parse_lrvalue(tokens, true).into_node(),
                            is_override_method,
                            visibility,
                        ));

                        continue 'tokenProcessing;
                    }

                    if is_override_method {
                        nodes.push(Node::new_parsing_error_node(
                            t.pos(),
                            ParsingError::LexerError,
                            "The override keyword can only be used for methods",
                        ));

                        return ast;
                    }

                    //Members
                    let mut is_static_member = tokens.len() >= 2 && matches!(t.token_type(), TokenType::Other) &&
                            t.value() == "static" && matches!(tokens[1].token_type(), TokenType::Operator) &&
                            tokens[1].value() == ":";
                    if is_static_member {
                        tokens.pop_front();
                        tokens.pop_front();

                        if tokens.is_empty() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::Eof,
                                "Missing identifier after static keyword",
                            ));

                            return ast;
                        }

                        t = tokens[0].clone();
                    }

                    let is_final_member = tokens.len() >= 2 && matches!(t.token_type(), TokenType::Other) &&
                            t.value() == "final" && matches!(tokens[1].token_type(), TokenType::Operator) &&
                            tokens[1].value() == ":";
                    if is_final_member {
                        tokens.pop_front();
                        tokens.pop_front();

                        if tokens.is_empty() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::Eof,
                                "Missing identifier after final keyword",
                            ));

                            return ast;
                        }

                        t = tokens[0].clone();
                    }

                    if !is_static_member && is_final_member && tokens.len() >= 2 &&
                            matches!(t.token_type(), TokenType::Other) && t.value() == "static" &&
                            matches!(tokens[1].token_type(), TokenType::Operator) && tokens[1].value() == ":" {
                        is_static_member = true;

                        tokens.pop_front();
                        tokens.pop_front();

                        if tokens.is_empty() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::Eof,
                                "Missing identifier after static keyword",
                            ));

                            return ast;
                        }

                        t = tokens[0].clone();
                    }

                    if !matches!(t.token_type(), TokenType::Identifier) {
                        nodes.push(Node::new_parsing_error_node(
                            t.pos(),
                            ParsingError::LexerError,
                            format!(
                                "Invalid token type for class definition expression: \"{:?}\"",
                                t.token_type(),
                            ),
                        ));

                        return ast;
                    }

                    if !regex_patterns::VAR_NAME_WITHOUT_PREFIX.is_match(t.value()) {
                        nodes.push(Node::new_parsing_error_node(
                            t.pos(),
                            ParsingError::InvalidAssignment,
                            format!(
                                "Invalid {}member name: \"{}\"",
                                if is_static_member {
                                    "static "
                                }else {
                                    ""
                                },
                                t.value(),
                            ),
                        ));

                        return ast;
                    }

                    let member_name_token = tokens.pop_front().unwrap();
                    let member_name = member_name_token.value();

                    let mut type_constraint = None;
                    if !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                            tokens[0].value() == "{" {
                        let bracket_end_index = utils::get_index_of_matching_bracket_tok(
                            tokens.make_contiguous(),
                            0, usize::MAX,
                            "{", "}", true,
                        );
                        let Some(bracket_end_index) = bracket_end_index else {
                            nodes.push(Node::new_parsing_error_node(
                                member_name_token.pos(),
                                ParsingError::BracketMismatch,
                                format!(
                                    "Bracket is missing in type constraint in class definition for {}member: \"{}\"",
                                    if is_static_member {
                                        "static"
                                    }else {
                                        ""
                                    },
                                    member_name,
                                ),
                            ));

                            return ast;
                        };

                        let mut type_constraint_tokens = tokens.split_off(bracket_end_index + 1);
                        mem::swap(tokens, &mut type_constraint_tokens);

                        type_constraint = self.parse_type_constraint(
                            &mut type_constraint_tokens,
                            false,
                            nodes,
                        );
                    }

                    let is_duplicate = if is_static_member {
                        static_members.iter()
                    }else {
                        members.iter()
                    }.any(|member| member.name() == member_name);
                    if is_duplicate {
                        nodes.push(Node::new_parsing_error_node(
                            t.pos(),
                            ParsingError::InvalidAssignment,
                            format!(
                                "Duplicated {}member name: \"{}\"",
                                if is_static_member {
                                    "static "
                                }else {
                                    ""
                                },
                                member_name,
                            ),
                        ));

                        return ast;
                    }

                    if is_static_member {
                        let mut static_member_value = None;
                        if !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::Assignment) {
                            let assignment_token = tokens.pop_front().unwrap();
                            let assignment_operator = assignment_token.value();

                            if tokens.is_empty() ||
                                    matches!(tokens[0].token_type(), TokenType::Eol | TokenType::Eof) {
                                if assignment_operator != "=" && assignment_operator != " =" {
                                    nodes.push(Node::new_parsing_error_node(
                                        t.pos(),
                                        ParsingError::InvalidAssignment,
                                        "Rvalue is missing in member assignment",
                                    ));

                                    return ast;
                                }

                                static_member_value = Some(if assignment_operator == "=" {
                                    Node::new_text_value_node(assignment_token.pos(), "")
                                }else {
                                    Node::new_null_value_node(assignment_token.pos())
                                });
                            }else {
                                match assignment_operator {
                                    "=" => {
                                        static_member_value = Some(self.parse_simple_assignment_value(
                                            tokens,
                                        ).into_node());
                                    },

                                    " = " => {
                                        static_member_value = Some(self.parse_lrvalue(
                                            tokens, true,
                                        ).into_node());
                                    },

                                    " ?= " => {
                                        static_member_value = Some(self.parse_condition_expr(
                                            tokens
                                        ).unwrap());
                                    },

                                    " := " => {
                                        static_member_value = Some(self.parse_math_expr(
                                            tokens
                                        ).unwrap());
                                    },

                                    " $= " => {
                                        static_member_value = Some(self.parse_operation_expr(
                                            tokens
                                        ).unwrap());
                                    },

                                    _ => {
                                        nodes.push(Node::new_parsing_error_node(
                                            t.pos(),
                                            ParsingError::InvalidAssignment,
                                            "Invalid assignment for static member (only the following operators are allowed: \"=\", \" = \", \" ?= \", \" := \", and \" $= \")",
                                        ));

                                        return ast;
                                    },
                                }
                            }
                        }

                        static_members.push(ClassMember::new(
                            Box::from(member_name),
                            type_constraint.map(Box::from),
                            static_member_value,
                            is_final_member,
                            visibility,
                        ));

                        continue 'tokenProcessing;
                    }

                    members.push(ClassMember::new(
                        Box::from(member_name),
                        type_constraint.map(Box::from),
                        None,
                        is_final_member,
                        visibility,
                    ));
                },

                TokenType::LexerError => {
                    tokens.pop_front();

                    self.parse_lexer_error_token(t, nodes);

                    break 'tokenProcessing;
                },

                TokenType::Identifier | TokenType::ParserFunctionIdentifier | TokenType::LiteralNull |
                TokenType::LiteralNumber | TokenType::LiteralText | TokenType::ArgumentSeparator |
                TokenType::Assignment | TokenType::OpeningBracket | TokenType::ClosingBracket |
                TokenType::OpeningBlockBracket | TokenType::EscapeSequence | TokenType::StartMultilineText |
                TokenType::LineContinuation | TokenType::EndComment | TokenType::EndMultilineText |
                TokenType::SingleLineTextQuotes => {
                    nodes.push(Node::new_parsing_error_node(
                        CodePosition::EMPTY,
                        ParsingError::LexerError,
                        format!(
                            "Invalid token type for class definition expression: \"{:?}\"",
                            t.token_type(),
                        ),
                    ));

                    return ast;
                },
            }
        }

        let pos = start_pos.combine(&end_pos);

        if !has_end_brace {
            nodes.push(Node::new_parsing_error_node(
                pos,
                ParsingError::Eof,
                "\"}\" is missing in class definition",
            ));

            return ast;
        }

        nodes.push(Node::new_class_definition_node(pos, ClassDefinition::new(
            class_name.map(Box::from),
            static_members,
            members,
            methods,
            constructors,
            parent_classes,
        )));

        ast
    }

    fn parse_token(
        &mut self,
        tokens: &mut VecDeque<Token>,
    ) -> AST {
        let mut ast = AST::new();
        let nodes = ast.nodes_mut();

        Self::trim_first_line(tokens);

        'tokenProcessing:
        while !tokens.is_empty() {
            let t = tokens[0].clone();

            match t.token_type() {
                TokenType::Eol | TokenType::Eof => {
                    break 'tokenProcessing;
                },

                TokenType::StartComment | TokenType::StartDocComment => {
                    self.parse_comment_tokens(tokens, nodes);
                },

                TokenType::LiteralNull => {
                    tokens.pop_front();

                    nodes.push(Node::new_null_value_node(t.pos()));
                },

                TokenType::LiteralText | TokenType::ArgumentSeparator | TokenType::Assignment |
                TokenType::ClosingBracket | TokenType::OpeningBlockBracket | TokenType::ClosingBlockBracket |
                TokenType::Whitespace => {
                    tokens.pop_front();

                    nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                },

                TokenType::Other => {
                    tokens.pop_front();

                    self.parse_text_and_char_value(&mut VecDeque::from([t]), nodes);
                },

                TokenType::Operator => {
                    tokens.pop_front();

                    if nodes.is_empty() && matches!(t.value(), "+" | "-") &&
                            !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::LiteralNumber) {
                        let number_token = tokens.pop_front().unwrap();

                        let combined_number_token = Token::new(
                            t.pos().combine(&number_token.pos()),
                            &(t.value().to_string() + number_token.value()),
                            TokenType::LiteralNumber,
                        );

                        self.parse_number_token(combined_number_token, nodes);

                        continue 'tokenProcessing;
                    }

                    nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                },

                TokenType::LiteralNumber => {
                    tokens.pop_front();

                    self.parse_number_token(t, nodes);
                },

                TokenType::OpeningBracket => {
                    if t.value() == "(" {
                        let end_index = utils::get_index_of_matching_bracket_tok(
                            tokens.make_contiguous(), 0, usize::MAX, "(", ")", true,
                        );
                        if let Some(end_index) = end_index {
                            let opening_bracket_token = &tokens[0];
                            let closing_bracket_token = &tokens[end_index];
                            let pos = opening_bracket_token.pos().combine(&closing_bracket_token.pos());

                            let mut function_call = utils::split_off_arguments(tokens, end_index);

                            nodes.push(Node::new_function_call_previous_node_value_node(
                                pos, "", "",
                                self.parse_function_parameter_list(&mut function_call, false).into_nodes(),
                            ));

                            continue 'tokenProcessing;
                        }
                    }

                    tokens.pop_front();

                    nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                },

                TokenType::EscapeSequence => {
                    tokens.pop_front();

                    self.parse_escape_sequence_token(t, nodes);
                },

                TokenType::LexerError => {
                    tokens.pop_front();

                    self.parse_lexer_error_token(t, nodes);
                },

                TokenType::StartMultilineText => {
                    tokens.pop_front();

                    loop {
                        if let Some(t) = tokens.pop_front() {
                            if matches!(t.token_type(), TokenType::EndMultilineText) {
                                break;
                            }

                            if matches!(t.token_type(), TokenType::LiteralText | TokenType::Eol) {
                                nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                            }else if matches!(t.token_type(), TokenType::EscapeSequence) {
                                self.parse_escape_sequence_token(t, nodes);
                            }else if matches!(t.token_type(), TokenType::LexerError) {
                                nodes.push(Node::new_parsing_error_node(
                                    t.pos(),
                                    ParsingError::LexerError,
                                    t.value()
                                ));
                            }else {
                                nodes.push(Node::new_parsing_error_node(
                                    CodePosition::EMPTY,
                                    ParsingError::Eof,
                                    format!(
                                        "Invalid token type ({:?}) in multiline text during token value parsing",
                                        t.token_type(),
                                    ),
                                ));
                            }
                        }else {
                            nodes.push(Node::new_parsing_error_node(
                                CodePosition::EMPTY,
                                ParsingError::Eof,
                                "Missing multiline text end token during token value parsing",
                            ));

                            break 'tokenProcessing;
                        }
                    }
                },

                TokenType::Identifier | TokenType::ParserFunctionIdentifier => {
                    let ret = if matches!(t.token_type(), TokenType::Identifier) {
                        self.parse_variable_name_and_function_call(tokens, None)
                    }else {
                        self.parse_parser_function_call(tokens)
                    };
                    if let Some(ret) = ret {
                        nodes.push(ret);
                    }
                },

                TokenType::LineContinuation | TokenType::EndComment | TokenType::EndMultilineText |
                TokenType::SingleLineTextQuotes => {
                    nodes.push(Node::new_parsing_error_node(
                        CodePosition::EMPTY,
                        ParsingError::LexerError,
                        format!(
                            "Invalid token type in token value expression: \"{:?}\"",
                            t.token_type(),
                        ),
                    ));

                    break 'tokenProcessing;
                },
            }
        }

        ast
    }

    fn parse_simple_assignment_value(&mut self, tokens: &mut VecDeque<Token>) -> AST {
        let mut ast = AST::new();
        let nodes = ast.nodes_mut();

        Self::trim_first_line(tokens);

        let token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());
        if token_count_first_line == 0 || token_count_first_line != tokens.len() {
            nodes.push(Node::new_text_value_node(CodePosition::EMPTY, ""));
        }


        'tokenProcessing:
        while !tokens.is_empty() {
            let t = tokens[0].clone();

            match t.token_type() {
                TokenType::Eol | TokenType::Eof => {
                    break 'tokenProcessing;
                },

                TokenType::StartComment | TokenType::StartDocComment => {
                    self.parse_comment_tokens(tokens, nodes);
                },

                TokenType::LiteralNull | TokenType::LiteralText | TokenType::LiteralNumber |
                TokenType::ArgumentSeparator | TokenType::Identifier | TokenType::ParserFunctionIdentifier |
                TokenType::Assignment | TokenType::Operator | TokenType::OpeningBracket |
                TokenType::ClosingBracket | TokenType::OpeningBlockBracket | TokenType::ClosingBlockBracket |
                TokenType::Whitespace | TokenType::Other => {
                    tokens.pop_front();

                    nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                },

                TokenType::EscapeSequence => {
                    tokens.pop_front();

                    self.parse_escape_sequence_token(t, nodes);
                },

                TokenType::LexerError => {
                    tokens.pop_front();

                    self.parse_lexer_error_token(t, nodes);
                },

                TokenType::StartMultilineText => {
                    tokens.pop_front();

                    loop {
                        if let Some(t) = tokens.pop_front() {
                            if matches!(t.token_type(), TokenType::EndMultilineText) {
                                break;
                            }

                            if matches!(t.token_type(), TokenType::LiteralText | TokenType::Eol) {
                                nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                            }else if matches!(t.token_type(), TokenType::EscapeSequence) {
                                self.parse_escape_sequence_token(t, nodes);
                            }else if matches!(t.token_type(), TokenType::LexerError) {
                                nodes.push(Node::new_parsing_error_node(
                                    t.pos(),
                                    ParsingError::LexerError,
                                    t.value()
                                ));
                            }else {
                                nodes.push(Node::new_parsing_error_node(
                                    CodePosition::EMPTY,
                                    ParsingError::Eof,
                                    format!(
                                        "Invalid token type ({:?}) in multiline text during simple assignment value parsing",
                                        t.token_type(),
                                    ),
                                ));
                            }
                        }else {
                            nodes.push(Node::new_parsing_error_node(
                                CodePosition::EMPTY,
                                ParsingError::Eof,
                                "Missing multiline text end token during simple assignment value parsing",
                            ));

                            break 'tokenProcessing;
                        }
                    }
                },

                TokenType::LineContinuation | TokenType::EndComment | TokenType::EndMultilineText |
                TokenType::SingleLineTextQuotes => {
                    nodes.push(Node::new_parsing_error_node(
                        CodePosition::EMPTY,
                        ParsingError::LexerError,
                        format!(
                            "Invalid token type for simple assignment value expression: \"{:?}\"",
                            t.token_type(),
                        ),
                    ));

                    break 'tokenProcessing;
                },
            }
        }

        ast
    }

    fn parse_text_and_char_value(&mut self, value_tokens: &mut VecDeque<Token>, nodes: &mut Vec<Node>) {
        if value_tokens.is_empty() {
            return;
        }

        let pos = value_tokens[0].pos().combine(&value_tokens[value_tokens.len() - 1].pos());
        let value = value_tokens.iter().
                map(|token| token.to_raw_string().to_string()).
                collect::<Vec<String>>().
                join("");

        if !value.is_empty() {
            let code_point = value.chars().next().unwrap();

            //CHAR
            if value.chars().count() == 1 {
                nodes.push(Node::new_char_value_node(pos, code_point));

                return;
            }
        }

        //TEXT
        nodes.push(Node::new_text_value_node(pos, value));
    }

    fn parse_escape_sequence_token(&mut self, escape_sequence_token: Token, nodes: &mut Vec<Node>) {
        if matches!(escape_sequence_token.value().len(), 5..=10) {
            if !escape_sequence_token.value().starts_with("\\u{") ||
                    !escape_sequence_token.value().ends_with("}") {
                nodes.push(Node::new_parsing_error_node(
                    escape_sequence_token.pos(),
                    ParsingError::LexerError,
                    format!(
                        "Invalid unicode escape sequence: {}",
                        escape_sequence_token.value(),
                    ),
                ));

                return;
            }

            let hex_codepoint = &escape_sequence_token.value()[3..escape_sequence_token.value().len() - 1];
            for c in hex_codepoint.bytes() {
                if !(c.is_ascii_digit() || matches!(c, b'a'..=b'f') || matches!(c, b'A'..=b'F')) {
                    nodes.push(Node::new_parsing_error_node(
                        escape_sequence_token.pos(),
                        ParsingError::LexerError,
                        format!(
                            "Invalid unicode escape sequence: {}",
                            escape_sequence_token.value(),
                        ),
                    ));

                    return;
                }
            }

            nodes.push(Node::new_unicode_escape_sequence_node(
                escape_sequence_token.pos(),
                hex_codepoint,
            ));

            return;
        }

        if escape_sequence_token.value().chars().count() != 2 || escape_sequence_token.value().as_bytes()[0] != b'\\' {
            nodes.push(Node::new_parsing_error_node(
                escape_sequence_token.pos(),
                ParsingError::LexerError,
                format!(
                    "Invalid escape sequence: {}",
                    escape_sequence_token.value(),
                ),
            ));

            return;
        }

        nodes.push(Node::new_escape_sequence_node(
            escape_sequence_token.pos(),
            escape_sequence_token.value().chars().nth(1).unwrap(),
        ));
    }

    fn parse_number_token(&mut self, number_token: Token, nodes: &mut Vec<Node>) {
        let token = number_token.value();

        //INT
        if let Ok(value) = i32::from_str(token) {
            nodes.push(Node::new_int_value_node(number_token.pos(), value));

            return;
        }

        //LONG
        if token.ends_with("l") || token.ends_with("L") {
            if let Ok(value) = i64::from_str(&token[..token.len() - 1]) {
                nodes.push(Node::new_long_value_node(number_token.pos(), value));

                return;
            }
        }else if let Ok(value) = i64::from_str(token) {
            nodes.push(Node::new_long_value_node(number_token.pos(), value));

            return;
        }

        //FLOAT
        if token.ends_with("f") || token.ends_with("F") {
            if let Ok(value) = f32::from_str(&token[..token.len() - 1]) {
                nodes.push(Node::new_float_value_node(number_token.pos(), value));

                return;
            }
        }

        //DOUBLE
        #[expect(clippy::needless_return)]
        if let Ok(value) = f64::from_str(token) {
            nodes.push(Node::new_double_value_node(number_token.pos(), value));

            return;
        }
    }

    fn parse_lexer_error_token(&mut self, lexer_error_token: Token, nodes: &mut Vec<Node>) {
        if matches!(lexer_error_token.token_type(), TokenType::LexerError) {
            nodes.push(Node::new_parsing_error_node(
                lexer_error_token.pos(),
                ParsingError::LexerError,
                lexer_error_token.value(),
            ));
        }
    }

    fn parse_function_parameter_list(&mut self, tokens: &mut VecDeque<Token>, function_definition: bool) -> AST {
        let mut ast = AST::new();
        let nodes = ast.nodes_mut();

        Self::trim_first_line(tokens);

        if function_definition {
            'tokenProcessing:
            while !tokens.is_empty() {
                let t = tokens[0].clone();

                match t.token_type() {
                    TokenType::Eol | TokenType::Eof => {
                        break 'tokenProcessing;
                    },

                    TokenType::StartComment | TokenType::StartDocComment => {
                        self.parse_comment_tokens(tokens, nodes);
                    },

                    TokenType::ArgumentSeparator => {
                        tokens.pop_front();

                        if nodes.is_empty() {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidParameter,
                                "Empty function parameter"
                            ));
                        }

                        if tokens.is_empty() || matches!(tokens[0].token_type(), TokenType::Eol | TokenType::Eof) {
                            nodes.push(Node::new_parsing_error_node(
                                t.pos(),
                                ParsingError::InvalidParameter,
                                "Empty function parameter"
                            ));
                        }
                    },

                    TokenType::Identifier => {
                        tokens.pop_front();

                        let mut variable_name = t.value().to_string();
                        let mut pos = t.pos();

                        let mut type_constraint = None;
                        if !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::OpeningBracket) &&
                                tokens[0].value() == "{" {
                            let bracket_end_index = utils::get_index_of_matching_bracket_tok(
                                tokens.make_contiguous(),
                                0, usize::MAX,
                                "{", "}", true,
                            );
                            let Some(bracket_end_index) = bracket_end_index else {
                                nodes.push(Node::new_parsing_error_node(
                                    t.pos(),
                                    ParsingError::BracketMismatch,
                                    format!(
                                        "Bracket is missing in return type constraint in function parameter list definition for parameter \"{}\"",
                                        variable_name,
                                    ),
                                ));

                                return ast;
                            };

                            pos = pos.combine(&tokens.get(bracket_end_index).unwrap().pos());

                            let mut type_constraint_tokens = tokens.split_off(bracket_end_index + 1);
                            mem::swap(tokens, &mut type_constraint_tokens);

                            type_constraint = self.parse_type_constraint(
                                &mut type_constraint_tokens,
                                true,
                                nodes,
                            );
                        }

                        if !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::Operator) &&
                                tokens[0].value() == "..." {
                            pos = pos.combine(&tokens[0].pos());

                            //Varargs parameter
                            tokens.pop_front();

                            variable_name += "...";
                        }

                        nodes.push(Node::new_variable_name_node(
                            pos,
                            variable_name,
                            type_constraint.map(Box::from),
                        ));
                    },

                    TokenType::LexerError => {
                        tokens.pop_front();

                        self.parse_lexer_error_token(t, nodes);
                    },

                    TokenType::LiteralNull | TokenType::LiteralNumber | TokenType::LiteralText |
                    TokenType::Assignment | TokenType::ClosingBracket | TokenType::Whitespace |
                    TokenType::Other | TokenType::Operator | TokenType::OpeningBracket |
                    TokenType::OpeningBlockBracket | TokenType::ClosingBlockBracket |
                    TokenType::EscapeSequence | TokenType::ParserFunctionIdentifier |
                    TokenType::StartMultilineText | TokenType::LineContinuation | TokenType::EndComment |
                    TokenType::EndMultilineText | TokenType::SingleLineTextQuotes => {
                        nodes.push(Node::new_parsing_error_node(
                            CodePosition::EMPTY,
                            ParsingError::LexerError,
                            format!(
                                "Invalid token type for function parameter list expression: \"{:?}\"",
                                t.token_type(),
                            ),
                        ));

                        return ast;
                    },
                }
            }
        }else {'tokenProcessing:
            while !tokens.is_empty() {
                let t = tokens[0].clone();

                match t.token_type() {
                    TokenType::Eol | TokenType::Eof => {
                        break 'tokenProcessing;
                    },

                    TokenType::StartComment | TokenType::StartDocComment => {
                        self.parse_comment_tokens(tokens, nodes);
                    },

                    TokenType::ArgumentSeparator => {
                        tokens.pop_front();

                        if nodes.is_empty() ||
                                matches!(nodes[nodes.len() - 1].node_data(), NodeData::ArgumentSeparator(..)) {
                            //Add empty TextObject in between two and before first argument separator
                            nodes.push(Node::new_text_value_node(t.pos(), ""));
                        }

                        nodes.push(Node::new_argument_separator_node(
                            t.pos(),
                            t.value(),
                        ));

                        if tokens.is_empty() || matches!(tokens[0].token_type(), TokenType::Eol | TokenType::Eof) {
                            //Add empty TextObject after last argument separator
                            nodes.push(Node::new_text_value_node(t.pos(), ""));
                        }
                    },

                    TokenType::LiteralNull => {
                        tokens.pop_front();

                        nodes.push(Node::new_null_value_node(t.pos()));
                    },

                    TokenType::LiteralText | TokenType::Assignment | TokenType::ClosingBracket |
                    TokenType::Whitespace => {
                        tokens.pop_front();

                        nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                    },

                    TokenType::Other => {
                        tokens.pop_front();

                        self.parse_text_and_char_value(&mut VecDeque::from([t]), nodes);
                    },

                    TokenType::Operator => {
                        tokens.pop_front();

                        if (nodes.is_empty() || matches!(nodes[nodes.len() - 1].node_data(), NodeData::ArgumentSeparator(..))) &&
                                matches!(t.value(), "+" | "-") && !tokens.is_empty() &&
                                matches!(tokens[0].token_type(), TokenType::LiteralNumber) {
                            let number_token = tokens.pop_front().unwrap();

                            let combined_number_token = Token::new(
                                t.pos().combine(&number_token.pos()),
                                &(t.value().to_string() + number_token.value()),
                                TokenType::LiteralNumber,
                            );

                            self.parse_number_token(combined_number_token, nodes);

                            continue 'tokenProcessing;
                        }

                        nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                    },

                    TokenType::LiteralNumber => {
                        tokens.pop_front();

                        self.parse_number_token(t, nodes);
                    },

                    TokenType::OpeningBracket => {
                        if t.value() == "(" {
                            let end_index = utils::get_index_of_matching_bracket_tok(
                                tokens.make_contiguous(), 0, usize::MAX, "(", ")", true,
                            );
                            if let Some(end_index) = end_index {
                                let opening_bracket_token = &tokens[0];
                                let closing_bracket_token = &tokens[end_index];
                                let pos = opening_bracket_token.pos().combine(&closing_bracket_token.pos());

                                let mut function_call = utils::split_off_arguments(tokens, end_index);

                                nodes.push(Node::new_function_call_previous_node_value_node(
                                    pos, "", "",
                                    self.parse_function_parameter_list(&mut function_call, false).into_nodes(),
                                ));

                                continue 'tokenProcessing;
                            }
                        }

                        tokens.pop_front();

                        nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                    },

                    TokenType::EscapeSequence => {
                        tokens.pop_front();

                        self.parse_escape_sequence_token(t, nodes);
                    },

                    TokenType::LexerError => {
                        tokens.pop_front();

                        self.parse_lexer_error_token(t, nodes);
                    },

                    TokenType::StartMultilineText => {
                        tokens.pop_front();

                        loop {
                            if let Some(t) = tokens.pop_front() {
                                if matches!(t.token_type(), TokenType::EndMultilineText) {
                                    break;
                                }

                                if matches!(t.token_type(), TokenType::LiteralText | TokenType::Eol) {
                                    nodes.push(Node::new_text_value_node(t.pos(), t.value()));
                                }else if matches!(t.token_type(), TokenType::EscapeSequence) {
                                    self.parse_escape_sequence_token(t, nodes);
                                }else if matches!(t.token_type(), TokenType::LexerError) {
                                    nodes.push(Node::new_parsing_error_node(
                                        t.pos(),
                                        ParsingError::LexerError,
                                        t.value()
                                    ));
                                }else {
                                    nodes.push(Node::new_parsing_error_node(
                                        CodePosition::EMPTY,
                                        ParsingError::Eof,
                                        format!(
                                            "Invalid token type ({:?}) in multiline text during simple assignment value parsing",
                                            t.token_type(),
                                        ),
                                    ));
                                }
                            }else {
                                nodes.push(Node::new_parsing_error_node(
                                    CodePosition::EMPTY,
                                    ParsingError::Eof,
                                    "Missing multiline text end token during simple assignment value parsing",
                                ));

                                break 'tokenProcessing;
                            }
                        }
                    },

                    TokenType::Identifier | TokenType::ParserFunctionIdentifier => {
                        let is_identifier = matches!(t.token_type(), TokenType::Identifier);

                        if is_identifier && tokens.len() >= 2 &&
                                matches!(tokens[1].token_type(), TokenType::Operator) &&
                                tokens[1].value() == "..." {
                            //Array unpacking

                            let identifier_token = tokens.pop_front().unwrap();
                            let operator_token = tokens.pop_front().unwrap();

                            let pos = identifier_token.pos().combine(&operator_token.pos());

                            nodes.push(Node::new_unprocessed_variable_name_node(
                                pos,
                                identifier_token.value().to_string() + operator_token.value(),
                            ));
                        }else {
                            let ret = if is_identifier {
                                self.parse_variable_name_and_function_call(tokens, None)
                            }else {
                                self.parse_parser_function_call(tokens)
                            };
                            if let Some(ret) = ret {
                                nodes.push(ret);
                            }
                        }
                    },

                    TokenType::OpeningBlockBracket | TokenType::ClosingBlockBracket |
                    TokenType::LineContinuation | TokenType::EndComment | TokenType::EndMultilineText |
                    TokenType::SingleLineTextQuotes => {
                        nodes.push(Node::new_parsing_error_node(
                            CodePosition::EMPTY,
                            ParsingError::LexerError,
                            format!(
                                "Invalid token type for function argument expression: \"{:?}\"",
                                t.token_type(),
                            ),
                        ));

                        break 'tokenProcessing;
                    },
                }
            }
        }

        ast
    }

    fn parse_function_call_without_prefix(&mut self, tokens: &mut VecDeque<Token>, operator_type: Option<OperatorType>) -> Option<Node> {
        if tokens.len() < 2 {
            return None;
        }

        let identifier_token = tokens.front().unwrap().clone();
        if !matches!(identifier_token.token_type(), TokenType::Other) ||
                !regex_patterns::WORD.is_match(identifier_token.value()) ||
                !matches!(tokens[1].token_type(), TokenType::OpeningBracket) ||
                tokens[1].value() != "(" {
            return None;
        }

        tokens.pop_front();

        self.parse_function_call(identifier_token, tokens, operator_type)
    }

    fn parse_variable_name_and_function_call(&mut self, tokens: &mut VecDeque<Token>, operator_type: Option<OperatorType>) -> Option<Node> {
        if tokens.is_empty() {
            return None;
        }

        let identifier_token = tokens.front().unwrap().clone();
        if !matches!(identifier_token.token_type(), TokenType::Identifier) {
            return None;
        }

        tokens.pop_front();

        if tokens.is_empty() || !matches!(tokens[0].token_type(), TokenType::OpeningBracket) ||
                tokens[0].value() != "(" || !regex_patterns::VAR_NAME_FUNCS_WITH_OPERATOR_AND_CONVERSION_METHOD.is_match(identifier_token.value()) {
            return Some(Node::new_unprocessed_variable_name_node(
                identifier_token.pos(),
                identifier_token.value(),
            ));
        }

        self.parse_function_call(identifier_token, tokens, operator_type)
    }

    fn parse_function_call(&mut self, identifier_token: Token, tokens: &mut VecDeque<Token>, operator_type: Option<OperatorType>) -> Option<Node> {
        let end_index = utils::get_index_of_matching_bracket_tok(
            tokens.make_contiguous(),
            0, usize::MAX,
            "(", ")", true,
        );
        let Some(end_index) = end_index else {
            return Some(Node::new_parsing_error_node(
                identifier_token.pos(),
                ParsingError::BracketMismatch,
                "Bracket is missing in function call",
            ));
        };

        let pos = identifier_token.pos().combine(&tokens[end_index].pos());

        let mut function_parameter_tokens = utils::split_off_arguments(tokens, end_index);

        if let Some(operator_type) = operator_type {
            let raw_function_args = self.parse_operator_expr(&mut function_parameter_tokens, operator_type).unwrap();

            return Some(Node::new_function_call_node(
                pos,
                Self::convert_comma_operators_to_argument_separators(raw_function_args),
                identifier_token.value(),
            ));
        }

        Some(Node::new_function_call_node(
            pos,
            self.parse_function_parameter_list(&mut function_parameter_tokens, false).into_nodes(),
            identifier_token.value(),
        ))
    }

    fn parse_parser_function_call(&mut self, tokens: &mut VecDeque<Token>) -> Option<Node> {
        if tokens.is_empty() {
            return None;
        }

        let parser_function_identifier_token = tokens.front().unwrap().clone();
        if !matches!(parser_function_identifier_token.token_type(), TokenType::ParserFunctionIdentifier) {
            return None;
        }

        tokens.pop_front();

        let end_index = utils::get_index_of_matching_bracket_tok(
            tokens.make_contiguous(),
            0, usize::MAX,
            "(", ")", true,
        );
        let Some(end_index) = end_index else {
            return Some(Node::new_parsing_error_node(
                parser_function_identifier_token.pos(),
                ParsingError::BracketMismatch,
                "Bracket is missing in parser function call",
            ));
        };

        let mut parameter_tokens = utils::split_off_arguments(tokens, end_index);

        match parser_function_identifier_token.value() {
            "parser.con" => Some(self.parse_condition_expr(&mut parameter_tokens).unwrap()),
            "parser.math" => Some(self.parse_math_expr(&mut parameter_tokens).unwrap()),
            "parser.norm" => Some(self.parse_token(&mut parameter_tokens).into_node()),
            "parser.op" => Some(self.parse_operation_expr(&mut parameter_tokens).unwrap()),

            _ => {
                Some(Node::new_parsing_error_node(
                    parser_function_identifier_token.pos(),
                    ParsingError::InvalidParameter,
                    format!(
                        "Invalid parser function: \"{}\"",
                        parser_function_identifier_token.value(),
                    ),
                ))
            },
        }
    }

    fn parse_type_constraint(
        &mut self,
        tokens: &mut VecDeque<Token>,
        allow_special_type_constraints: bool,
        error_nodes: &mut Vec<Node>,
    ) -> Option<String> {
        if tokens.is_empty() {
            return None;
        }

        let mut type_constraint = tokens.iter().
                map(|token| token.to_raw_string().to_string()).
                collect::<Vec<String>>().
                join("");
        let regex = if allow_special_type_constraints {
            &regex_patterns::TYPE_CONSTRAINT_WITH_SPECIAL_TYPES
        }else {
            &regex_patterns::PARSING_TYPE_CONSTRAINT
        };
        if !regex.is_match(&type_constraint) {
            let pos = tokens[0].pos().combine(&tokens[tokens.len() - 1].pos());

            error_nodes.push(Node::new_parsing_error_node(
                pos,
                ParsingError::LexerError,
                "Invalid type constraint syntax"
            ));

            return None;
        }

        //Remove "{" and "}"
        type_constraint.remove(0);
        type_constraint.remove(type_constraint.len() - 1);

        Some(type_constraint)
    }

    fn parse_comment_tokens(&mut self, tokens: &mut VecDeque<Token>, error_nodes: &mut Vec<Node>) {
        if tokens.is_empty() {
            return;
        }

        let mut current_token = tokens[0].clone();
        while matches!(current_token.token_type(), TokenType::StartComment | TokenType::StartDocComment) {
            tokens.pop_front();

            let is_doc_comment = matches!(current_token.token_type(), TokenType::StartDocComment);
            let mut comment = String::new();

            while !matches!(current_token.token_type(), TokenType::EndComment) {
                if tokens.is_empty() {
                    break;
                }

                current_token = tokens.pop_front().unwrap();
                if matches!(current_token.token_type(), TokenType::LexerError) {
                    error_nodes.push(Node::new_parsing_error_node(
                        current_token.pos(),
                        ParsingError::LexerError,
                        current_token.value(),
                    ));
                }

                if is_doc_comment {
                    match current_token.token_type() {
                        TokenType::LiteralText | TokenType::EscapeSequence => {
                            comment += current_token.value();
                        },

                        TokenType::Eol => {
                            comment += "\n";
                        },

                        _ => {},
                    }
                }
            }

            if is_doc_comment {
                if let Some(ref mut lang_doc_comment) = self.lang_doc_comment {
                    *lang_doc_comment += "\n";
                    *lang_doc_comment += comment.as_str();
                }else {
                    self.lang_doc_comment = Some(comment);
                }
            }

            if tokens.is_empty() {
                break;
            }

            current_token = tokens[0].clone();
        }
    }

    fn trim_first_line(tokens: &mut VecDeque<Token>) {
        while !tokens.is_empty() && matches!(tokens[0].token_type(), TokenType::Whitespace) {
            tokens.pop_front();
        }

        let token_count_first_line = Self::get_token_count_first_line(tokens.make_contiguous());

        if token_count_first_line == 0 {
            return;
        }

        let mut i = token_count_first_line - 1;
        while matches!(tokens[i].token_type(), TokenType::Whitespace | TokenType::EndComment) {
            //Trim before comment
            if matches!(tokens[i].token_type(), TokenType::EndComment) {
                while !matches!(tokens[i].token_type(), TokenType::StartComment | TokenType::StartDocComment) {
                    if i == 0 {
                        break;
                    }

                    i -= 1;
                }

                if i == 0 {
                    break;
                }

                i -= 1;

                continue;
            }

            tokens.remove(i);

            if i == 0 {
                break;
            }

            i -= 1;
        }
    }

    fn remove_line_continuation_and_single_line_text_quotes_tokens(tokens: &mut VecDeque<Token>) {
        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];

            if matches!(token.token_type(), TokenType::LineContinuation) {
                tokens.remove(i);

                if tokens.get(i).is_some_and(|token| matches!(token.token_type(), TokenType::Eol)) {
                    tokens.remove(i);
                    i -= 1;
                }

                i -= 1;
            }else if matches!(token.token_type(), TokenType::SingleLineTextQuotes) {
                tokens.remove(i);
                i -= 1;
            }

            i += 1;
        }

        tokens.make_contiguous();
    }

    fn get_token_count_first_line(tokens: &[Token]) -> usize {
        for (i, token) in tokens.iter().enumerate() {
            if matches!(token.token_type(), TokenType::Eol | TokenType::Eof) {
                return i;
            }
        }

        tokens.len()
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}
