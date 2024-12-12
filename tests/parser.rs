use lang_interpreter::lexer::CodePosition;
use lang_interpreter::parser::ast::{ConditionalNode, Node, OperationExpression, Operator, OperatorType, AST};
use lang_interpreter::parser::Parser;

#[test]
fn empty_string() {
    let mut parser = Parser::new();

    let ast = parser.parse_lines("");

    assert_eq!(ast, Some(AST::new()));
}

#[test]
fn empty_block() {
    let mut parser = Parser::new();

    let ast = parser.parse_lines("{\n}");

    assert_eq!(ast, Some(AST::new()));
}

#[test]
fn equals_sign_in_function_call() {
    let mut parser = Parser::new();

    let ast = parser.parse_lines("fn.println($val = 42)");

    assert_eq!(ast, Some(AST::from([
        Node::new_function_call_node(
            CodePosition::EMPTY,
            vec![
                Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$val"),
                Node::new_text_value_node(CodePosition::EMPTY, " = "),
                Node::new_int_value_node(CodePosition::EMPTY, 42),
            ],
            "fn.println",
        ),
    ])));
}

#[test]
fn fizz_buzz_example() {
    let mut parser = Parser::new();

    let ast = parser.parse_lines(
        r"lang.name = FizzBuzz
lang.version = v1.0.0

$MAX = 50

$i = 0
until($i > $MAX) {
	$3or5flag = 0

	if(!($i % 3)) {
		func.print(Fizz)
		$3or5flag = 1
	}
	if(!($i % 5)) {
		func.print(Buzz)
		$3or5flag = 1
	}
	if(!$3or5flag) {
		func.print($i)
	}
	func.println()

	$i += 1
}"
    );

    let var_node_i = Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$i");
    let var_node_3_or_5_flag = Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$3or5flag");

    assert_eq!(ast, Some(AST::from([
        Node::new_assignment_node(
            Node::new_text_value_node(CodePosition::EMPTY, "lang.name"),
            Node::new_text_value_node(CodePosition::EMPTY, "FizzBuzz"),
        ),
        Node::new_assignment_node(
            Node::new_text_value_node(CodePosition::EMPTY, "lang.version"),
            Node::new_text_value_node(CodePosition::EMPTY, "v1.0.0"),
        ),
        Node::new_assignment_node(
            Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$MAX"),
            Node::new_int_value_node(CodePosition::EMPTY, 50),
        ),
        Node::new_assignment_node(
            var_node_i.clone(),
            Node::new_int_value_node(CodePosition::EMPTY, 0),
        ),
        Node::new_loop_statement_node(CodePosition::EMPTY, vec![
            Node::new_loop_statement_part_until_node(CodePosition::EMPTY, AST::from([
                Node::new_assignment_node(
                    var_node_3_or_5_flag.clone(),
                    Node::new_int_value_node(CodePosition::EMPTY, 0),
                ),
                Node::new_if_statement_node(CodePosition::EMPTY, vec![
                    Node::new_if_statement_part_if_node(CodePosition::EMPTY, AST::from([
                        Node::new_function_call_node(CodePosition::EMPTY, vec![
                            Node::new_text_value_node(CodePosition::EMPTY, "Fizz"),
                        ], "func.print"),
                        Node::new_assignment_node(
                            var_node_3_or_5_flag.clone(),
                            Node::new_int_value_node(CodePosition::EMPTY, 1),
                        ),
                    ]), ConditionalNode::new(Node::new_operation_statement_node(
                        CodePosition::EMPTY,
                        OperationExpression::new(
                            Some(Box::new(Node::new_operation_statement_node(
                                CodePosition::EMPTY,
                                OperationExpression::new(
                                    Some(Box::new(Node::new_operation_statement_node(
                                        CodePosition::EMPTY,
                                        OperationExpression::new(
                                            Some(Box::new(var_node_i.clone())), None, Some(Box::new(Node::new_int_value_node(
                                                CodePosition::EMPTY, 3,
                                            ))),
                                            Operator::Mod, OperatorType::General,
                                        ),
                                    ))), None, None,
                                    Operator::Not, OperatorType::General,
                                ),
                            ))), None, None,
                            Operator::ConditionalNon, OperatorType::Condition,
                        ),
                    ))),
                ]),
                Node::new_if_statement_node(CodePosition::EMPTY, vec![
                    Node::new_if_statement_part_if_node(CodePosition::EMPTY, AST::from([
                        Node::new_function_call_node(CodePosition::EMPTY, vec![
                            Node::new_text_value_node(CodePosition::EMPTY, "Buzz"),
                        ], "func.print"),
                        Node::new_assignment_node(
                            var_node_3_or_5_flag.clone(),
                            Node::new_int_value_node(CodePosition::EMPTY, 1),
                        ),
                    ]), ConditionalNode::new(Node::new_operation_statement_node(
                        CodePosition::EMPTY,
                        OperationExpression::new(
                            Some(Box::new(Node::new_operation_statement_node(
                                CodePosition::EMPTY,
                                OperationExpression::new(
                                    Some(Box::new(Node::new_operation_statement_node(
                                        CodePosition::EMPTY,
                                        OperationExpression::new(
                                            Some(Box::new(var_node_i.clone())), None, Some(Box::new(Node::new_int_value_node(
                                                CodePosition::EMPTY, 5,
                                            ))),
                                            Operator::Mod, OperatorType::General,
                                        ),
                                    ))), None, None,
                                    Operator::Not, OperatorType::General,
                                ),
                            ))), None, None,
                            Operator::ConditionalNon, OperatorType::Condition,
                        ),
                    ))),
                ]),
                Node::new_if_statement_node(CodePosition::EMPTY, vec![
                    Node::new_if_statement_part_if_node(CodePosition::EMPTY, AST::from([
                        Node::new_function_call_node(CodePosition::EMPTY, vec![
                            var_node_i.clone(),
                        ], "func.print"),
                    ]), ConditionalNode::new(Node::new_operation_statement_node(
                        CodePosition::EMPTY,
                        OperationExpression::new(
                            Some(Box::new(Node::new_operation_statement_node(
                                CodePosition::EMPTY,
                                OperationExpression::new(
                                    Some(Box::new(var_node_3_or_5_flag.clone())), None, None,
                                    Operator::Not, OperatorType::General,
                                ),
                            ))), None, None,
                            Operator::ConditionalNon, OperatorType::Condition,
                        ),
                    ))),
                ]),
                Node::new_function_call_node(CodePosition::EMPTY, vec![], "func.println"),
                Node::new_assignment_node(
                    var_node_i.clone(),
                    Node::new_operation_statement_node(
                        CodePosition::EMPTY,
                        OperationExpression::new(
                            Some(Box::new(var_node_i.clone())), None, Some(Box::new(Node::new_operation_statement_node(
                                CodePosition::EMPTY,
                                OperationExpression::new(
                                    Some(Box::new(Node::new_int_value_node(
                                        CodePosition::EMPTY, 1,
                                    ))), None, None,
                                    Operator::Non, OperatorType::General,
                                ),
                            ))),
                            Operator::Add, OperatorType::Math,
                        ),
                    ),
                ),
            ]), ConditionalNode::new(Node::new_operation_statement_node(
                CodePosition::EMPTY,
                OperationExpression::new(
                    Some(Box::new(Node::new_operation_statement_node(
                        CodePosition::EMPTY,
                        OperationExpression::new(
                            Some(Box::new(var_node_i.clone())), None, Some(Box::new(Node::new_unprocessed_variable_name_node(
                                CodePosition::EMPTY, "$MAX",
                            ))),
                            Operator::GreaterThan, OperatorType::General,
                        ),
                    ))), None, None,
                    Operator::ConditionalNon, OperatorType::Condition,
                ),
            ))),
        ]),
    ])));
}

#[test]
fn quine_example() {
    let mut parser = Parser::new();

    let ast = parser.parse_lines(
        r"# Execute in command line without any arguments except this Lang file
lang.name = Quine
lang.version = v1.0.0

$h = {{{# Execute in command line without any arguments except this Lang file
lang.name = Quine
lang.version = v1.0.0}}}
$s = {{{func.printf($h\n\n\$h \= %s{{%s}}%s\n\$s \= %s{{%s}}%s\n%s\n, {, $h, }, {, $s, }, $s)}}}
func.printf($h\n\n\$h \= %s{{%s}}%s\n\$s \= %s{{%s}}%s\n%s\n, {, $h, }, {, $s, }, $s)
"
    );

    assert_eq!(ast, Some(AST::from([
        Node::new_assignment_node(
            Node::new_text_value_node(CodePosition::EMPTY, "lang.name"),
            Node::new_text_value_node(CodePosition::EMPTY, "Quine"),
        ),
        Node::new_assignment_node(
            Node::new_text_value_node(CodePosition::EMPTY, "lang.version"),
            Node::new_text_value_node(CodePosition::EMPTY, "v1.0.0"),
        ),
        Node::new_assignment_node(
            Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$h"),
            Node::new_text_value_node(
                CodePosition::EMPTY,
                r"# Execute in command line without any arguments except this Lang file
lang.name = Quine
lang.version = v1.0.0"
            ),
        ),
        Node::new_assignment_node(
            Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$s"),
            Node::new_text_value_node(CodePosition::EMPTY, r"func.printf($h\n\n\$h \= %s{{%s}}%s\n\$s \= %s{{%s}}%s\n%s\n, {, $h, }, {, $s, }, $s)"),
        ),
        Node::new_function_call_node(
            CodePosition::EMPTY,
            vec![
                Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$h"),
                Node::new_text_value_node(
                    CodePosition::EMPTY,
                    r"

$h = %s{{%s}}%s
$s = %s{{%s}}%s
%s
"
                ),
                Node::new_argument_separator_node(CodePosition::EMPTY, ", "),
                Node::new_text_value_node(CodePosition::EMPTY, "{"),
                Node::new_argument_separator_node(CodePosition::EMPTY, ", "),
                Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$h"),
                Node::new_argument_separator_node(CodePosition::EMPTY, ", "),
                Node::new_text_value_node(CodePosition::EMPTY, "}"),
                Node::new_argument_separator_node(CodePosition::EMPTY, ", "),
                Node::new_text_value_node(CodePosition::EMPTY, "{"),
                Node::new_argument_separator_node(CodePosition::EMPTY, ", "),
                Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$s"),
                Node::new_argument_separator_node(CodePosition::EMPTY, ", "),
                Node::new_text_value_node(CodePosition::EMPTY, "}"),
                Node::new_argument_separator_node(CodePosition::EMPTY, ", "),
                Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$s"),
            ],
            "func.printf",
        ),
    ])));
}
