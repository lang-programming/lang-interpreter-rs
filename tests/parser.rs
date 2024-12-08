use lang_interpreter::lexer::CodePosition;
use lang_interpreter::parser::ast::{Node, AST};
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
                Node::new_text_value_node(CodePosition::EMPTY, " "),
                Node::new_char_value_node(CodePosition::EMPTY, '='),
                Node::new_text_value_node(CodePosition::EMPTY, " "),
                Node::new_int_value_node(CodePosition::EMPTY, 42),
            ],
            "fn.println",
        ),
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
$s = {{{func.printf($h\n\n\$h \= %s{{%s}}%s\n\$s \= %s{{%s}}%s\n%s, {, $h, }, {, $s, }, $s)}}}
func.printf($h\n\n\$h \= %s{{%s}}%s\n\$s \= %s{{%s}}%s\n%s, {, $h, }, {, $s, }, $s)"
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
            Node::new_list_node(vec![
                Node::new_text_value_node(CodePosition::EMPTY, "# Execute in command line without any arguments except this Lang file"),
                Node::new_text_value_node(CodePosition::EMPTY, "\n"),
                Node::new_text_value_node(CodePosition::EMPTY, "lang.name = Quine"),
                Node::new_text_value_node(CodePosition::EMPTY, "\n"),
                Node::new_text_value_node(CodePosition::EMPTY, "lang.version = v1.0.0"),
            ]),
        ),
        Node::new_assignment_node(
            Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$s"),
            Node::new_text_value_node(CodePosition::EMPTY, r"func.printf($h\n\n\$h \= %s{{%s}}%s\n\$s \= %s{{%s}}%s\n%s, {, $h, }, {, $s, }, $s)"),
        ),
        Node::new_function_call_node(
            CodePosition::EMPTY,
            vec![
                Node::new_unprocessed_variable_name_node(CodePosition::EMPTY, "$h"),
                Node::new_escape_sequence_node(CodePosition::EMPTY, 'n'),
                Node::new_escape_sequence_node(CodePosition::EMPTY, 'n'),
                Node::new_escape_sequence_node(CodePosition::EMPTY, '$'),
                Node::new_char_value_node(CodePosition::EMPTY, 'h'),
                Node::new_text_value_node(CodePosition::EMPTY, " "),
                Node::new_escape_sequence_node(CodePosition::EMPTY, '='),
                Node::new_text_value_node(CodePosition::EMPTY, " "),
                Node::new_text_value_node(CodePosition::EMPTY, "%"),
                Node::new_char_value_node(CodePosition::EMPTY, 's'),
                Node::new_text_value_node(CodePosition::EMPTY, "{"),
                Node::new_text_value_node(CodePosition::EMPTY, "{"),
                Node::new_text_value_node(CodePosition::EMPTY, "%"),
                Node::new_char_value_node(CodePosition::EMPTY, 's'),
                Node::new_text_value_node(CodePosition::EMPTY, "}"),
                Node::new_text_value_node(CodePosition::EMPTY, "}"),
                Node::new_text_value_node(CodePosition::EMPTY, "%"),
                Node::new_char_value_node(CodePosition::EMPTY, 's'),
                Node::new_escape_sequence_node(CodePosition::EMPTY, 'n'),
                Node::new_escape_sequence_node(CodePosition::EMPTY, '$'),
                Node::new_char_value_node(CodePosition::EMPTY, 's'),
                Node::new_text_value_node(CodePosition::EMPTY, " "),
                Node::new_escape_sequence_node(CodePosition::EMPTY, '='),
                Node::new_text_value_node(CodePosition::EMPTY, " "),
                Node::new_text_value_node(CodePosition::EMPTY, "%"),
                Node::new_char_value_node(CodePosition::EMPTY, 's'),
                Node::new_text_value_node(CodePosition::EMPTY, "{"),
                Node::new_text_value_node(CodePosition::EMPTY, "{"),
                Node::new_text_value_node(CodePosition::EMPTY, "%"),
                Node::new_char_value_node(CodePosition::EMPTY, 's'),
                Node::new_text_value_node(CodePosition::EMPTY, "}"),
                Node::new_text_value_node(CodePosition::EMPTY, "}"),
                Node::new_text_value_node(CodePosition::EMPTY, "%"),
                Node::new_char_value_node(CodePosition::EMPTY, 's'),
                Node::new_escape_sequence_node(CodePosition::EMPTY, 'n'),
                Node::new_text_value_node(CodePosition::EMPTY, "%"),
                Node::new_char_value_node(CodePosition::EMPTY, 's'),
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
