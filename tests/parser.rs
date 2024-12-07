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
fn equals_sign_in_function_call_newline() {
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
