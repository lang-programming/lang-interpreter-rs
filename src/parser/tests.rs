use crate::parser::*;

#[test]
fn parse_number_token_int() {
    assert_eq!(
        helper::parse_number_token("0"),
        Some(Node::new_int_value_node(CodePosition::EMPTY, 0)),
    );

    assert_eq!(
        helper::parse_number_token("42"),
        Some(Node::new_int_value_node(CodePosition::EMPTY, 42)),
    );

    assert_eq!(
        helper::parse_number_token("2147483647"),
        Some(Node::new_int_value_node(CodePosition::EMPTY, 2147483647)),
    );

    assert_eq!(
        helper::parse_number_token("027"),
        Some(Node::new_int_value_node(CodePosition::EMPTY, 27)),
    );
}

#[test]
fn parse_number_token_long() {
    assert_eq!(
        helper::parse_number_token("0L"),
        Some(Node::new_long_value_node(CodePosition::EMPTY, 0)),
    );

    assert_eq!(
        helper::parse_number_token("1000000000000000"),
        Some(Node::new_long_value_node(CodePosition::EMPTY, 1000000000000000)),
    );

    assert_eq!(
        helper::parse_number_token("42L"),
        Some(Node::new_long_value_node(CodePosition::EMPTY, 42)),
    );

    assert_eq!(
        helper::parse_number_token("1000000000000000l"),
        Some(Node::new_long_value_node(CodePosition::EMPTY, 1000000000000000)),
    );

    assert_eq!(
        helper::parse_number_token("9223372036854775807"),
        Some(Node::new_long_value_node(CodePosition::EMPTY, 9223372036854775807)),
    );
}

#[test]
fn parse_number_token_float() {
    assert_eq!(
        helper::parse_number_token("0f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 0.0)),
    );

    assert_eq!(
        helper::parse_number_token(".0f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 0.0)),
    );

    assert_eq!(
        helper::parse_number_token("0.f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 0.0)),
    );

    assert_eq!(
        helper::parse_number_token("0.0f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 0.0)),
    );

    assert_eq!(
        helper::parse_number_token("1f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 1.0)),
    );

    assert_eq!(
        helper::parse_number_token("1.f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 1.0)),
    );

    assert_eq!(
        helper::parse_number_token("1.F"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 1.0)),
    );

    assert_eq!(
        helper::parse_number_token(".24f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 0.24)),
    );

    assert_eq!(
        helper::parse_number_token("24.f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 24.0)),
    );

    assert_eq!(
        helper::parse_number_token("1.e-12f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 1.0e-12)),
    );

    assert_eq!(
        helper::parse_number_token("1.e+12f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 1.0e12)),
    );

    assert_eq!(
        helper::parse_number_token("1.e12f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 1.0e12)),
    );

    assert_eq!(
        helper::parse_number_token("1.E12f"),
        Some(Node::new_float_value_node(CodePosition::EMPTY, 1.0e12)),
    );
}

#[test]
fn parse_number_token_double() {
    assert_eq!(
        helper::parse_number_token(".0"),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 0.0)),
    );

    assert_eq!(
        helper::parse_number_token("0."),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 0.0)),
    );

    assert_eq!(
        helper::parse_number_token("0.0"),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 0.0)),
    );

    assert_eq!(
        helper::parse_number_token("1."),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 1.0)),
    );

    assert_eq!(
        helper::parse_number_token(".24"),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 0.24)),
    );

    assert_eq!(
        helper::parse_number_token("24."),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 24.0)),
    );

    assert_eq!(
        helper::parse_number_token("1.e-12"),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 1.0e-12)),
    );

    assert_eq!(
        helper::parse_number_token("1.e+12"),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 1.0e12)),
    );

    assert_eq!(
        helper::parse_number_token("1.e12"),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 1.0e12)),
    );

    assert_eq!(
        helper::parse_number_token("1.E12"),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 1.0e12)),
    );

    assert_eq!(
        helper::parse_number_token("9223372036854775808"),
        Some(Node::new_double_value_node(CodePosition::EMPTY, 9223372036854775808.0)),
    );
}

#[test]
fn parse_number_token_invalid() {
    assert_eq!(helper::parse_number_token(""), None);

    assert_eq!(helper::parse_number_token("0b01"), None);
    assert_eq!(helper::parse_number_token("0xA"), None);

    assert_eq!(helper::parse_number_token("1i"), None);

    assert_eq!(helper::parse_number_token("1d"), None);
    assert_eq!(helper::parse_number_token("1.d"), None);
    assert_eq!(helper::parse_number_token("1.0d"), None);
    assert_eq!(helper::parse_number_token(".1d"), None);

    assert_eq!(helper::parse_number_token("."), None);
    assert_eq!(helper::parse_number_token(".f"), None);
    assert_eq!(helper::parse_number_token("1.2.f"), None);
    assert_eq!(helper::parse_number_token("1.2."), None);
    assert_eq!(helper::parse_number_token("1.."), None);
    assert_eq!(helper::parse_number_token(".e-12f"), None);
    assert_eq!(helper::parse_number_token(".e-12"), None);
    assert_eq!(helper::parse_number_token("1.e-1.2"), None);
    assert_eq!(helper::parse_number_token("1.e-1.2f"), None);
    
    assert_eq!(helper::parse_number_token("xyz"), None);
}

mod helper {
    use crate::parser::*;

    pub fn parse_number_token(number_value: impl Into<String>) -> Option<Node> {
        let mut parser = Parser::new();

        let mut values = Vec::new();

        parser.parse_number_token(create_number_token(number_value), &mut values);

        assert!(values.len() <= 1);

        values.into_iter().next()
    }

    pub fn create_number_token(value: impl Into<String>) -> Token {
        Token::new(CodePosition::EMPTY, value, TokenType::LiteralNumber)
    }
}
