use crate::lexer::*;

#[test]
fn empty_token_stream() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn literal_null_token_test() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("null");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 5), "null", TokenType::LiteralNull),
        Token::new(CodePosition::new(1, 1, 5, 6), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn literal_number_int_and_long_tokens_test() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("0");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 2), "0", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 2, 3), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens("42");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 3), "42", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 3, 4), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens("1000000000000000");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 17), "1000000000000000", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 17, 18), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn literal_number_float_tokens_test() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("4.2f");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 5), "4.2f", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 5, 6), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens("2.f");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 4), "2.f", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 4, 5), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens(".5f");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 4), ".5f", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 4, 5), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn literal_number_float_with_exp_tokens_test() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("4.2e+2f");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 8), "4.2e+2f", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 8, 9), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens("2.E-3f");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 7), "2.E-3f", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 7, 8), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens(".5e+200f");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 9), ".5e+200f", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 9, 10), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn literal_number_double_tokens_test() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("4.2");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 4), "4.2", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 4, 5), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens("2.");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 3), "2.", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 3, 4), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens(".5");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 3), ".5", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 3, 4), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn literal_number_double_with_exp_tokens_test() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("4.2e+2");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 7), "4.2e+2", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 7, 8), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens("2.E-3");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 6), "2.E-3", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 6, 7), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);

    lexer.reset_position_vars();

    let tokens = lexer.read_tokens(".5e+200");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 8), ".5e+200", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 8, 9), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn other_token_stream() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("\" -2");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 2), "\"", TokenType::Other),
        Token::new(CodePosition::new(1, 1, 2, 3), " ", TokenType::Whitespace),
        Token::new(CodePosition::new(1, 1, 3, 4), "-", TokenType::Operator),
        Token::new(CodePosition::new(1, 1, 4, 5), "2", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 5, 6), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn other_with_newline_token_stream() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("test\n");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 5), "test", TokenType::Other),
        Token::new(CodePosition::new(1, 1, 5, 6), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn equals_sign_in_function_call_newline_token_stream() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("fn.println($val = 42)");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 11), "fn.println", TokenType::Identifier),
        Token::new(CodePosition::new(1, 1, 11, 12), "(", TokenType::OpeningBracket),
        Token::new(CodePosition::new(1, 1, 12, 16), "$val", TokenType::Identifier),
        Token::new(CodePosition::new(1, 1, 16, 17), " ", TokenType::Whitespace),
        Token::new(CodePosition::new(1, 1, 17, 18), "=", TokenType::Other),
        Token::new(CodePosition::new(1, 1, 18, 19), " ", TokenType::Whitespace),
        Token::new(CodePosition::new(1, 1, 19, 21), "42", TokenType::LiteralNumber),
        Token::new(CodePosition::new(1, 1, 21, 22), ")", TokenType::ClosingBracket),
        Token::new(CodePosition::new(1, 1, 22, 23), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn code_with_empty_lines_token_stream() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("$xyz = \"A value\" # And a comment\n\nfn.println($xyz + \": 42\")\n");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 5), "$xyz", TokenType::Identifier),
        Token::new(CodePosition::new(1, 1, 5, 8), " = ", TokenType::Assignment),
        Token::new(CodePosition::new(1, 1, 8, 9), "\"", TokenType::SingleLineTextQuotes),
        Token::new(CodePosition::new(1, 1, 9, 16), "A value", TokenType::LiteralText),
        Token::new(CodePosition::new(1, 1, 16, 17), "\"", TokenType::SingleLineTextQuotes),
        Token::new(CodePosition::new(1, 1, 17, 18), " ", TokenType::Whitespace),
        Token::new(CodePosition::new(1, 1, 18, 19), "#", TokenType::StartComment),
        Token::new(CodePosition::new(1, 1, 19, 33), " And a comment", TokenType::LiteralText),
        Token::new(CodePosition::new(1, 1, 33, 33), "", TokenType::EndComment),
        Token::new(CodePosition::new(1, 1, 33, 34), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 2), "\n", TokenType::Eol),
        Token::new(CodePosition::new(3, 3, 1, 11), "fn.println", TokenType::Identifier),
        Token::new(CodePosition::new(3, 3, 11, 12), "(", TokenType::OpeningBracket),
        Token::new(CodePosition::new(3, 3, 12, 16), "$xyz", TokenType::Identifier),
        Token::new(CodePosition::new(3, 3, 16, 17), " ", TokenType::Whitespace),
        Token::new(CodePosition::new(3, 3, 17, 18), "+", TokenType::Operator),
        Token::new(CodePosition::new(3, 3, 18, 19), " ", TokenType::Whitespace),
        Token::new(CodePosition::new(3, 3, 19, 20), "\"", TokenType::SingleLineTextQuotes),
        Token::new(CodePosition::new(3, 3, 20, 24), ": 42", TokenType::LiteralText),
        Token::new(CodePosition::new(3, 3, 24, 25), "\"", TokenType::SingleLineTextQuotes),
        Token::new(CodePosition::new(3, 3, 25, 26), ")", TokenType::ClosingBracket),
        Token::new(CodePosition::new(3, 3, 26, 27), "\n", TokenType::Eol),
        Token::new(CodePosition::new(4, 4, 1, 1), "", TokenType::Eof),
    ]);
}

#[test]
fn other_utf_32_token_stream() {
    let mut lexer = Lexer::new();

    let tokens = lexer.read_tokens("$$$-€€€/ÄÖÜ***😀:xyz");

    assert_eq!(tokens, vec![
        Token::new(CodePosition::new(1, 1, 1, 4), "$$$", TokenType::Other),
        Token::new(CodePosition::new(1, 1, 4, 5), "-", TokenType::Operator),
        Token::new(CodePosition::new(1, 1, 5, 8), "€€€", TokenType::Other),
        Token::new(CodePosition::new(1, 1, 8, 9), "/", TokenType::Operator),
        Token::new(CodePosition::new(1, 1, 9, 12), "ÄÖÜ", TokenType::Other),
        Token::new(CodePosition::new(1, 1, 12, 14), "**", TokenType::Operator),
        Token::new(CodePosition::new(1, 1, 14, 15), "*", TokenType::Operator),
        Token::new(CodePosition::new(1, 1, 15, 16), "😀", TokenType::Other),
        Token::new(CodePosition::new(1, 1, 16, 17), ":", TokenType::Operator),
        Token::new(CodePosition::new(1, 1, 17, 20), "xyz", TokenType::Other),
        Token::new(CodePosition::new(1, 1, 20, 21), "\n", TokenType::Eol),
        Token::new(CodePosition::new(2, 2, 1, 1), "", TokenType::Eof),
    ]);
}
