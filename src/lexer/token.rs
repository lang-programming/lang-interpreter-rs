use crate::lexer::CodePosition;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TokenType {
    Other,

    LiteralNull,
    LiteralText,
    LiteralNumber,

    ArgumentSeparator,

    EscapeSequence,

    ParserFunctionIdentifier,

    Identifier,

    Operator,

    Assignment,

    OpeningBracket,
    ClosingBracket,

    OpeningBlockBracket,
    ClosingBlockBracket,

    SingleLineTextQuotes,

    StartMultilineText,
    EndMultilineText,

    StartComment,
    StartDocComment,
    EndComment,

    LineContinuation,

    Whitespace,
    Eol,
    Eof,

    LexerError,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TokenType::Other => "OTHER",

            TokenType::LiteralNull => "LITERAL_NULL",
            TokenType::LiteralText => "LITERAL_TEXT",
            TokenType::LiteralNumber => "LITERAL_NUMBER",

            TokenType::ArgumentSeparator => "ARGUMENT_SEPARATOR",

            TokenType::EscapeSequence => "ESCAPE_SEQUENCE",

            TokenType::ParserFunctionIdentifier => "PARSER_FUNCTION_IDENTIFIER",

            TokenType::Identifier => "IDENTIFIER",

            TokenType::Operator => "OPERATOR",

            TokenType::Assignment => "ASSIGNMENT",

            TokenType::OpeningBracket => "OPENING_BRACKET",
            TokenType::ClosingBracket => "CLOSING_BRACKET",

            TokenType::OpeningBlockBracket => "OPENING_BLOCK_BRACKET",
            TokenType::ClosingBlockBracket => "CLOSING_BLOCK_BRACKET",

            TokenType::SingleLineTextQuotes => "SINGLE_LINE_TEXT_QUOTES",

            TokenType::StartMultilineText => "START_MULTILINE_TEXT",
            TokenType::EndMultilineText => "END_MULTILINE_TEXT",

            TokenType::StartComment => "START_COMMENT",
            TokenType::StartDocComment => "START_DOC_COMMENT",
            TokenType::EndComment => "END_COMMENT",

            TokenType::LineContinuation => "LINE_CONTINUATION",

            TokenType::Whitespace => "WHITESPACE",
            TokenType::Eol => "EOL",
            TokenType::Eof => "EOF",

            TokenType::LexerError => "LEXER_ERROR",
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Token {
    pos: CodePosition,
    value: Box<str>,
    token_type: TokenType,
}

impl Token {
    pub fn new(pos: CodePosition, value: &str, token_type: TokenType) -> Self {
        Self {
            pos,
            value: value.into(),
            token_type,
        }
    }

    pub fn to_raw_string(&self) -> &str {
        match self.token_type {
            TokenType::LexerError => "",
            _ => &self.value,
        }
    }

    pub fn pos(&self) -> CodePosition {
        self.pos
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn token_type(&self) -> TokenType {
        self.token_type
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f, "Token ({:>30} at {}): \"{}\"",
            format!("{}", self.token_type), self.pos, self.value
        )
    }
}
