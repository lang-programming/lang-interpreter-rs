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
            format!("{:?}", self.token_type), self.pos, self.value
        )
    }
}
