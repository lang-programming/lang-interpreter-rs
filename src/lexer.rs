pub mod code_position;
pub mod token;

#[cfg(test)]
mod tests;

pub use code_position::CodePosition;
pub use token::{TokenType, Token};

use crate::regex_patterns;
use crate::utils;

use std::collections::VecDeque;
use std::str::FromStr;

#[derive(Debug)]
pub struct Lexer {
    line_number: usize,
    column: usize,

    opening_bracket_count: usize,
    opening_block_count: usize,

    lines_is_empty: bool,
    is_first_code_token_in_line: bool,
}

impl Lexer {
    const OPERATORS: &'static [&'static str] = &[
        /* Array unpacking */ "...",
        "!==", "!=~", "!=", "===", "=~", "==", "<=>", "<=", ">=", "|||", "&&", "||", "!", "&", "~~", "~/", "~",
        "\u{25b2}", "\u{25bc}", "**", "*", "//", "^/", "/", "%", "^", "|", "<<", ">>>", ">>", "+|", "->", "-|", "+", "-", "@", "?::",
        "?:", "<", ">", "??", /* Optional get item */ "?.", "::",
        /* Inline-if (1st part) */ "?",
        /* Inline-if (2nd part) */ ":",
    ];

    pub fn new() -> Self {
        Self {
            line_number: 1,
            column: 1,

            opening_bracket_count: 0,
            opening_block_count: 0,

            lines_is_empty: false,
            is_first_code_token_in_line: true,
        }
    }

    pub fn reset_position_vars(&mut self) {
        self.line_number = 1;
        self.column = 1;

        self.opening_bracket_count = 0;
        self.opening_block_count = 0;

        self.lines_is_empty = false;
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn set_line_number(&mut self, line_number: usize) {
        self.line_number = line_number;
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn set_column(&mut self, column: usize) {
        self.column = column;
    }

    pub fn opening_bracket_count(&self) -> usize {
        self.opening_bracket_count
    }

    pub fn opening_block_count(&self) -> usize {
        self.opening_block_count
    }

    pub fn read_tokens(&mut self, lines: impl Into<String>) -> Vec<Token> {
        self.lines_is_empty = false;

        let lines = lines.into();
        let mut lines = lines.lines().collect::<VecDeque<_>>();

        let mut tokens = Vec::new();

        if !lines.is_empty() {
            let mut current_line = lines.pop_front().map(|line| line.to_string());
            while current_line.is_some() {
                current_line = self.tokenize_next_tokens(&current_line.unwrap(), &mut lines, &mut tokens);
            }
        }

        tokens.push(Token::new(self.get_code_position(self.column), "", TokenType::Eof));

        tokens
    }

    fn tokenize_next_tokens(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        let was_lines_empty = lines.is_empty();
        let ret = self.try_tokenize_new_line(current_line, lines, tokens);
        if ret.is_some() || self.lines_is_empty {
            if self.lines_is_empty || (ret.as_ref().unwrap().is_empty() && was_lines_empty) {
                return None;
            }

            self.is_first_code_token_in_line = true;
            return ret;
        }

        let ret = self.try_tokenize_tokens(current_line, lines, tokens);
        if ret.is_some() {
            return ret;
        }

        //Tokenize as OTHER if not matched with anything else

        let token_index = tokens.len();
        let from_column = self.column as isize;
        let from_line_number = self.line_number as isize;

        let mut i = 0;
        while i < current_line.chars().count() {
            //Skip parsing of "+" and "-" if floating point number contains an "e" or an "E"
            if !regex_patterns::PARSING_FLOATING_POINT_E_SYNTAX_START.is_match(&current_line.chars().take(i).collect::<String>()) ||
                    !['+', '-'].contains(&current_line.chars().nth(i).unwrap()) {
                let ret = self.try_tokenize_tokens(&current_line.chars().skip(i).collect::<String>(), lines, tokens);
                if ret.is_some() {
                    let token = current_line.chars().take(i).collect::<String>();
                    tokens.insert(token_index, self.tokenize_other_value(
                        &token,
                        CodePosition::new(from_line_number, from_line_number, from_column, from_column + i as isize),
                    ));

                    return ret;
                }
            }

            self.column += 1;
            i += 1;
        }

        tokens.insert(token_index, self.tokenize_other_value(
            current_line,
            CodePosition::new(from_line_number, from_line_number, from_column, self.column as isize),
        ));

        Some(String::new())
    }

    fn try_tokenize_tokens(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        let ret = self.try_tokenize_multiline_text(current_line, lines, tokens);
        if ret.is_some() {
            self.is_first_code_token_in_line = false;

            return ret;
        }

        let ret = self.try_tokenize_line_continuation(current_line, lines, tokens);
        if ret.is_some() {
            return ret;
        }

        let ret = self.try_tokenize_escape_sequence(current_line, lines, tokens);
        if ret.is_some() {
            self.is_first_code_token_in_line = false;

            return ret;
        }

        let ret = self.try_tokenize_single_line_text(current_line, lines, tokens);
        if ret.is_some() {
            self.is_first_code_token_in_line = false;

            return ret;
        }

        let ret = self.try_tokenize_assignment(current_line, lines, tokens);
        if ret.is_some() {
            self.is_first_code_token_in_line = false;

            return ret;
        }

        let ret = self.try_tokenize_argument_separator(current_line, lines, tokens);
        if ret.is_some() {
            self.is_first_code_token_in_line = false;

            return ret;
        }

        let ret = self.try_tokenize_whitespace(current_line, lines, tokens);
        if ret.is_some() {
            return ret;
        }

        let ret = self.try_tokenize_comment(current_line, lines, tokens);
        if ret.is_some() {
            return ret;
        }

        let ret = self.try_tokenize_parser_function_identifier(current_line, lines, tokens);
        if ret.is_some() {
            self.is_first_code_token_in_line = false;

            return ret;
        }

        let ret = self.try_tokenize_identifier(current_line, lines, tokens);
        if ret.is_some() {
            self.is_first_code_token_in_line = false;

            return ret;
        }

        let ret = self.try_tokenize_bracket(current_line, lines, tokens);
        if ret.is_some() {
            self.is_first_code_token_in_line = false;

            return ret;
        }

        let ret = self.try_tokenize_operator(current_line, lines, tokens);
        if ret.is_some() {
            self.is_first_code_token_in_line = false;

            return ret;
        }

        self.is_first_code_token_in_line = false;
        None
    }

    fn try_tokenize_new_line(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        if current_line.is_empty() && !self.lines_is_empty {
            let from_column = self.column;
            self.column += 1;
            tokens.push(Token::new(self.get_code_position(from_column), "\n", TokenType::Eol));

            self.line_number += 1;
            self.column = 1;

            self.opening_bracket_count = 0;

            if lines.is_empty() {
                self.lines_is_empty = true;

                return Some(String::new());
            }else {
                return Some(lines.pop_front().unwrap().to_string())
            }
        }

        None
    }

    fn try_tokenize_whitespace(
        &mut self,
        current_line: &str,
        _lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        let mut i = 0;

        let mut chars = current_line.chars();
        while i < current_line.chars().count() {
            let c = chars.next().unwrap();

            if c != ' ' && c != '\t' {
                break;
            }

            i += 1;
        }

        if i > 0 {
            let from_column = self.column;
            self.column += i;

            let token = current_line.chars().take(i).collect::<String>();
            tokens.push(Token::new(self.get_code_position(from_column), &token, TokenType::Whitespace));

            return Some(current_line.chars().skip(i).collect::<String>());
        }

        None
    }

    fn try_tokenize_multiline_text(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        if current_line.starts_with("\"\"\"") {
            self.try_tokenize_multiline_text_with_escape_sequence_support(current_line, lines, tokens)
        }else {
            self.try_tokenize_multiline_text_without_escape_sequence_support(current_line, lines, tokens)
        }
    }

    fn try_tokenize_multiline_text_with_escape_sequence_support(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        if !current_line.starts_with("\"\"\"") {
            return None;
        }

        let mut from_column = self.column;
        self.column += 3;

        tokens.push(Token::new(self.get_code_position(from_column), "\"\"\"", TokenType::StartMultilineText));

        let mut current_line = current_line[3..].to_string();

        loop {
            let end_byte_index = current_line.find("\"\"\"");
            if end_byte_index.is_some_and(|index| index == 0) {
                break;
            }

            let byte_index = current_line.find("\\");
            if let Some(byte_index) = byte_index
                && (end_byte_index.is_some_and(|end_byte_index| byte_index < end_byte_index) ||
                        (end_byte_index.is_none() && byte_index < current_line.len() - 1)) {
                    let token = &current_line[..byte_index];

                    from_column = self.column;
                    self.column += token.chars().count();

                    tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::LiteralText));

                    current_line = current_line[byte_index..].to_string();

                    let ret = self.try_tokenize_escape_sequence(&current_line, lines, tokens);
                    if let Some(ret) = ret {
                        current_line = ret;
                    }

                    continue;
                }

            let was_lines_empty = lines.is_empty();
            let ret = self.try_tokenize_new_line(&current_line, lines, tokens);
            if let Some(ret) = ret {
                if was_lines_empty {
                    tokens.push(Token::new(self.get_code_position(self.column), "", TokenType::EndMultilineText));
                    tokens.push(Token::new(
                        self.get_code_position(self.column),
                        "Multiline text closing bracket '\"\"\"' is missing!",
                        TokenType::LexerError,
                    ));

                    return Some(String::new());
                }

                current_line = ret;
            }else {
                let byte_len = end_byte_index.unwrap_or(current_line.len());

                let token = &current_line[..byte_len];

                from_column = self.column;
                self.column += token.chars().count();

                tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::LiteralText));

                current_line = current_line[byte_len..].to_string();
            }
        }

        //Add empty LITERAL_TEXT token
        tokens.push(Token::new(self.get_code_position(self.column), "", TokenType::LiteralText));

        from_column = self.column;
        self.column += 3;

        tokens.push(Token::new(self.get_code_position(from_column), "\"\"\"", TokenType::EndMultilineText));

        Some(current_line[3..].to_string())
    }

    fn try_tokenize_multiline_text_without_escape_sequence_support(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        if !current_line.starts_with("{{{") {
            return None;
        }

        let mut from_column = self.column;
        self.column += 3;

        tokens.push(Token::new(self.get_code_position(from_column), "{{{", TokenType::StartMultilineText));

        let mut current_line = current_line[3..].to_string();

        while !current_line.contains("}}}") {
            let was_lines_empty = lines.is_empty();
            let ret = self.try_tokenize_new_line(&current_line, lines, tokens);
            if let Some(ret) = ret {
                if was_lines_empty {
                    tokens.push(Token::new(self.get_code_position(self.column), "", TokenType::EndMultilineText));
                    tokens.push(Token::new(
                        self.get_code_position(self.column),
                        "Multiline text closing bracket \"}}}\" is missing!",
                        TokenType::LexerError,
                    ));

                    return Some(String::new());
                }

                current_line = ret;
            }else {
                from_column = self.column;
                self.column += current_line.chars().count();

                tokens.push(Token::new(self.get_code_position(from_column), &current_line, TokenType::LiteralText));

                current_line = String::new();
            }
        }

        let byte_index = current_line.find("}}}").unwrap();

        let token = &current_line[..byte_index];

        //Add LITERAL_TEXT node even if text is empty
        from_column = self.column;
        self.column += token.chars().count();

        tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::LiteralText));

        current_line = current_line[byte_index..].to_string();

        from_column = self.column;
        self.column += 3;

        tokens.push(Token::new(self.get_code_position(from_column), "}}}", TokenType::EndMultilineText));

        Some(current_line[3..].to_string())
    }

    fn try_tokenize_single_line_text(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        if !current_line.starts_with("\"") {
            return None;
        }

        let mut byte_end_index = 1;
        if byte_end_index == current_line.len() {
            return None;
        }

        while byte_end_index < current_line.len() {
            //"?": Return None if no matching "\"" found
            byte_end_index = current_line[byte_end_index..].find("\"")? + byte_end_index;

            if current_line.as_bytes()[byte_end_index - 1] != b'\\' ||
                    utils::is_backslash_at_index_escaped(current_line, byte_end_index - 1) {
                break;
            }

            byte_end_index += 1;
        }

        byte_end_index -= 1;

        let from_column = self.column;
        self.column += 1;

        tokens.push(Token::new(self.get_code_position(from_column), "\"", TokenType::SingleLineTextQuotes));

        let mut current_line = current_line[1..].to_string();

        if byte_end_index == 0 {
            tokens.push(Token::new(self.get_code_position(self.column), "", TokenType::LiteralText));
        }

        while byte_end_index > 0 {
            let byte_index = current_line.find("\\");

            if let Some(byte_index) = byte_index
                && byte_index < byte_end_index {
                    let token = &current_line[..byte_index];
                    let index = token.chars().count();

                    let from_column = self.column;
                    self.column += index;
                    byte_end_index -= byte_index;

                    tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::LiteralText));

                    current_line = current_line[byte_index..].to_string();

                    let ret = self.try_tokenize_escape_sequence(&current_line, lines, tokens);
                    if let Some(ret) = ret {
                        byte_end_index -= current_line.len() - ret.len();
                        current_line = ret;
                    }

                    continue;
                }

            let token = &current_line[..byte_end_index];
            let end_index = token.chars().count();

            let from_column = self.column;
            self.column += end_index;

            tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::LiteralText));

            current_line = current_line[byte_end_index..].to_string();

            break;
        }

        let from_column = self.column;
        self.column += 1;

        tokens.push(Token::new(self.get_code_position(from_column), "\"", TokenType::SingleLineTextQuotes));

        Some(current_line[1..].to_string())
    }

    fn try_tokenize_line_continuation(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        if current_line.len() == 1 && current_line.as_bytes()[0] == b'\\' {
            let original_opening_bracket_count = self.opening_bracket_count;

            let from_column = self.column;
            self.column += 1;

            tokens.push(Token::new(self.get_code_position(from_column), "\\", TokenType::LineContinuation));

            let ret = self.try_tokenize_new_line("", lines, tokens);

            self.opening_bracket_count = original_opening_bracket_count;
            return ret.or(Some(String::new()));
        }

        None
    }

    fn try_tokenize_comment(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        let c = current_line.as_bytes()[0];
        if c != b'#' {
            return None;
        }

        let mut current_line = current_line.to_string();
        if current_line.len() > 1 && current_line.as_bytes()[1] == b'#' {
            let from_column = self.column;
            self.column += 2;

            tokens.push(Token::new(self.get_code_position(from_column), "##", TokenType::StartDocComment));

            current_line = current_line[2..].to_string();
        }else {
            let from_column = self.column;
            self.column += 1;

            tokens.push(Token::new(self.get_code_position(from_column), "#", TokenType::StartComment));

            current_line = current_line[1..].to_string();
        }

        while !current_line.is_empty() {
            let multiline_text_byte_start_index = match (current_line.find("{{{"), current_line.find("\"\"\"")) {
                (Some(byte_index_1), Some(byte_index_2)) => Some(byte_index_1.min(byte_index_2)),
                (byte_index_1, None) => byte_index_1,
                (None, byte_index_2) => byte_index_2,
            };

            if let Some(multiline_text_byte_start_index) = multiline_text_byte_start_index {
                if multiline_text_byte_start_index > 0 {
                    let token = &current_line[..multiline_text_byte_start_index];
                    let multiline_text_start_index = token.chars().count();

                    let from_column = self.column;
                    self.column += multiline_text_start_index;

                    tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::LiteralText));

                    current_line = current_line[multiline_text_byte_start_index..].to_string();
                }

                let ret = self.try_tokenize_multiline_text(&current_line, lines, tokens);
                if let Some(ret) = ret {
                    current_line = ret;
                }

                continue;
            }

            if current_line.ends_with("\\") {
                if current_line.len() > 1 {
                    let token_len = current_line.len() - 1;

                    let token = &current_line[..token_len];
                    let multiline_text_start_index = token.chars().count();

                    let from_column = self.column;
                    self.column += multiline_text_start_index;

                    tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::LiteralText));

                    current_line = current_line[token_len..].to_string();
                }

                let ret = self.try_tokenize_line_continuation(&current_line, lines, tokens);
                if let Some(ret) = ret {
                    current_line = ret;
                }

                continue;
            }

            let from_column = self.column;
            self.column += current_line.chars().count();

            tokens.push(Token::new(self.get_code_position(from_column), &current_line, TokenType::LiteralText));

            current_line = String::new();
        }

        tokens.push(Token::new(self.get_code_position(self.column), "", TokenType::EndComment));

        Some(current_line)
    }

    fn try_tokenize_bracket(
        &mut self,
        current_line: &str,
        lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        let c = current_line.as_bytes()[0];

        if c == b'{' || c == b'(' || c == b'[' {
            let mut is_last_code_token_in_line = c == b'{';

            let mut line_index = 0;
            let mut i = 1;
            let mut line = if is_last_code_token_in_line { current_line } else { "" };
            while i < line.len() {
                let c = line.as_bytes()[i];

                if c == b'\\' && i == line.len() - 1 && line_index < lines.len() {
                    line = lines[line_index];
                    line_index += 1;
                    i = 0;

                    continue;
                }

                if c == b'#' {
                    break;
                }

                if c != b' ' && c != b'\t' {
                    is_last_code_token_in_line = false;

                    break;
                }

                i += 1;
            }

            if c == b'{' && is_last_code_token_in_line {
                self.opening_block_count += 1;
            }else {
                self.opening_bracket_count += 1;
            }

            let from_column = self.column;
            self.column += 1;

            let token = &current_line[..1];

            tokens.push(Token::new(self.get_code_position(from_column), token, if is_last_code_token_in_line {
                TokenType::OpeningBlockBracket
            }else {
                TokenType::OpeningBracket
            }));

            return Some(current_line[1..].to_string());
        }else if c == b'}' || c == b')' || c == b']' {
            self.is_first_code_token_in_line &= c == b'}';

            if self.is_first_code_token_in_line {
                self.opening_block_count = self.opening_block_count.saturating_sub(1);
            }else {
                self.opening_bracket_count = self.opening_bracket_count.saturating_sub(1);
            }

            let from_column = self.column;
            self.column += 1;

            let token = &current_line[..1];

            tokens.push(Token::new(self.get_code_position(from_column), token, if self.is_first_code_token_in_line {
                TokenType::ClosingBlockBracket
            }else {
                TokenType::ClosingBracket
            }));

            return Some(current_line[1..].to_string());
        }

        None
    }

    fn try_tokenize_escape_sequence(
        &mut self,
        current_line: &str,
        _lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        if current_line.len() >= 2 && current_line.as_bytes()[0] == b'\\' {
            'unicodeEscapeSequence: {
                if current_line.as_bytes()[1] == b'u' && current_line.len() >= 5 &&
                        current_line.as_bytes()[2] == b'{' && current_line.as_bytes()[3] != b'}' {
                    let mut i = 3;
                    while i < 10 && i < current_line.len() {
                        let c = current_line.as_bytes()[i];
                        if c == b'}' {
                            break;
                        }

                        if !(c.is_ascii_digit() || matches!(c, b'a'..=b'f') || matches!(c, b'A'..=b'F')) {
                            break 'unicodeEscapeSequence;
                        }

                        i += 1;
                    }

                    if current_line.as_bytes()[i] != b'}' {
                        break 'unicodeEscapeSequence;
                    }

                    i += 1;

                    let token = &current_line[..i];

                    let from_column = self.column;
                    self.column += i;

                    tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::EscapeSequence));

                    return Some(current_line[i..].to_string())
                }
            }

            let token = &current_line[..2];

            let from_column = self.column;
            self.column += 2;

            tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::EscapeSequence));

            return Some(current_line[2..].to_string())
        }

        None
    }

    fn try_tokenize_parser_function_identifier(
        &mut self,
        current_line: &str,
        _lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        let match_result = regex_patterns::STARTS_WITH_FUNCTION_IDENTIFIER.find(current_line);
        if let Some(match_result) = match_result
            && match_result.start() == 0 {
                let token = match_result.as_str();

                let from_column = self.column;
                self.column += token.chars().count();

                tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::ParserFunctionIdentifier));

                return Some(current_line[token.len()..].to_string())
            }

        None
    }

    fn try_tokenize_identifier(
        &mut self,
        current_line: &str,
        _lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        let match_result = regex_patterns::STARTS_WITH_VAR_NAME_FULL_WITH_FUNCS_AND_PTR_AND_DEREFERENCE_WITH_OPERATOR_AND_CONVERSION_METHODS.find(current_line);
        if let Some(match_result) = match_result
            && match_result.start() == 0 {
                let mut token = match_result.as_str();

                //Check if var pointer brackets are closed correctly
                if regex_patterns::PARSING_VAR_NAME_PTR_AND_DEREFERENCE.is_match(token) {
                    let end_byte_index = utils::get_index_of_matching_bracket_str(
                        token,
                        token.find("$").map(|index| index + 1).unwrap_or(0),
                        usize::MAX,
                        b'[', b']',
                    );

                    if end_byte_index.is_none() {
                        tokens.push(Token::new(self.get_code_position(self.column), &format!(
                            "Bracket is missing in variable pointer: \"{token}\"",
                        ), TokenType::LexerError));

                        return Some(String::new());
                    }

                    //Limit token to end with closing "]"
                    token = &token[..=end_byte_index.unwrap()];
                }

                let from_column = self.column;
                self.column += token.chars().count();

                tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::Identifier));

                return Some(current_line[token.len()..].to_string())
            }

        None
    }

    fn try_tokenize_operator(
        &mut self,
        current_line: &str,
        _lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        for operator in Self::OPERATORS {
            if let Some(current_line) = current_line.strip_prefix(operator) {
                let from_column = self.column;
                self.column += operator.chars().count();

                tokens.push(Token::new(self.get_code_position(from_column), operator, TokenType::Operator));

                return Some(current_line.to_string())
            }
        }

        None
    }

    fn try_tokenize_argument_separator(
        &mut self,
        current_line: &str,
        _lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        let match_result = regex_patterns::STARTS_WITH_ARGUMENT_SEPARATOR.find(current_line);
        if let Some(match_result) = match_result
            && match_result.start() == 0 {
                let token = match_result.as_str();

                let from_column = self.column;
                self.column += token.chars().count();

                tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::ArgumentSeparator));

                return Some(current_line[token.len()..].to_string())
            }

        None
    }

    fn try_tokenize_assignment(
        &mut self,
        current_line: &str,
        _lines: &mut VecDeque<&str>,
        tokens: &mut Vec<Token>,
    ) -> Option<String> {
        if self.opening_bracket_count > 0 {
            return None;
        }

        let match_result = regex_patterns::STARTS_WITH_ASSIGNMENT_OPERATOR.find(current_line);
        if let Some(match_result) = match_result
            && match_result.start() == 0 {
                let token = match_result.as_str();

                let from_column = self.column;
                self.column += token.chars().count();

                tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::Assignment));

                return Some(current_line[token.len()..].to_string())
            }

        if let Some(end) = current_line.strip_prefix(" = ") {
            let token = &current_line[..3];

            let from_column = self.column;
            self.column += 3;

            tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::Assignment));

            return Some(end.to_string())
        }else if current_line == " =" {
            let token = &current_line[..2];

            let from_column = self.column;
            self.column += 2;

            tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::Assignment));

            return Some(current_line[2..].to_string())
        }else if let Some(end) = current_line.strip_prefix("=") {
            let token = &current_line[..1];

            let from_column = self.column;
            self.column += 1;

            tokens.push(Token::new(self.get_code_position(from_column), token, TokenType::Assignment));

            return Some(end.to_string())
        }

        None
    }

    pub(crate) fn tokenize_other_value(&mut self, token: &str, pos: CodePosition) -> Token {
        let token_type = match token {
            _ if Self::is_null_value(token) => TokenType::LiteralNull,
            _ if Self::is_numeric_value(token) => TokenType::LiteralNumber,
            _ => TokenType::Other,
        };

        Token::new(pos, token, token_type)
    }

    fn is_null_value(token: &str) -> bool {
        token == "null"
    }

    fn is_numeric_value(token: &str) -> bool {
        if token.is_empty() {
            return false;
        }

        let c = token.as_bytes()[0];
        if !c.is_ascii_digit() && c != b'.' {
            return false;
        }

        //INT
        if i32::from_str(token).is_ok() {
            return true;
        }

        //LONG
        if token.ends_with("l") || token.ends_with("L") {
            if i64::from_str(&token[..token.len() - 1]).is_ok() {
                return true;
            }
        }else if i64::from_str(token).is_ok() {
            return true;
        }

        //FLOAT
        if token.ends_with("f") || token.ends_with("F") {
            //Do not allow: NaN, Infinity, xX
            if token.contains("N") || token.contains("n") || token.contains("I") ||
                    token.contains("i") || token.contains("x") || token.contains("X") {
                return false;
            }

            if f32::from_str(&token[..token.len() - 1]).is_ok() {
                return true;
            }
        }

        //DOUBLE
        //Do not allow: NaN, Infinity, xX, dD
        if token.ends_with("d") || token.ends_with("D") || token.contains("N") || token.contains("n") ||
                token.contains("I") || token.contains("i") || token.contains("x") || token.contains("X") {
            return false;
        }

        f64::from_str(token).is_ok()
    }

    fn get_code_position(&self, column_from: usize) -> CodePosition {
        CodePosition::new(self.line_number as isize, self.line_number as isize, column_from as isize, self.column as isize)
    }
}

impl Default for Lexer {
    fn default() -> Self {
        Self::new()
    }
}
