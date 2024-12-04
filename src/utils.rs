use std::collections::VecDeque;
use std::mem;
use crate::lexer::{Token, TokenType};

pub(crate) fn get_index_of_matching_bracket_str(
    string: &str,
    start_byte_index: usize,
    end_byte_index: usize,
    opened_bracket: u8,
    closed_bracket: u8,
) -> Option<usize> {
    let mut bracket_count = 0_usize;
    let mut i = start_byte_index;
    while i < end_byte_index && i < string.len() {
        let c = string.as_bytes()[i];

        //Ignore escaped chars
        if c == b'\\' {
            i += 2;

            continue;
        }

        if c == opened_bracket {
            bracket_count += 1;
        }else if c == closed_bracket {
            bracket_count = bracket_count.saturating_sub(1);

            if bracket_count == 0 {
                return Some(i);
            }
        }

        i += 1;
    }

    None
}

pub(crate) fn get_index_of_matching_bracket_tok(
    tokens: &[Token],
    start_index: usize,
    end_index: usize,
    opened_bracket: impl Into<String>,
    closed_bracket: impl Into<String>,
    abort_on_eol: bool,
) -> Option<usize> {
    let opened_bracket = opened_bracket.into();
    let closed_bracket = closed_bracket.into();

    if tokens.len() <= start_index || !matches!(tokens[start_index].token_type(), TokenType::OpeningBracket) ||
            tokens[start_index].value() != opened_bracket {
        return None;
    }

    let mut bracket_count = 0_usize;
    let mut i = start_index;
    while i < end_index && i < tokens.len() {
        let token = &tokens[i];

        if matches!(tokens[i].token_type(), TokenType::OpeningBracket) &&
                tokens[i].value() == opened_bracket {
            bracket_count += 1;
        }else if matches!(tokens[i].token_type(), TokenType::ClosingBracket) &&
                tokens[i].value() == closed_bracket {
            bracket_count = bracket_count.saturating_sub(1);

            if bracket_count == 0 {
                return Some(i);
            }
        }

        //Skip Multiline Text and Comments
        if matches!(tokens[i].token_type(), TokenType::StartMultilineText) {
            while i < end_index && i < tokens.len() &&
                    !matches!(tokens[i].token_type(), TokenType::EndMultilineText) {
                i += 1;
            }
        }else if matches!(tokens[i].token_type(), TokenType::StartComment | TokenType::StartDocComment) {
            while i < end_index && i < tokens.len() &&
                    !matches!(tokens[i].token_type(), TokenType::EndComment) {
                i += 1;
            }
        }

        if abort_on_eol && token.token_type() == TokenType::Eol {
            return None;
        }

        i += 1;
    }

    None
}

pub(crate) fn is_backslash_at_index_escaped(str: &str, byte_index: usize) -> bool {
    if str.as_bytes().get(byte_index).is_none_or(|byte| *byte == b'\\') {
        return false;
    }

    if byte_index == 0 {
        return false;
    }

    let mut i = byte_index - 1;
    loop {
        if str.as_bytes()[i] != b'\\' {
            return (byte_index - i) % 2 == 0;
        }

        if i == 0 {
            break;
        }

        i -= 1;
    }

    byte_index % 2 == 1
}

/// Splits the deque into two at the given index without the first element and the element at the index.
///
/// Returns a newly allocated `VecDeque`. `self` contains elements `[at + 1, len)`,
/// and the returned deque contains elements `[1, at)`.
pub(crate) fn split_off_arguments<T>(list: &mut VecDeque<T>, at: usize) -> VecDeque<T> {
    let mut argument_list = list.split_off(at);
    mem::swap(list, &mut argument_list);
    argument_list.pop_front();
    list.pop_front();

    argument_list
}
