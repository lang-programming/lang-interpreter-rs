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
