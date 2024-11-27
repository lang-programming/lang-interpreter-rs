use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct CodePosition {
    line_number_from: isize,
    line_number_to: isize,
    column_from: isize,
    column_to: isize,
}

impl CodePosition {
    pub const EMPTY: CodePosition = CodePosition {
        line_number_from: -1, line_number_to: -1, column_from: -1, column_to: -1,
    };

    pub fn new(line_number_from: isize, line_number_to: isize, column_from: isize, column_to: isize) -> Self {
        Self { line_number_from, line_number_to, column_from, column_to }
    }

    pub fn combine(&self, code_position: &CodePosition) -> CodePosition {
        if *self == Self::EMPTY || *code_position == Self::EMPTY {
            return Self::EMPTY;
        }

        let column_from = if self.line_number_from == code_position.line_number_from {
            self.column_from.min(code_position.column_from)
        }else if self.line_number_from < code_position.line_number_from {
            self.column_from
        }else {
            code_position.column_from
        };

        let column_to = if self.line_number_to == code_position.line_number_to {
            self.column_to.min(code_position.column_to)
        }else if self.line_number_to > code_position.line_number_to {
            self.column_to
        }else {
            code_position.column_to
        };

        Self::new(
            self.line_number_from.min(code_position.line_number_from),
            self.line_number_to.min(code_position.line_number_to),
            column_from,
            column_to,
        )
    }

    pub fn to_compact_string(&self) -> String {
        format!(
            "{}:{}-{}:{}",
            self.line_number_from, self.column_from,

            self.line_number_to, self.column_to,
        )
    }

    pub fn line_number_from(&self) -> isize {
        self.line_number_from
    }

    pub fn line_number_to(&self) -> isize {
        self.line_number_to
    }

    pub fn column_from(&self) -> isize {
        self.column_from
    }

    pub fn column_to(&self) -> isize {
        self.column_to
    }
}

impl Display for CodePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f, "{:5}:{:3} - {:5}:{:3}",
            self.line_number_from, self.column_from,

            self.line_number_to, self.column_to,
        )
    }
}
