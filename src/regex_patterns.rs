use std::sync::LazyLock;
use regex::Regex;

pub static VAR_NAME_FULL_WITH_FUNCS_AND_PTR_AND_DEREFERENCE_WITH_OPERATOR_AND_CONVERSION_METHODS: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"(op:((len|deepCopy|inc|dec|pos|inv|not|abs|iter|hasNext|next)|((r-)?(concat|add|sub|mul|pow|div|truncDiv|floorDiv|ceilDiv|mod|and|or|xor|lshift|rshift|rzshift|isEquals|isStrictEquals|isLessThan|isGreaterThan))|(getItem|setItem|slice)|(call)))|(to:(text|char|int|long|float|double|byteBuffer|array|list|bool|number))|(((\[\[\w+\]\]::)?(\$\**|&|fp\.|mp\.)|func\.|fn\.|linker\.|ln\.)\w+|(\[\[\w+\]\]::)?\$\**\[+\w+\]+)").unwrap());

pub static PARSER_FUNCTION_IDENTIFIER: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"parser\.\w+").unwrap());

pub static ARGUMENT_SEPARATOR: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"\s*,\s*").unwrap());

pub static PARSING_VAR_NAME_PTR_AND_DEREFERENCE: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^(\[\[\w+\]\]::)?\$\**\[+\w+\]+$").unwrap());

pub static PARSING_FLOATING_POINT_E_SYNTAX_START: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^((([1-9]\d*|0)(\.\d*)?)|(\.\d+))[eE]$").unwrap());

pub static PARSING_ASSIGNMENT_OPERATOR: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r" [^\\= ]{1,3}= ").unwrap());
