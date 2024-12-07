use std::sync::LazyLock;
use regex::Regex;

pub static WORD: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^\w+$").unwrap());

pub static VAR_NAME_WITHOUT_PREFIX: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^(\$|&|fp\.)\w+$").unwrap());

pub static VAR_NAME_NORMAL_FUNCTION_WITHOUT_PREFIX: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^(\$|fp\.)\w+$").unwrap());

pub static VAR_NAME_NORMAL_ARRAY_WITHOUT_PREFIX: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^(\$|&)\w+$").unwrap());

pub static VAR_NAME_FULL: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^(\[\[\w+\]\]::)?(\$\**|&|fp\.)\w+$").unwrap());

pub static VAR_NAME_FUNCS_WITH_OPERATOR_AND_CONVERSION_METHOD: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^(op:((len|deepCopy|inc|dec|pos|inv|not|abs|iter|hasNext|next)|((r-)?(concat|add|sub|mul|pow|div|truncDiv|floorDiv|ceilDiv|mod|and|or|xor|lshift|rshift|rzshift|isEquals|isStrictEquals|isLessThan|isGreaterThan))|(getItem|setItem|slice)|(call)))|(to:(text|char|int|long|float|double|byteBuffer|array|list|bool|number))|(((\[\[\w+\]\]::)?fp|mp|func|fn|linker|ln)\.\w+)$").unwrap());

pub static METHOD_NAME: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^mp\.\w+$").unwrap());

pub static OPERATOR_METHOD_NAME: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^op:((len|deepCopy|inc|dec|pos|inv|not|abs|iter|hasNext|next)|((r-)?(concat|add|sub|mul|pow|div|truncDiv|floorDiv|ceilDiv|mod|and|or|xor|lshift|rshift|rzshift|isEquals|isStrictEquals|isLessThan|isGreaterThan))|(getItem|setItem|slice)|(call))$").unwrap());

pub static CONVERSION_METHOD_NAME: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^to:(text|char|int|long|float|double|byteBuffer|array|list|bool|number)$").unwrap());

pub static TYPE_CONSTRAINT_WITH_SPECIAL_TYPES: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^\{(([?!]?([A-Z_]+\|)*[A-Z_]+)|(bool|number|callable))\}$").unwrap());

pub static VAR_NAME_FUNC_PTR_WITH_FUNCS: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^((\[\[\w+\]\]::)?fp|func|fn|linker|ln)\.\w+$").unwrap());

pub static PARSING_VAR_NAME_PTR_AND_DEREFERENCE: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^(\[\[\w+\]\]::)?\$\**\[+\w+\]+$").unwrap());

pub static PARSING_TYPE_CONSTRAINT: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^\{[?!]?([A-Z_]+\|)*[A-Z_]+\}$").unwrap());

pub static PARSING_FLOATING_POINT_E_SYNTAX_START: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^((([1-9]\d*|0)(\.\d*)?)|(\.\d+))[eE]$").unwrap());

pub static PARSING_PARSER_FLAG: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^parser(\.\w+)+$").unwrap());

pub static PARSING_ASSIGNMENT_OPERATOR: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^ [^\\= ]{1,3}= $").unwrap());

pub static PARSING_SIMPLE_ASSIGNMENT_VARIABLE_NAME_LVALUE: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^(\[\[\w+\]\]::)?\$\**\w+$").unwrap());

pub static PARSING_SIMPLE_TRANSLATION_KEY: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^[\w\-\.\:]+$").unwrap());

pub static STARTS_WITH_VAR_NAME_FULL_WITH_FUNCS_AND_PTR_AND_DEREFERENCE_WITH_OPERATOR_AND_CONVERSION_METHODS: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^(op:((len|deepCopy|inc|dec|pos|inv|not|abs|iter|hasNext|next)|((r-)?(concat|add|sub|mul|pow|div|truncDiv|floorDiv|ceilDiv|mod|and|or|xor|lshift|rshift|rzshift|isEquals|isStrictEquals|isLessThan|isGreaterThan))|(getItem|setItem|slice)|(call)))|(to:(text|char|int|long|float|double|byteBuffer|array|list|bool|number))|(((\[\[\w+\]\]::)?(\$\**|&|fp\.|mp\.)|func\.|fn\.|linker\.|ln\.)\w+|(\[\[\w+\]\]::)?\$\**\[+\w+\]+)").unwrap());

pub static STARTS_WITH_ARGUMENT_SEPARATOR: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^\s*,\s*").unwrap());

pub static STARTS_WITH_FUNCTION_IDENTIFIER: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^parser\.\w+").unwrap());

pub static STARTS_WITH_ASSIGNMENT_OPERATOR: LazyLock<Regex, fn() -> Regex> =
    LazyLock::new(|| Regex::new(r"^ [^\\= ]{1,3}= ").unwrap());
