use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::mem;
use std::str::FromStr;
use crate::interpreter::data::{DataObject, DataObjectRef, DataType, OptionDataObjectRef};
use crate::interpreter::data::function::{Function, FunctionPointerObject, InternalFunction, Parameter};
use crate::interpreter::{conversions, Interpreter};
use crate::lexer::{CodePosition, Token, TokenType};
use crate::regex_patterns;

#[cfg(windows)]
pub(crate) const LINE_SEPARATOR: &str = "\r\n";
#[cfg(not(windows))]
pub(crate) const LINE_SEPARATOR: &str = "\n";

pub(crate) fn get_os_name() -> &'static str {
    std::env::consts::OS
}

pub(crate) fn get_os_version() -> String {
    //TODO

    "TODO: os.version".to_string()
}

pub(crate) fn get_os_arch() -> &'static str {
    if cfg!(any(
        target_arch = "aarch64",
        target_arch = "arm64ec",
        target_arch = "riscv64",
        target_arch = "powerpc64",
        target_arch = "loongarch64",
        target_arch = "mips64",
        target_arch = "mips64r6",
        target_arch = "sparc64",
    )) {
        "ia64"
    }else if cfg!(any(target_arch = "x86_64")) {
        "amd64"
    }else if cfg!(any(target_arch = "x86")) {
        "x86"
    }else if cfg!(any(target_arch = "wasm64")) {
        "wasm64"
    }else if cfg!(any(target_arch = "wasm32")) {
        "wasm32"
    }else {
        "unknown"
    }
}

pub(crate) mod math {
    #[cfg(test)]
    mod tests;

    pub trait SpecialDiv {
        fn wrapping_floor_div(self, rhs: Self) -> Self;
        fn wrapping_ceil_div(self, rhs: Self) -> Self;
    }

    macro_rules! impl_special_div {
        ( $selfT:ty ) => {
            impl SpecialDiv for $selfT {
                fn wrapping_floor_div(self, rhs: Self) -> Self {
                    let mut ret = self.wrapping_div(rhs);

                    //Round down if signs are different and modulo != 0
                    if (self ^ rhs) < 0 && ret.wrapping_mul(rhs) != self {
                        ret = ret.wrapping_sub(1);
                    }

                    ret
                }

                fn wrapping_ceil_div(self, rhs: Self) -> Self {
                    let mut ret = self.wrapping_div(rhs);

                    //Round up if signs are equals and modulo != 0
                    if (self ^ rhs) >= 0 && ret.wrapping_mul(rhs) != self {
                        ret = ret.wrapping_add(1);
                    }

                    ret
                }
            }
        };
    }

    impl_special_div! { i32 }
    impl_special_div! { i64 }

    pub trait ToNumberBase {
        fn to_number_base(self, radix: u32) -> String;
    }

    macro_rules! impl_to_number_base {
        ( $selfT:ty, $unsignedT:ty ) => {
            impl ToNumberBase for $selfT {
                fn to_number_base(self, radix: u32) -> String {
                    const DIGITS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";

                    if !(2..=36).contains(&radix) {
                        panic!("Radix must be between 2 and 36");
                    }

                    let is_negative = self < 0;
                    let mut i = self.unsigned_abs();

                    let mut string = String::new();

                    while i >= radix as $unsignedT {
                        string.push(DIGITS[(i % radix as $unsignedT) as usize] as char);
                        i /= radix as $unsignedT;
                    }
                    string.push(DIGITS[i as usize] as char);

                    if is_negative {
                        string.push('-');
                    }

                    string.chars().rev().collect::<String>()
                }
            }
        };
    }

    impl_to_number_base! { i32, u32 }
    impl_to_number_base! { i64, u64 }
}

/// Converts negative indices to positive indices (e.g. `-1` => `len - 1` => last element)
///
/// This function will return None if the converted index is `< 0` or `>= len`
/// The returned value is guaranteed to be `< len`
pub fn wrap_index(index: i32, len: usize) -> Option<usize> {
    let index = index as isize;

    let index = if index < 0 {
        index.wrapping_add(len as isize)
    }else {
        index
    };

    if index < 0 || index as usize >= len {
        None
    }else {
        Some(index as usize)
    }
}

/// Converts negative indices to positive indices (e.g. `-1` => `len - 1` => last element)
///
/// This function will return None if the converted index is `< 0` or `> len`
/// The returned value is guaranteed to be `<= len`
pub fn wrap_index_allow_len(index: i32, len: usize) -> Option<usize> {
    let index = index as isize;

    let index = if index < 0 {
        index.wrapping_add(len as isize)
    }else {
        index
    };

    if index < 0 || index as usize > len {
        None
    }else {
        Some(index as usize)
    }
}

pub(crate) fn remove_dots_from_file_path(mut file: String) -> String {
    //Remove "/./"
    while file.contains("/./") {
        file = file.replace("/./", "/").to_string();
    }

    //Remove "/../" and go to parent
    while regex_patterns::UTILS_PARENT_FOLDER.is_match(&file) {
        file = regex_patterns::UTILS_PARENT_FOLDER.replace_all(&file, "/").to_string();
    }

    file
}

/**
 * @return Will return a new DataObject of type VOID if the dataObject is None or the dataObject itself
 */
pub fn none_to_lang_void(data_object: OptionDataObjectRef) -> DataObjectRef {
    data_object.unwrap_or_else(|| {
        DataObjectRef::new(DataObject::new_void())
    })
}

/**
 * Will return None if the dataObjects is empty or if dataObjects only contains Rust None values
 */
pub fn combine_data_objects(
    data_objects: &[DataObjectRef],
    interpreter: &mut Interpreter,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let mut data_objects = Vec::from(data_objects);

    if data_objects.is_empty() {
        return None;
    }

    if data_objects.len() == 1 {
        return Some(data_objects.into_iter().next().unwrap());
    }

    //Remove all void objects
    data_objects.retain(|data_object| data_object.data_type() != DataType::VOID);

    //Return a single void object if every data object is a void object
    if data_objects.is_empty() {
        return Some(DataObjectRef::new(DataObject::new_void()));
    }

    if data_objects.len() == 1 {
        return Some(data_objects.into_iter().next().unwrap());
    }

    //Combine everything to a single text object
    let mut builder = String::new();
    for ele in data_objects {
        builder += &conversions::to_text(interpreter, &ele, pos);
    }

    Some(DataObjectRef::new(DataObject::new_text(builder)))
}

pub fn combine_arguments_without_argument_separators(
    argument_list: &[DataObjectRef],
    interpreter: &mut Interpreter,
    pos: CodePosition,
) -> Vec<DataObjectRef> {
    if argument_list.is_empty() {
        return Vec::new();
    }

    let mut combined_argument_list = Vec::new();
    let mut argument_tmp_list = Vec::new();
    for current_data_object in argument_list {
        if current_data_object.data_type() == DataType::ARGUMENT_SEPARATOR {
            if argument_tmp_list.is_empty() {
                argument_tmp_list.push(DataObjectRef::new(DataObject::new_void()));
            }

            combined_argument_list.push(combine_data_objects(&argument_tmp_list, interpreter, pos).unwrap());
            argument_tmp_list.clear();

            continue;
        }

        argument_tmp_list.push(current_data_object.clone());
    }

    if argument_tmp_list.is_empty() {
        argument_tmp_list.push(DataObjectRef::new(DataObject::new_void()));
    }

    combined_argument_list.push(combine_data_objects(&argument_tmp_list, interpreter, pos).unwrap());

    combined_argument_list
}

/**
 * @return Returns a list of DataObjects where all arguments are separated by an ARGUMENT_SEPARATOR
 */
pub fn separate_arguments_with_argument_separators(
    argument_list: &[DataObjectRef],
) -> Vec<DataObjectRef> {
    if argument_list.is_empty() {
        return Vec::new();
    }

    let argument_separator = DataObjectRef::new(DataObject::with_update(|data_object| {
        data_object.set_argument_separator(", ")
    }).unwrap());

    //An argument separator is appended after every element [2 * len] except the last one [2 * len - 1]
    let new_len = 2 * argument_list.len() - 1;

    let argument_list = argument_list.iter().
            flat_map(|ele| [
                ele.clone(),
                argument_separator.clone(),
            ]).
            take(new_len).
            collect::<Vec<_>>();

    argument_list
}

/**
 * @param funcA Function A
 * @param funcB Function B
 * @return Returns true if the function signature of funcA and funcB are equals
 */
pub fn are_function_signatures_equals(func_a: &Function, func_b: &Function) -> bool {
    if func_a.var_args_parameter().map(|var_args| var_args.0) != func_b.var_args_parameter().
            map(|var_args| var_args.0) || func_a.parameter_list().len() != func_b.parameter_list().len() {
        return false;
    }

    for (parameter_a, parameter_b) in func_a.parameter_list().iter().zip(func_b.parameter_list().iter()) {
        if parameter_a.type_constraint() != parameter_b.type_constraint() {
            return false;
        }
    }

    true
}

/**
 * @param functions Function signatures will be extracted from the FunctionPointerObject
 * @param argumentList The combined argument list
 *
 * @return Returns the most restrictive function for the provided arguments or null if no function signature matches the arguments
 */
pub fn get_most_restrictive_function<'a>(
    functions: &'a FunctionPointerObject,
    argument_list: &[DataObjectRef],
) -> Option<&'a InternalFunction> {
    let function_index = get_most_restrictive_function_index(functions, argument_list);

    function_index.and_then(|function_index| functions.get_function(function_index))
}

/**
 * @param functions Function signatures will be extracted from the FunctionPointerObject
 * @param argumentList The combined argument list
 *
 * @return Returns the index of the most restrictive function for the provided arguments or -1 if no function signature matches the arguments
 */
pub fn get_most_restrictive_function_index(
    functions: &FunctionPointerObject,
    argument_list: &[DataObjectRef],
) -> Option<usize> {
    let function_signatures = functions.functions().iter().
            map(|function| function.function()).
            map(|function| (function.parameter_list(), function.var_args_parameter().
                    map(|var_args_parameter| var_args_parameter.0))).
            collect::<Vec<_>>();

    get_most_restrictive_function_signature_index_internal(&function_signatures, argument_list)
}

/**
 * @param functionSignatures Function signatures will be extracted from the LangBaseFunctions
 * @param argumentList The combined argument list
 *
 * @return Returns the index of the most restrictive function signature for the provided arguments
 */
pub fn get_most_restrictive_function_signature_index(
    function_signatures: &[Function],
    argument_list: &[DataObjectRef],
) -> Option<usize> {
    let function_signatures = function_signatures.iter().
            map(|function| (function.parameter_list(), function.var_args_parameter().
                    map(|var_args_parameter| var_args_parameter.0))).
            collect::<Vec<_>>();

    get_most_restrictive_function_signature_index_internal(&function_signatures, argument_list)
}
/**
 * @param varArgsParameterIndices Index of the var args argument of the function signature, if there is no var args argument the value must be set to -1
 * @param argumentList The combined argument list
 *
 * @return Returns the index of the most restrictive function signature for the provided arguments
 */
fn get_most_restrictive_function_signature_index_internal(
    function_signatures: &[(&[Parameter], Option<usize>)],
    argument_list: &[DataObjectRef],
) -> Option<usize> {
    let mut best_function_signature: Option<&[Parameter]> = None;
    let mut best_function_index = None;
    let mut best_allowed_types_count = None;
    let mut best_var_args_parameter_index = None;
    let mut best_var_args_penalty = None;

    'outer:
    for (i, function_signature) in function_signatures.iter().
            enumerate() {
        if function_signature.1.is_some() {
            if function_signature.0.len() - 1 > argument_list.len() {
                continue; //Argument count does not match
            }
        }else if function_signature.0.len() != argument_list.len() {
            continue; //Argument count does not match
        }

        let var_args_penalty = function_signature.1.map(|var_args_parameter_index| {
            function_signature.0[var_args_parameter_index].type_constraint().allowed_types().len()
        }).unwrap_or_default();

        let mut argument_index = 0;
        for (j, parameter) in function_signature.0.iter().
                enumerate() {
            if function_signature.1.is_some_and(|var_args_parameter_index| var_args_parameter_index == j) {
                let old_argument_index = argument_index;

                argument_index = argument_list.len() + j + 1 - function_signature.0.len();

                //Check if types are allowed for var args parameter
                for argument in argument_list[old_argument_index..argument_index].iter() {
                    if !parameter.type_constraint().is_type_allowed(argument.data_type()) {
                        continue 'outer;
                    }

                }

                continue;
            }

            if !parameter.type_constraint().is_type_allowed(argument_list[argument_index].data_type()) {
                continue 'outer;
            }

            argument_index += 1;
        }

        let allowed_types_count: usize = function_signature.0.iter().
                map(|parameter| parameter.type_constraint().allowed_types().len()).
                sum();
        let size_diff = best_function_signature.map(|best_function_signature|
                best_function_signature.len() as isize - function_signature.0.len() as isize
        ).unwrap_or_default();
        if best_function_index.is_none() || (function_signature.1.is_none() && best_var_args_parameter_index.is_some()) ||
                (function_signature.1.is_none() && best_var_args_parameter_index.is_none() && allowed_types_count < best_allowed_types_count.unwrap_or_default() ||
                (function_signature.1.is_some() && best_var_args_parameter_index.is_some() &&
                        match size_diff {
                            0 => var_args_penalty < best_var_args_penalty.unwrap_or_default(),
                            ..0 => allowed_types_count < best_allowed_types_count.unwrap_or_default() + best_var_args_penalty.unwrap_or_default() * (-size_diff as usize),
                            _ => allowed_types_count + var_args_penalty * (size_diff as usize) < best_allowed_types_count.unwrap_or_default(),
                        })) {
            best_function_signature = Some(function_signature.0);
            best_function_index = Some(i);
            best_allowed_types_count = Some(allowed_types_count);
            best_var_args_parameter_index = function_signature.1;
            best_var_args_penalty = Some(var_args_penalty);
        }
    }

    best_function_index
}

/**
 * @return Returns the version as a tuple or None if the version is invalid
 */
pub fn get_version_components(version: &str) -> Option<(i32, i32, i32)> {
    if version.is_empty() {
        return None;
    }

    if version.as_bytes()[0] != b'v' {
        return None;
    }

    if version.contains("-0") {
        return None;
    }

    let major_minor_separator_index = version.find('.')?;

    let minor_bugfix_separator_index = version[major_minor_separator_index + 1..].find('.')?;
    let minor_bugfix_separator_index = minor_bugfix_separator_index + major_minor_separator_index + 1;

    let major_str = &version[1..major_minor_separator_index];
    let minor_str = &version[major_minor_separator_index + 1..minor_bugfix_separator_index];
    let bugfix_str = &version[minor_bugfix_separator_index + 1..];

    let major = i32::from_str(major_str).ok()?;
    let minor = i32::from_str(minor_str).ok()?;
    let bugfix = i32::from_str(bugfix_str).ok()?;

    if major < 0 || minor < 0 || bugfix < 0 {
        None
    }else {
        Some((major, minor, bugfix))
    }
}

/**
 * @return Returns Greater if versionA is older than versionB<br>
 * returns Equal if versionA is newer than versionB<br>
 * returns None if versionA is equalsTo versionB<br>
 */
pub fn compare_versions_components(version_a: (i32, i32, i32), version_b: (i32, i32, i32)) -> Ordering {
    if version_a.0 != version_b.0 {
        return version_a.0.cmp(&version_b.0);
    }

    if version_a.1 != version_b.1 {
        return version_a.1.cmp(&version_b.1);
    }

    version_a.2.cmp(&version_b.2)
}

/**
 * @return Returns Less if versionA is older than versionB<br>
 * returns Greater if versionA is newer than versionB<br>
 * returns Equal if versionA is equalsTo versionB<br>
 * returns None if at least one argument is invalid
 */
pub fn compare_versions_str(version_a: &str, version_b: &str) -> Option<Ordering> {
    let version_a = get_version_components(version_a)?;
    let version_b = get_version_components(version_b)?;

    Some(compare_versions_components(version_a, version_b))
}

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

/**
 * Returns true if the call operator is defined for the provided DataObject else false
 */
pub fn is_callable(data_object: &DataObjectRef) -> bool {
    let has_op_method = data_object.object_value().is_some_and(|object_value| {
        !object_value.borrow().is_class() && object_value.borrow().methods().contains_key("op:call")
    });

    let is_struct_definition = data_object.struct_value().is_some_and(|struct_value| struct_value.is_definition());
    let is_object_class = data_object.object_value().is_some_and(|object_value| object_value.borrow().is_class());

    has_op_method || matches!(data_object.data_type(), DataType::FUNCTION_POINTER | DataType::TYPE) ||
            is_struct_definition || is_object_class
}

pub fn is_member_access_allowed(value_object: &DataObjectRef) -> bool {
    matches!(value_object.data_type(), DataType::ERROR | DataType::STRUCT | DataType::OBJECT)
}

#[derive(Debug)]
pub struct InvalidTranslationTemplateSyntaxError {
    message: String
}

impl InvalidTranslationTemplateSyntaxError {
    pub fn new(message: impl Into<String>) -> Self {
        Self { message: message.into() }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl Display for InvalidTranslationTemplateSyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl Error for InvalidTranslationTemplateSyntaxError {}

#[derive(Debug, Copy, Clone)]
struct CountRange {
    start_count: i32,
    /**
     * If None: All values >= startCount
     */
    end_count: Option<i32>,
}

impl CountRange {
    fn new(start_count: i32, end_count: Option<i32>) -> Self {
        Self { start_count, end_count }
    }

    fn is_count_in_range(self, count: i32) -> bool {
        count == self.start_count || (count > self.start_count && (self.end_count.is_none_or(|end_count| count <= end_count)))
    }
}

#[derive(Debug)]
struct TranslationPluralizationTemplate {
    count_values: Box<[CountRange]>,
    raw_translation_value: Box<str>,
}

impl TranslationPluralizationTemplate {
    pub fn new(count_values: Box<[CountRange]>, raw_translation_value: Box<str>) -> Self {
        Self { count_values, raw_translation_value }
    }

    pub fn count_values(&self) -> &[CountRange] {
        &self.count_values
    }

    pub fn raw_translation_value(&self) -> &str {
        &self.raw_translation_value
    }
}

/**
 * @return Will return a formatted template translation ("{" can be escaped with "{{")
 */
pub fn format_translation_template(
    translation_value: &str,
    template_map: HashMap<Box<str>, Box<str>>,
) -> Result<String, InvalidTranslationTemplateSyntaxError> {
    if translation_value.is_empty() {
        return Ok(String::new());
    }

    let mut builder = String::new();

    let mut i;
    let mut start_index = 0;
    loop {
        let index = translation_value[start_index..].find('{');
        if let Some(index) = index {
            i = start_index + index;
        }else {
            builder += &translation_value[start_index..];

            break;
        }

        builder += &translation_value[start_index..i];
        start_index = i;

        if i < translation_value.len() - 1 && translation_value.as_bytes()[i + 1] == b'{' {
            builder += &translation_value[start_index..i + 1]; //Ignore second '{'
            start_index = i + 2;

            continue;
        }

        let matching_bracket_index = translation_value[i..].find('}');
        let Some(matching_bracket_index) = matching_bracket_index else {
            return Err(InvalidTranslationTemplateSyntaxError::new("Template closing bracket is missing"));
        };
        let matching_bracket_index = i + matching_bracket_index;

        start_index = matching_bracket_index + 1;
        let template_name = &translation_value[i + 1..matching_bracket_index];
        let template_replacement = template_map.get(template_name);
        if let Some(template_replacement) = template_replacement {
            builder += template_replacement;
        }else {
            return Err(InvalidTranslationTemplateSyntaxError::new(format!(
                "Template with the name \"{template_name}\" was not defined",
            )));
        }
    }

    Ok(builder)
}

/**
 * @return Will return a formatted translation with the correct pluralization (";" can be escaped with ";;" and "{" can be escaped with "{{")
 */
pub fn format_translation_template_pluralization(
    translation_value: &str,
    count: i32,
) -> Result<String, InvalidTranslationTemplateSyntaxError> {
    format_translation_template_pluralization_with_template(translation_value, count, HashMap::new())
}

/**
 * @return Will return a formatted translation with the correct pluralization and additional template values [the count template value will be overridden]
 * (";" can be escaped with ";;" and "{" can be escaped with "{{")
 */
pub fn format_translation_template_pluralization_with_template(
    translation_value: &str,
    count: i32,
    template_map: HashMap<Box<str>, Box<str>>,
) -> Result<String, InvalidTranslationTemplateSyntaxError> {
    let mut template_tokens = Vec::new();

    let mut start_index = 0;
    let mut i = 0;
    while i < translation_value.len() {
        if i == translation_value.len() - 1 {
            template_tokens.push(translation_value[start_index..i + 1].replace(";;", ";")); //Ignore second ";"s

            break;
        }

        if translation_value.as_bytes()[i] == b';' {
            if translation_value.as_bytes()[i + 1] == b';' {
                i += 2; //Skip two ';'

                continue;
            }

            template_tokens.push(translation_value[start_index..i + 1].replace(";;", ";")); //Ignore second ";"s
            start_index = i + 1;
        }

        i += 1;
    }

    let mut templates = Vec::with_capacity(template_tokens.len());
    for (i, mut template_token) in template_tokens.iter().
            map(|str| &**str).
            enumerate() {
        if template_token.is_empty() || template_token.as_bytes()[0] != b'[' {
            return Err(InvalidTranslationTemplateSyntaxError::new("Pluralization template token must start with \"[\""));
        }

        if template_token.as_bytes()[template_token.len() - 1] == b';' {
            template_token = &template_token[0..template_token.len() - 1];
        }else if i != template_tokens.len() - 1 {
            return Err(InvalidTranslationTemplateSyntaxError::new("Pluralization template token must end with \";\""));
        }

        let matching_bracket_index = template_token.find(']');
        let Some(matching_bracket_index) = matching_bracket_index else {
            return Err(InvalidTranslationTemplateSyntaxError::new("Count range closing bracket is missing"));
        };

        let raw_count_values = &template_token[1..matching_bracket_index]; //Ignore '[' and ']'
        let raw_translation_value = &template_token[matching_bracket_index + 1..];

        let mut count_values = Vec::new();
        start_index = 0;
        for (j, c) in raw_count_values.bytes().
                enumerate() {
            if c.is_ascii_digit() || c == b'-' || c == b'+' {
                if j < raw_count_values.len() - 1 {
                    continue;
                }
            }else if c != b',' {
                return Err(InvalidTranslationTemplateSyntaxError::new("Invalid token in count range"));
            }

            let end_index = if j < raw_count_values.len() - 1 { j } else { j + 1 };
            let raw_count_value = &raw_count_values[start_index..end_index];
            start_index = j + 1;

            let mut start_count = -2;
            let mut end_count = -2;
            let mut number_start_index = 0;
            for (k, c) in raw_count_value.bytes().
                    enumerate() {
                if c.is_ascii_digit() {
                    if k == raw_count_value.len() - 1 {
                        let number_count = &raw_count_value[number_start_index..k + 1];

                        if start_count == -2 {
                            let ret = i32::from_str(number_count);
                            match ret {
                                Ok(ret) => start_count = ret,
                                Err(e) => {
                                    return Err(InvalidTranslationTemplateSyntaxError::new(format!(
                                        "Invalid count value: {e}",
                                    )));
                                },
                            }

                            end_count = start_count;
                        }else if end_count == -2 {
                            let ret = i32::from_str(number_count);
                            match ret {
                                Ok(ret) => end_count = ret,
                                Err(e) => {
                                    return Err(InvalidTranslationTemplateSyntaxError::new(format!(
                                        "Invalid end count value: {e}",
                                    )));
                                },
                            }
                        }else {
                            return Err(InvalidTranslationTemplateSyntaxError::new("Too many value in range inside a count range"));
                        }
                    }

                    continue;
                }

                if c == b'-' {
                    if number_start_index != 0 {
                        return Err(InvalidTranslationTemplateSyntaxError::new("Invalid character \"-\" can not be used twice in a range inside a count range"));
                    }

                    let number_start_count = &raw_count_value[number_start_index..k];
                    number_start_index = k + 1;

                    let ret = i32::from_str(number_start_count);
                    match ret {
                        Ok(ret) => start_count = ret,
                        Err(e) => {
                            return Err(InvalidTranslationTemplateSyntaxError::new(format!(
                                "Invalid start count value: {e}",
                            )));
                        },
                    }
                }else if c == b'+' {
                    if start_count != -2 || end_count != -2 || k < raw_count_value.len() - 1 {
                        return Err(InvalidTranslationTemplateSyntaxError::new(
                            "Invalid character \"+\" can not be used twice or with multiple values in count range",
                        ));
                    }

                    let number_start_count = &raw_count_value[number_start_index..k];
                    let ret = i32::from_str(number_start_count);
                    match ret {
                        Ok(ret) => start_count = ret,
                        Err(e) => {
                            return Err(InvalidTranslationTemplateSyntaxError::new(format!(
                                "Invalid start count value: {e}",
                            )));
                        },
                    }

                    end_count = -1;
                }
            }

            if start_count == -2 || end_count == -2 {
                return Err(InvalidTranslationTemplateSyntaxError::new("Empty count range sequence"));
            }

            count_values.push(CountRange::new(start_count, if end_count == -1 {
                None
            }else {
                Some(end_count)
            }));
        }

        templates.push(TranslationPluralizationTemplate::new(Box::from(count_values), Box::from(raw_translation_value)));
    }

    for template in templates {
        for count_range in template.count_values() {
            if count_range.is_count_in_range(count) {
                let mut template_map = template_map;
                template_map.insert(Box::from("count"), Box::from(&*count.to_string()));

                return format_translation_template(template.raw_translation_value(), template_map);
            }
        }
    }

    Err(InvalidTranslationTemplateSyntaxError::new(format!("No pluralization for count \"{count}\" was defined")))
}
