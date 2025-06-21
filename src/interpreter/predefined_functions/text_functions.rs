use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::{conversions, Interpreter, InterpretingError};
use crate::interpreter::data::{DataObject, DataObjectRef, Number};
use crate::interpreter::regex;
use crate::lexer::CodePosition;
use crate::utils;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            to_upper_function,
            crate::lang_func_metadata!(
                name="toUpper",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$text",
                ),
            ),
        ));
    fn to_upper_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::new_text(
            conversions::to_text(interpreter, &text_object, CodePosition::EMPTY).to_uppercase(),
        ))
    }

    functions.push(crate::lang_func!(
            to_lower_function,
            crate::lang_func_metadata!(
                name="toLower",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$text",
                ),
            ),
        ));
    fn to_lower_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::new_text(
            conversions::to_text(interpreter, &text_object, CodePosition::EMPTY).to_lowercase(),
        ))
    }

    functions.push(crate::lang_func!(
            trim_function,
            crate::lang_func_metadata!(
                name="trim",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$text",
                ),
            ),
        ));
    fn trim_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::new_text(
            conversions::to_text(interpreter, &text_object, CodePosition::EMPTY).trim(),
        ))
    }

    functions.push(crate::lang_func!(
            replace_function,
            crate::lang_func_metadata!(
                name="replace",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$regex",
                ),
                parameter(
                    name="$replacement",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn replace_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        regex_object: DataObjectRef,
        replacement_object: DataObjectRef,
    ) -> DataObjectRef {
        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
        let regex = conversions::to_text(interpreter, &regex_object, CodePosition::EMPTY);
        let replacement = conversions::to_text(interpreter, &replacement_object, CodePosition::EMPTY);

        let ret = regex::replace(&text, &regex, replacement.into_string());
        match ret {
            Ok(ret) => DataObjectRef::new(DataObject::new_text(ret)),
            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::InvalidRegexSyntax,
                    Some(e.message()),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
            lpad_function,
            crate::lang_func_metadata!(
                name="lpad",
                info="Adds padding to the left of the $text value if needed",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$paddingText",
                ),
                parameter(
                    name="$len",
                    parameter_type(number),
                ),
            ),
        ));
    fn lpad_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        padding_text_object: DataObjectRef,
        len: DataObjectRef,
    ) -> DataObjectRef {
        let len = len.number_value().unwrap();
        let len = len.int_value();
        if len < 0 {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 3 \"$len\" must be >= 0"),
                CodePosition::EMPTY,
            );
        }

        let padding = conversions::to_text(interpreter, &padding_text_object, CodePosition::EMPTY);
        if padding.is_empty() {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The padding text must not be empty"),
                CodePosition::EMPTY,
            );
        }

        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

        if text.len() >= len as usize {
            return DataObjectRef::new(DataObject::new_text(text));
        }

        let mut builder = text.into_string();
        while builder.len() < len as usize {
            builder.insert_str(0, &padding);
        }

        if builder.len() > len as usize {
            builder = builder[builder.len() - len as usize..].to_string();
        }

        DataObjectRef::new(DataObject::new_text(builder))
    }

    functions.push(crate::lang_func!(
            rpad_function,
            crate::lang_func_metadata!(
                name="rpad",
                info="Adds padding to the right of the $text value if needed",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$paddingText",
                ),
                parameter(
                    name="$len",
                    parameter_type(number),
                ),
            ),
        ));
    fn rpad_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        padding_text_object: DataObjectRef,
        len: DataObjectRef,
    ) -> DataObjectRef {
        let len = len.number_value().unwrap();
        let len = len.int_value();
        if len < 0 {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 3 \"$len\" must be >= 0"),
                CodePosition::EMPTY,
            );
        }

        let padding = conversions::to_text(interpreter, &padding_text_object, CodePosition::EMPTY);
        if padding.is_empty() {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The padding text must not be empty"),
                CodePosition::EMPTY,
            );
        }

        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

        if text.len() >= len as usize {
            return DataObjectRef::new(DataObject::new_text(text));
        }

        let mut builder = text.into_string();
        while builder.len() < len as usize {
            builder.push_str(&padding);
        }

        builder = builder[..len as usize].to_string();

        DataObjectRef::new(DataObject::new_text(builder))
    }

    functions.push(crate::lang_func!(
            format_template_pluralization_function,
            crate::lang_func_metadata!(
                name="formatTemplatePluralization",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$count",
                    parameter_type(number),
                ),
                parameter(
                    name="$translationValue",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn format_template_pluralization_function(
        interpreter: &mut Interpreter,
        count: DataObjectRef,
        translation_value_object: DataObjectRef,
    ) -> DataObjectRef {
        let count = count.number_value().unwrap();
        if count.int_value() < 0 {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Count must be >= 0"),
                CodePosition::EMPTY,
            );
        }

        let translation_value = conversions::to_text(interpreter, &translation_value_object, CodePosition::EMPTY);

        let ret = utils::format_translation_template_pluralization(&translation_value, count.int_value());
        match ret {
            Ok(ret) => DataObjectRef::new(DataObject::new_text(ret)),

            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::InvalidTemplateSyntax,
                    Some(e.message()),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
            contains_function,
            crate::lang_func_metadata!(
                name="contains",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$haystack",
                ),
                parameter(
                    name="$needle",
                ),
            ),
        ));
    fn contains_function(
        interpreter: &mut Interpreter,
        haystack_object: DataObjectRef,
        needle_object: DataObjectRef,
    ) -> DataObjectRef {
        let haystack = conversions::to_text(interpreter, &haystack_object, CodePosition::EMPTY);
        let needle = conversions::to_text(interpreter, &needle_object, CodePosition::EMPTY);

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(haystack.contains(&*needle))
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            index_of_without_from_index_function,
            crate::lang_func_metadata!(
                name="indexOf",
                has_info=true,
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$searchText",
                ),
            ),
        ));
    fn index_of_without_from_index_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        search_text_object: DataObjectRef,
    ) -> DataObjectRef {
        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
        let search_text = conversions::to_text(interpreter, &search_text_object, CodePosition::EMPTY);

        let byte_index = text.find(&*search_text);
        if let Some(byte_index) = byte_index {
            let index = text[..byte_index].chars().count();
            DataObjectRef::new(DataObject::new_number(index as i32))
        }else {
            DataObjectRef::new(DataObject::new_number(-1_i32))
        }
    }

    functions.push(crate::lang_func!(
            index_of_with_from_index_function,
            crate::lang_func_metadata!(
                name="indexOf",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$searchText",
                ),
                parameter(
                    name="$fromIndex",
                    parameter_type(number),
                ),
            ),
        ));
    fn index_of_with_from_index_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        search_text_object: DataObjectRef,
        from_index_number: DataObjectRef,
    ) -> DataObjectRef {
        let from_index_number = from_index_number.number_value().unwrap();

        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
        let search_text = conversions::to_text(interpreter, &search_text_object, CodePosition::EMPTY);
        let len = text.chars().count();

        let from_index = from_index_number.int_value() as isize;
        let from_index = if from_index < 0 {
            from_index.wrapping_add(len as isize)
        }else {
            from_index
        };

        if from_index < 0 || from_index as usize >= len {
            return interpreter.set_errno_error_object_error_only(InterpretingError::IndexOutOfBounds);
        }

        let from_index = from_index as usize;

        let from_byte_index = text.chars().take(from_index).
                map(char::len_utf8).sum();

        let byte_index = text[from_byte_index..].find(&*search_text);
        if let Some(byte_index) = byte_index {
            let index = text[..from_byte_index + byte_index].chars().count();
            DataObjectRef::new(DataObject::new_number(index as i32))
        }else {
            DataObjectRef::new(DataObject::new_number(-1_i32))
        }
    }

    functions.push(crate::lang_func!(
            last_index_of_without_to_index_function,
            crate::lang_func_metadata!(
                name="lastIndexOf",
                has_info=true,
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$searchText",
                ),
            ),
        ));
    fn last_index_of_without_to_index_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        search_text_object: DataObjectRef,
    ) -> DataObjectRef {
        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
        let search_text = conversions::to_text(interpreter, &search_text_object, CodePosition::EMPTY);

        let byte_index = text.rfind(&*search_text);
        if let Some(byte_index) = byte_index {
            let index = text[..byte_index].chars().count();
            DataObjectRef::new(DataObject::new_number(index as i32))
        }else {
            DataObjectRef::new(DataObject::new_number(-1_i32))
        }
    }

    functions.push(crate::lang_func!(
            last_index_of_with_to_index_function,
            crate::lang_func_metadata!(
                name="lastIndexOf",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$searchText",
                ),
                parameter(
                    name="$toIndex",
                    parameter_type(number),
                ),
            ),
        ));
    fn last_index_of_with_to_index_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        search_text_object: DataObjectRef,
        to_index_number: DataObjectRef,
    ) -> DataObjectRef {
        let to_index_number = to_index_number.number_value().unwrap();

        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
        let search_text = conversions::to_text(interpreter, &search_text_object, CodePosition::EMPTY);
        let len = text.chars().count();

        let to_index = to_index_number.int_value() as isize;
        let to_index = if to_index < 0 {
            to_index.wrapping_add(len as isize)
        }else {
            to_index
        };

        if to_index < 0 || to_index as usize >= len {
            return interpreter.set_errno_error_object_error_only(InterpretingError::IndexOutOfBounds);
        }

        let to_index = to_index as usize;

        let to_byte_index = text.chars().take(to_index + 1).
                map(char::len_utf8).sum();

        let byte_index = text[..to_byte_index].rfind(&*search_text);
        if let Some(byte_index) = byte_index {
            let index = text[..byte_index].chars().count();
            DataObjectRef::new(DataObject::new_number(index as i32))
        }else {
            DataObjectRef::new(DataObject::new_number(-1_i32))
        }
    }

    functions.push(crate::lang_func!(
            starts_with_index_function,
            crate::lang_func_metadata!(
                name="startsWith",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$prefix",
                ),
            ),
        ));
    fn starts_with_index_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        prefix_object: DataObjectRef,
    ) -> DataObjectRef {
        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
        let prefix = conversions::to_text(interpreter, &prefix_object, CodePosition::EMPTY);

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(text.starts_with(&*prefix))
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            ends_with_index_function,
            crate::lang_func_metadata!(
                name="endsWith",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$suffix",
                ),
            ),
        ));
    fn ends_with_index_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        suffix_object: DataObjectRef,
    ) -> DataObjectRef {
        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
        let suffix = conversions::to_text(interpreter, &suffix_object, CodePosition::EMPTY);

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(text.ends_with(&*suffix))
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            matches_function,
            crate::lang_func_metadata!(
                name="matches",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$text",
                ),
                parameter(
                    name="$regex",
                ),
            ),
        ));
    fn matches_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        regex_object: DataObjectRef,
    ) -> DataObjectRef {
        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
        let regex = conversions::to_text(interpreter, &regex_object, CodePosition::EMPTY);

        let ret = regex::matches(&text, &regex);
        match ret {
            Ok(ret) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(ret)
                }).unwrap())
            },

            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::InvalidRegexSyntax,
                    Some(e.message()),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
            repeat_text_function,
            crate::lang_func_metadata!(
                name="repeatText",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$count",
                    parameter_type(number),
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn repeat_text_function(
        interpreter: &mut Interpreter,
        count: DataObjectRef,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        let count = count.number_value().unwrap();
        if count.int_value() < 0 {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Count must be >= 0"),
                CodePosition::EMPTY,
            );
        }

        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

        let mut builder = String::new();
        for _ in 0..count.int_value() {
            builder += &text;
        }

        DataObjectRef::new(DataObject::new_text(builder))
    }

    functions.push(crate::lang_func!(
            chars_of_function,
            crate::lang_func_metadata!(
                name="charsOf",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn chars_of_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

        let arr = text.chars().
                map(|char| {
                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_char(char)
                    }).unwrap())
                }).
                collect::<Box<_>>();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(arr)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            join_function,
            crate::lang_func_metadata!(
                name="join",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
                parameter(
                    name="&collection",
                    type_constraint(
                        allowed=["ARRAY", "LIST"],
                    ),
                ),
            ),
        ));
    fn join_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
        collection_object: DataObjectRef,
    ) -> DataObjectRef {
        let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

        let joined_text = if let Some(array) = collection_object.array_value() {
            array.borrow().iter().
                    map(|data_object| conversions::to_text(interpreter, data_object, CodePosition::EMPTY)).
                    collect::<Vec<_>>().
                    join(&text)
        }else {
            collection_object.list_value().unwrap().borrow().iter().
                    map(|data_object| conversions::to_text(interpreter, data_object, CodePosition::EMPTY)).
                    collect::<Vec<_>>().
                    join(&text)
        };

        DataObjectRef::new(DataObject::new_text(joined_text))
    }

    {
        functions.push(crate::lang_func!(
                split_without_max_split_count_function,
                crate::lang_func_metadata!(
                    name="split",
                    has_info=true,
                    return_type_constraint(
                        allowed=["ARRAY"],
                    ),
                    parameter(
                        name="$text",
                    ),
                    parameter(
                        name="$regex",
                    ),
                ),
            ));
        fn split_without_max_split_count_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
            regex_object: DataObjectRef,
        ) -> DataObjectRef {
            split_internal_function(interpreter, text_object, regex_object, None)
        }

        functions.push(crate::lang_func!(
                split_with_max_split_count_function,
                crate::lang_func_metadata!(
                    name="split",
                    return_type_constraint(
                        allowed=["ARRAY"],
                    ),
                    parameter(
                        name="$text",
                    ),
                    parameter(
                        name="$regex",
                    ),
                    parameter(
                        name="$maxSplitCount",
                        parameter_type(number),
                    ),
                ),
            ));
        fn split_with_max_split_count_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
            regex_object: DataObjectRef,
            max_split_count: DataObjectRef,
        ) -> DataObjectRef {
            split_internal_function(interpreter, text_object, regex_object, Some(max_split_count.number_value().unwrap()))
        }

        fn split_internal_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
            regex_object: DataObjectRef,
            max_split_count: Option<Number>,
        ) -> DataObjectRef {
            let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
            let regex = conversions::to_text(interpreter, &regex_object, CodePosition::EMPTY);

            let max_split_count = max_split_count.and_then(|number| {
                let number = number.int_value();

                (number > 0).then_some(number as usize)
            });

            let ret = regex::split(&text, &regex, max_split_count);
            let arr = match ret {
                Ok(ret) => ret,
                Err(e) => {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidRegexSyntax,
                        Some(e.message()),
                        CodePosition::EMPTY,
                    );
                },
            };

            let arr = arr.into_iter().
                    map(|ele| DataObjectRef::new(DataObject::new_text(ele))).
                    collect::<Box<_>>();

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(arr)
            }).unwrap())
        }
    }
}
