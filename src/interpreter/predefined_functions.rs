use std::rc::Rc;
use ahash::AHashMap;
use crate::interpreter::data::function::FunctionPointerObject;
use crate::interpreter::data::FunctionPointerObjectRef;

pub fn add_predefined_functions(funcs: &mut AHashMap<Box<str>, FunctionPointerObjectRef>) {
    let mut functions = Vec::new();

    reset_functions::add_functions(&mut functions);
    error_functions::add_functions(&mut functions);
    lang_functions::add_functions(&mut functions);
    system_functions::add_functions(&mut functions);
    io_functions::add_functions(&mut functions);
    number_functions::add_functions(&mut functions);
    text_functions::add_functions(&mut functions);
    conversion_functions::add_functions(&mut functions);
    operation_functions::add_functions(&mut functions);
    math_functions::add_functions(&mut functions);
    combinator_functions::add_functions(&mut functions);
    func_ptr_functions::add_functions(&mut functions);
    byte_buffer_functions::add_functions(&mut functions);
    array_functions::add_functions(&mut functions);
    list_functions::add_functions(&mut functions);
    struct_functions::add_functions(&mut functions);
    //TODO module functions
    lang_test_functions::add_functions(&mut functions);

    let functions = FunctionPointerObject::create_function_pointer_objects_from_native_functions(functions);
    for (function_name, functions) in functions {
        funcs.insert(function_name, Rc::new(functions));
    }
}

pub fn add_predefined_linker_functions(funcs: &mut AHashMap<Box<str>, FunctionPointerObjectRef>) {
    let mut functions = Vec::new();

    linker_functions::add_functions(&mut functions);

    let functions = FunctionPointerObject::create_function_pointer_objects_from_native_functions(functions);
    for (function_name, functions) in functions {
        funcs.insert(function_name, Rc::new(functions));
    }
}

mod reset_functions {
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::data::{DataObjectRef, OptionDataObjectRef};
    use crate::interpreter::{Interpreter, InterpretingError};

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            free_var_function,
            crate::lang_func_metadata!(
                name="freeVar",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$pointer",
                    type_constraint(
                        allowed=["VAR_POINTER"],
                    ),
                ),
            ),
        ));
        fn free_var_function(
            interpreter: &mut Interpreter,
            pointer_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            let dereferenced_var_pointer = pointer_object.var_pointer_value();
            let Some(dereferenced_var_pointer) = dereferenced_var_pointer else {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::InvalidArguments,
                ));
            };

            let variable_name = dereferenced_var_pointer.variable_name();
            let Some(variable_name) = variable_name else {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::InvalidArguments,
                ));
            };

            if dereferenced_var_pointer.is_final_data() || dereferenced_var_pointer.is_final_data() {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::InvalidArguments,
                ));
            }

            interpreter.data_mut().var.remove(&*variable_name);

            None
        }

        functions.push(crate::lang_func!(
            free_all_vars_function,
            crate::lang_func_metadata!(
                name="freeAllVars",
                return_type_constraint(
                    allowed=["VOID"],
                ),
            ),
        ));
        fn free_all_vars_function(
            interpreter: &mut Interpreter,
        ) {
            interpreter.reset_vars();
        }

        functions.push(crate::lang_func!(
            reset_errno_function,
            crate::lang_func_metadata!(
                name="resetErrno",
                return_type_constraint(
                    allowed=["VOID"],
                ),
            ),
        ));
        fn reset_errno_function(
            interpreter: &mut Interpreter,
        ) {
            interpreter.get_and_clear_errno_error_object();
        }
    }
}

mod error_functions {
    use std::rc::Rc;
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::data::{DataObject, DataObjectRef, ErrorObject};
    use crate::interpreter::{conversions, Interpreter};
    use crate::lexer::CodePosition;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            get_error_text_function,
            crate::lang_func_metadata!(
                name="getErrorText",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
            ),
        ));
        fn get_error_text_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::new_text(
                interpreter.get_and_clear_errno_error_object().error_text(),
            ))
        }

        functions.push(crate::lang_func!(
            with_error_message_function,
            crate::lang_func_metadata!(
                name="withErrorMessage",
                return_type_constraint(
                    allowed=["ERROR"],
                ),
                parameter(
                    name="$error",
                    type_constraint(
                        allowed=["ERROR"],
                    ),
                ),
                parameter(
                    name="$text",
                ),
            ),
        ));
        fn with_error_message_function(
            interpreter: &mut Interpreter,
            error_object: DataObjectRef,
            text_object: DataObjectRef,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_error(Rc::new(ErrorObject::new(
                    error_object.error_value().unwrap().err(),
                    Some(&conversions::to_text(interpreter, &text_object, CodePosition::EMPTY))
                )))
            }).unwrap())
        }
    }
}

mod lang_functions {
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::data::{DataObject, DataObjectRef};
    use crate::interpreter::{Interpreter, InterpretingError};
    use crate::lexer::CodePosition;
    use crate::utils;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            is_lang_version_newer_function,
            crate::lang_func_metadata!(
                name="isLangVersionNewer",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
        fn is_lang_version_newer_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            //If lang.version = null -> return false
            let comp_ver = {
                let data = interpreter.data_ref();
                let lang_ver = data.lang.get("lang.version").
                        map(|str| &**str).
                        unwrap_or(Interpreter::VERSION);

                utils::compare_versions_str(Interpreter::VERSION, lang_ver)
            };

            let Some(comp_ver) = comp_ver else {
                return interpreter.set_errno_error_object(
                    InterpretingError::LangVerError,
                    Some("lang.version has an invalid format"),
                    CodePosition::EMPTY,
                );
            };

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(comp_ver.is_gt())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            is_lang_version_older_function,
            crate::lang_func_metadata!(
                name="isLangVersionOlder",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
        fn is_lang_version_older_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            //If lang.version = null -> return false
            let comp_ver = {
                let data = interpreter.data_ref();
                let lang_ver = data.lang.get("lang.version").
                        map(|str| &**str).
                        unwrap_or(Interpreter::VERSION);

                utils::compare_versions_str(Interpreter::VERSION, lang_ver)
            };

            let Some(comp_ver) = comp_ver else {
                return interpreter.set_errno_error_object(
                    InterpretingError::LangVerError,
                    Some("lang.version has an invalid format"),
                    CodePosition::EMPTY,
                );
            };

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(comp_ver.is_lt())
            }).unwrap())
        }
    }
}

mod system_functions {
    use std::rc::Rc;
    use std::{ptr, thread};
    use std::ops::Deref;
    use std::time::{Duration, SystemTime, UNIX_EPOCH};
    use crate::interpreter::data::function::{native, Function, FunctionMetadata};
    use crate::interpreter::data::{DataObject, DataObjectRef, DataTypeConstraint, OptionDataObjectRef, StructObject};
    use crate::interpreter::{conversions, Interpreter, InterpretingError, StackElement};
    use crate::interpreter::data::function::native::NativeError;
    use crate::lexer::CodePosition;
    use crate::utils;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            sleep_function,
            crate::lang_func_metadata!(
                name="sleep",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$milliSeconds",
                    parameter_type(number),
                ),
            ),
        ));
        fn sleep_function(
            interpreter: &mut Interpreter,
            milli_seconds: DataObjectRef,
        ) -> OptionDataObjectRef {
            let milli_seconds = milli_seconds.number_value().unwrap();
            let milli_seconds = milli_seconds.long_value();
            if milli_seconds < 0 {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"$milliSeconds\") must be >= 0"),
                    CodePosition::EMPTY,
                ));
            }

            thread::sleep(Duration::from_millis(milli_seconds as u64));

            None
        }

        functions.push(crate::lang_func!(
            nano_time_function,
            crate::lang_func_metadata!(
                name="nanoTime",
                return_type_constraint(
                    allowed=["LONG"],
                ),
            ),
        ));
        fn nano_time_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            let nano_time = interpreter.origin_time.elapsed();
            let nano_time = (nano_time.as_nanos() as u64 & (i64::MAX as u64)) as i64;

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_long(nano_time)
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            current_time_millis_function,
            crate::lang_func_metadata!(
                name="currentTimeMillis",
                return_type_constraint(
                    allowed=["LONG"],
                ),
            ),
        ));
        fn current_time_millis_function(
            _: &mut Interpreter,
        ) -> native::Result<DataObjectRef> {
            let current_time_millis = SystemTime::now().
                    duration_since(UNIX_EPOCH).map_err(NativeError::apply)?;
            let current_time_millis = current_time_millis.as_millis() as u64 as i64;

            Ok(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_long(current_time_millis)
            })?))
        }

        functions.push(crate::lang_func!(
            current_unix_time_function,
            crate::lang_func_metadata!(
                name="currentUnixTime",
                return_type_constraint(
                    allowed=["LONG"],
                ),
            ),
        ));
        fn current_unix_time_function(
            _: &mut Interpreter,
        ) -> native::Result<DataObjectRef> {
            let current_time_millis = SystemTime::now().
                    duration_since(UNIX_EPOCH).map_err(NativeError::apply)?;
            let current_time_millis = current_time_millis.as_secs() as i64;

            Ok(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_long(current_time_millis)
            })?))
        }

        functions.push(crate::lang_func!(
            get_translation_value_function,
            crate::lang_func_metadata!(
                name="getTranslationValue",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$translationKey",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn get_translation_value_function(
            interpreter: &mut Interpreter,
            translation_key_object: DataObjectRef,
        ) -> DataObjectRef {
            let translation_key = conversions::to_text(
                interpreter,
                &translation_key_object,
                CodePosition::EMPTY
            );

            let translation_value = interpreter.data_ref().lang.get(&*translation_key).cloned();
            let Some(translation_value) = translation_value else {
                return interpreter.set_errno_error_object_error_only(InterpretingError::TransKeyNotFound);
            };

            DataObjectRef::new(DataObject::new_text(translation_value))
        }

        functions.push(crate::lang_func!(
            get_translation_value_template_pluralization_function,
            crate::lang_func_metadata!(
                name="getTranslationValueTemplatePluralization",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$count",
                    parameter_type(number),
                ),
                parameter(
                    name="$translationKey",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn get_translation_value_template_pluralization_function(
            interpreter: &mut Interpreter,
            count: DataObjectRef,
            translation_key_object: DataObjectRef,
        ) -> DataObjectRef {
            let count = count.number_value().unwrap();
            let count = count.int_value();
            if count < 0 {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"$count\") must be >= 0"),
                    CodePosition::EMPTY,
                );
            }

            let translation_key = conversions::to_text(
                interpreter,
                &translation_key_object,
                CodePosition::EMPTY
            );

            let translation_value = interpreter.data_ref().lang.get(&*translation_key).cloned();
            let Some(translation_value) = translation_value else {
                return interpreter.set_errno_error_object_error_only(InterpretingError::TransKeyNotFound);
            };

            let translation_value = utils::format_translation_template_pluralization(
                &translation_value, count,
            );
            match translation_value {
                Ok(translation_value) => {
                    DataObjectRef::new(DataObject::new_text(translation_value))
                },

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
            make_final_function,
            crate::lang_func_metadata!(
                name="makeFinal",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$ptr",
                    type_constraint(
                        allowed=["VAR_POINTER"],
                    ),
                ),
            ),
        ));
        fn make_final_function(
            interpreter: &mut Interpreter,
            pointer_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            let dereferenced_var_pointer = pointer_object.var_pointer_value();
            let Some(dereferenced_var_pointer) = dereferenced_var_pointer else {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::InvalidArguments,
                ));
            };

            let variable_name = dereferenced_var_pointer.variable_name();
            if variable_name.is_none() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Anonymous values can not be modified"),
                    CodePosition::EMPTY,
                ));
            };

            if dereferenced_var_pointer.is_lang_var() {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::FinalVarChange,
                ));
            }

            dereferenced_var_pointer.borrow_mut().set_final_data(true);

            None
        }

        functions.push(crate::lang_func!(
            as_final_function,
            crate::lang_func_metadata!(
                name="asFinal",
                parameter(
                    name="$value",
                ),
            ),
        ));
        fn as_final_function(
            _: &mut Interpreter,
            value_object: DataObjectRef,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_copy_static_and_final_modifiers(true).
                        set_static_data(true).
                        set_data(&value_object.borrow())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            is_final_function,
            crate::lang_func_metadata!(
                name="isFinal",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$value",
                    parameter_type(call_by_pointer),
                ),
            ),
        ));
        fn is_final_function(
            interpreter: &mut Interpreter,
            pointer_object: DataObjectRef,
        ) -> DataObjectRef {
            let dereferenced_var_pointer = pointer_object.var_pointer_value();
            let Some(dereferenced_var_pointer) = dereferenced_var_pointer else {
                return interpreter.set_errno_error_object_error_only(
                    InterpretingError::InvalidArguments,
                );
            };

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(dereferenced_var_pointer.is_final_data())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            make_static_function,
            crate::lang_func_metadata!(
                name="makeStatic",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$ptr",
                    type_constraint(
                        allowed=["VAR_POINTER"],
                    ),
                ),
            ),
        ));
        fn make_static_function(
            interpreter: &mut Interpreter,
            pointer_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            let dereferenced_var_pointer = pointer_object.var_pointer_value();
            let Some(dereferenced_var_pointer) = dereferenced_var_pointer else {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::InvalidArguments,
                ));
            };

            let variable_name = dereferenced_var_pointer.variable_name();
            if variable_name.is_none() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Anonymous values can not be modified"),
                    CodePosition::EMPTY,
                ));
            };

            if dereferenced_var_pointer.is_lang_var() {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::FinalVarChange,
                ));
            }

            dereferenced_var_pointer.borrow_mut().set_static_data(true);

            None
        }

        functions.push(crate::lang_func!(
            as_static_function,
            crate::lang_func_metadata!(
                name="asStatic",
                parameter(
                    name="$value",
                ),
            ),
        ));
        fn as_static_function(
            _: &mut Interpreter,
            value_object: DataObjectRef,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_copy_static_and_final_modifiers(true).
                        set_static_data(true).
                        set_data(&value_object.borrow())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            is_static_function,
            crate::lang_func_metadata!(
                name="isStatic",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$value",
                    parameter_type(call_by_pointer),
                ),
            ),
        ));
        fn is_static_function(
            interpreter: &mut Interpreter,
            pointer_object: DataObjectRef,
        ) -> DataObjectRef {
            let dereferenced_var_pointer = pointer_object.var_pointer_value();
            let Some(dereferenced_var_pointer) = dereferenced_var_pointer else {
                return interpreter.set_errno_error_object_error_only(
                    InterpretingError::InvalidArguments,
                );
            };

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(dereferenced_var_pointer.is_static_data())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            constrain_variable_allowed_types_function,
            crate::lang_func_metadata!(
                name="constrainVariableAllowedTypes",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$ptr",
                    type_constraint(
                        allowed=["VAR_POINTER"],
                    ),
                ),
                parameter(
                    name="&types",
                    parameter_type(var_args),
                    type_constraint(
                        allowed=["TYPE"],
                    ),
                ),
            ),
        ));
        fn constrain_variable_allowed_types_function(
            interpreter: &mut Interpreter,
            pointer_object: DataObjectRef,
            type_objects: Vec<DataObjectRef>,
        ) -> OptionDataObjectRef {
            let dereferenced_var_pointer = pointer_object.var_pointer_value();
            let Some(dereferenced_var_pointer) = dereferenced_var_pointer else {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::InvalidArguments,
                ));
            };

            let variable_name = dereferenced_var_pointer.variable_name();
            if variable_name.is_none() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Anonymous values can not be modified"),
                    CodePosition::EMPTY,
                ));
            };

            if dereferenced_var_pointer.is_lang_var() {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::FinalVarChange,
                ));
            }

            let types = type_objects.iter().
                    map(|type_value| type_value.type_value().unwrap()).
                    collect::<Box<_>>();

            let ret = dereferenced_var_pointer.borrow_mut().
                    set_type_constraint(Box::new(DataTypeConstraint::from_allowed_types(&types))).err();
            if let Some(e) = ret {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(e.message()),
                    CodePosition::EMPTY,
                ));
            }

            None
        }

        functions.push(crate::lang_func!(
            constrain_variable_not_allowed_types_function,
            crate::lang_func_metadata!(
                name="constrainVariableNotAllowedTypes",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$ptr",
                    type_constraint(
                        allowed=["VAR_POINTER"],
                    ),
                ),
                parameter(
                    name="&types",
                    parameter_type(var_args),
                    type_constraint(
                        allowed=["TYPE"],
                    ),
                ),
            ),
        ));
        fn constrain_variable_not_allowed_types_function(
            interpreter: &mut Interpreter,
            pointer_object: DataObjectRef,
            type_objects: Vec<DataObjectRef>,
        ) -> OptionDataObjectRef {
            let dereferenced_var_pointer = pointer_object.var_pointer_value();
            let Some(dereferenced_var_pointer) = dereferenced_var_pointer else {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::InvalidArguments,
                ));
            };

            let variable_name = dereferenced_var_pointer.variable_name();
            if variable_name.is_none() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Anonymous values can not be modified"),
                    CodePosition::EMPTY,
                ));
            };

            if dereferenced_var_pointer.is_lang_var() {
                return Some(interpreter.set_errno_error_object_error_only(
                    InterpretingError::FinalVarChange,
                ));
            }

            let types = type_objects.iter().
                    map(|type_value| type_value.type_value().unwrap()).
                    collect::<Box<_>>();

            let ret = dereferenced_var_pointer.borrow_mut().
                    set_type_constraint(Box::new(DataTypeConstraint::from_not_allowed_types(&types))).err();
            if let Some(e) = ret {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(e.message()),
                    CodePosition::EMPTY,
                ));
            }

            None
        }

        functions.push(crate::lang_func!(
            exec_function,
            crate::lang_func_metadata!(
                name="exec",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn exec_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            let lines = &conversions::to_text(
                interpreter,
                &text_object,
                CodePosition::EMPTY
            );

            //Update call stack
            let current_stack_element = interpreter.current_call_stack_element();
            let new_stack_element = StackElement::new(
                current_stack_element.lang_path(),
                current_stack_element.lang_file(),
                current_stack_element.lang_class().cloned(),
                current_stack_element.lang_class_name(),
                Some("<exec-code>"),
                current_stack_element.module(),
            );
            interpreter.push_stack_element(new_stack_element, CodePosition::EMPTY);

            let original_line_number = interpreter.parser_line_number();

            interpreter.reset_parser_positional_vars();
            interpreter.interpret_lines(&**lines);

            interpreter.set_parser_line_number(original_line_number);

            //Get returned value from executed Lang file
            let ret_tmp = interpreter.get_and_reset_return_value();

            //Update call stack
            interpreter.pop_stack_element();

            ret_tmp
        }

        functions.push(crate::lang_func!(
            is_terminal_available_function,
            crate::lang_func_metadata!(
                name="isTerminalAvailable",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
        fn is_terminal_available_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(interpreter.term.is_some())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            is_callable_function,
            crate::lang_func_metadata!(
                name="isCallable",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
        fn is_callable_function(
            _: &mut Interpreter,
            value_object: DataObjectRef,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(utils::is_callable(&value_object))
            }).unwrap())
        }

        {
            functions.push(crate::lang_func!(
                is_instance_of_function,
                crate::lang_func_metadata!(
                    name="isInstanceOf",
                    has_info=true,
                    return_type_constraint(
                        allowed=["INT"],
                    ),
                    parameter(
                        name="$value",
                    ),
                    parameter(
                        name="$type",
                        type_constraint(
                            allowed=["TYPE"],
                        ),
                    ),
                ),
            ));
            fn is_instance_of_function(
                _: &mut Interpreter,
                value_object: DataObjectRef,
                type_object: DataObjectRef,
            ) -> DataObjectRef {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(value_object.data_type() == type_object.type_value().unwrap())
                }).unwrap())
            }

            functions.push(crate::lang_func!(
                is_instance_of_with_struct_parameters_function,
                crate::lang_func_metadata!(
                    name="isInstanceOf",
                    return_type_constraint(
                        allowed=["INT"],
                    ),
                    parameter(
                        name="$value",
                        type_constraint(
                            allowed=["STRUCT"],
                        ),
                    ),
                    parameter(
                        name="$type",
                        type_constraint(
                            allowed=["STRUCT"],
                        ),
                    ),
                ),
            ));
            fn is_instance_of_with_struct_parameters_function(
                interpreter: &mut Interpreter,
                value_object: DataObjectRef,
                type_object: DataObjectRef,
            ) -> DataObjectRef {
                let value_struct = value_object.struct_value().unwrap();
                let type_struct = type_object.struct_value().unwrap();

                if !type_struct.is_definition() {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some("Argument 2 (\"$type\") must be a definition struct"),
                        CodePosition::EMPTY,
                    );
                }

                if value_struct.is_definition() {
                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_bool(false)
                    }).unwrap())
                }else {
                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        //Check for same reference only
                        data_object.set_bool(ptr::eq(value_struct.base_definition().unwrap().deref(), type_struct.deref()))
                    }).unwrap())
                }
            }

            functions.push(crate::lang_func!(
                is_instance_of_with_struct_type_parameter_function,
                crate::lang_func_metadata!(
                    name="isInstanceOf",
                    return_type_constraint(
                        allowed=["INT"],
                    ),
                    parameter(
                        name="$value",
                    ),
                    parameter(
                        name="$type",
                        type_constraint(
                            allowed=["STRUCT"],
                        ),
                    ),
                ),
            ));
            fn is_instance_of_with_struct_type_parameter_function(
                interpreter: &mut Interpreter,
                _: DataObjectRef,
                type_object: DataObjectRef,
            ) -> DataObjectRef {
                let type_struct = type_object.struct_value().unwrap();

                if !type_struct.is_definition() {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some("Argument 2 (\"$type\") must be a definition struct"),
                        CodePosition::EMPTY,
                    );
                }

                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(false)
                }).unwrap())
            }

            functions.push(crate::lang_func!(
                is_instance_of_with_object_parameters_function,
                crate::lang_func_metadata!(
                    name="isInstanceOf",
                    return_type_constraint(
                        allowed=["INT"],
                    ),
                    parameter(
                        name="$value",
                        type_constraint(
                            allowed=["OBJECT"],
                        ),
                    ),
                    parameter(
                        name="$type",
                        type_constraint(
                            allowed=["OBJECT"],
                        ),
                    ),
                ),
            ));
            fn is_instance_of_with_object_parameters_function(
                interpreter: &mut Interpreter,
                value_object: DataObjectRef,
                type_object: DataObjectRef,
            ) -> DataObjectRef {
                let lang_object = value_object.object_value().unwrap();
                let type_object = type_object.object_value().unwrap();

                if !type_object.borrow().is_class() {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some("Argument 2 (\"$type\") must be a class"),
                        CodePosition::EMPTY,
                    );
                }

                if lang_object.borrow().is_class() {
                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_bool(false)
                    }).unwrap())
                }else {
                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_bool(lang_object.borrow().is_instance_of(&type_object.borrow()))
                    }).unwrap())
                }
            }

            functions.push(crate::lang_func!(
                is_instance_of_with_object_type_parameter_function,
                crate::lang_func_metadata!(
                    name="isInstanceOf",
                    return_type_constraint(
                        allowed=["INT"],
                    ),
                    parameter(
                        name="$value",
                    ),
                    parameter(
                        name="$type",
                        type_constraint(
                            allowed=["OBJECT"],
                        ),
                    ),
                ),
            ));
            fn is_instance_of_with_object_type_parameter_function(
                interpreter: &mut Interpreter,
                _: DataObjectRef,
                type_object: DataObjectRef,
            ) -> DataObjectRef {
                let type_object = type_object.object_value().unwrap();

                if !type_object.borrow().is_class() {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some("Argument 2 (\"$type\") must be a class"),
                        CodePosition::EMPTY,
                    );
                }

                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_bool(false)
                }).unwrap())
            }
        }

        functions.push(crate::lang_func!(
            type_of_function,
            crate::lang_func_metadata!(
                name="typeOf",
                return_type_constraint(
                    allowed=["TYPE"],
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
        fn type_of_function(
            _: &mut Interpreter,
            value_object: DataObjectRef,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_type(value_object.data_type())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            get_current_stack_trace_element_function,
            crate::lang_func_metadata!(
                name="getCurrentStackTraceElement",
                return_type_constraint(
                    allowed=["STRUCT"],
                ),
            ),
        ));
        fn get_current_stack_trace_element_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            let current_stack_element = &interpreter.call_stack[interpreter.call_stack.len() - 1];

            let module_path: Option<String> = None;
            let module_file: Option<String> = None;
            if let Some(module) = current_stack_element.module() {
                todo!("Implement modules: {module:?}");
            }

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_struct(Rc::new(StructObject::new_instance(
                    interpreter.standard_types["&StackTraceElement"].struct_value().unwrap(),
                    &[
                        DataObjectRef::new(DataObject::new_text(current_stack_element.lang_path())),
                        DataObjectRef::new(DataObject::new_optional_text(current_stack_element.lang_file())),
                        DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_struct(Rc::new(StructObject::new_instance(
                                interpreter.standard_types["&CodePosition"].struct_value().unwrap(),
                                &[
                                    DataObjectRef::new(DataObject::new_number(current_stack_element.pos().line_number_from() as i32)),
                                    DataObjectRef::new(DataObject::new_number(current_stack_element.pos().line_number_to() as i32)),
                                    DataObjectRef::new(DataObject::new_number(current_stack_element.pos().column_from() as i32)),
                                    DataObjectRef::new(DataObject::new_number(current_stack_element.pos().column_to() as i32)),
                                ],
                            )?))
                        })?),
                        DataObjectRef::new(DataObject::new_optional_text(current_stack_element.lang_class_name())),
                        DataObjectRef::new(DataObject::new_optional_text(current_stack_element.lang_function_name())),
                        DataObjectRef::new(DataObject::new_optional_text(module_path)),
                        DataObjectRef::new(DataObject::new_optional_text(module_file))
                    ],
                )?))
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            get_stack_trace_elements_element_function,
            crate::lang_func_metadata!(
                name="getStackTraceElements",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
            ),
        ));
        fn get_stack_trace_elements_element_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            let stack_trace_elements = interpreter.call_stack_elements();
            let array = stack_trace_elements.iter().
                    map(|ele| {
                        let module_path: Option<String> = None;
                        let module_file: Option<String> = None;
                        if let Some(module) = ele.module() {
                            todo!("Implement modules: {module:?}");
                        }

                        DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_struct(Rc::new(StructObject::new_instance(
                                interpreter.standard_types["&StackTraceElement"].struct_value().unwrap(),
                                &[
                                    DataObjectRef::new(DataObject::new_text(ele.lang_path())),
                                    DataObjectRef::new(DataObject::new_optional_text(ele.lang_file())),
                                    DataObjectRef::new(DataObject::with_update(|data_object| {
                                        data_object.set_struct(Rc::new(StructObject::new_instance(
                                            interpreter.standard_types["&CodePosition"].struct_value().unwrap(),
                                            &[
                                                DataObjectRef::new(DataObject::new_number(ele.pos().line_number_from() as i32)),
                                                DataObjectRef::new(DataObject::new_number(ele.pos().line_number_to() as i32)),
                                                DataObjectRef::new(DataObject::new_number(ele.pos().column_from() as i32)),
                                                DataObjectRef::new(DataObject::new_number(ele.pos().column_to() as i32)),
                                            ],
                                        )?))
                                    })?),
                                    DataObjectRef::new(DataObject::new_optional_text(ele.lang_class_name())),
                                    DataObjectRef::new(DataObject::new_optional_text(ele.lang_function_name())),
                                    DataObjectRef::new(DataObject::new_optional_text(module_path)),
                                    DataObjectRef::new(DataObject::new_optional_text(module_file))
                                ],
                            )?))
                        }).unwrap())
                    }).
                    collect::<Box<_>>();

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(array)
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            get_stack_trace_function,
            crate::lang_func_metadata!(
                name="getStackTrace",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
            ),
        ));
        fn get_stack_trace_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::new_text(interpreter.print_stack_trace(CodePosition::EMPTY)))
        }
    }
}

mod io_functions {
    use std::io::{stdin, Read};
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::data::{DataObject, DataObjectRef, DataType, Number, OptionDataObjectRef};
    use crate::interpreter::{conversions, Interpreter, InterpretingError};
    use crate::lexer::CodePosition;
    use crate::terminal_io::Level;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            read_terminal_function,
            crate::lang_func_metadata!(
                name="readTerminal",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$message",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn read_terminal_function(
            interpreter: &mut Interpreter,
            message_object: DataObjectRef,
        ) -> DataObjectRef {
            if interpreter.term.is_none() && !interpreter.execution_flags.allow_term_redirect {
                return interpreter.set_errno_error_object_error_only(InterpretingError::NoTerminal);
            }

            let message = conversions::to_text(interpreter, &message_object, CodePosition::EMPTY);
            if interpreter.term.is_none() {
                interpreter.set_errno(InterpretingError::NoTerminalWarning, None, CodePosition::EMPTY);

                if !message.is_empty() {
                    println!("{}", message);
                }
                print!("Input: ");

                let mut line = String::new();
                let count = stdin().read_line(&mut line);
                let count = match count {
                    Ok(count) => count,

                    Err(e) => {
                        return interpreter.set_errno_error_object(
                            InterpretingError::SystemError,
                            Some(&format!("{e}")),
                            CodePosition::EMPTY,
                        );
                    },
                };

                if count == 0 {
                    return interpreter.set_errno_error_object_error_only(
                        InterpretingError::SystemError,
                    );
                }

                //Remove trailing newlines
                let line = line.trim_end_matches(['\n', '\r']);

                DataObjectRef::new(DataObject::new_text(line))
            }else {
                let ret = interpreter.platform_api.show_input_dialog(&message);
                match ret {
                    Ok(ret) => {
                        DataObjectRef::new(DataObject::new_text(ret))
                    },

                    Err(_) => {
                        interpreter.set_errno_error_object_error_only(InterpretingError::FunctionNotSupported)
                    },
                }
            }
        }

        functions.push(crate::lang_func!(
            print_terminal_function,
            crate::lang_func_metadata!(
                name="printTerminal",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$logLevel",
                    parameter_type(number),
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn print_terminal_function(
            interpreter: &mut Interpreter,
            log_level_number: DataObjectRef,
            text_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            if interpreter.term.is_none() && !interpreter.execution_flags.allow_term_redirect {
                return Some(interpreter.set_errno_error_object_error_only(InterpretingError::NoTerminal));
            }

            let log_level_number = log_level_number.number_value().unwrap();

            let log_level = log_level_number.int_value();
            let mut level = None;
            for lvl in Level::VALUES {
                if lvl.level() as i32 == log_level {
                    level = Some(lvl);

                    break;
                }
            }

            let Some(level) = level else {
                return Some(interpreter.set_errno_error_object_error_only(InterpretingError::InvalidLogLevel));
            };

            let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

            if let Some(term) = &mut interpreter.term {
                term.logln(level, format!("[From Lang file]: {text}"), Interpreter::LOG_TAG);
            }else {
                interpreter.set_errno(InterpretingError::NoTerminalWarning, None, CodePosition::EMPTY);

                //Write to standard error if the log level is WARNING or higher
                if log_level > 3 {
                    eprintln!("[{:<8}]: {text}", level.name());
                }else {
                    println!("[{:<8}]: {text}", level.name());
                }
            }

            None
        }

        functions.push(crate::lang_func!(
            print_error_function,
            crate::lang_func_metadata!(
                name="printError",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn print_error_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            if interpreter.term.is_none() && !interpreter.execution_flags.allow_term_redirect {
                return Some(interpreter.set_errno_error_object_error_only(InterpretingError::NoTerminal));
            }

            let error = interpreter.get_and_clear_errno_error_object();
            let errno = error.error_code();
            let level = match errno {
                0 => Level::Info,
                1.. => Level::Error,
                ..0 => Level::Warning,
            };

            let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
            let text = if text.is_empty() { "" } else { &format!("{text}: ") };

            if let Some(term) = &mut interpreter.term {
                term.logln(level, format!("[From Lang file]: {text}"), Interpreter::LOG_TAG);
            }else {
                interpreter.set_errno(InterpretingError::NoTerminalWarning, None, CodePosition::EMPTY);

                //Write to standard error if the log level is WARNING or higher
                if level.level() > 3 {
                    eprintln!("[{:<8}]: {text}", level.name());
                }else {
                    println!("[{:<8}]: {text}", level.name());
                }
            }

            None
        }

        {
            functions.push(crate::lang_func!(
                input_without_limit_function,
                crate::lang_func_metadata!(
                    name="input",
                    has_info=true,
                    return_type_constraint(
                        allowed=["TEXT"],
                    ),
                ),
            ));
            fn input_without_limit_function(
                interpreter: &mut Interpreter,
            ) -> DataObjectRef {
                input_internal_function(interpreter, None)
            }

            functions.push(crate::lang_func!(
                input_with_limit_function,
                crate::lang_func_metadata!(
                    name="input",
                    return_type_constraint(
                        allowed=["TEXT"],
                    ),
                    parameter(
                        name="$maxByteCount",
                        parameter_type(number),
                    ),
                ),
            ));

            fn input_with_limit_function(
                interpreter: &mut Interpreter,
                max_byte_count: DataObjectRef,
            ) -> DataObjectRef {
                input_internal_function(interpreter, Some(max_byte_count.number_value().unwrap()))
            }

            fn input_internal_function(
                interpreter: &mut Interpreter,
                max_byte_count: Option<Number>,
            ) -> DataObjectRef {
                let max_byte_count = max_byte_count.map(Number::int_value);

                if max_byte_count.is_some_and(|max_byte_count| max_byte_count < 0) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some("Argument 1 (\"$maxByteCount\") must be >= 0"),
                        CodePosition::EMPTY,
                    );
                }

                if let Some(max_byte_count) = max_byte_count {
                    let mut buf = Vec::with_capacity(max_byte_count as usize);
                    let count = stdin().read(&mut buf);
                    let count = match count {
                        Ok(count) => count,

                        Err(e) => {
                            return interpreter.set_errno_error_object(
                                InterpretingError::SystemError,
                                Some(&format!("{e}")),
                                CodePosition::EMPTY,
                            );
                        },
                    };

                    if count == 0 {
                        return interpreter.set_errno_error_object_error_only(
                            InterpretingError::SystemError,
                        );
                    }

                    DataObjectRef::new(DataObject::new_text(String::from_utf8_lossy(&buf)))
                }else {
                    let mut line = String::new();
                    let err = stdin().read_line(&mut line);
                    if let Err(e) = err {
                        return interpreter.set_errno_error_object(
                            InterpretingError::SystemError,
                            Some(&format!("{e}")),
                            CodePosition::EMPTY,
                        );
                    }

                    DataObjectRef::new(DataObject::new_text(line))
                }
            }
        }

        functions.push(crate::lang_func!(
            print_function,
            crate::lang_func_metadata!(
                name="print",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn print_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) {
            let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
            print!("{text}");
        }

        functions.push(crate::lang_func!(
            println_function,
            crate::lang_func_metadata!(
                name="println",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn println_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) {
            let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
            println!("{text}");
        }

        functions.push(crate::lang_func!(
            printf_function,
            crate::lang_func_metadata!(
                name="printf",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$format",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn printf_function(
            interpreter: &mut Interpreter,
            format_object: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> OptionDataObjectRef {
            let format = conversions::to_text(interpreter, &format_object, CodePosition::EMPTY);

            let out = interpreter.format_text(&format, &args);
            if out.data_type() == DataType::ERROR {
                return Some(out);
            }

            let out = conversions::to_text(interpreter, &out, CodePosition::EMPTY);

            print!("{out}");

            None
        }

        functions.push(crate::lang_func!(
            error_function,
            crate::lang_func_metadata!(
                name="error",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn error_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) {
            let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
            eprint!("{text}");
        }

        functions.push(crate::lang_func!(
            errorln_function,
            crate::lang_func_metadata!(
                name="errorln",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn errorln_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) {
            let text = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);
            eprintln!("{text}");
        }

        functions.push(crate::lang_func!(
            errorf_function,
            crate::lang_func_metadata!(
                name="errorf",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$format",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn errorf_function(
            interpreter: &mut Interpreter,
            format_object: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> OptionDataObjectRef {
            let format = conversions::to_text(interpreter, &format_object, CodePosition::EMPTY);

            let out = interpreter.format_text(&format, &args);
            if out.data_type() == DataType::ERROR {
                return Some(out);
            }

            let out = conversions::to_text(interpreter, &out, CodePosition::EMPTY);

            eprint!("{out}");

            None
        }
    }
}

mod number_functions {
    use std::str::FromStr;
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::{conversions, Interpreter, InterpretingError};
    use crate::interpreter::data::{DataObject, DataObjectRef, Number};
    use crate::lexer::CodePosition;
    use crate::utils::math::ToNumberBase;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            bin_to_dec_function,
            crate::lang_func_metadata!(
                name="binToDec",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$bin",
                ),
            ),
        ));
        fn bin_to_dec_function(
            interpreter: &mut Interpreter,
            bin_object: DataObjectRef,
        ) -> DataObjectRef {
            let bin_string = conversions::to_text(interpreter, &bin_object, CodePosition::EMPTY);
            if bin_string.starts_with("0b") && !bin_string.starts_with("0B") {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoBinNum,
                    Some("Wrong prefix (Should be 0b or 0B)"),
                    CodePosition::EMPTY,
                );
            }

            let number = i32::from_str_radix(&bin_string[2..], 2);
            match number {
                Ok(number) => {
                    DataObjectRef::new(DataObject::new_number(number))
                },

                Err(e) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::NoBinNum,
                        Some(&format!("{e}")),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            oct_to_dec_function,
            crate::lang_func_metadata!(
                name="octToDec",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$oct",
                ),
            ),
        ));
        fn oct_to_dec_function(
            interpreter: &mut Interpreter,
            oct_object: DataObjectRef,
        ) -> DataObjectRef {
            let oct_string = conversions::to_text(interpreter, &oct_object, CodePosition::EMPTY);
            if oct_string.starts_with("0o") && !oct_string.starts_with("0O") {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoOctNum,
                    Some("Wrong prefix (Should be 0o or 0O)"),
                    CodePosition::EMPTY,
                );
            }

            let number = i32::from_str_radix(&oct_string[2..], 8);
            match number {
                Ok(number) => {
                    DataObjectRef::new(DataObject::new_number(number))
                },

                Err(e) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::NoOctNum,
                        Some(&format!("{e}")),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            hex_to_dec_function,
            crate::lang_func_metadata!(
                name="hexToDec",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$hex",
                ),
            ),
        ));
        fn hex_to_dec_function(
            interpreter: &mut Interpreter,
            hex_object: DataObjectRef,
        ) -> DataObjectRef {
            let hex_string = conversions::to_text(interpreter, &hex_object, CodePosition::EMPTY);
            if hex_string.starts_with("0x") && !hex_string.starts_with("0X") {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoHexNum,
                    Some("Wrong prefix (Should be 0x or 0X)"),
                    CodePosition::EMPTY,
                );
            }

            let number = i32::from_str_radix(&hex_string[2..], 16);
            match number {
                Ok(number) => {
                    DataObjectRef::new(DataObject::new_number(number))
                },

                Err(e) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::NoHexNum,
                        Some(&format!("{e}")),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            to_number_base_function,
            crate::lang_func_metadata!(
                name="toNumberBase",
                return_type_constraint(
                    allowed=["INT", "LONG"],
                ),
                parameter(
                    name="$number",
                ),
                parameter(
                    name="$base",
                    parameter_type(number),
                ),
            ),
        ));
        fn to_number_base_function(
            interpreter: &mut Interpreter,
            number_object: DataObjectRef,
            base: DataObjectRef,
        ) -> DataObjectRef {
            let base = base.number_value().unwrap();

            if base.int_value() < 2 || base.int_value() > 36 {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidNumberBase,
                    Some("Argument 2 (\"$base\") must be between 2 (inclusive) and 36 (inclusive)"),
                    CodePosition::EMPTY,
                );
            }

            let number_string = conversions::to_text(interpreter, &number_object, CodePosition::EMPTY);

            let number = i32::from_str_radix(&number_string, base.int_value() as u32);
            match number {
                Ok(number) => {
                    DataObjectRef::new(DataObject::new_number(number))
                },

                Err(_) => {
                    let number = i64::from_str_radix(&number_string, base.int_value() as u32);
                    match number {
                        Ok(number) => {
                            DataObjectRef::new(DataObject::new_number(number))
                        },

                        Err(_) => {
                            interpreter.set_errno_error_object(
                                InterpretingError::NoBaseNNum,
                                Some(&format!(
                                    "Argument 1 (\"$number\" = \"{number_string}\") is not in base \"{}\"",
                                    base.int_value(),
                                )),
                                CodePosition::EMPTY,
                            )
                        },
                    }
                },
            }
        }

        functions.push(crate::lang_func!(
            to_text_base_function,
            crate::lang_func_metadata!(
                name="toTextBase",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
                parameter(
                    name="$base",
                    parameter_type(number),
                ),
            ),
        ));
        fn to_text_base_function(
            interpreter: &mut Interpreter,
            number: DataObjectRef,
            base: DataObjectRef,
        ) -> DataObjectRef {
            let base = base.number_value().unwrap();

            if base.int_value() < 2 || base.int_value() > 36 {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidNumberBase,
                    Some("Argument 2 (\"$base\") must be between 2 (inclusive) and 36 (inclusive)"),
                    CodePosition::EMPTY,
                );
            }

            let number = number.number_value().unwrap();

            let number_int = number.int_value();
            let number_long = number.long_value();

            let text = if number_long == number_int as i64 {
                number_int.to_number_base(base.int_value() as u32)
            }else {
                number_long.to_number_base(base.int_value() as u32)
            };

            DataObjectRef::new(DataObject::new_text(text))
        }

        functions.push(crate::lang_func!(
            to_int_bits_function,
            crate::lang_func_metadata!(
                name="toIntBits",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn to_int_bits_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap();

            DataObjectRef::new(DataObject::new_number(number.float_value().to_bits() as i32))
        }

        functions.push(crate::lang_func!(
            to_float_bits_function,
            crate::lang_func_metadata!(
                name="toFloatBits",
                return_type_constraint(
                    allowed=["FLOAT"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn to_float_bits_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap();

            DataObjectRef::new(DataObject::new_number(f32::from_bits(number.int_value() as u32)))
        }

        functions.push(crate::lang_func!(
            to_long_bits_function,
            crate::lang_func_metadata!(
                name="toLongBits",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn to_long_bits_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap();

            DataObjectRef::new(DataObject::new_number(number.double_value().to_bits() as i64))
        }

        functions.push(crate::lang_func!(
            to_double_bits_function,
            crate::lang_func_metadata!(
                name="toDoubleBits",
                return_type_constraint(
                    allowed=["DOUBLE"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn to_double_bits_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap();

            DataObjectRef::new(DataObject::new_number(f64::from_bits(number.int_value() as u64)))
        }

        functions.push(crate::lang_func!(
            ttoi_function,
            crate::lang_func_metadata!(
                name="ttoi",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$text",
                ),
            ),
        ));
        fn ttoi_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) -> DataObjectRef {
            let str = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

            let number = i32::from_str(&str);
            match number {
                Ok(number) => {
                    DataObjectRef::new(DataObject::new_number(number))
                },

                Err(_) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some("Argument 1 (\"$text\") can not be converted to an INT value"),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            ttol_function,
            crate::lang_func_metadata!(
                name="ttol",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="$text",
                ),
            ),
        ));
        fn ttol_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) -> DataObjectRef {
            let str = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

            let number = i64::from_str(&str);
            match number {
                Ok(number) => {
                    DataObjectRef::new(DataObject::new_number(number))
                },

                Err(_) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some("Argument 1 (\"$text\") can not be converted to a LONG value"),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            ttof_function,
            crate::lang_func_metadata!(
                name="ttof",
                return_type_constraint(
                    allowed=["FLOAT"],
                ),
                parameter(
                    name="$text",
                ),
            ),
        ));
        fn ttof_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) -> DataObjectRef {
            let str = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

            if str.contains(['x', 'X']) {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some("Argument 1 (\"$text\") can not be converted to a FLOAT value"),
                    CodePosition::EMPTY,
                );
            }

            let number = f32::from_str(&str);
            match number {
                Ok(number) => {
                    DataObjectRef::new(DataObject::new_number(number))
                },

                Err(_) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some("Argument 1 (\"$text\") can not be converted to a FLOAT value"),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            ttod_function,
            crate::lang_func_metadata!(
                name="ttod",
                return_type_constraint(
                    allowed=["DOUBLE"],
                ),
                parameter(
                    name="$text",
                ),
            ),
        ));
        fn ttod_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) -> DataObjectRef {
            let str = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

            if str.contains(['x', 'X']) {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some("Argument 1 (\"$text\") can not be converted to a DOUBLE value"),
                    CodePosition::EMPTY,
                );
            }

            let number = f64::from_str(&str);
            match number {
                Ok(number) => {
                    DataObjectRef::new(DataObject::new_number(number))
                },

                Err(_) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some("Argument 1 (\"$text\") can not be converted to a DOUBLE value"),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            is_nan_function,
            crate::lang_func_metadata!(
                name="isNaN",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn is_nan_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap();

            let is_nan = match number {
                Number::Float(number) => number.is_nan(),
                Number::Double(number) => number.is_nan(),

                _ => false,
            };

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(is_nan)
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            is_infinite_function,
            crate::lang_func_metadata!(
                name="isInfinite",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn is_infinite_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap();

            let is_nan = match number {
                Number::Float(number) => number.is_infinite(),
                Number::Double(number) => number.is_infinite(),

                _ => false,
            };

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(is_nan)
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            is_finite_function,
            crate::lang_func_metadata!(
                name="isFinite",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn is_finite_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap();

            let is_nan = match number {
                Number::Float(number) => number.is_finite(),
                Number::Double(number) => number.is_finite(),

                _ => false,
            };

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(is_nan)
            }).unwrap())
        }
    }
}

mod text_functions {
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
}

mod conversion_functions {
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::{conversions, Interpreter, InterpretingError};
    use crate::interpreter::data::{DataObject, DataObjectRef};
    use crate::lexer::CodePosition;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            number_function,
            crate::lang_func_metadata!(
                name="number",
                return_type_constraint(
                    allowed=["INT", "LONG", "FLOAT", "DOUBLE"],
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
        fn number_function(
            interpreter: &mut Interpreter,
            value_object: DataObjectRef,
        ) -> DataObjectRef {
            let ret = conversions::to_number(interpreter, &value_object, CodePosition::EMPTY);
            let Some(ret) = ret else {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"$value\") can not be converted to type number"),
                    CodePosition::EMPTY,
                );
            };

            DataObjectRef::new(DataObject::new_number(ret))
        }
    }
}

mod operation_functions {
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::{operators, Interpreter, InterpretingError};
    use crate::interpreter::data::DataObjectRef;
    use crate::lexer::CodePosition;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            iter_function,
            crate::lang_func_metadata!(
                name="iter",
                parameter(
                    name="$operand",
                ),
            ),
        ));
        fn iter_function(
            interpreter: &mut Interpreter,
            operand: DataObjectRef,
        ) -> DataObjectRef {
            let ret = operators::op_iter(interpreter, &operand, CodePosition::EMPTY);
            let Some(ret) = ret else {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!("The iter operator is not defined for {:?}", operand.data_type())),
                    CodePosition::EMPTY,
                );
            };

            ret
        }

        functions.push(crate::lang_func!(
            has_next_function,
            crate::lang_func_metadata!(
                name="hasNext",
                parameter(
                    name="$operand",
                ),
            ),
        ));
        fn has_next_function(
            interpreter: &mut Interpreter,
            operand: DataObjectRef,
        ) -> DataObjectRef {
            let ret = operators::op_has_next(interpreter, &operand, CodePosition::EMPTY);
            let Some(ret) = ret else {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!("The hasNext operator is not defined for {:?}", operand.data_type())),
                    CodePosition::EMPTY,
                );
            };

            ret
        }

        functions.push(crate::lang_func!(
            next_function,
            crate::lang_func_metadata!(
                name="next",
                parameter(
                    name="$operand",
                ),
            ),
        ));
        fn next_function(
            interpreter: &mut Interpreter,
            operand: DataObjectRef,
        ) -> DataObjectRef {
            let ret = operators::op_next(interpreter, &operand, CodePosition::EMPTY);
            let Some(ret) = ret else {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!("The next operator is not defined for {:?}", operand.data_type())),
                    CodePosition::EMPTY,
                );
            };

            ret
        }

        functions.push(crate::lang_func!(
            cast_function,
            crate::lang_func_metadata!(
                name="cast",
                parameter(
                    name="$leftSideOperand",
                ),
                parameter(
                    name="$rightSideOperand",
                ),
            ),
        ));
        fn cast_function(
            interpreter: &mut Interpreter,
            left_side_operand: DataObjectRef,
            right_side_operand: DataObjectRef,
        ) -> DataObjectRef {
            let ret = operators::op_cast(interpreter, &left_side_operand, &right_side_operand, CodePosition::EMPTY);
            let Some(ret) = ret else {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The cast operator is not defined for {:?} and {:?}",
                        left_side_operand.data_type(),
                        right_side_operand.data_type(),
                    )),
                    CodePosition::EMPTY,
                );
            };

            ret
        }

        functions.push(crate::lang_func!(
            call_function,
            crate::lang_func_metadata!(
                name="call",
                parameter(
                    name="$callee",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn call_function(
            interpreter: &mut Interpreter,
            callee: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let ret = operators::op_call(interpreter, &callee, &args, CodePosition::EMPTY);
            let Some(ret) = ret else {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Invalid arguments for the call operator."),
                    CodePosition::EMPTY,
                );
            };

            ret
        }
    }
}

mod math_functions {
    use rand::{Rng, RngCore, SeedableRng};
    use rand::rngs::SmallRng;
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::{conversions, operators, Interpreter, InterpretingError};
    use crate::interpreter::data::{DataObject, DataObjectRef};
    use crate::lexer::CodePosition;

    macro_rules! math_1_arg_function {
        ( $func_name:literal, $op:ident $(,)? ) => {{
            fn math_function(
                _: &mut Interpreter,
                number: DataObjectRef,
            ) -> DataObjectRef {
                let number = number.number_value().unwrap().double_value();

                DataObjectRef::new(DataObject::new_number(number.$op()))
            }

            crate::lang_func!(
                math_function,
                crate::lang_func_metadata!(
                    name=$func_name,
                    return_type_constraint(
                        allowed=["DOUBLE"],
                    ),
                    parameter(
                        name="$number",
                        parameter_type(number),
                    ),
                ),
            )
        }};
    }

    macro_rules! math_2_arg_function {
        ( $func_name:literal, $op:ident $(,)? ) => {{
            fn math_function(
                _: &mut Interpreter,
                left_number: DataObjectRef,
                right_number: DataObjectRef,
            ) -> DataObjectRef {
                let left_number = left_number.number_value().unwrap().double_value();
                let right_number = right_number.number_value().unwrap().double_value();

                DataObjectRef::new(DataObject::new_number(left_number.$op(right_number)))
            }

            crate::lang_func!(
                math_function,
                crate::lang_func_metadata!(
                    name=$func_name,
                    return_type_constraint(
                        allowed=["DOUBLE"],
                    ),
                    parameter(
                        name="$a",
                        parameter_type(number),
                    ),
                    parameter(
                        name="$b",
                        parameter_type(number),
                    ),
                ),
            )
        }};
    }

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            rand_function,
            crate::lang_func_metadata!(
                name="rand",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
        fn rand_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            let lang_rand_max = interpreter.data_ref().var["$LANG_RAND_MAX"].int_value().unwrap();

            DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0..lang_rand_max)))
        }

        functions.push(crate::lang_func!(
            randi_function,
            crate::lang_func_metadata!(
                name="randi",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
        fn randi_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::new_number(interpreter.ran.next_u32() as i32))
        }

        functions.push(crate::lang_func!(
            randl_function,
            crate::lang_func_metadata!(
                name="randl",
                return_type_constraint(
                    allowed=["LONG"],
                ),
            ),
        ));
        fn randl_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::new_number(interpreter.ran.next_u64() as i64))
        }

        functions.push(crate::lang_func!(
            randf_function,
            crate::lang_func_metadata!(
                name="randf",
                return_type_constraint(
                    allowed=["FLOAT"],
                ),
            ),
        ));
        fn randf_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0.0..1.0_f32)))
        }

        functions.push(crate::lang_func!(
            randd_function,
            crate::lang_func_metadata!(
                name="randd",
                return_type_constraint(
                    allowed=["DOUBLE"],
                ),
            ),
        ));
        fn randd_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0.0..1.0_f64)))
        }

        functions.push(crate::lang_func!(
            randb_function,
            crate::lang_func_metadata!(
                name="randb",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
        fn randb_function(
            interpreter: &mut Interpreter,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0..=1_i32)))
        }

        functions.push(crate::lang_func!(
            rand_range_function,
            crate::lang_func_metadata!(
                name="randRange",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$bound",
                    parameter_type(number),
                ),
            ),
        ));
        fn rand_range_function(
            interpreter: &mut Interpreter,
            bound: DataObjectRef,
        ) -> DataObjectRef {
            let bound = bound.number_value().unwrap();
            let bound = bound.int_value();
            if bound <= 0 {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"$bound\") must be positive"),
                    CodePosition::EMPTY,
                );
            }

            DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0..bound)))
        }

        {
            functions.push(crate::lang_func!(
                rand_choice_with_array_parameter_function,
                crate::lang_func_metadata!(
                    name="randChoice",
                    has_info=true,
                    parameter(
                        name="&arr",
                        type_constraint(
                            allowed=["ARRAY"],
                        ),
                    ),
                ),
            ));
            fn rand_choice_with_array_parameter_function(
                interpreter: &mut Interpreter,
                array_object: DataObjectRef,
            ) -> DataObjectRef {
                let arr = array_object.array_value().unwrap();
                if arr.borrow().is_empty() {
                    return DataObjectRef::new(DataObject::new_void());
                }

                let arr = arr.borrow();
                let index = interpreter.ran.gen_range(0..arr.len());

                arr[index].clone()
            }

            functions.push(crate::lang_func!(
                rand_choice_with_list_parameter_function,
                crate::lang_func_metadata!(
                    name="randChoice",
                    parameter(
                        name="&list",
                        type_constraint(
                            allowed=["LIST"],
                        ),
                    ),
                ),
            ));
            fn rand_choice_with_list_parameter_function(
                interpreter: &mut Interpreter,
                list_object: DataObjectRef,
            ) -> DataObjectRef {
                let list = list_object.list_value().unwrap();
                if list.borrow().is_empty() {
                    return DataObjectRef::new(DataObject::new_void());
                }

                let list = list.borrow();
                let index = interpreter.ran.gen_range(0..list.len());

                list[index].clone()
            }

            functions.push(crate::lang_func!(
                rand_choice_with_struct_parameter_function,
                crate::lang_func_metadata!(
                    name="randChoice",
                    parameter(
                        name="&struct",
                        type_constraint(
                            allowed=["STRUCT"],
                        ),
                    ),
                ),
            ));
            fn rand_choice_with_struct_parameter_function(
                interpreter: &mut Interpreter,
                struct_object: DataObjectRef,
            ) -> DataObjectRef {
                let struct_value = struct_object.struct_value().unwrap();
                let member_names = struct_value.member_names();
                if member_names.is_empty() {
                    return DataObjectRef::new(DataObject::new_void());
                }

                let index = interpreter.ran.gen_range(0..member_names.len());

                if struct_value.is_definition() {
                    DataObjectRef::new(DataObject::new_text(&*member_names[index]))
                }else {
                    struct_value.get_member(&member_names[index]).unwrap()
                }
            }

            functions.push(crate::lang_func!(
                rand_choice_function,
                crate::lang_func_metadata!(
                    name="randChoice",
                    parameter(
                        name="&args",
                        parameter_type(var_args),
                    ),
                ),
            ));
            fn rand_choice_function(
                interpreter: &mut Interpreter,
                args: Vec<DataObjectRef>,
            ) -> DataObjectRef {
                if args.is_empty() {
                    return DataObjectRef::new(DataObject::new_void());
                }

                let index = interpreter.ran.gen_range(0..args.len());

                args[index].clone()
            }
        }

        functions.push(crate::lang_func!(
            set_seed_function,
            crate::lang_func_metadata!(
                name="setSeed",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$seed",
                    parameter_type(number),
                ),
            ),
        ));
        fn set_seed_function(
            interpreter: &mut Interpreter,
            seed: DataObjectRef,
        ) {
            let seed = seed.number_value().unwrap();
            let seed = seed.long_value();

            interpreter.ran = SmallRng::seed_from_u64(seed as u64);
        }

        functions.push(crate::lang_func!(
            addi_function,
            crate::lang_func_metadata!(
                name="addi",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn addi_function(
            interpreter: &mut Interpreter,
            number_objects: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut sum = 0;

            for (i, number_object) in number_objects.iter().
                    enumerate() {
                let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
                let Some(number) = number else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some(&format!(
                            "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                sum += number.int_value();
            }

            DataObjectRef::new(DataObject::new_number(sum))
        }

        functions.push(crate::lang_func!(
            muli_function,
            crate::lang_func_metadata!(
                name="muli",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn muli_function(
            interpreter: &mut Interpreter,
            number_objects: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut prod = 1;

            for (i, number_object) in number_objects.iter().
                    enumerate() {
                let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
                let Some(number) = number else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some(&format!(
                            "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                prod *= number.int_value();
            }

            DataObjectRef::new(DataObject::new_number(prod))
        }

        functions.push(crate::lang_func!(
            addl_function,
            crate::lang_func_metadata!(
                name="addl",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn addl_function(
            interpreter: &mut Interpreter,
            number_objects: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut sum = 0;

            for (i, number_object) in number_objects.iter().
                    enumerate() {
                let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
                let Some(number) = number else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some(&format!(
                            "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                sum += number.long_value();
            }

            DataObjectRef::new(DataObject::new_number(sum))
        }

        functions.push(crate::lang_func!(
            mull_function,
            crate::lang_func_metadata!(
                name="mull",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn mull_function(
            interpreter: &mut Interpreter,
            number_objects: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut prod = 1;

            for (i, number_object) in number_objects.iter().
                    enumerate() {
                let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
                let Some(number) = number else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some(&format!(
                            "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                prod *= number.long_value();
            }

            DataObjectRef::new(DataObject::new_number(prod))
        }

        functions.push(crate::lang_func!(
            addf_function,
            crate::lang_func_metadata!(
                name="addf",
                return_type_constraint(
                    allowed=["FLOAT"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn addf_function(
            interpreter: &mut Interpreter,
            number_objects: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut sum = 0.0;

            for (i, number_object) in number_objects.iter().
                    enumerate() {
                let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
                let Some(number) = number else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some(&format!(
                            "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                sum += number.float_value();
            }

            DataObjectRef::new(DataObject::new_number(sum))
        }

        functions.push(crate::lang_func!(
            mulf_function,
            crate::lang_func_metadata!(
                name="mulf",
                return_type_constraint(
                    allowed=["FLOAT"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn mulf_function(
            interpreter: &mut Interpreter,
            number_objects: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut prod = 1.0;

            for (i, number_object) in number_objects.iter().
                    enumerate() {
                let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
                let Some(number) = number else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some(&format!(
                            "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                prod *= number.float_value();
            }

            DataObjectRef::new(DataObject::new_number(prod))
        }

        functions.push(crate::lang_func!(
            addd_function,
            crate::lang_func_metadata!(
                name="addd",
                return_type_constraint(
                    allowed=["DOUBLE"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn addd_function(
            interpreter: &mut Interpreter,
            number_objects: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut sum = 0.0;

            for (i, number_object) in number_objects.iter().
                    enumerate() {
                let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
                let Some(number) = number else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some(&format!(
                            "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                sum += number.double_value();
            }

            DataObjectRef::new(DataObject::new_number(sum))
        }

        functions.push(crate::lang_func!(
            muld_function,
            crate::lang_func_metadata!(
                name="muld",
                return_type_constraint(
                    allowed=["DOUBLE"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn muld_function(
            interpreter: &mut Interpreter,
            number_objects: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut prod = 1.0;

            for (i, number_object) in number_objects.iter().
                    enumerate() {
                let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
                let Some(number) = number else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some(&format!(
                            "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                prod *= number.double_value();
            }

            DataObjectRef::new(DataObject::new_number(prod))
        }

        functions.push(math_1_arg_function!("sqrt", sqrt));
        functions.push(math_1_arg_function!("cbrt", cbrt));

        functions.push(math_2_arg_function!("hypot", hypot));

        functions.push(math_1_arg_function!("toRadians", to_radians));
        functions.push(math_1_arg_function!("toDegrees", to_degrees));

        functions.push(math_1_arg_function!("sin", sin));
        functions.push(math_1_arg_function!("cos", cos));
        functions.push(math_1_arg_function!("tan", tan));

        functions.push(math_1_arg_function!("asin", asin));
        functions.push(math_1_arg_function!("acos", acos));
        functions.push(math_1_arg_function!("atan", atan));

        functions.push(math_2_arg_function!("atan2", atan2));

        functions.push(math_1_arg_function!("sinh", sinh));
        functions.push(math_1_arg_function!("cosh", cosh));
        functions.push(math_1_arg_function!("tanh", tanh));

        functions.push(math_1_arg_function!("exp", exp));

        functions.push(math_1_arg_function!("loge", ln));
        functions.push(math_1_arg_function!("log10", log10));

        functions.push(crate::lang_func!(
            round_function,
            crate::lang_func_metadata!(
                name="round",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn round_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap().double_value();

            let r = if number.signum() < 0.0 { -1 } else { 1 } * number.abs().round() as i64;

            DataObjectRef::new(DataObject::new_number(r))
        }

        functions.push(crate::lang_func!(
            ceil_function,
            crate::lang_func_metadata!(
                name="ceil",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn ceil_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap().double_value();

            DataObjectRef::new(DataObject::new_number(number.ceil() as i64))
        }

        functions.push(crate::lang_func!(
            floor_function,
            crate::lang_func_metadata!(
                name="floor",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
        fn floor_function(
            _: &mut Interpreter,
            number: DataObjectRef,
        ) -> DataObjectRef {
            let number = number.number_value().unwrap().double_value();

            DataObjectRef::new(DataObject::new_number(number.floor() as i64))
        }

        functions.push(crate::lang_func!(
            abs_function,
            crate::lang_func_metadata!(
                name="abs",
                parameter(
                    name="$operand",
                ),
            ),
        ));
        fn abs_function(
            interpreter: &mut Interpreter,
            operand: DataObjectRef,
        ) -> DataObjectRef {
            let ret = operators::op_abs(interpreter, &operand, CodePosition::EMPTY);
            let Some(ret) = ret else {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!("The abs operator is not defined for {:?}", operand.data_type())),
                    CodePosition::EMPTY,
                );
            };

            ret
        }
    }
}

mod combinator_functions {
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::{operators, Interpreter, InterpretingError};
    use crate::interpreter::data::{DataObjectRef, OptionDataObjectRef};
    use crate::lexer::CodePosition;
    use crate::utils;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            comb_bn_function,
            crate::lang_func_metadata!(
                name="combBN",
                combinator_function=true,
                info="Combinator execution: a(b(args[0]), b(args[1]), ...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn comb_bn_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            b: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> OptionDataObjectRef {
            let mut args_a = Vec::with_capacity(args.len());
            for arg in args {
                let ret_b = operators::op_call(
                    interpreter,
                    &b,
                    &[arg],
                    CodePosition::EMPTY,
                );
                args_a.push(utils::none_to_lang_void(ret_b));
            }
            args_a = utils::separate_arguments_with_argument_separators(&args_a);

            operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
        }

        functions.push(crate::lang_func!(
            comb_bv_function,
            crate::lang_func_metadata!(
                name="combBV",
                combinator_function=true,
                info="Combinator execution: a(b(args[0]), b(args[1]), ...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
            ),
        ));
        fn comb_bv_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            b: DataObjectRef,
            args: DataObjectRef,
        ) -> OptionDataObjectRef {
            let args = args.array_value().unwrap().borrow().clone();

            let mut args_a = Vec::with_capacity(args.len());
            for arg in args {
                let ret_b = operators::op_call(
                    interpreter,
                    &b,
                    &[arg],
                    CodePosition::EMPTY,
                );
                args_a.push(utils::none_to_lang_void(ret_b));
            }
            args_a = utils::separate_arguments_with_argument_separators(&args_a);

            operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
        }

        functions.push(crate::lang_func!(
            comb_bz_function,
            crate::lang_func_metadata!(
                name="combBZ",
                combinator_function=true,
                info="Combinator execution: a(..., b(args[1]), b(args[0]))",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn comb_bz_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            b: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> OptionDataObjectRef {
            let mut args_a = Vec::with_capacity(args.len());
            for arg in args.into_iter().rev() {
                let ret_b = operators::op_call(
                    interpreter,
                    &b,
                    &[arg],
                    CodePosition::EMPTY,
                );
                args_a.push(utils::none_to_lang_void(ret_b));
            }
            args_a = utils::separate_arguments_with_argument_separators(&args_a);

            operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
        }

        functions.push(crate::lang_func!(
            comb_nn_function,
            crate::lang_func_metadata!(
                name="combNN",
                combinator_function=true,
                info="Combinator execution: a(args[0])(args[1])(...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn comb_nn_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut ret = a;
            for (i, n) in args.into_iter().
                    enumerate() {
                if !utils::is_callable(&ret) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The return value after iteration {} must be callable",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                }

                ret = utils::none_to_lang_void(
                    operators::op_call(interpreter, &ret, &[n], CodePosition::EMPTY),
                );
            }

            ret
        }

        functions.push(crate::lang_func!(
            comb_nv_function,
            crate::lang_func_metadata!(
                name="combNV",
                combinator_function=true,
                info="Combinator execution: a(args[0])(args[1])(...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
            ),
        ));
        fn comb_nv_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            args: DataObjectRef,
        ) -> DataObjectRef {
            let args = args.array_value().unwrap().borrow().clone();

            let mut ret = a;
            for (i, n) in <Box<[_]> as IntoIterator>::into_iter(args).
                    enumerate() {
                if !utils::is_callable(&ret) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The return value after iteration {} must be callable",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                }

                ret = utils::none_to_lang_void(
                    operators::op_call(interpreter, &ret, &[n], CodePosition::EMPTY),
                );
            }

            ret
        }

        functions.push(crate::lang_func!(
            comb_nz_function,
            crate::lang_func_metadata!(
                name="combNZ",
                combinator_function=true,
                info="Combinator execution: a(...)(args[1])(args[0])",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn comb_nz_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut ret = a;
            for (i, n) in args.into_iter().
                    rev().
                    enumerate() {
                if !utils::is_callable(&ret) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The return value after iteration {} must be callable",
                            i + 1,
                        )),
                        CodePosition::EMPTY,
                    );
                }

                ret = utils::none_to_lang_void(
                    operators::op_call(interpreter, &ret, &[n], CodePosition::EMPTY),
                );
            }

            ret
        }

        functions.push(crate::lang_func!(
            comb_pn_function,
            crate::lang_func_metadata!(
                name="combPN",
                combinator_function=true,
                info="Combinator execution: a(args[0](b), args[1](b), ...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn comb_pn_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            b: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> OptionDataObjectRef {
            let mut args_a = Vec::with_capacity(args.len());
            for (i, n) in args.iter().
                    enumerate() {
                if !utils::is_callable(n) {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The value at index {} must be callable",
                            i + 3,
                        )),
                        CodePosition::EMPTY,
                    ));
                }

                let ret_n = operators::op_call(
                    interpreter,
                    n,
                    &[b.clone()],
                    CodePosition::EMPTY,
                );
                args_a.push(utils::none_to_lang_void(ret_n));
            }
            args_a = utils::separate_arguments_with_argument_separators(&args_a);

            operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
        }

        functions.push(crate::lang_func!(
            comb_pv_function,
            crate::lang_func_metadata!(
                name="combPV",
                combinator_function=true,
                info="Combinator execution: a(args[0](b), args[1](b), ...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                ),
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
            ),
        ));
        fn comb_pv_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            b: DataObjectRef,
            args: DataObjectRef,
        ) -> OptionDataObjectRef {
            let args = args.array_value().unwrap().borrow().clone();

            let mut args_a = Vec::with_capacity(args.len());
            for (i, n) in args.iter().
                    enumerate() {
                if !utils::is_callable(n) {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "Value at index {} of Argument 3 (\"&args\") must be callable",
                            i,
                        )),
                        CodePosition::EMPTY,
                    ));
                }

                let ret_n = operators::op_call(
                    interpreter,
                    n,
                    &[b.clone()],
                    CodePosition::EMPTY,
                );
                args_a.push(utils::none_to_lang_void(ret_n));
            }
            args_a = utils::separate_arguments_with_argument_separators(&args_a);

            operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
        }

        functions.push(crate::lang_func!(
            comb_pz_function,
            crate::lang_func_metadata!(
                name="combPZ",
                combinator_function=true,
                info="Combinator execution: a(..., args[1](b), args[0](b))",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn comb_pz_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            b: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> OptionDataObjectRef {
            let mut args_a = Vec::with_capacity(args.len());
            for (i, n) in args.iter().
                    enumerate().
                    rev() {
                if !utils::is_callable(n) {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The value at index {} must be callable",
                            i + 3,
                        )),
                        CodePosition::EMPTY,
                    ));
                }

                let ret_n = operators::op_call(
                    interpreter,
                    n,
                    &[b.clone()],
                    CodePosition::EMPTY,
                );
                args_a.push(utils::none_to_lang_void(ret_n));
            }
            args_a = utils::separate_arguments_with_argument_separators(&args_a);

            operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
        }

        functions.push(crate::lang_func!(
            comb_qn_function,
            crate::lang_func_metadata!(
                name="combQN",
                combinator_function=true,
                info="Combinator execution: ...(args[1](args[0](a)))",
                parameter(
                    name="$a",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn comb_qn_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut ret = a;
            for (i, n) in args.into_iter().
                    enumerate() {
                if !utils::is_callable(&n) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The value at index {} must be callable",
                            i + 2,
                        )),
                        CodePosition::EMPTY,
                    );
                }

                ret = utils::none_to_lang_void(
                    operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
                );
            }

            ret
        }

        functions.push(crate::lang_func!(
            comb_qv_function,
            crate::lang_func_metadata!(
                name="combQV",
                combinator_function=true,
                info="Combinator execution: ...(args[1](args[0](a)))",
                parameter(
                    name="$a",
                ),
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
            ),
        ));
        fn comb_qv_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            args: DataObjectRef,
        ) -> DataObjectRef {
            let args = args.array_value().unwrap().borrow().clone();

            let mut ret = a;
            for (i, n) in <Box<[_]> as IntoIterator>::into_iter(args).
                    enumerate() {
                if !utils::is_callable(&n) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "Value at index {} of Argument 2 (\"&args\") must be callable",
                            i,
                        )),
                        CodePosition::EMPTY,
                    );
                }

                ret = utils::none_to_lang_void(
                    operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
                );
            }

            ret
        }

        functions.push(crate::lang_func!(
            comb_qz_function,
            crate::lang_func_metadata!(
                name="combQZ",
                combinator_function=true,
                info="Combinator execution: args[0](args[1](...(a)))",
                parameter(
                    name="$a",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn comb_qz_function(
            interpreter: &mut Interpreter,
            a: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let mut ret = a;
            for (i, n) in args.into_iter().
                    enumerate().
                    rev() {
                if !utils::is_callable(&n) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The value at index {} must be callable",
                            i + 2,
                        )),
                        CodePosition::EMPTY,
                    );
                }

                ret = utils::none_to_lang_void(
                    operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
                );
            }

            ret
        }

        functions.push(crate::lang_func!(
            comb_tn_function,
            crate::lang_func_metadata!(
                name="combTN",
                combinator_function=true,
                info="Combinator execution: ...(args[1](args[0](z)))",
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
                parameter(
                    name="$z",
                ),
            ),
        ));
        fn comb_tn_function(
            interpreter: &mut Interpreter,
            args: Vec<DataObjectRef>,
            z: DataObjectRef,
        ) -> DataObjectRef {
            let mut ret = z;
            for (i, n) in args.into_iter().
                    enumerate() {
                if !utils::is_callable(&n) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The value at index {} must be callable",
                            i + 2,
                        )),
                        CodePosition::EMPTY,
                    );
                }

                ret = utils::none_to_lang_void(
                    operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
                );
            }

            ret
        }

        functions.push(crate::lang_func!(
            comb_tv_function,
            crate::lang_func_metadata!(
                name="combTV",
                combinator_function=true,
                info="Combinator execution: ...(args[1](args[0](z)))",
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
                parameter(
                    name="$z",
                ),
            ),
        ));
        fn comb_tv_function(
            interpreter: &mut Interpreter,
            args: DataObjectRef,
            z: DataObjectRef,
        ) -> DataObjectRef {
            let args = args.array_value().unwrap().borrow().clone();

            let mut ret = z;
            for (i, n) in <Box<[_]> as IntoIterator>::into_iter(args).
                    enumerate() {
                if !utils::is_callable(&n) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "Value at index {} of Argument 2 (\"&args\") must be callable",
                            i,
                        )),
                        CodePosition::EMPTY,
                    );
                }

                ret = utils::none_to_lang_void(
                    operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
                );
            }

            ret
        }

        functions.push(crate::lang_func!(
            comb_tz_function,
            crate::lang_func_metadata!(
                name="combTZ",
                combinator_function=true,
                info="Combinator execution: args[0](args[1](...(z)))",
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
                parameter(
                    name="$z",
                ),
            ),
        ));
        fn comb_tz_function(
            interpreter: &mut Interpreter,
            args: Vec<DataObjectRef>,
            z: DataObjectRef,
        ) -> DataObjectRef {
            let mut ret = z;
            for (i, n) in args.into_iter().
                    enumerate().
                    rev() {
                if !utils::is_callable(&n) {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The value at index {} must be callable",
                            i + 2,
                        )),
                        CodePosition::EMPTY,
                    );
                }

                ret = utils::none_to_lang_void(
                    operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
                );
            }

            ret
        }
    }
}

mod func_ptr_functions {
    use std::rc::Rc;
    use crate::interpreter::data::function::{Function, FunctionMetadata, FunctionPointerObject};
    use crate::interpreter::{operators, Interpreter};
    use crate::interpreter::data::{DataObject, DataObjectRef, OptionDataObjectRef};
    use crate::lexer::CodePosition;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            arg_cnt_0_function,
            crate::lang_func_metadata!(
                name="argCnt0",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
        fn arg_cnt_0_function(
            _: &mut Interpreter,
            func_object: DataObjectRef,
        ) -> DataObjectRef {
            let function_name = if let Some(function_value) = func_object.function_pointer_value() {
                let function_name = function_value.function_name();
                if let Some(function_name) = function_name {
                    function_name.to_string()
                }else {
                    func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
                }
            }else {
                "<arg>".to_string()
            };

            let arg_cnt_0_func_function = {
                let func_object = func_object.clone();

                move |interpreter: &mut Interpreter| -> OptionDataObjectRef {
                    operators::op_call(interpreter, &func_object, &[], CodePosition::EMPTY)
                }
            };
            let arg_cnt_0_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_0_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt0-func",
                ),
            ));

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(arg_cnt_0_func_function.copy_with_function_name(&format!(
                    "<argCnt0({function_name})>",
                ))))
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            arg_cnt_1_function,
            crate::lang_func_metadata!(
                name="argCnt1",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
        fn arg_cnt_1_function(
            _: &mut Interpreter,
            func_object: DataObjectRef,
        ) -> DataObjectRef {
            let function_name = if let Some(function_value) = func_object.function_pointer_value() {
                let function_name = function_value.function_name();
                if let Some(function_name) = function_name {
                    function_name.to_string()
                }else {
                    func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
                }
            }else {
                "<arg>".to_string()
            };

            let arg_cnt_1_func_function = {
                let func_object = func_object.clone();

                move |interpreter: &mut Interpreter, a: DataObjectRef| -> OptionDataObjectRef {
                    operators::op_call(interpreter, &func_object, &[
                        a,
                    ], CodePosition::EMPTY)
                }
            };
            let arg_cnt_1_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_1_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt1-func",
                    parameter(
                        name="$a",
                    ),
                ),
            ));

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(arg_cnt_1_func_function.copy_with_function_name(&format!(
                    "<argCnt1({function_name})>",
                ))))
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            arg_cnt_2_function,
            crate::lang_func_metadata!(
                name="argCnt2",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
        fn arg_cnt_2_function(
            _: &mut Interpreter,
            func_object: DataObjectRef,
        ) -> DataObjectRef {
            let function_name = if let Some(function_value) = func_object.function_pointer_value() {
                let function_name = function_value.function_name();
                if let Some(function_name) = function_name {
                    function_name.to_string()
                }else {
                    func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
                }
            }else {
                "<arg>".to_string()
            };

            let arg_cnt_2_func_function = {
                let func_object = func_object.clone();

                move |interpreter: &mut Interpreter, a: DataObjectRef, b: DataObjectRef| -> OptionDataObjectRef {
                    operators::op_call(interpreter, &func_object, &[
                        a, b,
                    ], CodePosition::EMPTY)
                }
            };
            let arg_cnt_2_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_2_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt2-func",
                    parameter(
                        name="$a",
                    ),
                    parameter(
                        name="$b",
                    ),
                ),
            ));

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(arg_cnt_2_func_function.copy_with_function_name(&format!(
                    "<argCnt2({function_name})>",
                ))))
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            arg_cnt_3_function,
            crate::lang_func_metadata!(
                name="argCnt3",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
        fn arg_cnt_3_function(
            _: &mut Interpreter,
            func_object: DataObjectRef,
        ) -> DataObjectRef {
            let function_name = if let Some(function_value) = func_object.function_pointer_value() {
                let function_name = function_value.function_name();
                if let Some(function_name) = function_name {
                    function_name.to_string()
                }else {
                    func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
                }
            }else {
                "<arg>".to_string()
            };

            let arg_cnt_3_func_function = {
                let func_object = func_object.clone();

                move |interpreter: &mut Interpreter, a: DataObjectRef, b: DataObjectRef, c: DataObjectRef| -> OptionDataObjectRef {
                    operators::op_call(interpreter, &func_object, &[
                        a, b, c,
                    ], CodePosition::EMPTY)
                }
            };
            let arg_cnt_3_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_3_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt3-func",
                    parameter(
                        name="$a",
                    ),
                    parameter(
                        name="$b",
                    ),
                    parameter(
                        name="$c",
                    ),
                ),
            ));

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(arg_cnt_3_func_function.copy_with_function_name(&format!(
                    "<argCnt3({function_name})>",
                ))))
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            arg_cnt_4_function,
            crate::lang_func_metadata!(
                name="argCnt4",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
        fn arg_cnt_4_function(
            _: &mut Interpreter,
            func_object: DataObjectRef,
        ) -> DataObjectRef {
            let function_name = if let Some(function_value) = func_object.function_pointer_value() {
                let function_name = function_value.function_name();
                if let Some(function_name) = function_name {
                    function_name.to_string()
                }else {
                    func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
                }
            }else {
                "<arg>".to_string()
            };

            let arg_cnt_4_func_function = {
                let func_object = func_object.clone();

                move |interpreter: &mut Interpreter, a: DataObjectRef, b: DataObjectRef, c: DataObjectRef, d: DataObjectRef| -> OptionDataObjectRef {
                    operators::op_call(interpreter, &func_object, &[
                        a, b, c, d,
                    ], CodePosition::EMPTY)
                }
            };
            let arg_cnt_4_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_4_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt4-func",
                    parameter(
                        name="$a",
                    ),
                    parameter(
                        name="$b",
                    ),
                    parameter(
                        name="$c",
                    ),
                    parameter(
                        name="$d",
                    ),
                ),
            ));

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(arg_cnt_4_func_function.copy_with_function_name(&format!(
                    "<argCnt4({function_name})>",
                ))))
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            arg_cnt_5_function,
            crate::lang_func_metadata!(
                name="argCnt5",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
        fn arg_cnt_5_function(
            _: &mut Interpreter,
            func_object: DataObjectRef,
        ) -> DataObjectRef {
            let function_name = if let Some(function_value) = func_object.function_pointer_value() {
                let function_name = function_value.function_name();
                if let Some(function_name) = function_name {
                    function_name.to_string()
                }else {
                    func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
                }
            }else {
                "<arg>".to_string()
            };

            let arg_cnt_5_func_function = {
                let func_object = func_object.clone();

                move |interpreter: &mut Interpreter, a: DataObjectRef, b: DataObjectRef, c: DataObjectRef, d: DataObjectRef, e: DataObjectRef| -> OptionDataObjectRef {
                    operators::op_call(interpreter, &func_object, &[
                        a, b, c, d, e,
                    ], CodePosition::EMPTY)
                }
            };
            let arg_cnt_5_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_5_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt4-func",
                    parameter(
                        name="$a",
                    ),
                    parameter(
                        name="$b",
                    ),
                    parameter(
                        name="$c",
                    ),
                    parameter(
                        name="$d",
                    ),
                    parameter(
                        name="$e",
                    ),
                ),
            ));

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(arg_cnt_5_func_function.copy_with_function_name(&format!(
                    "<argCnt5({function_name})>",
                ))))
            }).unwrap())
        }
    }
}

mod byte_buffer_functions {
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::{Interpreter, InterpretingError};
    use crate::interpreter::data::{DataObject, DataObjectRef};

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            byte_buffer_create_function,
            crate::lang_func_metadata!(
                name="byteBufferCreate",
                return_type_constraint(
                    allowed=["BYTE_BUFFER"],
                ),
                parameter(
                    name="$length",
                    parameter_type(number),
                ),
            ),
        ));
        fn byte_buffer_create_function(
            interpreter: &mut Interpreter,
            length_number: DataObjectRef,
        ) -> DataObjectRef {
            let length_number = length_number.number_value().unwrap();
            let length = length_number.int_value();
            if length < 0 {
                return interpreter.set_errno_error_object_error_only(InterpretingError::NegativeArrayLen);
            }

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_byte_buffer(vec![0; length as usize].into_boxed_slice())
            }).unwrap())
        }
    }
}

mod array_functions {
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::{Interpreter, InterpretingError};
    use crate::interpreter::data::{DataObject, DataObjectRef};
    use crate::lexer::CodePosition;
    use crate::utils;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            array_create_function,
            crate::lang_func_metadata!(
                name="arrayCreate",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="$length",
                    parameter_type(number),
                ),
            ),
        ));
        fn array_create_function(
            interpreter: &mut Interpreter,
            length_number: DataObjectRef,
        ) -> DataObjectRef {
            let length_number = length_number.number_value().unwrap();
            let length = length_number.int_value();
            if length < 0 {
                return interpreter.set_errno_error_object_error_only(InterpretingError::NegativeArrayLen);
            }

            let arr = (0..length).
                    map(|_| DataObjectRef::new(DataObject::new())).
                    collect::<Box<_>>();

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(arr)
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            array_of_function,
            crate::lang_func_metadata!(
                name="arrayOf",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&elements",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn array_of_function(
            _: &mut Interpreter,
            elements: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let arr = elements.iter().
                    map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_data(&ele.borrow())
                    }).unwrap())).
                    collect::<Box<_>>();

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(arr)
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            array_generate_from_function,
            crate::lang_func_metadata!(
                name="arrayGenerateFrom",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="fp.func",
                    parameter_type(var_args),
                ),
                parameter(
                    name="$count",
                    parameter_type(number),
                ),
            ),
        ));
        fn array_generate_from_function(
            interpreter: &mut Interpreter,
            func_pointer_object: DataObjectRef,
            count_number: DataObjectRef,
        ) -> DataObjectRef {
            let count_number = count_number.number_value().unwrap();
            let count = count_number.int_value();
            if count < 0 {
                return interpreter.set_errno_error_object_error_only(InterpretingError::NegativeArrayLen);
            }

            let function_pointer = func_pointer_object.function_pointer_value().unwrap();

            let arr = (0..count).
                    map(|i| {
                        utils::none_to_lang_void(interpreter.call_function_pointer(
                            &function_pointer,
                            func_pointer_object.variable_name().as_deref(),
                            &[
                                DataObjectRef::new(DataObject::new_number(i)),
                            ],
                            CodePosition::EMPTY,
                        ))
                    }).
                    collect::<Box<_>>();

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(arr)
            }).unwrap())
        }

        //TODO
    }
}

mod list_functions {
    use std::collections::VecDeque;
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::{operators, Interpreter, InterpretingError};
    use crate::interpreter::data::{DataObject, DataObjectRef};
    use crate::lexer::CodePosition;
    use crate::utils;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            list_create_function,
            crate::lang_func_metadata!(
                name="listCreate",
                return_type_constraint(
                    allowed=["LIST"],
                ),
            ),
        ));
        fn list_create_function(
            _: &mut Interpreter,
        ) -> DataObjectRef {
            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_list(VecDeque::new())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            list_of_function,
            crate::lang_func_metadata!(
                name="listOf",
                return_type_constraint(
                    allowed=["LIST"],
                ),
                parameter(
                    name="&elements",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn list_of_function(
            _: &mut Interpreter,
            elements: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let list = elements.iter().
                    map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_data(&ele.borrow())
                    }).unwrap())).
                    collect::<VecDeque<_>>();

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_list(list)
            }).unwrap())
        }

        //TODO

        functions.push(crate::lang_func!(
            list_count_of_function,
            crate::lang_func_metadata!(
                name="listCountOf",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
        fn list_count_of_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            value_object: DataObjectRef,
        ) -> DataObjectRef {
            let list = list_object.list_value().unwrap().borrow().clone();

            let count = list.iter().
                    filter(|ele| operators::is_strict_equals(
                        interpreter, ele, &value_object, CodePosition::EMPTY,
                    )).count();

            DataObjectRef::new(DataObject::new_number(count as i32))
        }

        //TODO

        functions.push(crate::lang_func!(
            list_remove_at_function,
            crate::lang_func_metadata!(
                name="listRemoveAt",
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$index",
                    parameter_type(number),
                ),
            ),
        ));
        fn list_remove_at_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            index_number: DataObjectRef,
        ) -> DataObjectRef {
            let list = list_object.list_value().unwrap();

            let index_number = index_number.number_value().unwrap();
            let index = index_number.int_value();

            let mut list = list.borrow_mut();

            let index = utils::wrap_index(index, list.len());
            let Some(index) = index else {
                return interpreter.set_errno_error_object_error_only(InterpretingError::IndexOutOfBounds);
            };

            list.remove(index).unwrap()
        }

        //TODO
    }
}

mod struct_functions {
    use std::rc::Rc;
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::{Interpreter, InterpretingError};
    use crate::interpreter::data::{DataObject, DataObjectRef, OptionDataObjectRef, StructObject};
    use crate::lexer::CodePosition;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            struct_create_function,
            crate::lang_func_metadata!(
                name="structCreate",
                info="Returns an empty struct object of type &Struct. \
                This function is not compatible with all struct types, \
                because all values will be set to null - use \"func.structOf()\" for those struct types instead.",
                return_type_constraint(
                    allowed=["STRUCT"],
                ),
                parameter(
                    name="&Struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
        fn struct_create_function(
            interpreter: &mut Interpreter,
            struct_object: DataObjectRef,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            if !struct_value.is_definition() {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"&Struct\") must be a struct definition"),
                    CodePosition::EMPTY,
                );
            }

            let struct_fields = struct_value.member_names().iter().
                    map(|_| DataObjectRef::new(DataObject::new())).
                    collect::<Box<_>>();

            let ret = StructObject::new_instance(struct_value, &struct_fields);
            match ret {
                Ok(ret) => {
                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_struct(Rc::new(ret))
                    }).unwrap())
                },

                Err(e) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(e.message()),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            struct_of_function,
            crate::lang_func_metadata!(
                name="structOf",
                return_type_constraint(
                    allowed=["STRUCT"],
                ),
                parameter(
                    name="&Struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn struct_of_function(
            interpreter: &mut Interpreter,
            struct_object: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            if !struct_value.is_definition() {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"&Struct\") must be a struct definition"),
                    CodePosition::EMPTY,
                );
            }

            let member_count = struct_value.member_names().len();
            if args.len() != member_count {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The var args argument count is not equals to the count of the member names ({member_count})",
                    )),
                    CodePosition::EMPTY,
                );
            }

            let ret = StructObject::new_instance(struct_value, &args);
            match ret {
                Ok(ret) => {
                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_struct(Rc::new(ret))
                    }).unwrap())
                },

                Err(e) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(e.message()),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            struct_set_function,
            crate::lang_func_metadata!(
                name="structSet",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
                parameter(
                    name="$memberName",
                    type_constraint(
                        allowed=["TEXT"],
                    ),
                ),
                parameter(
                    name="$memberObject",
                ),
            ),
        ));
        fn struct_set_function(
            interpreter: &mut Interpreter,
            struct_object: DataObjectRef,
            member_name_object: DataObjectRef,
            member_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            if struct_value.is_definition() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"&struct\") must be a struct instance"),
                    CodePosition::EMPTY,
                ));
            }

            let member_name = member_name_object.text_value().unwrap();

            let ret = struct_value.set_member(&member_name, &member_object.borrow());
            if let Err(e) = ret {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(e.message()),
                    CodePosition::EMPTY,
                ));
            };

            None
        }

        functions.push(crate::lang_func!(
            struct_set_all_function,
            crate::lang_func_metadata!(
                name="structSetAll",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn struct_set_all_function(
            interpreter: &mut Interpreter,
            struct_object: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> OptionDataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            if struct_value.is_definition() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"&struct\") must be a struct instance"),
                    CodePosition::EMPTY,
                ));
            }

            let member_names = struct_value.member_names();
            let member_count = member_names.len();
            if args.len() != member_count {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The var args argument count is not equals to the count of the member names ({member_count})",
                    )),
                    CodePosition::EMPTY,
                ));
            }

            for (i, (member_name, arg)) in member_names.iter().
                    zip(args.iter()).
                    enumerate() {
                let ret = struct_value.set_member(member_name, &arg.borrow());
                if let Err(e) = ret {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(&format!(
                            "Argument {}: {}",
                            i + 2,
                            e.message(),
                        )),
                        CodePosition::EMPTY,
                    ));
                };
            }

            None
        }

        functions.push(crate::lang_func!(
            struct_get_function,
            crate::lang_func_metadata!(
                name="structGet",
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
                parameter(
                    name="$memberName",
                    type_constraint(
                        allowed=["TEXT"],
                    ),
                ),
            ),
        ));
        fn struct_get_function(
            interpreter: &mut Interpreter,
            struct_object: DataObjectRef,
            member_name_object: DataObjectRef,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            if struct_value.is_definition() {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"&struct\") must be a struct instance"),
                    CodePosition::EMPTY,
                );
            }

            let member_name = member_name_object.text_value().unwrap();

            let ret = struct_value.get_member(&member_name);
            match ret {
                Ok(ret) => ret,

                Err(e) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(e.message()),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            struct_get_all_function,
            crate::lang_func_metadata!(
                name="structGetAll",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
        fn struct_get_all_function(
            interpreter: &mut Interpreter,
            struct_object: DataObjectRef,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            if struct_value.is_definition() {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"&struct\") must be a struct instance"),
                    CodePosition::EMPTY,
                );
            }

            let arr = struct_value.member_names().iter().
                    map(|member_name| struct_value.get_member(member_name)).
                    collect::<Result<Box<_>, _>>();
            match arr {
                Ok(arr) => {
                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_array(arr)
                    }).unwrap())
                },

                Err(e) => {
                    interpreter.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(e.message()),
                        CodePosition::EMPTY,
                    )
                },
            }
        }

        functions.push(crate::lang_func!(
            struct_get_member_names_function,
            crate::lang_func_metadata!(
                name="structGetMemberNames",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
        fn struct_get_member_names_function(
            _: &mut Interpreter,
            struct_object: DataObjectRef,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            let arr = struct_value.member_names().iter().
                    map(|member_name| DataObjectRef::new(DataObject::new_text(&**member_name))).
                    collect::<Box<_>>();
            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(arr)
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            struct_get_member_count_function,
            crate::lang_func_metadata!(
                name="structGetMemberCount",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
        fn struct_get_member_count_function(
            _: &mut Interpreter,
            struct_object: DataObjectRef,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            DataObjectRef::new(DataObject::new_number(struct_value.member_names().len() as i32))
        }

        functions.push(crate::lang_func!(
            struct_is_definition_function,
            crate::lang_func_metadata!(
                name="structIsDefinition",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
        fn struct_is_definition_function(
            _: &mut Interpreter,
            struct_object: DataObjectRef,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(struct_value.is_definition())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            struct_is_instance_function,
            crate::lang_func_metadata!(
                name="structIsInstance",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
        fn struct_is_instance_function(
            _: &mut Interpreter,
            struct_object: DataObjectRef,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_bool(!struct_value.is_definition())
            }).unwrap())
        }

        functions.push(crate::lang_func!(
            struct_definition_type_of_function,
            crate::lang_func_metadata!(
                name="structDefinitionTypeOf",
                return_type_constraint(
                    allowed=["STRUCT"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
        fn struct_definition_type_of_function(
            interpreter: &mut Interpreter,
            struct_object: DataObjectRef,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();

            if struct_value.is_definition() {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 1 (\"&struct\") must be a struct instance"),
                    CodePosition::EMPTY,
                );
            }

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_struct(struct_value.base_definition().unwrap())
            }).unwrap())
        }
    }
}

mod lang_test_functions {
    use crate::interpreter::data::function::{Function, FunctionMetadata};
    use crate::interpreter::data::{DataObjectRef, DataType, OptionDataObjectRef};
    use crate::interpreter::{conversions, operators, Interpreter, InterpretingError};
    use crate::interpreter::lang_test::AssertResult;
    use crate::lexer::CodePosition;

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            test_unit_function,
            crate::lang_func_metadata!(
                name="testUnit",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn test_unit_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            if !interpreter.execution_flags.lang_test {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("langTest functions can only be used if the langTest flag is true"),
                    CodePosition::EMPTY,
                ));
            }

            let text = &conversions::to_text(
                interpreter,
                &text_object,
                CodePosition::EMPTY
            );

            interpreter.lang_test_store.add_unit(text);

            None
        }

        functions.push(crate::lang_func!(
            test_sub_unit_function,
            crate::lang_func_metadata!(
                name="testSubUnit",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$text",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn test_sub_unit_function(
            interpreter: &mut Interpreter,
            text_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            if !interpreter.execution_flags.lang_test {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("langTest functions can only be used if the langTest flag is true"),
                    CodePosition::EMPTY,
                ));
            }

            let text = &conversions::to_text(
                interpreter,
                &text_object,
                CodePosition::EMPTY
            );

            let ret = interpreter.lang_test_store.add_sub_unit(text);
            if let Err(e) = ret {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(e.message()),
                    CodePosition::EMPTY,
                ));
            }

            None
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertError",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$error",
                        type_constraint(
                            allowed=["ERROR"],
                        ),
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                error_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, error_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertError",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$error",
                        type_constraint(
                            allowed=["ERROR"],
                        ),
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));
            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                error_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, error_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                error_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let lang_errno = interpreter.get_and_clear_errno_error_object();
                let expected_error = error_object.error_value().unwrap().err();

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                interpreter.lang_test_store.add_assert_result(AssertResult::new_error_result(
                    lang_errno == expected_error,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    lang_errno,
                    expected_error,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = operators::is_equals(
                    interpreter,
                    &actual_value_object,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                interpreter.lang_test_store.add_assert_result(AssertResult::new_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    &expected_value_object.borrow(), &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = !operators::is_equals(
                    interpreter,
                    &actual_value_object,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                interpreter.lang_test_store.add_assert_result(AssertResult::new_not_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    &expected_value_object.borrow(), &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertLessThan",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertLessThan",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = operators::is_less_than(
                    interpreter,
                    &actual_value_object,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                interpreter.lang_test_store.add_assert_result(AssertResult::new_less_than_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    &expected_value_object.borrow(), &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertLessThanOrEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertLessThanOrEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = operators::is_less_than_or_equals(
                    interpreter,
                    &actual_value_object,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                interpreter.lang_test_store.add_assert_result(AssertResult::new_less_than_or_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    &expected_value_object.borrow(), &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertGreaterThan",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertGreaterThan",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = operators::is_greater_than(
                    interpreter,
                    &actual_value_object,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                interpreter.lang_test_store.add_assert_result(AssertResult::new_greater_than_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    &expected_value_object.borrow(), &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertGreaterThanOrEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertGreaterThanOrEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = operators::is_greater_than_or_equals(
                    interpreter,
                    &actual_value_object,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                interpreter.lang_test_store.add_assert_result(AssertResult::new_greater_than_or_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    &expected_value_object.borrow(), &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertStrictEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertStrictEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = operators::is_strict_equals(
                    interpreter,
                    &actual_value_object,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                interpreter.lang_test_store.add_assert_result(AssertResult::new_strict_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    &expected_value_object.borrow(), &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertStrictNotEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertStrictNotEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = !operators::is_strict_equals(
                    interpreter,
                    &actual_value_object,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                interpreter.lang_test_store.add_assert_result(AssertResult::new_strict_not_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    &expected_value_object.borrow(), &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTranslationValueEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$translationKey",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, translation_key_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTranslationValueEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$translationKey",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, translation_key_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let translation_key = conversions::to_text(
                    interpreter,
                    &translation_key_object,
                    CodePosition::EMPTY,
                );

                let translation_value = interpreter.data_ref().lang.get(&*translation_key).cloned();

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = translation_value.as_ref().is_some_and(|translation_value| **translation_value == *expected_value_object_text);

                interpreter.lang_test_store.add_assert_result(AssertResult::new_translation_value_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &translation_key,
                    translation_value.as_deref(),
                    &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTranslationValueNotEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$translationKey",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                expected_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, translation_key_object, expected_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTranslationValueNotEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$translationKey",
                    ),
                    parameter(
                        name="$expectedValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, translation_key_object, expected_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                expected_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let translation_key = conversions::to_text(
                    interpreter,
                    &translation_key_object,
                    CodePosition::EMPTY,
                );

                let translation_value = interpreter.data_ref().lang.get(&*translation_key).cloned();

                let expected_value_object_text = conversions::to_text(
                    interpreter,
                    &expected_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = translation_value.as_ref().is_some_and(|translation_value| **translation_value != *expected_value_object_text);

                interpreter.lang_test_store.add_assert_result(AssertResult::new_translation_value_not_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &translation_key,
                    translation_value.as_deref(),
                    &expected_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTranslationKeyFound",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$translationKey",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, translation_key_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTranslationKeyFound",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$translationKey",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, translation_key_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let translation_key = conversions::to_text(
                    interpreter,
                    &translation_key_object,
                    CodePosition::EMPTY,
                );

                let translation_value = interpreter.data_ref().lang.get(&*translation_key).cloned();

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = translation_value.as_ref().is_some();

                interpreter.lang_test_store.add_assert_result(AssertResult::new_translation_key_found_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &translation_key,
                    translation_value.as_deref(),
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTranslationKeyNotFound",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$translationKey",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, translation_key_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTranslationKeyNotFound",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$translationKey",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, translation_key_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                translation_key_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let translation_key = conversions::to_text(
                    interpreter,
                    &translation_key_object,
                    CodePosition::EMPTY,
                );

                let translation_value = interpreter.data_ref().lang.get(&*translation_key).cloned();

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = translation_value.as_ref().is_none();

                interpreter.lang_test_store.add_assert_result(AssertResult::new_translation_key_not_found_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &translation_key,
                    translation_value.as_deref(),
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTypeEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedType",
                        type_constraint(
                            allowed=["TYPE"],
                        ),
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_type_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_type_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTypeEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedType",
                        type_constraint(
                            allowed=["TYPE"],
                        ),
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_type_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_type_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_type_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_type = expected_type_object.type_value().unwrap();

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = actual_value_object.data_type() == expected_type;

                interpreter.lang_test_store.add_assert_result(AssertResult::new_type_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    expected_type,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTypeNotEquals",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedType",
                        type_constraint(
                            allowed=["TYPE"],
                        ),
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_type_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_type_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertTypeNotEquals",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$expectedType",
                        type_constraint(
                            allowed=["TYPE"],
                        ),
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_type_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, expected_type_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                expected_type_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let expected_type = expected_type_object.type_value().unwrap();

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = actual_value_object.data_type() != expected_type;

                interpreter.lang_test_store.add_assert_result(AssertResult::new_type_not_equals_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                    expected_type,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNull",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNull",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = actual_value_object.data_type() == DataType::NULL;

                interpreter.lang_test_store.add_assert_result(AssertResult::new_null_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotNull",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotNull",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = actual_value_object.data_type() != DataType::NULL;

                interpreter.lang_test_store.add_assert_result(AssertResult::new_not_null_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertVoid",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertVoid",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = actual_value_object.data_type() == DataType::VOID;

                interpreter.lang_test_store.add_assert_result(AssertResult::new_void_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotVoid",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotVoid",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = actual_value_object.data_type() != DataType::VOID;

                interpreter.lang_test_store.add_assert_result(AssertResult::new_not_void_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertFinal",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertFinal",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = actual_value_object.is_final_data();

                interpreter.lang_test_store.add_assert_result(AssertResult::new_final_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotFinal",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotFinal",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = !actual_value_object.is_final_data();

                interpreter.lang_test_store.add_assert_result(AssertResult::new_not_final_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertStatic",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertStatic",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = actual_value_object.is_static_data();

                interpreter.lang_test_store.add_assert_result(AssertResult::new_static_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotStatic",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNotStatic",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$actualValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, actual_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                actual_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let actual_value_object_text = conversions::to_text(
                    interpreter,
                    &actual_value_object,
                    CodePosition::EMPTY,
                );

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                let passed = !actual_value_object.is_static_data();

                interpreter.lang_test_store.add_assert_result(AssertResult::new_not_static_result(
                    passed,
                    Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                    message.as_deref(),
                    &actual_value_object.borrow(), &actual_value_object_text,
                ));

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertThrow",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$expectedThrownValue",
                        type_constraint(
                            allowed=["ERROR"],
                        ),
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                expected_thrown_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, expected_thrown_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertThrow",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$expectedThrownValue",
                        type_constraint(
                            allowed=["ERROR"],
                        ),
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                expected_thrown_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, expected_thrown_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                expected_thrown_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let expected_error = expected_thrown_value_object.error_value().unwrap().err();

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                interpreter.lang_test_expected_return_value_scope_id = interpreter.scope_id();
                interpreter.lang_test_expected_throw_value = Some(expected_error);
                interpreter.lang_test_message_for_last_test_result = message;

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertReturn",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$expectedReturnValue",
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
                expected_return_value_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, expected_return_value_object, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertReturn",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$expectedReturnValue",
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                expected_return_value_object: DataObjectRef,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, expected_return_value_object, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                expected_return_value_object: DataObjectRef,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                interpreter.lang_test_expected_return_value_scope_id = interpreter.scope_id();
                interpreter.lang_test_expected_return_value = Some(expected_return_value_object);
                interpreter.lang_test_message_for_last_test_result = message;

                None
            }
        }

        {
            functions.push(crate::lang_func!(
                test_assert_without_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNoReturn",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                ),
            ));
            fn test_assert_without_message_function(
                interpreter: &mut Interpreter,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, None)
            }

            functions.push(crate::lang_func!(
                test_assert_with_message_function,
                crate::lang_func_metadata!(
                    name="testAssertNoReturn",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$message",
                    ),
                ),
            ));

            fn test_assert_with_message_function(
                interpreter: &mut Interpreter,
                message_object: DataObjectRef,
            ) -> OptionDataObjectRef {
                test_assert_internal_function(interpreter, Some(message_object))
            }

            fn test_assert_internal_function(
                interpreter: &mut Interpreter,
                message_object: OptionDataObjectRef,
            ) -> OptionDataObjectRef {
                if !interpreter.execution_flags.lang_test {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::FunctionNotSupported,
                        Some("langTest functions can only be used if the langTest flag is true"),
                        CodePosition::EMPTY,
                    ));
                }

                let message = message_object.map(|message_object| conversions::to_text(
                    interpreter,
                    &message_object,
                    CodePosition::EMPTY,
                ));

                interpreter.lang_test_expected_return_value_scope_id = interpreter.scope_id();
                interpreter.lang_test_expected_no_return_value = true;
                interpreter.lang_test_message_for_last_test_result = message;

                None
            }
        }

        functions.push(crate::lang_func!(
            test_assert_fail_function,
            crate::lang_func_metadata!(
                name="testAssertFail",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$message",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn test_assert_fail_function(
            interpreter: &mut Interpreter,
            message_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            if !interpreter.execution_flags.lang_test {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("langTest functions can only be used if the langTest flag is true"),
                    CodePosition::EMPTY,
                ));
            }

            let message = &conversions::to_text(
                interpreter,
                &message_object,
                CodePosition::EMPTY
            );

            interpreter.lang_test_store.add_assert_result(AssertResult::new_fail_result(
                Some(&interpreter.print_stack_trace(CodePosition::EMPTY)),
                Some(message),
            ));

            None
        }

        functions.push(crate::lang_func!(
            test_clear_all_translations_function,
            crate::lang_func_metadata!(
                name="testClearAllTranslations",
                return_type_constraint(
                    allowed=["VOID"],
                ),
            ),
        ));
        fn test_clear_all_translations_function(
            interpreter: &mut Interpreter,
        ) -> OptionDataObjectRef {
            if !interpreter.execution_flags.lang_test {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("langTest functions can only be used if the langTest flag is true"),
                    CodePosition::EMPTY,
                ));
            }

            interpreter.data_mut().lang.retain(|translation_key, _| translation_key.starts_with("lang."));

            None
        }

        functions.push(crate::lang_func!(
            test_print_results_function,
            crate::lang_func_metadata!(
                name="testPrintResults",
                return_type_constraint(
                    allowed=["VOID"],
                ),
            ),
        ));
        fn test_print_results_function(
            interpreter: &mut Interpreter,
        ) -> OptionDataObjectRef {
            if !interpreter.execution_flags.lang_test {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("langTest functions can only be used if the langTest flag is true"),
                    CodePosition::EMPTY,
                ));
            }

            if let Some(term) = &mut interpreter.term {
                interpreter.lang_test_store.print_results_to_terminal(term);
            }else {
                println!("{}", interpreter.lang_test_store.print_results());
            }

            None
        }
    }
}

mod linker_functions {
    use std::io::Read;
    use std::path::{Path, PathBuf};
    use std::str;
    use crate::interpreter::data::function::{native, Function, FunctionMetadata};
    use crate::interpreter::data::{DataObjectRef, OptionDataObjectRef};
    use crate::interpreter::{conversions, Data, Interpreter, InterpretingError, StackElement};
    use crate::interpreter::data::function::native::NativeError;
    use crate::lexer::CodePosition;
    use crate::utils;

    fn execute_linker_function(
        interpreter: &mut Interpreter,
        lang_file_name: &str,
        args: Vec<DataObjectRef>,
        function: impl Fn(&mut Interpreter, &mut Data),
    ) -> native::Result<OptionDataObjectRef> {
        if !lang_file_name.ends_with(".lang") {
            return Ok(Some(interpreter.set_errno_error_object_error_only(InterpretingError::NoLangFile)));
        }

        let lang_args = args.iter().
                map(|ele| conversions::to_text(interpreter, ele, CodePosition::EMPTY)).
                collect::<Vec<_>>();

        let inside_lang_standard_implementation = interpreter.current_call_stack_element().
                lang_path().starts_with("<standard>");

        let module = interpreter.current_call_stack_element().module.clone();
        let absolute_path = if inside_lang_standard_implementation {
            "lang".to_string() + &utils::remove_dots_from_file_path(
                &(interpreter.current_call_stack_element().lang_path()[10..].to_string() + "/" + lang_file_name))
        }else if let Some(module) = &module {
            //absolutePath = LangModuleManager.getModuleFilePath(module, interpreter.getCurrentCallStackElement().getLangPath(), langFileName);
            todo!("Implement modules: {module:?}")
        }else if PathBuf::from(lang_file_name).is_absolute() {
            lang_file_name.to_string()
        }else {
            interpreter.current_call_stack_element().lang_path().to_string() + "/" + lang_file_name
        };

        let lang_path_tmp;
        if inside_lang_standard_implementation {
            lang_path_tmp = absolute_path[4..absolute_path.rfind('/').unwrap()].to_string();

            //Update call stack
            interpreter.push_stack_element(
                StackElement::new(
                    &("<standard>".to_string() + &lang_path_tmp),
                    Some(&lang_file_name[lang_file_name.rfind('/').map(|i| i + 1).unwrap_or_default()..]),
                    None,
                    None,
                    None,
                    module.clone(),
                ),
                CodePosition::EMPTY,
            );
        }else if let Some(module) = &module {
            lang_path_tmp = absolute_path[0..absolute_path.rfind('/').unwrap()].to_string();

            //Update call stack
            todo!("Implement modules: {module:?} {lang_path_tmp:?}")
        }else {
            lang_path_tmp = interpreter.platform_api.get_lang_path(Path::new(&absolute_path)).
                    map_err(NativeError::apply_with_message("Invalid path"))?.
                    to_string_lossy().to_string();

            let lang_file_name = interpreter.platform_api.get_lang_file_name(Path::new(&lang_file_name)).
                    unwrap().to_string_lossy().to_string();

            //Update call stack
            interpreter.push_stack_element(
                StackElement::new(
                    &lang_path_tmp,
                    Some(&lang_file_name),
                    None,
                    None,
                    None,
                    None,
                ),
                CodePosition::EMPTY,
            );
        }

        let caller_data = interpreter.data().clone();

        interpreter.enter_scope(Some(lang_args));

        let lang_code = if inside_lang_standard_implementation {
            let file = Interpreter::RESOURCES_DIR.
                    get_file(absolute_path);

            let Some(file) = file else {
                return Ok(Some(interpreter.set_errno_error_object(
                    InterpretingError::FileNotFound,
                    Some("File not found during loading of lang standard implementation."),
                    CodePosition::EMPTY,
                )));
            };

            String::from_utf8_lossy(file.contents()).to_string()
        }else if let Some(module) = &module {
            todo!("Implement modules: {module:?}")
        }else {
            let mut file = interpreter.platform_api.get_lang_reader(Path::new(&absolute_path)).
                    map_err(NativeError::apply_with_message("File not found"))?;

            let mut lang_code = String::new();
            file.read_to_string(&mut lang_code).
                    map_err(NativeError::apply_with_message("Can not read file"))?;

            lang_code
        };

        let original_line_number = interpreter.parser_line_number();

        interpreter.reset_parser_positional_vars();
        interpreter.interpret_lines(lang_code);

        let mut caller_data = caller_data.borrow_mut();
        function(interpreter, &mut caller_data);

        interpreter.set_parser_line_number(original_line_number);

        interpreter.exit_scope();

        //Get returned value from executed Lang file
        let ret_tmp = interpreter.get_and_reset_return_value();

        //Update call stack
        interpreter.pop_stack_element();

        Ok(ret_tmp)
    }

    pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
        functions.push(crate::lang_func!(
            bind_library_function,
            crate::lang_func_metadata!(
                name="bindLibrary",
                info="Executes a lang file and copy all variables to the current scope",
                linker_function=true,
                parameter(
                    name="$fileName",
                    info="The path of the lang file",
                ),
                parameter(
                    name="&args",
                    info="Arguments which are set as &LANG_ARGS of the executed lang file",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn bind_library_function(
            interpreter: &mut Interpreter,
            file_name_object: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> native::Result<OptionDataObjectRef> {
            let lang_file_name = conversions::to_text(interpreter, &file_name_object, CodePosition::EMPTY);

            execute_linker_function(
                interpreter,
                &lang_file_name,
                args,
                |interpreter, caller_data| {
                    //Copy all vars
                    for (name, val) in &interpreter.data_ref().var {
                        let old_data = caller_data.var.get(name);
                        if old_data.is_none_or(|old_data| !old_data.is_final_data() && !old_data.is_lang_var()) {
                            //No LANG data vars nor final data
                            caller_data.var.insert(name.clone(), val.clone());
                        }
                    }
                },
            )
        }

        functions.push(crate::lang_func!(
            link_function,
            crate::lang_func_metadata!(
                name="link",
                info="Executes a lang file and copy all translation to the main scope",
                linker_function=true,
                parameter(
                    name="$fileName",
                    info="The path of the lang file",
                ),
                parameter(
                    name="&args",
                    info="Arguments which are set as &LANG_ARGS of the executed lang file",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn link_function(
            interpreter: &mut Interpreter,
            file_name_object: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> native::Result<OptionDataObjectRef> {
            let lang_file_name = conversions::to_text(interpreter, &file_name_object, CodePosition::EMPTY);

            execute_linker_function(
                interpreter,
                &lang_file_name,
                args,
                |interpreter, caller_data| {
                    //Copy linked translation map (except "lang.* = *") to the "link caller"'s translation map
                    for (k, v) in &interpreter.data_ref().lang {
                        if !k.starts_with("lang.") {
                            caller_data.lang.insert(k.clone(), v.clone());
                        }
                    }
                },
            )
        }

        functions.push(crate::lang_func!(
            include_function,
            crate::lang_func_metadata!(
                name="include",
                info="Executes a lang file and copy all variables to the current scope and all translations to the main scope",
                linker_function=true,
                parameter(
                    name="$fileName",
                    info="The path of the lang file",
                ),
                parameter(
                    name="&args",
                    info="Arguments which are set as &LANG_ARGS of the executed lang file",
                    parameter_type(var_args),
                ),
            ),
        ));
        fn include_function(
            interpreter: &mut Interpreter,
            file_name_object: DataObjectRef,
            args: Vec<DataObjectRef>,
        ) -> native::Result<OptionDataObjectRef> {
            let lang_file_name = conversions::to_text(interpreter, &file_name_object, CodePosition::EMPTY);

            execute_linker_function(
                interpreter,
                &lang_file_name,
                args,
                |interpreter, caller_data| {
                    //Copy linked translation map (except "lang.* = *") to the "link caller"'s translation map
                    for (k, v) in &interpreter.data_ref().lang {
                        if !k.starts_with("lang.") {
                            caller_data.lang.insert(k.clone(), v.clone());
                        }
                    }

                    //Copy all vars
                    for (name, val) in &interpreter.data_ref().var {
                        let old_data = caller_data.var.get(name);
                        if old_data.is_none_or(|old_data| !old_data.is_final_data() && !old_data.is_lang_var()) {
                            //No LANG data vars nor final data
                            caller_data.var.insert(name.clone(), val.clone());
                        }
                    }
                },
            )
        }

        //TODO loadModule
        //TODO unloadModule
        //TODO moduleLoadNative
        //TODO moduleUnloadNative
    }
}
