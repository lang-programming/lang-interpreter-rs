use std::collections::HashMap;
use std::rc::Rc;
use crate::interpreter::data::function::FunctionPointerObject;
use crate::interpreter::data::FunctionPointerObjectRef;

pub fn add_predefined_functions(funcs: &mut HashMap<Box<str>, FunctionPointerObjectRef>) {
    let mut functions = Vec::new();

    reset_functions::add_functions(&mut functions);
    error_functions::add_functions(&mut functions);
    lang_functions::add_functions(&mut functions);
    system_functions::add_functions(&mut functions);
    //TODO
    lang_test_functions::add_functions(&mut functions);

    let functions = FunctionPointerObject::create_function_pointer_objects_from_native_functions(functions);
    for (function_name, functions) in functions {
        funcs.insert(function_name, Rc::new(functions));
    }
}

pub fn add_predefined_linker_functions(funcs: &mut HashMap<Box<str>, FunctionPointerObjectRef>) {
    let mut functions = Vec::new();

    linker_functions::add_functions(&mut functions);

    let functions = FunctionPointerObject::create_function_pointer_objects_from_native_functions(functions);
    for (function_name, functions) in functions {
        funcs.insert(function_name, Rc::new(functions));
    }
}

mod reset_functions {
    use std::rc::Rc;
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

            interpreter.data_mut().var.remove(&Rc::from(variable_name));

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

            let translation_value = interpreter.data_ref().lang.get(&Rc::from(&*translation_key)).cloned();
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

            let translation_value = interpreter.data_ref().lang.get(&Rc::from(&*translation_key)).cloned();
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

mod lang_test_functions {
    use std::rc::Rc;
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

                let translation_value = interpreter.data_ref().lang.get(&Rc::from(&*translation_key)).cloned();

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

                let translation_value = interpreter.data_ref().lang.get(&Rc::from(&*translation_key)).cloned();

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

                let translation_value = interpreter.data_ref().lang.get(&Rc::from(&*translation_key)).cloned();

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

                let translation_value = interpreter.data_ref().lang.get(&Rc::from(&*translation_key)).cloned();

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

            str::from_utf8(file.contents()).map_err(NativeError::apply)?.to_string()
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
