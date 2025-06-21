use std::ptr;
use std::ops::Deref;
use gc::Gc;
use crate::interpreter::data::function::{native, Function, FunctionMetadata};
use crate::interpreter::data::{DataObject, DataObjectRef, DataTypeConstraint, OptionDataObjectRef, StructObject};
use crate::interpreter::{conversions, Interpreter, InterpretingError, StackElement};
use crate::interpreter::data::function::native::NativeError;
use crate::lexer::CodePosition;
use crate::utils;

#[cfg(not(feature = "wasm"))]
use std::thread;

#[cfg(not(feature = "wasm"))]
use std::time::{Duration, SystemTime, UNIX_EPOCH};
#[cfg(feature = "wasm")]
use web_time::{SystemTime, UNIX_EPOCH};

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

        #[cfg(not(feature = "wasm"))]
        {
            thread::sleep(Duration::from_millis(milli_seconds as u64));
        }

        if cfg!(feature = "wasm") {
            Some(interpreter.set_errno_error_object(
                InterpretingError::FunctionNotSupported,
                Some("func.sleep is not supported for WASM"),
                CodePosition::EMPTY,
            ))
        }else {
            None
        }
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

        let mut module_path: Option<String> = None;
        let mut module_file: Option<&str> = None;
        //TODO improve when if let chains become stable
        if current_stack_element.lang_path().starts_with("<module:") {
            if let Some(module) = current_stack_element.module() {
                let prefix = format!(
                    "<module:{}[{}]>",
                    module.file(),
                    module.lang_module_configuration().name()
                );

                let mut path = current_stack_element.lang_path()[prefix.len()..].to_string();
                if !path.starts_with("/") {
                    path = "/".to_string() + &path;
                }

                module_path = Some(path);
                module_file = current_stack_element.lang_file();
            }
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_struct(Gc::new(StructObject::new_instance(
                interpreter.standard_types["&StackTraceElement"].struct_value().unwrap(),
                &[
                    DataObjectRef::new(DataObject::new_text(current_stack_element.lang_path())),
                    DataObjectRef::new(DataObject::new_optional_text(current_stack_element.lang_file())),
                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_struct(Gc::new(StructObject::new_instance(
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
                    let mut module_path: Option<String> = None;
                    let mut module_file: Option<&str> = None;
                    //TODO improve when if let chains become stable
                    if ele.lang_path().starts_with("<module:") {
                        if let Some(module) = ele.module() {
                            let prefix = format!(
                                "<module:{}[{}]>",
                                module.file(),
                                module.lang_module_configuration().name()
                            );

                            let mut path = ele.lang_path()[prefix.len()..].to_string();
                            if !path.starts_with("/") {
                                path = "/".to_string() + &path;
                            }

                            module_path = Some(path);
                            module_file = ele.lang_file();
                        }
                    }

                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_struct(Gc::new(StructObject::new_instance(
                            interpreter.standard_types["&StackTraceElement"].struct_value().unwrap(),
                            &[
                                DataObjectRef::new(DataObject::new_text(ele.lang_path())),
                                DataObjectRef::new(DataObject::new_optional_text(ele.lang_file())),
                                DataObjectRef::new(DataObject::with_update(|data_object| {
                                    data_object.set_struct(Gc::new(StructObject::new_instance(
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
