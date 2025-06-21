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
            interpreter.platform_api.println(&interpreter.lang_test_store.print_results());
        }

        None
    }
}
