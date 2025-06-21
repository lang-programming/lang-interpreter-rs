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
                interpreter.platform_api.println(&message);
            }
            interpreter.platform_api.print("Input: ");

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
            term.log(level, format!("[From Lang file]: {text}"), Interpreter::LOG_TAG);
        }else {
            interpreter.set_errno(InterpretingError::NoTerminalWarning, None, CodePosition::EMPTY);

            //Write to standard error if the log level is WARNING or higher
            if log_level > 3 {
                interpreter.platform_api.println_error(&format!("[{:<8}]: {text}", level.name()));
            }else {
                interpreter.platform_api.println(&format!("[{:<8}]: {text}", level.name()));
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
            term.log(level, format!("[From Lang file]: {text}"), Interpreter::LOG_TAG);
        }else {
            interpreter.set_errno(InterpretingError::NoTerminalWarning, None, CodePosition::EMPTY);

            //Write to standard error if the log level is WARNING or higher
            if level.level() > 3 {
                interpreter.platform_api.println_error(&format!("[{:<8}]: {text}", level.name()));
            }else {
                interpreter.platform_api.println(&format!("[{:<8}]: {text}", level.name()));
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

                //Remove trailing newlines
                let line = line.trim_end_matches(['\n', '\r']);

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
        interpreter.platform_api.print(&text);
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
        interpreter.platform_api.println(&text);
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
        interpreter.platform_api.print(&out);

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
        interpreter.platform_api.print_error(&text);
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
        interpreter.platform_api.println_error(&text);
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
        interpreter.platform_api.print_error(&out);

        None
    }
}
