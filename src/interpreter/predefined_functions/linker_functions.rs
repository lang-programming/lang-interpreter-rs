use std::path::{Path, PathBuf};
use std::str;
use crate::interpreter::data::function::{native, Function, FunctionMetadata};
use crate::interpreter::data::{DataObjectRef, OptionDataObjectRef};
use crate::interpreter::{conversions, module_manager, Data, Interpreter, InterpretingError, StackElement};
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

    let module = interpreter.current_call_stack_element().lang_path().starts_with("<module:").
            then(|| interpreter.current_call_stack_element().module.clone()).flatten();
    let absolute_path = if inside_lang_standard_implementation {
        "lang".to_string() + &utils::remove_dots_from_file_path(
            interpreter.current_call_stack_element().lang_path()[10..].to_string() + "/" + lang_file_name)
    }else if let Some(module) = &module {
        module_manager::get_module_file_path(module, interpreter.current_call_stack_element().lang_path(), lang_file_name)
    }else if PathBuf::from(lang_file_name).is_absolute() || lang_file_name.starts_with("/") {
        lang_file_name.to_string()
    }else {
        let mut original_path = interpreter.current_call_stack_element().lang_path();

        if original_path.ends_with("/") {
            original_path = &original_path[..original_path.len() - 1];
        }

        original_path.to_string() + "/" + lang_file_name
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
        interpreter.push_stack_element(
            StackElement::new(
                &format!(
                    "<module:{}[{}]>{}",
                    module.file(),
                    module.lang_module_configuration().name(),
                    lang_path_tmp,
                ),
                Some(&lang_file_name[lang_file_name.rfind('/').map(|index| index + 1).unwrap_or_default()..]),
                None,
                None,
                None,
                Some(module.clone()),
            ),
            CodePosition::EMPTY,
        );
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
        let file = Interpreter::RESOURCES_DIR.get_file(absolute_path);

        let Some(file) = file else {
            interpreter.exit_scope();

            //Update call stack
            interpreter.pop_stack_element();

            return Ok(Some(interpreter.set_errno_error_object(
                InterpretingError::FileNotFound,
                Some("File not found during loading of lang standard implementation."),
                CodePosition::EMPTY,
            )));
        };

        String::from_utf8_lossy(file.contents()).to_string()
    }else if let Some(module) = &module {
        let file = module_manager::read_module_lang_file(module, &absolute_path);

        let file = match file {
            Ok(file) => file,
            Err(e) => {
                interpreter.exit_scope();

                //Update call stack
                interpreter.pop_stack_element();

                return Ok(Some(interpreter.set_errno_error_object(
                    InterpretingError::FileNotFound,
                    Some(&e.to_string()),
                    CodePosition::EMPTY,
                )));
            },
        };

        String::from_utf8_lossy(&file).to_string()
    }else {
        let file_bytes = interpreter.platform_api.get_lang_reader(Path::new(&absolute_path)).
                map_err(NativeError::apply_with_message("File not found")).inspect_err(|_| {
            interpreter.exit_scope();

            //Update call stack
            interpreter.pop_stack_element();
        })?;

        String::from_utf8_lossy(&file_bytes).to_string()
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

    functions.push(crate::lang_func!(
            load_module_function,
            crate::lang_func_metadata!(
                name="loadModule",
                linker_function=true,
                parameter(
                    name="$moduleFile",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn load_module_function(
        interpreter: &mut Interpreter,
        module_file_object: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let mut module_file = conversions::to_text(interpreter, &module_file_object, CodePosition::EMPTY).to_string();

        if !module_file.ends_with(".lm") {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::NoLangFile,
                Some("Modules must have a file extension of\".lm\""),
                CodePosition::EMPTY,
            ));
        }

        if !PathBuf::from(module_file.clone()).is_absolute() {
            module_file = format!(
                "{}/{module_file}",
                interpreter.current_call_stack_element().lang_path(),
            );
        }

        module_manager::load(interpreter, &module_file, &utils::separate_arguments_with_argument_separators(&args))
    }

    functions.push(crate::lang_func!(
            unload_module_function,
            crate::lang_func_metadata!(
                name="unloadModule",
                linker_function=true,
                parameter(
                    name="$moduleName",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn unload_module_function(
        interpreter: &mut Interpreter,
        module_name_object: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let module_name = conversions::to_text(interpreter, &module_name_object, CodePosition::EMPTY).to_string();
        for c in module_name.bytes() {
            if !c.is_ascii_alphanumeric() && c != b'_' {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("The module name may only contain alphanumeric characters and underscores (_)"),
                    CodePosition::EMPTY,
                ));
            }
        }

        module_manager::unload(interpreter, &module_name, &utils::separate_arguments_with_argument_separators(&args))
    }

    //TODO moduleLoadNative
    //TODO moduleUnloadNative
}
