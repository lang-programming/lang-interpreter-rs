use std::io::{Cursor, Error, ErrorKind, Read};
use std::rc::Rc;
use ahash::AHashMap;
use zip::ZipArchive;
use crate::interpreter::data::{DataObjectRef, OptionDataObjectRef};
use crate::interpreter::{conversions, Interpreter, InterpretingError, StackElement};
use crate::interpreter::module::{LangModuleConfiguration, Module, ModuleType, ZipEntry};
use crate::lexer::CodePosition;
use crate::utils;

const MAX_MODULE_FILE_SIZE: usize = 1024 * 1024 * 1024; //1 GiB

pub fn get_module_file_path(module: &Module, mut current_path: &str, file: &str) -> String {
    if file.starts_with("/") {
        return utils::remove_dots_from_file_path(file);
    }

    let prefix = format!(
        "<module:{}[{}]>",
        module.file(),
        module.lang_module_configuration().name()
    );
    current_path = &current_path[prefix.len()..];

    let path = format!("{current_path}/{file}");

    if !path.starts_with("/") {
        return "/".to_string() + &path;
    }

    utils::remove_dots_from_file_path(file)
}

/**
 * @param file Must be absolute (Starts with "/")
 */
pub fn read_module_lang_file(module: &Module, file: &str) -> Result<Box<[u8]>, Error> {
    let module_lang_bytes = module.zip_data().get(&*("lang".to_string() + file));
    if let Some(module_lang_bytes) = module_lang_bytes {
        Ok(module_lang_bytes.clone())
    }else {
        Err(Error::new(ErrorKind::NotFound, format!(
            "File \"{file}\" was not found inside the module \"{}\"",
            module.lang_module_configuration().name(),
        )))
    }
}

pub fn load(interpreter: &mut Interpreter, module_file: &str, args: &[DataObjectRef]) -> OptionDataObjectRef {
    load_unload(interpreter, true, module_file, args)
}

pub fn unload(interpreter: &mut Interpreter, module_name: &str, args: &[DataObjectRef]) -> OptionDataObjectRef {
    load_unload(interpreter, false, module_name, args)
}

//TODO loadNative

//TODO unloadNative

fn load_unload(interpreter: &mut Interpreter, load: bool, module_file_or_name: &str, args: &[DataObjectRef]) -> OptionDataObjectRef {
    let module = if load {
        let mut lmc = None;
        let mut zip_entries = AHashMap::new();
        let mut zip_data = AHashMap::new();

        let error_object = read_module_data(interpreter, module_file_or_name, &mut zip_entries, &mut zip_data, &mut lmc);
        if let Some(error_object) = error_object {
            return Some(error_object);
        };

        let lmc = lmc.unwrap();

        let module_name = Box::from(lmc.name());

        let module = Rc::new(Module::new(
            Box::from(module_file_or_name),
            load,
            zip_entries,
            zip_data,
            lmc,
        ));

        if interpreter.modules.contains_key(&module_name) {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!(
                    "The Lang module \"{}\" was already loaded",
                    module_name,
                )),
                CodePosition::EMPTY,
            ));
        }

        interpreter.modules.insert(module_name, module.clone());

        module
    }else {
        let module = interpreter.modules.remove(module_file_or_name);
        let Some(module) = module else {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!(
                    "The Lang module \"{}\" was not loaded",
                    module_file_or_name,
                )),
                CodePosition::EMPTY,
            ));
        };

        module.set_load(false);

        module
    };

    //Update call stack (Path inside module archive)
    interpreter.push_stack_element(
        StackElement::new(
            &format!(
                "<module:{}[{}]>",
                module.file(),
                module.lang_module_configuration().name(),
            ),
            Some("<entryPoint>"),
            None,
            None,
            None,
            Some(module.clone())
        ),
        CodePosition::EMPTY,
    );

    let lang_args = utils::combine_arguments_without_argument_separators(
        args,
        interpreter,
        CodePosition::EMPTY
    ).iter().map(|ele| conversions::to_text(interpreter, ele, CodePosition::EMPTY)).collect();

    interpreter.enter_scope(Some(lang_args));

    let ret = load_unload_internal(interpreter, &module, load, args);

    interpreter.pop_stack_element();

    interpreter.exit_scope();

    if !load {
        //Remove exported functions and variables
        for func in module.exported_functions().borrow().iter() {
            interpreter.funcs.remove(func);
        }
    }

    ret
}

fn load_unload_internal(interpreter: &mut Interpreter, module: &Module, load: bool, args: &[DataObjectRef]) -> OptionDataObjectRef {
    let zip_entries = module.zip_entries();
    let zip_data = module.zip_data();
    let lmc = module.lang_module_configuration();

    let load_unload_str = if load { "load" } else { "unload" };

    match lmc.module_type() {
        ModuleType::Lang => {
            if !zip_entries.contains_key("lang/module.lang") {
                let module_lang = zip_entries.get("lang/module.lang/");
                if module_lang.is_some_and(|data_lmc| data_lmc.is_directory()) {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::InvalidModule,
                        Some("\"lang/module.lang\" must be a file"),
                        CodePosition::EMPTY,
                    ));
                }

                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidModule,
                    Some("\"lang/module.lang\" was not found"),
                    CodePosition::EMPTY,
                ));
            };

            let module_lang_bytes = zip_data.get("lang/module.lang").unwrap();
            let lang_code = String::from_utf8_lossy(module_lang_bytes);

            let original_line_number = interpreter.parser_line_number();

            interpreter.reset_parser_positional_vars();
            interpreter.interpret_lines(lang_code);

            interpreter.set_parser_line_number(original_line_number);

            let load_unload_fp = interpreter.data_ref().var.get(&*("fp.".to_string() + load_unload_str)).cloned();
            load_unload_fp.and_then(|load_unload_fp| {
                let Some(function_value) = load_unload_fp.function_pointer_value() else {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::InvalidFuncPtr,
                        Some(&format!("\"fp.{load_unload_str}\" is invalid")),
                        CodePosition::EMPTY,
                    ));
                };

                interpreter.call_function_pointer(&function_value, load_unload_fp.variable_name().as_deref(), args, CodePosition::EMPTY)
            })
        },

        ModuleType::Native => {
            todo!("Implement native modules")
        },
    }
}

//TODO

fn read_module_data(
    interpreter: &mut Interpreter,
    module_file: &str,
    zip_entries: &mut AHashMap<Box<str>, ZipEntry>,
    zip_data: &mut AHashMap<Box<str>, Box<[u8]>>,
    lmc_ptr: &mut Option<LangModuleConfiguration>,
) -> OptionDataObjectRef {
    let reader = interpreter.platform_api.get_lang_reader(module_file.as_ref());
    let mut reader = match reader {
        Ok(reader) => reader,
        Err(e) => {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::FileNotFound,
                Some(&e.to_string()),
                CodePosition::EMPTY,
            ));
        },
    };
    let mut zip_archive = Vec::new();
    let ret = reader.read_to_end(&mut zip_archive);
    if let Err(e) = ret {
        return Some(interpreter.set_errno_error_object(
            InterpretingError::FileNotFound,
            Some(&e.to_string()),
            CodePosition::EMPTY,
        ));
    };

    let zip_archive = ZipArchive::new(Cursor::new(zip_archive));
    let mut zip_archive = match zip_archive {
        Ok(zip_archive) => zip_archive,
        Err(e) => {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::FileNotFound,
                Some(&e.to_string()),
                CodePosition::EMPTY,
            ));
        },
    };

    for i in 0..zip_archive.len() {
        let zip_entry = zip_archive.by_index(i);
        let mut zip_entry = match zip_entry {
            Ok(zip_entry) => zip_entry,
            Err(e) => {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FileNotFound,
                    Some(&e.to_string()),
                    CodePosition::EMPTY,
                ));
            },
        };

        if zip_entry.size() as usize > MAX_MODULE_FILE_SIZE {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidModule,
                Some(&format!(
                    "\"/{}\" is larger than {MAX_MODULE_FILE_SIZE}",
                    zip_entry.name(),
                )),
                CodePosition::EMPTY,
            ));
        }

        let mut buffer = Vec::new();
        let ret = zip_entry.read_to_end(&mut buffer);
        if let Err(e) = ret {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::FileNotFound,
                Some(&e.to_string()),
                CodePosition::EMPTY,
            ));
        };

        zip_entries.insert(Box::from(zip_entry.name()), ZipEntry::new(zip_entry.is_dir()));
        zip_data.insert(Box::from(zip_entry.name()), buffer.into_boxed_slice());
    }

    if !zip_entries.contains_key("data.lmc") {
        let data_lmc = zip_entries.get("data.lmc/");
        if data_lmc.is_some_and(|data_lmc| data_lmc.is_directory()) {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidModule,
                Some("\"/data.lmc\" must be a file"),
                CodePosition::EMPTY,
            ));
        }

        return Some(interpreter.set_errno_error_object(
            InterpretingError::InvalidModule,
            Some("\"/data.lmc\" was not found"),
            CodePosition::EMPTY,
        ));
    };

    let data_lmc_bytes = zip_data.get("data.lmc").unwrap();
    let lmc = LangModuleConfiguration::parse_lmc(&String::from_utf8_lossy(data_lmc_bytes));
    let lmc = match lmc {
        Ok(lmc) => lmc,
        Err(e) => {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidModule,
                Some(&format!("\"/data.lmc\" is invalid: {e}")),
                CodePosition::EMPTY,
            ));
        },
    };

    let supported_implementations = lmc.supported_implementations();
    if let Some(supported_implementations) = supported_implementations {
        let mut standard_lang_supported = false;
        for implementation in supported_implementations {
            if **implementation == *"langRS" {
                standard_lang_supported = true;

                break;
            }
        }

        if !standard_lang_supported {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidModule,
                Some("The module is not supported in langRS!"),
                CodePosition::EMPTY,
            ));
        }
    }

    if let Some(min_supported_version) = lmc.min_supported_version() {
        let min_comp_ver = utils::compare_versions_str(min_supported_version, Interpreter::VERSION);
        if let Some(min_comp_ver) = min_comp_ver {
            if min_comp_ver.is_gt() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidModule,
                    Some(&format!(
                        "The minimal supported version of the module is higher than {}: {min_supported_version}!",
                        Interpreter::VERSION,
                    )),
                    CodePosition::EMPTY,
                ));
            }
        }else {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidModule,
                Some("The min supported version has an invalid format!"),
                CodePosition::EMPTY,
            ));
        }
    }

    if let Some(max_supported_version) = lmc.max_supported_version() {
        let max_comp_ver = utils::compare_versions_str(max_supported_version, Interpreter::VERSION);
        if let Some(max_comp_ver) = max_comp_ver {
            if max_comp_ver.is_lt() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidModule,
                    Some(&format!(
                        "The maximal supported version of the module is lower than {}: {max_supported_version}!",
                        Interpreter::VERSION,
                    )),
                    CodePosition::EMPTY,
                ));
            }
        }else {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidModule,
                Some("The max supported version has an invalid format!"),
                CodePosition::EMPTY,
            ));
        }
    }

    *lmc_ptr = Some(lmc);

    None
}

//TODO
