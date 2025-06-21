use std::rc::Rc;
use gc::Gc;
use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::data::{DataObject, DataObjectRef, OptionDataObjectRef};
use crate::interpreter::{conversions, Interpreter, InterpretingError};
use crate::lexer::CodePosition;

fn contains_non_word_chars(module_name: &str) -> bool {
    if module_name.is_empty() {
        return true;
    }

    for c in module_name.bytes() {
        if !c.is_ascii_alphanumeric() && c != b'_' {
            return true;
        }
    }

    false
}

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            get_loaded_modules_function,
            crate::lang_func_metadata!(
                name="getLoadedModules",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
            ),
        ));
    fn get_loaded_modules_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(interpreter.modules.keys().
                    map(|str| DataObjectRef::new(DataObject::new_text(&**str))).
                    collect())
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            get_module_variable_names_function,
            crate::lang_func_metadata!(
                name="getModuleVariableNames",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="$moduleName",
                ),
            ),
        ));
    fn get_module_variable_names_function(
        interpreter: &mut Interpreter,
        module_name_object: DataObjectRef,
    ) -> DataObjectRef {
        let module_name = conversions::to_text(interpreter, &module_name_object, CodePosition::EMPTY);

        if contains_non_word_chars(&module_name) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The module name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        let module = interpreter.modules.get(&module_name);
        let Some(module) = module else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!("The module \"{module_name}\" was not found")),
                CodePosition::EMPTY,
            );
        };

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(module.exported_variables().borrow().keys().
                    map(|str| DataObjectRef::new(DataObject::new_text(&**str))).
                    collect())
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            get_module_function_names_function,
            crate::lang_func_metadata!(
                name="getModuleFunctionNames",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="$moduleName",
                ),
            ),
        ));
    fn get_module_function_names_function(
        interpreter: &mut Interpreter,
        module_name_object: DataObjectRef,
    ) -> DataObjectRef {
        let module_name = conversions::to_text(interpreter, &module_name_object, CodePosition::EMPTY);

        if contains_non_word_chars(&module_name) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The module name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        let module = interpreter.modules.get(&module_name);
        let Some(module) = module else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!("The module \"{module_name}\" was not found")),
                CodePosition::EMPTY,
            );
        };

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(module.exported_functions().borrow().iter().
                    map(|str| DataObjectRef::new(DataObject::new_text(&**str))).
                    collect())
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            get_module_variable_function,
            crate::lang_func_metadata!(
                name="getModuleVariable",
                parameter(
                    name="$moduleName",
                ),
                parameter(
                    name="$variableName",
                ),
            ),
        ));
    fn get_module_variable_function(
        interpreter: &mut Interpreter,
        module_name_object: DataObjectRef,
        variable_name_object: DataObjectRef,
    ) -> DataObjectRef {
        let module_name = conversions::to_text(interpreter, &module_name_object, CodePosition::EMPTY);
        let variable_name = conversions::to_text(interpreter, &variable_name_object, CodePosition::EMPTY);

        if contains_non_word_chars(&module_name) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The module name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        if !variable_name.starts_with("$") && !variable_name.starts_with("&") && !variable_name.starts_with("fp.") {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The variable name must start with \"$\", \"&\", or \"fp.\""),
                CodePosition::EMPTY,
            );
        }

        let variable_prefix_len = if matches!(variable_name.as_bytes()[0], b'$' | b'&') { 1 } else { 3 };

        if contains_non_word_chars(&variable_name[variable_prefix_len..]) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The variable name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        let module = interpreter.modules.get(&module_name);
        let Some(module) = module else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!("The module \"{module_name}\" was not found")),
                CodePosition::EMPTY,
            );
        };

        let variable = module.exported_variables().borrow().get(&*variable_name).cloned();
        let Some(variable) = variable else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!(
                    "The variable \"{variable_name}\" was not found in the module \"{module_name}\"",
                )),
                CodePosition::EMPTY,
            );
        };

        variable
    }

    functions.push(crate::lang_func!(
            get_module_variable_normal_function,
            crate::lang_func_metadata!(
                name="getModuleVariableNormal",
                parameter(
                    name="$moduleName",
                ),
                parameter(
                    name="$variableName",
                ),
            ),
        ));
    fn get_module_variable_normal_function(
        interpreter: &mut Interpreter,
        module_name_object: DataObjectRef,
        variable_name_object: DataObjectRef,
    ) -> DataObjectRef {
        let module_name = conversions::to_text(interpreter, &module_name_object, CodePosition::EMPTY);
        let variable_name = conversions::to_text(interpreter, &variable_name_object, CodePosition::EMPTY);

        if contains_non_word_chars(&module_name) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The module name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        if contains_non_word_chars(&variable_name) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The variable name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        let variable_name = "$".to_string() + &variable_name;

        let module = interpreter.modules.get(&module_name);
        let Some(module) = module else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!("The module \"{module_name}\" was not found")),
                CodePosition::EMPTY,
            );
        };

        let variable = module.exported_variables().borrow().get(&*variable_name).cloned();
        let Some(variable) = variable else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!(
                    "The variable \"{variable_name}\" was not found in the module \"{module_name}\"",
                )),
                CodePosition::EMPTY,
            );
        };

        variable
    }

    functions.push(crate::lang_func!(
            get_module_variable_composite_function,
            crate::lang_func_metadata!(
                name="getModuleVariableComposite",
                parameter(
                    name="$moduleName",
                ),
                parameter(
                    name="$variableName",
                ),
            ),
        ));
    fn get_module_variable_composite_function(
        interpreter: &mut Interpreter,
        module_name_object: DataObjectRef,
        variable_name_object: DataObjectRef,
    ) -> DataObjectRef {
        let module_name = conversions::to_text(interpreter, &module_name_object, CodePosition::EMPTY);
        let variable_name = conversions::to_text(interpreter, &variable_name_object, CodePosition::EMPTY);

        if contains_non_word_chars(&module_name) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The module name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        if contains_non_word_chars(&variable_name) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The variable name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        let variable_name = "&".to_string() + &variable_name;

        let module = interpreter.modules.get(&module_name);
        let Some(module) = module else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!("The module \"{module_name}\" was not found")),
                CodePosition::EMPTY,
            );
        };

        let variable = module.exported_variables().borrow().get(&*variable_name).cloned();
        let Some(variable) = variable else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!(
                    "The variable \"{variable_name}\" was not found in the module \"{module_name}\"",
                )),
                CodePosition::EMPTY,
            );
        };

        variable
    }

    functions.push(crate::lang_func!(
            get_module_variable_function_pointer_function,
            crate::lang_func_metadata!(
                name="getModuleVariableFunctionPointer",
                parameter(
                    name="$moduleName",
                ),
                parameter(
                    name="$variableName",
                ),
            ),
        ));
    fn get_module_variable_function_pointer_function(
        interpreter: &mut Interpreter,
        module_name_object: DataObjectRef,
        variable_name_object: DataObjectRef,
    ) -> DataObjectRef {
        let module_name = conversions::to_text(interpreter, &module_name_object, CodePosition::EMPTY);
        let variable_name = conversions::to_text(interpreter, &variable_name_object, CodePosition::EMPTY);

        if contains_non_word_chars(&module_name) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The module name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        if contains_non_word_chars(&variable_name) {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The variable name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            );
        }

        let variable_name = "fp.".to_string() + &variable_name;

        let module = interpreter.modules.get(&module_name);
        let Some(module) = module else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!("The module \"{module_name}\" was not found")),
                CodePosition::EMPTY,
            );
        };

        let variable = module.exported_variables().borrow().get(&*variable_name).cloned();
        let Some(variable) = variable else {
            return interpreter.set_errno_error_object(
                InterpretingError::ModuleLoadUnloadErr,
                Some(&format!(
                    "The variable \"{variable_name}\" was not found in the module \"{module_name}\"",
                )),
                CodePosition::EMPTY,
            );
        };

        variable
    }

    functions.push(crate::lang_func!(
            module_export_function_function,
            crate::lang_func_metadata!(
                name="moduleExportFunction",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$functionName",
                ),
                parameter(
                    name="fp.func",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"],
                    ),
                ),
            ),
        ));
    fn module_export_function_function(
        interpreter: &mut Interpreter,
        function_name_object: DataObjectRef,
        function_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let module = interpreter.current_call_stack_element().module();
        //TODO improve when if let chains become stable
        let Some(module) = module else {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::FunctionNotSupported,
                Some("\"func.moduleExportFunction\" can only be used inside a module which is in the \"load\" state"),
                CodePosition::EMPTY,
            ));
        };
        if !module.is_load() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::FunctionNotSupported,
                Some("\"func.moduleExportFunction\" can only be used inside a module which is in the \"load\" state"),
                CodePosition::EMPTY,
            ));
        }

        let function_name = conversions::to_text(interpreter, &function_name_object, CodePosition::EMPTY);

        if contains_non_word_chars(&function_name) {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The function name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            ));
        }

        module.exported_functions().borrow_mut().push(function_name.clone());

        let function = function_object.function_pointer_value().unwrap();

        interpreter.funcs.insert(function_name, Gc::new(function.copy_with_linker(false)));

        None
    }

    functions.push(crate::lang_func!(
            module_export_linker_function_function,
            crate::lang_func_metadata!(
                name="moduleExportLinkerFunction",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$functionName",
                ),
                parameter(
                    name="fp.func",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"],
                    ),
                ),
            ),
        ));
    fn module_export_linker_function_function(
        interpreter: &mut Interpreter,
        function_name_object: DataObjectRef,
        function_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let module = interpreter.current_call_stack_element().module();
        //TODO improve when if let chains become stable
        let Some(module) = module else {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::FunctionNotSupported,
                Some("\"func.moduleExportLinkerFunction\" can only be used inside a module which is in the \"load\" state"),
                CodePosition::EMPTY,
            ));
        };
        if !module.is_load() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::FunctionNotSupported,
                Some("\"func.moduleExportLinkerFunction\" can only be used inside a module which is in the \"load\" state"),
                CodePosition::EMPTY,
            ));
        }

        let function_name = conversions::to_text(interpreter, &function_name_object, CodePosition::EMPTY);

        if contains_non_word_chars(&function_name) {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("The function name may only contain alphanumeric characters and underscore (_)"),
                CodePosition::EMPTY,
            ));
        }

        module.exported_functions().borrow_mut().push(function_name.clone());

        let function = function_object.function_pointer_value().unwrap();

        interpreter.funcs.insert(function_name, Gc::new(function.copy_with_linker(true)));

        None
    }

    {
        functions.push(crate::lang_func!(
                module_export_normal_variable_without_final_function,
                crate::lang_func_metadata!(
                    name="moduleExportNormalVariable",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$variableName",
                    ),
                    parameter(
                        name="$variable",
                    ),
                ),
            ));
        fn module_export_normal_variable_without_final_function(
            interpreter: &mut Interpreter,
            variable_name_object: DataObjectRef,
            variable_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            module_export_normal_variable_internal_function(interpreter, variable_name_object, variable_object, false)
        }

        functions.push(crate::lang_func!(
                module_export_normal_variable_with_final_function,
                crate::lang_func_metadata!(
                    name="moduleExportNormalVariable",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$variableName",
                    ),
                    parameter(
                        name="$variable",
                    ),
                    parameter(
                        name="$final",
                        parameter_type(boolean),
                    ),
                ),
            ));
        fn module_export_normal_variable_with_final_function(
            interpreter: &mut Interpreter,
            variable_name_object: DataObjectRef,
            variable_object: DataObjectRef,
            final_data_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            let final_data = conversions::to_bool(interpreter, &final_data_object, CodePosition::EMPTY);

            module_export_normal_variable_internal_function(interpreter, variable_name_object, variable_object, final_data)
        }

        fn module_export_normal_variable_internal_function(
            interpreter: &mut Interpreter,
            variable_name_object: DataObjectRef,
            variable_object: DataObjectRef,
            final_data: bool,
        ) -> OptionDataObjectRef {
            let module = interpreter.current_call_stack_element().module();
            //TODO improve when if let chains become stable
            let Some(module) = module else {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("\"func.moduleExportNormalVariable\" can only be used inside a module which is in the \"load\" state"),
                    CodePosition::EMPTY,
                ));
            };
            if !module.is_load() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("\"func.moduleExportNormalVariable\" can only be used inside a module which is in the \"load\" state"),
                    CodePosition::EMPTY,
                ));
            }

            let variable_name = conversions::to_text(interpreter, &variable_name_object, CodePosition::EMPTY);

            if contains_non_word_chars(&variable_name) {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("The variable name may only contain alphanumeric characters and underscore (_)"),
                    CodePosition::EMPTY,
                ));
            }

            let variable_name = "$".to_string() + &variable_name;

            module.exported_variables().borrow_mut().insert(
                Rc::from(&*variable_name),
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&variable_object.borrow())?.
                            set_final_data(final_data).
                            set_variable_name(Some(&variable_name))
                }).unwrap()),
            );

            None
        }
    }

    {
        functions.push(crate::lang_func!(
                module_export_composite_variable_without_final_function,
                crate::lang_func_metadata!(
                    name="moduleExportCompositeVariable",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$variableName",
                    ),
                    parameter(
                        name="&variable",
                    ),
                ),
            ));
        fn module_export_composite_variable_without_final_function(
            interpreter: &mut Interpreter,
            variable_name_object: DataObjectRef,
            variable_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            module_export_composite_variable_internal_function(interpreter, variable_name_object, variable_object, false)
        }

        functions.push(crate::lang_func!(
                module_export_composite_variable_with_final_function,
                crate::lang_func_metadata!(
                    name="moduleExportCompositeVariable",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$variableName",
                    ),
                    parameter(
                        name="&variable",
                    ),
                    parameter(
                        name="$final",
                        parameter_type(boolean),
                    ),
                ),
            ));
        fn module_export_composite_variable_with_final_function(
            interpreter: &mut Interpreter,
            variable_name_object: DataObjectRef,
            variable_object: DataObjectRef,
            final_data_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            let final_data = conversions::to_bool(interpreter, &final_data_object, CodePosition::EMPTY);

            module_export_composite_variable_internal_function(interpreter, variable_name_object, variable_object, final_data)
        }

        fn module_export_composite_variable_internal_function(
            interpreter: &mut Interpreter,
            variable_name_object: DataObjectRef,
            variable_object: DataObjectRef,
            final_data: bool,
        ) -> OptionDataObjectRef {
            let module = interpreter.current_call_stack_element().module();
            //TODO improve when if let chains become stable
            let Some(module) = module else {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("\"func.moduleExportCompositeVariable\" can only be used inside a module which is in the \"load\" state"),
                    CodePosition::EMPTY,
                ));
            };
            if !module.is_load() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("\"func.moduleExportCompositeVariable\" can only be used inside a module which is in the \"load\" state"),
                    CodePosition::EMPTY,
                ));
            }

            let variable_name = conversions::to_text(interpreter, &variable_name_object, CodePosition::EMPTY);

            if contains_non_word_chars(&variable_name) {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("The variable name may only contain alphanumeric characters and underscore (_)"),
                    CodePosition::EMPTY,
                ));
            }

            let variable_name = "&".to_string() + &variable_name;

            module.exported_variables().borrow_mut().insert(
                Rc::from(&*variable_name),
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&variable_object.borrow())?.
                            set_final_data(final_data).
                            set_variable_name(Some(&variable_name))
                }).unwrap()),
            );

            None
        }
    }

    {
        functions.push(crate::lang_func!(
                module_export_function_pointer_variable_without_final_function,
                crate::lang_func_metadata!(
                    name="moduleExportFunctionPointerVariable",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$variableName",
                    ),
                    parameter(
                        name="fp.variable",
                    ),
                ),
            ));
        fn module_export_function_pointer_variable_without_final_function(
            interpreter: &mut Interpreter,
            variable_name_object: DataObjectRef,
            variable_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            module_export_function_pointer_variable_internal_function(interpreter, variable_name_object, variable_object, false)
        }

        functions.push(crate::lang_func!(
                module_export_function_pointer_variable_with_final_function,
                crate::lang_func_metadata!(
                    name="moduleExportFunctionPointerVariable",
                    return_type_constraint(
                        allowed=["VOID"],
                    ),
                    parameter(
                        name="$variableName",
                    ),
                    parameter(
                        name="fp.variable",
                    ),
                    parameter(
                        name="$final",
                        parameter_type(boolean),
                    ),
                ),
            ));
        fn module_export_function_pointer_variable_with_final_function(
            interpreter: &mut Interpreter,
            variable_name_object: DataObjectRef,
            variable_object: DataObjectRef,
            final_data_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            let final_data = conversions::to_bool(interpreter, &final_data_object, CodePosition::EMPTY);

            module_export_function_pointer_variable_internal_function(interpreter, variable_name_object, variable_object, final_data)
        }

        fn module_export_function_pointer_variable_internal_function(
            interpreter: &mut Interpreter,
            variable_name_object: DataObjectRef,
            variable_object: DataObjectRef,
            final_data: bool,
        ) -> OptionDataObjectRef {
            let module = interpreter.current_call_stack_element().module();
            //TODO improve when if let chains become stable
            let Some(module) = module else {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("\"func.moduleExportFunctionPointerVariable\" can only be used inside a module which is in the \"load\" state"),
                    CodePosition::EMPTY,
                ));
            };
            if !module.is_load() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FunctionNotSupported,
                    Some("\"func.moduleExportFunctionPointerVariable\" can only be used inside a module which is in the \"load\" state"),
                    CodePosition::EMPTY,
                ));
            }

            let variable_name = conversions::to_text(interpreter, &variable_name_object, CodePosition::EMPTY);

            if contains_non_word_chars(&variable_name) {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("The variable name may only contain alphanumeric characters and underscore (_)"),
                    CodePosition::EMPTY,
                ));
            }

            let variable_name = "fp.".to_string() + &variable_name;

            module.exported_variables().borrow_mut().insert(
                Rc::from(&*variable_name),
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&variable_object.borrow())?.
                            set_final_data(final_data).
                            set_variable_name(Some(&variable_name))
                }).unwrap()),
            );

            None
        }
    }
}
