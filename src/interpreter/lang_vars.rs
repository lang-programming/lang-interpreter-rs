use std::collections::hash_map::Entry;
use std::path;
use std::rc::Rc;
use gc::Gc;
use crate::interpreter::data::{DataObject, DataObjectRef, DataType, ErrorObject, OptionDataObjectRef};
use crate::interpreter::{Interpreter, InterpretingError};
use crate::utils;

fn add_lang_var(interpreter: &mut Interpreter, variable_name: &str, mut lang_var: DataObject) {
    lang_var.update(|data_object| {
        data_object.set_lang_var().
                set_variable_name(Some(variable_name))
    }).unwrap();

    interpreter.data_mut().var.insert(Rc::from(variable_name), DataObjectRef::new(lang_var));
}

fn add_static_lang_var(interpreter: &mut Interpreter, variable_name: &str, mut lang_var: DataObject) {
    lang_var.update(|data_object| {
        data_object.set_static_data(true).
                set_lang_var().
                set_variable_name(Some(variable_name))
    }).unwrap();

    let mut data = interpreter.data_mut();
    let entry = data.var.entry(Rc::from(variable_name));
    if let Entry::Vacant(entry) = entry {
        entry.insert(DataObjectRef::new(lang_var));
    }
}

pub fn add_essential_lang_vars(interpreter: &mut Interpreter, lang_args: OptionDataObjectRef) {
    interpreter.data_mut().var.insert(Rc::from("&LANG_ARGS"), lang_args.
            unwrap_or_else(|| DataObjectRef::new(DataObject::with_update_final(|data_object| {
                data_object.set_array(Box::from([]))?.
                        set_lang_var().
                        set_variable_name(Some("&LANG_ARGS"))
            }).unwrap())));

    add_system_lang_vars(interpreter);
    add_basic_number_lang_vars(interpreter);
    add_error_lang_vars(interpreter);
    add_type_lang_vars(interpreter);

    //Non-final
    add_static_lang_var(interpreter, "$LANG_ERRNO", DataObject::with_update(|data_object| {
        data_object.set_int(InterpretingError::NoError.error_code())
    }).unwrap());
}
fn add_system_lang_vars(interpreter: &mut Interpreter) {
    add_lang_var(interpreter, "$LANG_VERSION", DataObject::new_final_text(Interpreter::VERSION));
    add_lang_var(interpreter, "$LANG_NAME", DataObject::new_final_text("langRS"));
    add_lang_var(interpreter, "$LANG_RAND_MAX", DataObject::with_update_final(|data_object| {
        data_object.set_int(i32::MAX)
    }).unwrap());
    add_lang_var(interpreter, "$LANG_OS_NAME", DataObject::new_final_text(utils::get_os_name()));
    add_lang_var(interpreter, "$LANG_OS_VER", DataObject::new_final_text(utils::get_os_version()));
    add_lang_var(interpreter, "$LANG_OS_ARCH", DataObject::new_final_text(utils::get_os_arch()));
    add_lang_var(interpreter, "$LANG_OS_FILE_SEPARATOR", DataObject::new_final_text(path::MAIN_SEPARATOR_STR));
    add_lang_var(interpreter, "$LANG_OS_LINE_SEPARATOR", DataObject::new_final_text(utils::LINE_SEPARATOR));
}
fn add_basic_number_lang_vars(interpreter: &mut Interpreter) {
    add_lang_var(interpreter, "$LANG_INT_MIN", DataObject::with_update_final(|data_object| {
        data_object.set_int(i32::MIN)
    }).unwrap());
    add_lang_var(interpreter, "$LANG_INT_MAX", DataObject::with_update_final(|data_object| {
        data_object.set_int(i32::MAX)
    }).unwrap());

    add_lang_var(interpreter, "$LANG_LONG_MIN", DataObject::with_update_final(|data_object| {
        data_object.set_long(i64::MIN)
    }).unwrap());
    add_lang_var(interpreter, "$LANG_LONG_MAX", DataObject::with_update_final(|data_object| {
        data_object.set_long(i64::MAX)
    }).unwrap());

    add_lang_var(interpreter, "$LANG_FLOAT_NAN", DataObject::with_update_final(|data_object| {
        data_object.set_float(f32::NAN)
    }).unwrap());
    add_lang_var(interpreter, "$LANG_FLOAT_POS_INF", DataObject::with_update_final(|data_object| {
        data_object.set_float(f32::INFINITY)
    }).unwrap());
    add_lang_var(interpreter, "$LANG_FLOAT_NEG_INF", DataObject::with_update_final(|data_object| {
        data_object.set_float(f32::NEG_INFINITY)
    }).unwrap());

    add_lang_var(interpreter, "$LANG_DOUBLE_NAN", DataObject::with_update_final(|data_object| {
        data_object.set_double(f64::NAN)
    }).unwrap());
    add_lang_var(interpreter, "$LANG_DOUBLE_POS_INF", DataObject::with_update_final(|data_object| {
        data_object.set_double(f64::INFINITY)
    }).unwrap());
    add_lang_var(interpreter, "$LANG_DOUBLE_NEG_INF", DataObject::with_update_final(|data_object| {
        data_object.set_double(f64::NEG_INFINITY)
    }).unwrap());
}
fn add_error_lang_vars(interpreter: &mut Interpreter) {
    for error in InterpretingError::VALUES {
        let error_name = error.name();
        let variable_name = "$LANG_ERROR_".to_string() + error_name;
        add_lang_var(interpreter, &variable_name, DataObject::with_update_final(|data_object| {
            data_object.set_error(Gc::new(ErrorObject::new(error, None)))
        }).unwrap());
        let variable_name = "$LANG_ERRNO_".to_string() + error_name;
        add_lang_var(interpreter, &variable_name, DataObject::with_update_final(|data_object| {
            data_object.set_int(error.error_code())
        }).unwrap());
    }
}
fn add_type_lang_vars(interpreter: &mut Interpreter) {
    for data_type in DataType::VALUES {
        let type_name = data_type.to_string();
        let variable_name = "$LANG_TYPE_".to_string() + &type_name;
        add_lang_var(interpreter, &variable_name, DataObject::with_update_final(|data_object| {
            data_object.set_type(data_type)
        }).unwrap());
    }
}

pub fn add_lang_vars(interpreter: &mut Interpreter, lang_args: OptionDataObjectRef) {
    add_essential_lang_vars(interpreter, lang_args);

    add_execution_lang_vars(interpreter);
    add_number_lang_vars(interpreter);
    add_struct_definition_lang_vars(interpreter);
    add_class_definition_lang_vars(interpreter);
}
fn add_execution_lang_vars(interpreter: &mut Interpreter) {
    let current_stack_element = interpreter.current_call_stack_element().clone();

    add_lang_var(interpreter, "$LANG_PATH", DataObject::new_final_text(current_stack_element.lang_path()));
    add_lang_var(interpreter, "$LANG_FILE", DataObject::with_update_final(|data_object| {
        if let Some(lang_file) = current_stack_element.lang_file() {
            data_object.set_text(lang_file)
        }else {
            data_object.set_null()
        }
    }).unwrap());
    add_lang_var(interpreter, "$LANG_CURRENT_FUNCTION", DataObject::with_update_final(|data_object| {
        if let Some(lang_function_name) = current_stack_element.lang_function_name() {
            data_object.set_text(lang_function_name)
        }else {
            data_object.set_null()
        }
    }).unwrap());

    //Module vars
    if let Some(module) = &current_stack_element.module {
        add_lang_var(interpreter, "$LANG_MODULE_STATE", DataObject::new_final_text(if module.is_load() { "load" } else { "unload" }));

        let prefix = format!(
            "<module:{}[{}]>",
            module.file(),
            module.lang_module_configuration().name()
        );

        let mut module_path = current_stack_element.lang_path()[prefix.len()..].to_string();
        if !module_path.starts_with("/") {
            module_path = "/".to_string() + &module_path;
        }

        add_lang_var(interpreter, "$LANG_MODULE_PATH", DataObject::new_final_text(module_path));
        add_lang_var(interpreter, "$LANG_MODULE_FILE", DataObject::with_update_final(|data_object| {
            if let Some(lang_file) = current_stack_element.lang_file() {
                data_object.set_text(lang_file)
            }else {
                data_object.set_null()
            }
        }).unwrap());
    }
}
fn add_number_lang_vars(interpreter: &mut Interpreter) {
    add_lang_var(interpreter, "$LANG_MATH_PI", DataObject::with_update_final(|data_object| {
        data_object.set_double(std::f64::consts::PI)
    }).unwrap());
    add_lang_var(interpreter, "$LANG_MATH_E", DataObject::with_update_final(|data_object| {
        data_object.set_double(std::f64::consts::E)
    }).unwrap());
    add_lang_var(interpreter, "$LANG_MATH_I", DataObject::with_update_final(|data_object| {
        data_object.set_data(&interpreter.standard_types.get("$COMPLEX_I").unwrap().borrow())
    }).unwrap());
}
fn add_struct_definition_lang_vars(interpreter: &mut Interpreter) {
    add_static_lang_var(interpreter, "&CodePosition", DataObject::with_update_final(|data_object| {
        data_object.set_data(&interpreter.standard_types.get("&CodePosition").unwrap().borrow())
    }).unwrap());
    add_static_lang_var(interpreter, "&StackTraceElement", DataObject::with_update_final(|data_object| {
        data_object.set_data(&interpreter.standard_types.get("&StackTraceElement").unwrap().borrow())
    }).unwrap());
    add_static_lang_var(interpreter, "&Pair", DataObject::with_update_final(|data_object| {
        data_object.set_data(&interpreter.standard_types.get("&Pair").unwrap().borrow())
    }).unwrap());
}
fn add_class_definition_lang_vars(interpreter: &mut Interpreter) {
    add_static_lang_var(interpreter, "&Object", DataObject::with_update_final(|data_object| {
        data_object.set_object(interpreter.get_object_class().clone())
    }).unwrap());
    add_static_lang_var(interpreter, "&Maybe", DataObject::with_update_final(|data_object| {
        data_object.set_data(&interpreter.standard_types.get("&Maybe").unwrap().borrow())
    }).unwrap());
    add_static_lang_var(interpreter, "&Complex", DataObject::with_update_final(|data_object| {
        data_object.set_data(&interpreter.standard_types.get("&Complex").unwrap().borrow())
    }).unwrap());
    add_static_lang_var(interpreter, "&BasicIterator", DataObject::with_update_final(|data_object| {
        data_object.set_data(&interpreter.standard_types.get("&BasicIterator").unwrap().borrow())
    }).unwrap());
}
