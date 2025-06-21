mod reset_functions;
mod error_functions;
mod lang_functions;
mod system_functions;
mod io_functions;
mod number_functions;
mod text_functions;
mod conversion_functions;
mod operation_functions;
mod math_functions;
mod combinator_functions;
mod func_ptr_functions;
mod byte_buffer_functions;
mod array_functions;
mod list_functions;
mod struct_functions;
mod module_functions;
mod lang_test_functions;
mod linker_functions;

use ahash::AHashMap;
use gc::Gc;
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
    module_functions::add_functions(&mut functions);
    lang_test_functions::add_functions(&mut functions);

    let functions = FunctionPointerObject::create_function_pointer_objects_from_native_functions(functions);
    for (function_name, functions) in functions {
        funcs.insert(function_name, Gc::new(functions));
    }
}

pub fn add_predefined_linker_functions(funcs: &mut AHashMap<Box<str>, FunctionPointerObjectRef>) {
    let mut functions = Vec::new();

    linker_functions::add_functions(&mut functions);

    let functions = FunctionPointerObject::create_function_pointer_objects_from_native_functions(functions);
    for (function_name, functions) in functions {
        funcs.insert(function_name, Gc::new(functions));
    }
}
