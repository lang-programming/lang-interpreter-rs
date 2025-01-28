use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::rc::Rc;
use ahash::AHashMap;
use crate::interpreter::data::{DataObjectRef, DataTypeConstraintError, FunctionPointerObjectRef, OptionDataObjectRef};
use crate::interpreter::{ErrorOutputFlag, Interpreter, InterpretingError, StackElement};
use crate::interpreter::data::function::{FunctionPointerObject};
use crate::interpreter::module::Module;
use crate::lexer::CodePosition;
use crate::parser::ast::{Node, AST};

pub fn get_translation(interpreter: &Interpreter, key: &str) -> Option<Rc<str>> {
    interpreter.data_ref().lang.get(key).cloned()
}

pub fn set_translation(interpreter: &mut Interpreter, key: impl Into<Rc<str>>, value: impl Into<Rc<str>>) {
    interpreter.data_mut().lang.insert(key.into(), value.into());
}

pub fn get_var(interpreter: &Interpreter, var_name: &str) -> Option<DataObjectRef> {
    interpreter.data_ref().var.get(var_name).cloned()
}

pub fn set_var(interpreter: &mut Interpreter, var_name: &str, value: DataObjectRef, ignore_final: bool) -> Result<(), DataTypeConstraintError> {
    let vars = &mut interpreter.data_mut().var;

    let entry = vars.entry(Rc::from(var_name));
    match entry {
        Entry::Vacant(entry) => {
            value.borrow_mut().set_variable_name(Some(var_name))?;

            entry.insert(value);
        },

        Entry::Occupied(entry) => {
            if ignore_final || !entry.get().is_final_data() {
                entry.get().borrow_mut().set_data(&value.borrow())?;
            }
        },
    }

    Ok(())
}

pub fn set_errno(
    interpreter: &mut Interpreter, error: InterpretingError,
    message: Option<&str>,
    pos: CodePosition,
) {
    interpreter.set_errno(error, message, pos);
}

#[must_use]
pub fn set_errno_error_object(
    interpreter: &mut Interpreter,
    error: InterpretingError,
    message: Option<&str>,
    pos: CodePosition,
) -> DataObjectRef {
    interpreter.set_errno_error_object(error, message, pos)
}

pub fn get_and_clear_errno_error_object(interpreter: &mut Interpreter) -> InterpretingError {
    interpreter.get_and_clear_errno_error_object()
}

/// Creates a function which is accessible globally in the Interpreter (= in all scopes)
///
/// If function already exists, it will be overridden
///
/// Function can be accessed with `func.funcName`/`fn.funcName` or with `linker.funcName`/`ln.funcName` and can't be removed nor changed by the Lang file
pub fn add_predefined_function(interpreter: &mut Interpreter, func_name: impl Into<Box<str>>, function: FunctionPointerObjectRef) {
    interpreter.funcs.insert(func_name.into(), function);
}

pub fn add_predefined_functions(interpreter: &mut Interpreter, funcs: impl IntoIterator<Item = (Box<str>, FunctionPointerObjectRef)>) {
    interpreter.funcs.extend(funcs);
}

pub fn exec(interpreter: &mut Interpreter, lines: &str) -> OptionDataObjectRef {
    interpreter.get_and_reset_return_value(); //Reset returned value else the interpreter would stop immediately

    interpreter.interpret_lines(lines)
}

//TODO stop
//TODO resetStopFlag

pub fn set_error_output_flag(interpreter: &mut Interpreter, error_output: ErrorOutputFlag) {
    interpreter.execution_flags.error_output = error_output;
}

pub fn current_call_stack_element(interpreter: &Interpreter) -> &StackElement {
    interpreter.current_call_stack_element()
}

/// Must be called before calling the [get_and_reset_return_value()] method
pub fn is_returned_value_throw_value(interpreter: &Interpreter) -> bool {
    interpreter.execution_state.is_thrown_value
}

pub fn get_throw_statement_pos(interpreter: &Interpreter) -> CodePosition {
    interpreter.execution_state.return_or_throw_statement_pos
}

pub fn get_and_reset_return_value(interpreter: &mut Interpreter) -> OptionDataObjectRef {
    interpreter.get_and_reset_return_value()
}

pub fn parse_lines(interpreter: &mut Interpreter, lines: impl Into<String>) -> Option<AST> {
    interpreter.parser.parse_lines(lines)
}

pub fn interpret_ast(interpreter: &mut Interpreter, ast: &AST) -> OptionDataObjectRef {
    interpreter.get_and_reset_return_value(); //Reset returned value else the interpreter would stop immediately
    interpreter.interpret_ast(ast)
}

pub fn interpret_node(interpreter: &mut Interpreter, node: &Node) -> OptionDataObjectRef {
    interpreter.interpret_node(None, node)
}

pub fn interpret_function_pointer(
    interpreter: &mut Interpreter,
    fp: &FunctionPointerObject,
    function_name: Option<&str>,
    argument_list: &[Node],
    parent_pos: CodePosition,
) -> OptionDataObjectRef {
    let argument_list = interpreter.interpret_function_pointer_arguments(argument_list);

    interpreter.call_function_pointer(fp, function_name, &argument_list, parent_pos)
}

pub fn parser_line_number(interpreter: &Interpreter) -> usize {
    interpreter.parser_line_number()
}

pub fn set_parser_line_number(interpreter: &mut Interpreter, line_number: usize) {
    interpreter.parser.set_line_number(line_number);
}

pub fn reset_parser_positional_vars(interpreter: &mut Interpreter) {
    interpreter.parser.reset_position_vars();
}

pub fn call_function_pointer(
    interpreter: &mut Interpreter,
    fp: &FunctionPointerObject,
    function_name: Option<&str>,
    argument_list: &[DataObjectRef],
    parent_pos: CodePosition,
) -> OptionDataObjectRef {
    interpreter.call_function_pointer(fp, function_name, argument_list, parent_pos)
}

pub fn modules(interpreter: &Interpreter) -> &HashMap<Box<str>, Rc<Module>> {
    &interpreter.modules
}

pub fn modules_mut(interpreter: &mut Interpreter) -> &mut HashMap<Box<str>, Rc<Module>> {
    &mut interpreter.modules
}

pub fn get_module_exported_functions<'a>(interpreter: &'a Interpreter, module_name: &str) -> Option<&'a RefCell<Vec<Box<str>>>> {
    let module = interpreter.modules.get(module_name)?;

    Some(module.exported_functions())
}

pub fn get_module_exported_variables<'a>(interpreter: &'a Interpreter, module_name: &str) -> Option<&'a RefCell<AHashMap<Rc<str>, DataObjectRef>>> {
    let module = interpreter.modules.get(module_name)?;

    Some(module.exported_variables())
}
