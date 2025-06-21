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

        interpreter.data_mut().var.remove(&*variable_name);

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
