use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::{operators, Interpreter, InterpretingError};
use crate::interpreter::data::DataObjectRef;
use crate::lexer::CodePosition;
use crate::utils;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            iter_function,
            crate::lang_func_metadata!(
                name="iter",
                parameter(
                    name="$operand",
                ),
            ),
        ));
    fn iter_function(
        interpreter: &mut Interpreter,
        operand: DataObjectRef,
    ) -> DataObjectRef {
        let ret = operators::op_iter(interpreter, &operand, CodePosition::EMPTY);
        let Some(ret) = ret else {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!("The iter operator is not defined for {}", operand.data_type())),
                CodePosition::EMPTY,
            );
        };

        ret
    }

    functions.push(crate::lang_func!(
            has_next_function,
            crate::lang_func_metadata!(
                name="hasNext",
                parameter(
                    name="$operand",
                ),
            ),
        ));
    fn has_next_function(
        interpreter: &mut Interpreter,
        operand: DataObjectRef,
    ) -> DataObjectRef {
        let ret = operators::op_has_next(interpreter, &operand, CodePosition::EMPTY);
        let Some(ret) = ret else {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!("The hasNext operator is not defined for {}", operand.data_type())),
                CodePosition::EMPTY,
            );
        };

        ret
    }

    functions.push(crate::lang_func!(
            next_function,
            crate::lang_func_metadata!(
                name="next",
                parameter(
                    name="$operand",
                ),
            ),
        ));
    fn next_function(
        interpreter: &mut Interpreter,
        operand: DataObjectRef,
    ) -> DataObjectRef {
        let ret = operators::op_next(interpreter, &operand, CodePosition::EMPTY);
        let Some(ret) = ret else {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!("The next operator is not defined for {}", operand.data_type())),
                CodePosition::EMPTY,
            );
        };

        ret
    }

    functions.push(crate::lang_func!(
            cast_function,
            crate::lang_func_metadata!(
                name="cast",
                parameter(
                    name="$leftSideOperand",
                ),
                parameter(
                    name="$rightSideOperand",
                ),
            ),
        ));
    fn cast_function(
        interpreter: &mut Interpreter,
        left_side_operand: DataObjectRef,
        right_side_operand: DataObjectRef,
    ) -> DataObjectRef {
        let ret = operators::op_cast(interpreter, &left_side_operand, &right_side_operand, CodePosition::EMPTY);
        let Some(ret) = ret else {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!(
                    "The cast operator is not defined for {} and {}",
                    left_side_operand.data_type(),
                    right_side_operand.data_type(),
                )),
                CodePosition::EMPTY,
            );
        };

        ret
    }

    functions.push(crate::lang_func!(
            call_function,
            crate::lang_func_metadata!(
                name="call",
                parameter(
                    name="$callee",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn call_function(
        interpreter: &mut Interpreter,
        callee: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let ret = operators::op_call(
            interpreter,
            &callee,
            &utils::separate_arguments_with_argument_separators(&args),
            CodePosition::EMPTY,
        );
        let Some(ret) = ret else {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Invalid arguments for the call operator."),
                CodePosition::EMPTY,
            );
        };

        ret
    }
}
