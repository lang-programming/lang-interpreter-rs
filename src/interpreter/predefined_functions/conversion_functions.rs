use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::{conversions, Interpreter, InterpretingError};
use crate::interpreter::data::{DataObject, DataObjectRef};
use crate::lexer::CodePosition;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            number_function,
            crate::lang_func_metadata!(
                name="number",
                return_type_constraint(
                    allowed=["INT", "LONG", "FLOAT", "DOUBLE"],
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn number_function(
        interpreter: &mut Interpreter,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let ret = conversions::to_number(interpreter, &value_object, CodePosition::EMPTY);
        let Some(ret) = ret else {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 1 (\"$value\") can not be converted to type number"),
                CodePosition::EMPTY,
            );
        };

        DataObjectRef::new(DataObject::new_number(ret))
    }
}
