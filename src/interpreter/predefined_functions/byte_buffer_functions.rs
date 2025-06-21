use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::{Interpreter, InterpretingError};
use crate::interpreter::data::{DataObject, DataObjectRef};

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            byte_buffer_create_function,
            crate::lang_func_metadata!(
                name="byteBufferCreate",
                return_type_constraint(
                    allowed=["BYTE_BUFFER"],
                ),
                parameter(
                    name="$length",
                    parameter_type(number),
                ),
            ),
        ));
    fn byte_buffer_create_function(
        interpreter: &mut Interpreter,
        length_number: DataObjectRef,
    ) -> DataObjectRef {
        let length_number = length_number.number_value().unwrap();
        let length = length_number.int_value();
        if length < 0 {
            return interpreter.set_errno_error_object_error_only(InterpretingError::NegativeArrayLen);
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_byte_buffer(vec![0; length as usize].into_boxed_slice())
        }).unwrap())
    }
}
