use gc::Gc;
use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::data::{DataObject, DataObjectRef, ErrorObject};
use crate::interpreter::{conversions, Interpreter};
use crate::lexer::CodePosition;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            get_error_text_function,
            crate::lang_func_metadata!(
                name="getErrorText",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
            ),
        ));
    fn get_error_text_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::new_text(
            interpreter.get_and_clear_errno_error_object().error_text(),
        ))
    }

    functions.push(crate::lang_func!(
            with_error_message_function,
            crate::lang_func_metadata!(
                name="withErrorMessage",
                return_type_constraint(
                    allowed=["ERROR"],
                ),
                parameter(
                    name="$error",
                    type_constraint(
                        allowed=["ERROR"],
                    ),
                ),
                parameter(
                    name="$text",
                ),
            ),
        ));
    fn with_error_message_function(
        interpreter: &mut Interpreter,
        error_object: DataObjectRef,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_error(Gc::new(ErrorObject::new(
                error_object.error_value().unwrap().err(),
                Some(&conversions::to_text(interpreter, &text_object, CodePosition::EMPTY))
            )))
        }).unwrap())
    }
}
