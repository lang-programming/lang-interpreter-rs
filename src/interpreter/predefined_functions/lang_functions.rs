use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::data::{DataObject, DataObjectRef};
use crate::interpreter::{Interpreter, InterpretingError};
use crate::lexer::CodePosition;
use crate::utils;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            is_lang_version_newer_function,
            crate::lang_func_metadata!(
                name="isLangVersionNewer",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
    fn is_lang_version_newer_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        //If lang.version = null -> return false
        let comp_ver = {
            let data = interpreter.data_ref();
            let lang_ver = data.lang.get("lang.version").
                    map(|str| &**str).
                    unwrap_or(Interpreter::VERSION);

            utils::compare_versions_str(Interpreter::VERSION, lang_ver)
        };

        let Some(comp_ver) = comp_ver else {
            return interpreter.set_errno_error_object(
                InterpretingError::LangVerError,
                Some("lang.version has an invalid format"),
                CodePosition::EMPTY,
            );
        };

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(comp_ver.is_gt())
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            is_lang_version_older_function,
            crate::lang_func_metadata!(
                name="isLangVersionOlder",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
    fn is_lang_version_older_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        //If lang.version = null -> return false
        let comp_ver = {
            let data = interpreter.data_ref();
            let lang_ver = data.lang.get("lang.version").
                    map(|str| &**str).
                    unwrap_or(Interpreter::VERSION);

            utils::compare_versions_str(Interpreter::VERSION, lang_ver)
        };

        let Some(comp_ver) = comp_ver else {
            return interpreter.set_errno_error_object(
                InterpretingError::LangVerError,
                Some("lang.version has an invalid format"),
                CodePosition::EMPTY,
            );
        };

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(comp_ver.is_lt())
        }).unwrap())
    }
}
