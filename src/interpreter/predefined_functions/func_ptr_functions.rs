use gc::Gc;
use crate::interpreter::data::function::{Function, FunctionMetadata, FunctionPointerObject};
use crate::interpreter::{operators, Interpreter};
use crate::interpreter::data::{DataObject, DataObjectRef, OptionDataObjectRef};
use crate::lexer::CodePosition;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            arg_cnt_0_function,
            crate::lang_func_metadata!(
                name="argCnt0",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
    fn arg_cnt_0_function(
        _: &mut Interpreter,
        func_object: DataObjectRef,
    ) -> DataObjectRef {
        let function_name = if let Some(function_value) = func_object.function_pointer_value() {
            let function_name = function_value.function_name();
            if let Some(function_name) = function_name {
                function_name.to_string()
            }else {
                func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
            }
        }else {
            "<arg>".to_string()
        };

        let arg_cnt_0_func_function = {
            let func_object = func_object.clone();

            move |interpreter: &mut Interpreter| -> OptionDataObjectRef {
                operators::op_call(interpreter, &func_object, &[], CodePosition::EMPTY)
            }
        };
        let arg_cnt_0_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_0_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt0-func",
                ),
            ));

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_function_pointer(Gc::new(arg_cnt_0_func_function.copy_with_function_name(&format!(
                "<argCnt0({function_name})>",
            ))))
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            arg_cnt_1_function,
            crate::lang_func_metadata!(
                name="argCnt1",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
    fn arg_cnt_1_function(
        _: &mut Interpreter,
        func_object: DataObjectRef,
    ) -> DataObjectRef {
        let function_name = if let Some(function_value) = func_object.function_pointer_value() {
            let function_name = function_value.function_name();
            if let Some(function_name) = function_name {
                function_name.to_string()
            }else {
                func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
            }
        }else {
            "<arg>".to_string()
        };

        let arg_cnt_1_func_function = {
            let func_object = func_object.clone();

            move |interpreter: &mut Interpreter, a: DataObjectRef| -> OptionDataObjectRef {
                operators::op_call(interpreter, &func_object, &[
                    a,
                ], CodePosition::EMPTY)
            }
        };
        let arg_cnt_1_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_1_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt1-func",
                    parameter(
                        name="$a",
                    ),
                ),
            ));

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_function_pointer(Gc::new(arg_cnt_1_func_function.copy_with_function_name(&format!(
                "<argCnt1({function_name})>",
            ))))
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            arg_cnt_2_function,
            crate::lang_func_metadata!(
                name="argCnt2",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
    fn arg_cnt_2_function(
        _: &mut Interpreter,
        func_object: DataObjectRef,
    ) -> DataObjectRef {
        let function_name = if let Some(function_value) = func_object.function_pointer_value() {
            let function_name = function_value.function_name();
            if let Some(function_name) = function_name {
                function_name.to_string()
            }else {
                func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
            }
        }else {
            "<arg>".to_string()
        };

        let arg_cnt_2_func_function = {
            let func_object = func_object.clone();

            move |interpreter: &mut Interpreter, a: DataObjectRef, b: DataObjectRef| -> OptionDataObjectRef {
                operators::op_call(interpreter, &func_object, &[
                    a, b,
                ], CodePosition::EMPTY)
            }
        };
        let arg_cnt_2_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_2_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt2-func",
                    parameter(
                        name="$a",
                    ),
                    parameter(
                        name="$b",
                    ),
                ),
            ));

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_function_pointer(Gc::new(arg_cnt_2_func_function.copy_with_function_name(&format!(
                "<argCnt2({function_name})>",
            ))))
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            arg_cnt_3_function,
            crate::lang_func_metadata!(
                name="argCnt3",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
    fn arg_cnt_3_function(
        _: &mut Interpreter,
        func_object: DataObjectRef,
    ) -> DataObjectRef {
        let function_name = if let Some(function_value) = func_object.function_pointer_value() {
            let function_name = function_value.function_name();
            if let Some(function_name) = function_name {
                function_name.to_string()
            }else {
                func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
            }
        }else {
            "<arg>".to_string()
        };

        let arg_cnt_3_func_function = {
            let func_object = func_object.clone();

            move |interpreter: &mut Interpreter, a: DataObjectRef, b: DataObjectRef, c: DataObjectRef| -> OptionDataObjectRef {
                operators::op_call(interpreter, &func_object, &[
                    a, b, c,
                ], CodePosition::EMPTY)
            }
        };
        let arg_cnt_3_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_3_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt3-func",
                    parameter(
                        name="$a",
                    ),
                    parameter(
                        name="$b",
                    ),
                    parameter(
                        name="$c",
                    ),
                ),
            ));

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_function_pointer(Gc::new(arg_cnt_3_func_function.copy_with_function_name(&format!(
                "<argCnt3({function_name})>",
            ))))
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            arg_cnt_4_function,
            crate::lang_func_metadata!(
                name="argCnt4",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
    fn arg_cnt_4_function(
        _: &mut Interpreter,
        func_object: DataObjectRef,
    ) -> DataObjectRef {
        let function_name = if let Some(function_value) = func_object.function_pointer_value() {
            let function_name = function_value.function_name();
            if let Some(function_name) = function_name {
                function_name.to_string()
            }else {
                func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
            }
        }else {
            "<arg>".to_string()
        };

        let arg_cnt_4_func_function = {
            let func_object = func_object.clone();

            move |interpreter: &mut Interpreter, a: DataObjectRef, b: DataObjectRef, c: DataObjectRef, d: DataObjectRef| -> OptionDataObjectRef {
                operators::op_call(interpreter, &func_object, &[
                    a, b, c, d,
                ], CodePosition::EMPTY)
            }
        };
        let arg_cnt_4_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_4_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt4-func",
                    parameter(
                        name="$a",
                    ),
                    parameter(
                        name="$b",
                    ),
                    parameter(
                        name="$c",
                    ),
                    parameter(
                        name="$d",
                    ),
                ),
            ));

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_function_pointer(Gc::new(arg_cnt_4_func_function.copy_with_function_name(&format!(
                "<argCnt4({function_name})>",
            ))))
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            arg_cnt_5_function,
            crate::lang_func_metadata!(
                name="argCnt5",
                return_type_constraint(
                    allowed=["FUNCTION_POINTER"],
                ),
                parameter(
                    name="$func",
                    parameter_type(callable),
                ),
            ),
        ));
    fn arg_cnt_5_function(
        _: &mut Interpreter,
        func_object: DataObjectRef,
    ) -> DataObjectRef {
        let function_name = if let Some(function_value) = func_object.function_pointer_value() {
            let function_name = function_value.function_name();
            if let Some(function_name) = function_name {
                function_name.to_string()
            }else {
                func_object.variable_name().map(|str| str.to_string()).unwrap_or_else(|| "null".to_string())
            }
        }else {
            "<arg>".to_string()
        };

        let arg_cnt_5_func_function = {
            let func_object = func_object.clone();

            move |interpreter: &mut Interpreter, a: DataObjectRef, b: DataObjectRef, c: DataObjectRef, d: DataObjectRef, e: DataObjectRef| -> OptionDataObjectRef {
                operators::op_call(interpreter, &func_object, &[
                    a, b, c, d, e,
                ], CodePosition::EMPTY)
            }
        };
        let arg_cnt_5_func_function = FunctionPointerObject::from(crate::lang_func!(
                arg_cnt_5_func_function,
                vec![
                    Box::new(func_object),
                ],
                crate::lang_func_metadata!(
                    name="argCnt4-func",
                    parameter(
                        name="$a",
                    ),
                    parameter(
                        name="$b",
                    ),
                    parameter(
                        name="$c",
                    ),
                    parameter(
                        name="$d",
                    ),
                    parameter(
                        name="$e",
                    ),
                ),
            ));

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_function_pointer(Gc::new(arg_cnt_5_func_function.copy_with_function_name(&format!(
                "<argCnt5({function_name})>",
            ))))
        }).unwrap())
    }
}
