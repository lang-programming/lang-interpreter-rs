use gc::Gc;
use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::{Interpreter, InterpretingError};
use crate::interpreter::data::{DataObject, DataObjectRef, OptionDataObjectRef, StructObject};
use crate::lexer::CodePosition;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            struct_create_function,
            crate::lang_func_metadata!(
                name="structCreate",
                info="Returns an empty struct object of type &Struct. \
                This function is not compatible with all struct types, \
                because all values will be set to null - use \"func.structOf()\" for those struct types instead.",
                return_type_constraint(
                    allowed=["STRUCT"],
                ),
                parameter(
                    name="&Struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
    fn struct_create_function(
        interpreter: &mut Interpreter,
        struct_object: DataObjectRef,
    ) -> DataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        if !struct_value.is_definition() {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 1 (\"&Struct\") must be a struct definition"),
                CodePosition::EMPTY,
            );
        }

        let struct_fields = struct_value.member_names().iter().
                map(|_| DataObjectRef::new(DataObject::new())).
                collect::<Box<_>>();

        let ret = StructObject::new_instance(struct_value, &struct_fields);
        match ret {
            Ok(ret) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_struct(Gc::new(ret))
                }).unwrap())
            },

            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(e.message()),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
            struct_of_function,
            crate::lang_func_metadata!(
                name="structOf",
                return_type_constraint(
                    allowed=["STRUCT"],
                ),
                parameter(
                    name="&Struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn struct_of_function(
        interpreter: &mut Interpreter,
        struct_object: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        if !struct_value.is_definition() {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 1 (\"&Struct\") must be a struct definition"),
                CodePosition::EMPTY,
            );
        }

        let member_count = struct_value.member_names().len();
        if args.len() != member_count {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!(
                    "The var args argument count is not equals to the count of the member names ({member_count})",
                )),
                CodePosition::EMPTY,
            );
        }

        let ret = StructObject::new_instance(struct_value, &args);
        match ret {
            Ok(ret) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_struct(Gc::new(ret))
                }).unwrap())
            },

            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(e.message()),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
            struct_set_function,
            crate::lang_func_metadata!(
                name="structSet",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
                parameter(
                    name="$memberName",
                    type_constraint(
                        allowed=["TEXT"],
                    ),
                ),
                parameter(
                    name="$memberObject",
                ),
            ),
        ));
    fn struct_set_function(
        interpreter: &mut Interpreter,
        struct_object: DataObjectRef,
        member_name_object: DataObjectRef,
        member_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        if struct_value.is_definition() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 1 (\"&struct\") must be a struct instance"),
                CodePosition::EMPTY,
            ));
        }

        let member_name = member_name_object.text_value().unwrap();

        let ret = struct_value.set_member(&member_name, &member_object.borrow());
        if let Err(e) = ret {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::IncompatibleDataType,
                Some(e.message()),
                CodePosition::EMPTY,
            ));
        };

        None
    }

    functions.push(crate::lang_func!(
            struct_set_all_function,
            crate::lang_func_metadata!(
                name="structSetAll",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn struct_set_all_function(
        interpreter: &mut Interpreter,
        struct_object: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        if struct_value.is_definition() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 1 (\"&struct\") must be a struct instance"),
                CodePosition::EMPTY,
            ));
        }

        let member_names = struct_value.member_names();
        let member_count = member_names.len();
        if args.len() != member_count {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!(
                    "The var args argument count is not equals to the count of the member names ({member_count})",
                )),
                CodePosition::EMPTY,
            ));
        }

        for (i, (member_name, arg)) in member_names.iter().
                zip(args.iter()).
                enumerate() {
            let ret = struct_value.set_member(member_name, &arg.borrow());
            if let Err(e) = ret {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(&format!(
                        "Argument {}: {}",
                        i + 2,
                        e.message(),
                    )),
                    CodePosition::EMPTY,
                ));
            };
        }

        None
    }

    functions.push(crate::lang_func!(
            struct_get_function,
            crate::lang_func_metadata!(
                name="structGet",
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
                parameter(
                    name="$memberName",
                    type_constraint(
                        allowed=["TEXT"],
                    ),
                ),
            ),
        ));
    fn struct_get_function(
        interpreter: &mut Interpreter,
        struct_object: DataObjectRef,
        member_name_object: DataObjectRef,
    ) -> DataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        if struct_value.is_definition() {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 1 (\"&struct\") must be a struct instance"),
                CodePosition::EMPTY,
            );
        }

        let member_name = member_name_object.text_value().unwrap();

        let ret = struct_value.get_member(&member_name);
        match ret {
            Ok(ret) => ret,

            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(e.message()),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
            struct_get_all_function,
            crate::lang_func_metadata!(
                name="structGetAll",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
    fn struct_get_all_function(
        interpreter: &mut Interpreter,
        struct_object: DataObjectRef,
    ) -> DataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        if struct_value.is_definition() {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 1 (\"&struct\") must be a struct instance"),
                CodePosition::EMPTY,
            );
        }

        let arr = struct_value.member_names().iter().
                map(|member_name| struct_value.get_member(member_name)).
                collect::<Result<Box<_>, _>>();
        match arr {
            Ok(arr) => {
                DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_array(arr)
                }).unwrap())
            },

            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(e.message()),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
            struct_get_member_names_function,
            crate::lang_func_metadata!(
                name="structGetMemberNames",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
    fn struct_get_member_names_function(
        _: &mut Interpreter,
        struct_object: DataObjectRef,
    ) -> DataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        let arr = struct_value.member_names().iter().
                map(|member_name| DataObjectRef::new(DataObject::new_text(&**member_name))).
                collect::<Box<_>>();
        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(arr)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            struct_get_member_count_function,
            crate::lang_func_metadata!(
                name="structGetMemberCount",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
    fn struct_get_member_count_function(
        _: &mut Interpreter,
        struct_object: DataObjectRef,
    ) -> DataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        DataObjectRef::new(DataObject::new_number(struct_value.member_names().len() as i32))
    }

    functions.push(crate::lang_func!(
            struct_is_definition_function,
            crate::lang_func_metadata!(
                name="structIsDefinition",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
    fn struct_is_definition_function(
        _: &mut Interpreter,
        struct_object: DataObjectRef,
    ) -> DataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(struct_value.is_definition())
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            struct_is_instance_function,
            crate::lang_func_metadata!(
                name="structIsInstance",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
    fn struct_is_instance_function(
        _: &mut Interpreter,
        struct_object: DataObjectRef,
    ) -> DataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(!struct_value.is_definition())
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            struct_definition_type_of_function,
            crate::lang_func_metadata!(
                name="structDefinitionTypeOf",
                return_type_constraint(
                    allowed=["STRUCT"],
                ),
                parameter(
                    name="&struct",
                    type_constraint(
                        allowed=["STRUCT"],
                    ),
                ),
            ),
        ));
    fn struct_definition_type_of_function(
        interpreter: &mut Interpreter,
        struct_object: DataObjectRef,
    ) -> DataObjectRef {
        let struct_value = struct_object.struct_value().unwrap();

        if struct_value.is_definition() {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 1 (\"&struct\") must be a struct instance"),
                CodePosition::EMPTY,
            );
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_struct(struct_value.base_definition().unwrap())
        }).unwrap())
    }
}
