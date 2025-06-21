use rand::{Rng, RngCore, SeedableRng};
use rand::rngs::SmallRng;
use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::{conversions, operators, Interpreter, InterpretingError};
use crate::interpreter::data::{DataObject, DataObjectRef};
use crate::lexer::CodePosition;

macro_rules! math_1_arg_function {
        ( $func_name:literal, $op:ident $(,)? ) => {{
            fn math_function(
                _: &mut Interpreter,
                number: DataObjectRef,
            ) -> DataObjectRef {
                let number = number.number_value().unwrap().double_value();

                DataObjectRef::new(DataObject::new_number(number.$op()))
            }

            crate::lang_func!(
                math_function,
                crate::lang_func_metadata!(
                    name=$func_name,
                    return_type_constraint(
                        allowed=["DOUBLE"],
                    ),
                    parameter(
                        name="$number",
                        parameter_type(number),
                    ),
                ),
            )
        }};
    }

macro_rules! math_2_arg_function {
        ( $func_name:literal, $op:ident $(,)? ) => {{
            fn math_function(
                _: &mut Interpreter,
                left_number: DataObjectRef,
                right_number: DataObjectRef,
            ) -> DataObjectRef {
                let left_number = left_number.number_value().unwrap().double_value();
                let right_number = right_number.number_value().unwrap().double_value();

                DataObjectRef::new(DataObject::new_number(left_number.$op(right_number)))
            }

            crate::lang_func!(
                math_function,
                crate::lang_func_metadata!(
                    name=$func_name,
                    return_type_constraint(
                        allowed=["DOUBLE"],
                    ),
                    parameter(
                        name="$a",
                        parameter_type(number),
                    ),
                    parameter(
                        name="$b",
                        parameter_type(number),
                    ),
                ),
            )
        }};
    }

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            rand_function,
            crate::lang_func_metadata!(
                name="rand",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
    fn rand_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        let lang_rand_max = interpreter.data_ref().var["$LANG_RAND_MAX"].int_value().unwrap();

        DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0..lang_rand_max)))
    }

    functions.push(crate::lang_func!(
            randi_function,
            crate::lang_func_metadata!(
                name="randi",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
    fn randi_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::new_number(interpreter.ran.next_u32() as i32))
    }

    functions.push(crate::lang_func!(
            randl_function,
            crate::lang_func_metadata!(
                name="randl",
                return_type_constraint(
                    allowed=["LONG"],
                ),
            ),
        ));
    fn randl_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::new_number(interpreter.ran.next_u64() as i64))
    }

    functions.push(crate::lang_func!(
            randf_function,
            crate::lang_func_metadata!(
                name="randf",
                return_type_constraint(
                    allowed=["FLOAT"],
                ),
            ),
        ));
    fn randf_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0.0..1.0_f32)))
    }

    functions.push(crate::lang_func!(
            randd_function,
            crate::lang_func_metadata!(
                name="randd",
                return_type_constraint(
                    allowed=["DOUBLE"],
                ),
            ),
        ));
    fn randd_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0.0..1.0_f64)))
    }

    functions.push(crate::lang_func!(
            randb_function,
            crate::lang_func_metadata!(
                name="randb",
                return_type_constraint(
                    allowed=["INT"],
                ),
            ),
        ));
    fn randb_function(
        interpreter: &mut Interpreter,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0..=1_i32)))
    }

    functions.push(crate::lang_func!(
            rand_range_function,
            crate::lang_func_metadata!(
                name="randRange",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="$bound",
                    parameter_type(number),
                ),
            ),
        ));
    fn rand_range_function(
        interpreter: &mut Interpreter,
        bound: DataObjectRef,
    ) -> DataObjectRef {
        let bound = bound.number_value().unwrap();
        let bound = bound.int_value();
        if bound <= 0 {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("Argument 1 (\"$bound\") must be positive"),
                CodePosition::EMPTY,
            );
        }

        DataObjectRef::new(DataObject::new_number(interpreter.ran.gen_range(0..bound)))
    }

    {
        functions.push(crate::lang_func!(
                rand_choice_with_array_parameter_function,
                crate::lang_func_metadata!(
                    name="randChoice",
                    has_info=true,
                    parameter(
                        name="&arr",
                        type_constraint(
                            allowed=["ARRAY"],
                        ),
                    ),
                ),
            ));
        fn rand_choice_with_array_parameter_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
        ) -> DataObjectRef {
            let arr = array_object.array_value().unwrap();
            if arr.borrow().is_empty() {
                return DataObjectRef::new(DataObject::new_void());
            }

            let arr = arr.borrow();
            let index = interpreter.ran.gen_range(0..arr.len());

            arr[index].clone()
        }

        functions.push(crate::lang_func!(
                rand_choice_with_list_parameter_function,
                crate::lang_func_metadata!(
                    name="randChoice",
                    parameter(
                        name="&list",
                        type_constraint(
                            allowed=["LIST"],
                        ),
                    ),
                ),
            ));
        fn rand_choice_with_list_parameter_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
        ) -> DataObjectRef {
            let list = list_object.list_value().unwrap();
            if list.borrow().is_empty() {
                return DataObjectRef::new(DataObject::new_void());
            }

            let list = list.borrow();
            let index = interpreter.ran.gen_range(0..list.len());

            list[index].clone()
        }

        functions.push(crate::lang_func!(
                rand_choice_with_struct_parameter_function,
                crate::lang_func_metadata!(
                    name="randChoice",
                    parameter(
                        name="&struct",
                        type_constraint(
                            allowed=["STRUCT"],
                        ),
                    ),
                ),
            ));
        fn rand_choice_with_struct_parameter_function(
            interpreter: &mut Interpreter,
            struct_object: DataObjectRef,
        ) -> DataObjectRef {
            let struct_value = struct_object.struct_value().unwrap();
            let member_names = struct_value.member_names();
            if member_names.is_empty() {
                return DataObjectRef::new(DataObject::new_void());
            }

            let index = interpreter.ran.gen_range(0..member_names.len());

            if struct_value.is_definition() {
                DataObjectRef::new(DataObject::new_text(&*member_names[index]))
            }else {
                struct_value.get_member(&member_names[index]).unwrap()
            }
        }

        functions.push(crate::lang_func!(
                rand_choice_function,
                crate::lang_func_metadata!(
                    name="randChoice",
                    parameter(
                        name="&args",
                        parameter_type(var_args),
                    ),
                ),
            ));
        fn rand_choice_function(
            interpreter: &mut Interpreter,
            args: Vec<DataObjectRef>,
        ) -> DataObjectRef {
            if args.is_empty() {
                return DataObjectRef::new(DataObject::new_void());
            }

            let index = interpreter.ran.gen_range(0..args.len());

            args[index].clone()
        }
    }

    functions.push(crate::lang_func!(
            set_seed_function,
            crate::lang_func_metadata!(
                name="setSeed",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="$seed",
                    parameter_type(number),
                ),
            ),
        ));
    fn set_seed_function(
        interpreter: &mut Interpreter,
        seed: DataObjectRef,
    ) {
        let seed = seed.number_value().unwrap();
        let seed = seed.long_value();

        interpreter.ran = SmallRng::seed_from_u64(seed as u64);
    }

    functions.push(crate::lang_func!(
            addi_function,
            crate::lang_func_metadata!(
                name="addi",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn addi_function(
        interpreter: &mut Interpreter,
        number_objects: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut sum = 0;

        for (i, number_object) in number_objects.iter().
                enumerate() {
            let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some(&format!(
                        "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            };

            sum += number.int_value();
        }

        DataObjectRef::new(DataObject::new_number(sum))
    }

    functions.push(crate::lang_func!(
            muli_function,
            crate::lang_func_metadata!(
                name="muli",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn muli_function(
        interpreter: &mut Interpreter,
        number_objects: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut prod = 1;

        for (i, number_object) in number_objects.iter().
                enumerate() {
            let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some(&format!(
                        "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            };

            prod *= number.int_value();
        }

        DataObjectRef::new(DataObject::new_number(prod))
    }

    functions.push(crate::lang_func!(
            addl_function,
            crate::lang_func_metadata!(
                name="addl",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn addl_function(
        interpreter: &mut Interpreter,
        number_objects: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut sum = 0;

        for (i, number_object) in number_objects.iter().
                enumerate() {
            let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some(&format!(
                        "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            };

            sum += number.long_value();
        }

        DataObjectRef::new(DataObject::new_number(sum))
    }

    functions.push(crate::lang_func!(
            mull_function,
            crate::lang_func_metadata!(
                name="mull",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn mull_function(
        interpreter: &mut Interpreter,
        number_objects: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut prod = 1;

        for (i, number_object) in number_objects.iter().
                enumerate() {
            let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some(&format!(
                        "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            };

            prod *= number.long_value();
        }

        DataObjectRef::new(DataObject::new_number(prod))
    }

    functions.push(crate::lang_func!(
            addf_function,
            crate::lang_func_metadata!(
                name="addf",
                return_type_constraint(
                    allowed=["FLOAT"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn addf_function(
        interpreter: &mut Interpreter,
        number_objects: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut sum = 0.0;

        for (i, number_object) in number_objects.iter().
                enumerate() {
            let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some(&format!(
                        "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            };

            sum += number.float_value();
        }

        DataObjectRef::new(DataObject::new_number(sum))
    }

    functions.push(crate::lang_func!(
            mulf_function,
            crate::lang_func_metadata!(
                name="mulf",
                return_type_constraint(
                    allowed=["FLOAT"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn mulf_function(
        interpreter: &mut Interpreter,
        number_objects: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut prod = 1.0;

        for (i, number_object) in number_objects.iter().
                enumerate() {
            let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some(&format!(
                        "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            };

            prod *= number.float_value();
        }

        DataObjectRef::new(DataObject::new_number(prod))
    }

    functions.push(crate::lang_func!(
            addd_function,
            crate::lang_func_metadata!(
                name="addd",
                return_type_constraint(
                    allowed=["DOUBLE"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn addd_function(
        interpreter: &mut Interpreter,
        number_objects: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut sum = 0.0;

        for (i, number_object) in number_objects.iter().
                enumerate() {
            let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some(&format!(
                        "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            };

            sum += number.double_value();
        }

        DataObjectRef::new(DataObject::new_number(sum))
    }

    functions.push(crate::lang_func!(
            muld_function,
            crate::lang_func_metadata!(
                name="muld",
                return_type_constraint(
                    allowed=["DOUBLE"],
                ),
                parameter(
                    name="&numbers",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn muld_function(
        interpreter: &mut Interpreter,
        number_objects: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut prod = 1.0;

        for (i, number_object) in number_objects.iter().
                enumerate() {
            let number = conversions::to_number(interpreter, number_object, CodePosition::EMPTY);
            let Some(number) = number else {
                return interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some(&format!(
                        "The type of argument {} (for var args parameter \"&numbers\") must be a number",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            };

            prod *= number.double_value();
        }

        DataObjectRef::new(DataObject::new_number(prod))
    }

    functions.push(math_1_arg_function!("sqrt", sqrt));
    functions.push(math_1_arg_function!("cbrt", cbrt));

    functions.push(math_2_arg_function!("hypot", hypot));

    functions.push(math_1_arg_function!("toRadians", to_radians));
    functions.push(math_1_arg_function!("toDegrees", to_degrees));

    functions.push(math_1_arg_function!("sin", sin));
    functions.push(math_1_arg_function!("cos", cos));
    functions.push(math_1_arg_function!("tan", tan));

    functions.push(math_1_arg_function!("asin", asin));
    functions.push(math_1_arg_function!("acos", acos));
    functions.push(math_1_arg_function!("atan", atan));

    functions.push(math_2_arg_function!("atan2", atan2));

    functions.push(math_1_arg_function!("sinh", sinh));
    functions.push(math_1_arg_function!("cosh", cosh));
    functions.push(math_1_arg_function!("tanh", tanh));

    functions.push(math_1_arg_function!("exp", exp));

    functions.push(math_1_arg_function!("loge", ln));
    functions.push(math_1_arg_function!("log10", log10));

    functions.push(crate::lang_func!(
            round_function,
            crate::lang_func_metadata!(
                name="round",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
    fn round_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap().double_value();

        let r = if number.signum() < 0.0 { -1 } else { 1 } * number.abs().round() as i64;

        DataObjectRef::new(DataObject::new_number(r))
    }

    functions.push(crate::lang_func!(
            ceil_function,
            crate::lang_func_metadata!(
                name="ceil",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
    fn ceil_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap().double_value();

        DataObjectRef::new(DataObject::new_number(number.ceil() as i64))
    }

    functions.push(crate::lang_func!(
            floor_function,
            crate::lang_func_metadata!(
                name="floor",
                return_type_constraint(
                    allowed=["LONG"],
                ),
                parameter(
                    name="$number",
                    parameter_type(number),
                ),
            ),
        ));
    fn floor_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap().double_value();

        DataObjectRef::new(DataObject::new_number(number.floor() as i64))
    }

    functions.push(crate::lang_func!(
            abs_function,
            crate::lang_func_metadata!(
                name="abs",
                parameter(
                    name="$operand",
                ),
            ),
        ));
    fn abs_function(
        interpreter: &mut Interpreter,
        operand: DataObjectRef,
    ) -> DataObjectRef {
        let ret = operators::op_abs(interpreter, &operand, CodePosition::EMPTY);
        let Some(ret) = ret else {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some(&format!("The abs operator is not defined for {}", operand.data_type())),
                CodePosition::EMPTY,
            );
        };

        ret
    }
}
