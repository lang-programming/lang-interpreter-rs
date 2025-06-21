use std::str::FromStr;
use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::{conversions, Interpreter, InterpretingError};
use crate::interpreter::data::{DataObject, DataObjectRef, Number};
use crate::lexer::CodePosition;
use crate::utils::math::ToNumberBase;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
        bin_to_dec_function,
        crate::lang_func_metadata!(
            name="binToDec",
            return_type_constraint(
                allowed=["INT"],
            ),
            parameter(
                name="$bin",
            ),
        ),
    ));
    fn bin_to_dec_function(
        interpreter: &mut Interpreter,
        bin_object: DataObjectRef,
    ) -> DataObjectRef {
        let bin_string = conversions::to_text(interpreter, &bin_object, CodePosition::EMPTY);
        if bin_string.starts_with("0b") && !bin_string.starts_with("0B") {
            return interpreter.set_errno_error_object(
                InterpretingError::NoBinNum,
                Some("Wrong prefix (Should be 0b or 0B)"),
                CodePosition::EMPTY,
            );
        }

        let number = i32::from_str_radix(&bin_string[2..], 2);
        match number {
            Ok(number) => {
                DataObjectRef::new(DataObject::new_number(number))
            },

            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::NoBinNum,
                    Some(&format!("{e}")),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
        oct_to_dec_function,
        crate::lang_func_metadata!(
            name="octToDec",
            return_type_constraint(
                allowed=["INT"],
            ),
            parameter(
                name="$oct",
            ),
        ),
    ));
    fn oct_to_dec_function(
        interpreter: &mut Interpreter,
        oct_object: DataObjectRef,
    ) -> DataObjectRef {
        let oct_string = conversions::to_text(interpreter, &oct_object, CodePosition::EMPTY);
        if oct_string.starts_with("0o") && !oct_string.starts_with("0O") {
            return interpreter.set_errno_error_object(
                InterpretingError::NoOctNum,
                Some("Wrong prefix (Should be 0o or 0O)"),
                CodePosition::EMPTY,
            );
        }

        let number = i32::from_str_radix(&oct_string[2..], 8);
        match number {
            Ok(number) => {
                DataObjectRef::new(DataObject::new_number(number))
            },

            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::NoOctNum,
                    Some(&format!("{e}")),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
        hex_to_dec_function,
        crate::lang_func_metadata!(
            name="hexToDec",
            return_type_constraint(
                allowed=["INT"],
            ),
            parameter(
                name="$hex",
            ),
        ),
    ));
    fn hex_to_dec_function(
        interpreter: &mut Interpreter,
        hex_object: DataObjectRef,
    ) -> DataObjectRef {
        let hex_string = conversions::to_text(interpreter, &hex_object, CodePosition::EMPTY);
        if hex_string.starts_with("0x") && !hex_string.starts_with("0X") {
            return interpreter.set_errno_error_object(
                InterpretingError::NoHexNum,
                Some("Wrong prefix (Should be 0x or 0X)"),
                CodePosition::EMPTY,
            );
        }

        let number = i32::from_str_radix(&hex_string[2..], 16);
        match number {
            Ok(number) => {
                DataObjectRef::new(DataObject::new_number(number))
            },

            Err(e) => {
                interpreter.set_errno_error_object(
                    InterpretingError::NoHexNum,
                    Some(&format!("{e}")),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
        to_number_base_function,
        crate::lang_func_metadata!(
            name="toNumberBase",
            return_type_constraint(
                allowed=["INT", "LONG"],
            ),
            parameter(
                name="$number",
            ),
            parameter(
                name="$base",
                parameter_type(number),
            ),
        ),
    ));
    fn to_number_base_function(
        interpreter: &mut Interpreter,
        number_object: DataObjectRef,
        base: DataObjectRef,
    ) -> DataObjectRef {
        let base = base.number_value().unwrap();

        if base.int_value() < 2 || base.int_value() > 36 {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidNumberBase,
                Some("Argument 2 (\"$base\") must be between 2 (inclusive) and 36 (inclusive)"),
                CodePosition::EMPTY,
            );
        }

        let number_string = conversions::to_text(interpreter, &number_object, CodePosition::EMPTY);

        let number = i32::from_str_radix(&number_string, base.int_value() as u32);
        match number {
            Ok(number) => {
                DataObjectRef::new(DataObject::new_number(number))
            },

            Err(_) => {
                let number = i64::from_str_radix(&number_string, base.int_value() as u32);
                match number {
                    Ok(number) => {
                        DataObjectRef::new(DataObject::new_number(number))
                    },

                    Err(_) => {
                        interpreter.set_errno_error_object(
                            InterpretingError::NoBaseNNum,
                            Some(&format!(
                                "Argument 1 (\"$number\" = \"{number_string}\") is not in base \"{}\"",
                                base.int_value(),
                            )),
                            CodePosition::EMPTY,
                        )
                    },
                }
            },
        }
    }

    functions.push(crate::lang_func!(
        to_text_base_function,
        crate::lang_func_metadata!(
            name="toTextBase",
            return_type_constraint(
                allowed=["TEXT"],
            ),
            parameter(
                name="$number",
                parameter_type(number),
            ),
            parameter(
                name="$base",
                parameter_type(number),
            ),
        ),
    ));
    fn to_text_base_function(
        interpreter: &mut Interpreter,
        number: DataObjectRef,
        base: DataObjectRef,
    ) -> DataObjectRef {
        let base = base.number_value().unwrap();

        if base.int_value() < 2 || base.int_value() > 36 {
            return interpreter.set_errno_error_object(
                InterpretingError::InvalidNumberBase,
                Some("Argument 2 (\"$base\") must be between 2 (inclusive) and 36 (inclusive)"),
                CodePosition::EMPTY,
            );
        }

        let number = number.number_value().unwrap();

        let number_int = number.int_value();
        let number_long = number.long_value();

        let text = if number_long == number_int as i64 {
            number_int.to_number_base(base.int_value() as u32)
        }else {
            number_long.to_number_base(base.int_value() as u32)
        };

        DataObjectRef::new(DataObject::new_text(text))
    }

    functions.push(crate::lang_func!(
        to_int_bits_function,
        crate::lang_func_metadata!(
            name="toIntBits",
            return_type_constraint(
                allowed=["INT"],
            ),
            parameter(
                name="$number",
                parameter_type(number),
            ),
        ),
    ));
    fn to_int_bits_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap();

        DataObjectRef::new(DataObject::new_number(number.float_value().to_bits() as i32))
    }

    functions.push(crate::lang_func!(
        to_float_bits_function,
        crate::lang_func_metadata!(
            name="toFloatBits",
            return_type_constraint(
                allowed=["FLOAT"],
            ),
            parameter(
                name="$number",
                parameter_type(number),
            ),
        ),
    ));
    fn to_float_bits_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap();

        DataObjectRef::new(DataObject::new_number(f32::from_bits(number.int_value() as u32)))
    }

    functions.push(crate::lang_func!(
        to_long_bits_function,
        crate::lang_func_metadata!(
            name="toLongBits",
            return_type_constraint(
                allowed=["LONG"],
            ),
            parameter(
                name="$number",
                parameter_type(number),
            ),
        ),
    ));
    fn to_long_bits_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap();

        DataObjectRef::new(DataObject::new_number(number.double_value().to_bits() as i64))
    }

    functions.push(crate::lang_func!(
        to_double_bits_function,
        crate::lang_func_metadata!(
            name="toDoubleBits",
            return_type_constraint(
                allowed=["DOUBLE"],
            ),
            parameter(
                name="$number",
                parameter_type(number),
            ),
        ),
    ));
    fn to_double_bits_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap();

        DataObjectRef::new(DataObject::new_number(f64::from_bits(number.int_value() as u64)))
    }

    functions.push(crate::lang_func!(
        ttoi_function,
        crate::lang_func_metadata!(
            name="ttoi",
            return_type_constraint(
                allowed=["INT"],
            ),
            parameter(
                name="$text",
            ),
        ),
    ));
    fn ttoi_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        let str = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

        let number = i32::from_str(&str);
        match number {
            Ok(number) => {
                DataObjectRef::new(DataObject::new_number(number))
            },

            Err(_) => {
                interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some("Argument 1 (\"$text\") can not be converted to an INT value"),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
        ttol_function,
        crate::lang_func_metadata!(
            name="ttol",
            return_type_constraint(
                allowed=["LONG"],
            ),
            parameter(
                name="$text",
            ),
        ),
    ));
    fn ttol_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        let str = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

        let number = i64::from_str(&str);
        match number {
            Ok(number) => {
                DataObjectRef::new(DataObject::new_number(number))
            },

            Err(_) => {
                interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some("Argument 1 (\"$text\") can not be converted to a LONG value"),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
        ttof_function,
        crate::lang_func_metadata!(
            name="ttof",
            return_type_constraint(
                allowed=["FLOAT"],
            ),
            parameter(
                name="$text",
            ),
        ),
    ));
    fn ttof_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        let str = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

        if str.contains(['x', 'X']) {
            return interpreter.set_errno_error_object(
                InterpretingError::NoNum,
                Some("Argument 1 (\"$text\") can not be converted to a FLOAT value"),
                CodePosition::EMPTY,
            );
        }

        let number = f32::from_str(&str);
        match number {
            Ok(number) => {
                DataObjectRef::new(DataObject::new_number(number))
            },

            Err(_) => {
                interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some("Argument 1 (\"$text\") can not be converted to a FLOAT value"),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
        ttod_function,
        crate::lang_func_metadata!(
            name="ttod",
            return_type_constraint(
                allowed=["DOUBLE"],
            ),
            parameter(
                name="$text",
            ),
        ),
    ));
    fn ttod_function(
        interpreter: &mut Interpreter,
        text_object: DataObjectRef,
    ) -> DataObjectRef {
        let str = conversions::to_text(interpreter, &text_object, CodePosition::EMPTY);

        if str.contains(['x', 'X']) {
            return interpreter.set_errno_error_object(
                InterpretingError::NoNum,
                Some("Argument 1 (\"$text\") can not be converted to a DOUBLE value"),
                CodePosition::EMPTY,
            );
        }

        let number = f64::from_str(&str);
        match number {
            Ok(number) => {
                DataObjectRef::new(DataObject::new_number(number))
            },

            Err(_) => {
                interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    Some("Argument 1 (\"$text\") can not be converted to a DOUBLE value"),
                    CodePosition::EMPTY,
                )
            },
        }
    }

    functions.push(crate::lang_func!(
        is_nan_function,
        crate::lang_func_metadata!(
            name="isNaN",
            return_type_constraint(
                allowed=["INT"],
            ),
            parameter(
                name="$number",
                parameter_type(number),
            ),
        ),
    ));
    fn is_nan_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap();

        let is_nan = match number {
            Number::Float(number) => number.is_nan(),
            Number::Double(number) => number.is_nan(),

            _ => false,
        };

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(is_nan)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
        is_infinite_function,
        crate::lang_func_metadata!(
            name="isInfinite",
            return_type_constraint(
                allowed=["INT"],
            ),
            parameter(
                name="$number",
                parameter_type(number),
            ),
        ),
    ));
    fn is_infinite_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap();

        let is_nan = match number {
            Number::Float(number) => number.is_infinite(),
            Number::Double(number) => number.is_infinite(),

            _ => false,
        };

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(is_nan)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
        is_finite_function,
        crate::lang_func_metadata!(
            name="isFinite",
            return_type_constraint(
                allowed=["INT"],
            ),
            parameter(
                name="$number",
                parameter_type(number),
            ),
        ),
    ));
    fn is_finite_function(
        _: &mut Interpreter,
        number: DataObjectRef,
    ) -> DataObjectRef {
        let number = number.number_value().unwrap();

        let is_nan = match number {
            Number::Float(number) => number.is_finite(),
            Number::Double(number) => number.is_finite(),

            _ => false,
        };

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(is_nan)
        }).unwrap())
    }
}
