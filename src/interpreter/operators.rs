use std::collections::VecDeque;
use std::ops::Deref;
use std::ptr;
use std::rc::Rc;
use crate::interpreter::data::{DataObject, DataObjectRef, DataType, DataValue, OptionDataObjectRef, StructObject};
use crate::interpreter::{conversions, Interpreter, InterpretingError};
use crate::interpreter::data::function::FunctionPointerObject;
use crate::interpreter::data::object::LangObject;
use crate::lexer::CodePosition;
use crate::utils;
use crate::utils::math::SpecialDiv;

fn call_operator_method_1_arg(
    interpreter: &mut Interpreter,
    operator_name: &str,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    call_operator_method(
        interpreter,
        operand,
        &("op:".to_string() + operator_name),
        &[],
        pos,
    )
}

fn call_operator_method_2_arg(
    interpreter: &mut Interpreter,
    operator_name: &str,
    has_reverse: bool,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method(
        interpreter,
        left_side_operand,
        &("op:".to_string() + operator_name),
        &[right_side_operand.clone()],
        pos,
    );

    if ret.is_some() || !has_reverse {
        return ret;
    }

    call_operator_method(
        interpreter,
        right_side_operand,
        &("op:r-".to_string() + operator_name),
        &[left_side_operand.clone()],
        pos,
    )
}

fn call_operator_method_3_arg(
    interpreter: &mut Interpreter,
    operator_name: &str,
    left_side_operand: &DataObjectRef,
    middle_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    call_operator_method(
        interpreter,
        left_side_operand,
        &("op:".to_string() + operator_name),
        &utils::separate_arguments_with_argument_separators(&[
            middle_operand.clone(),
            right_side_operand.clone(),
        ]),
        pos,
    )
}

fn call_operator_method(
    interpreter: &mut Interpreter,
    lang_object: &DataObjectRef,
    method_name: &str,
    argument_list: &[DataObjectRef],
    pos: CodePosition,
) -> OptionDataObjectRef {
    let lang_object = lang_object.object_value()?;

    if lang_object.borrow().is_class() {
        return None;
    }

    let method = lang_object.borrow().methods().get(method_name)?.clone();

    let ret = interpreter.call_function_pointer(&method, Some(method_name), argument_list, pos);

    Some(ret.unwrap_or_else(|| DataObjectRef::new(DataObject::new_void())))
}

//General operation functions
/**
 * For "@"
 */
pub fn op_len(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "len",
        operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match operand.data_value() {
        DataValue::ByteBuffer(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value.borrow().len() as i32)
            }).unwrap()))
        },

        DataValue::Array(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value.borrow().len() as i32)
            }).unwrap()))
        },

        DataValue::List(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value.borrow().len() as i32)
            }).unwrap()))
        },

        DataValue::Text(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value.chars().count() as i32)
            }).unwrap()))
        },

        DataValue::Char(_) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(1)
            }).unwrap()))
        },

        DataValue::Struct(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value.member_names().len() as i32)
            }).unwrap()))
        },

        _ => None,
    }
}
/**
 * For "^"
 */
pub fn op_deep_copy(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "deepCopy",
        operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match operand.data_value() {
        DataValue::ByteBuffer(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_byte_buffer(value.borrow().clone())
            }).unwrap()))
        },

        DataValue::Array(value) => {
            let arr_copy = value.borrow().iter().
                    map(|data_object| op_deep_copy(interpreter, data_object, pos)).
                    collect::<Option<_>>()?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(arr_copy)
            }).unwrap()))
        },

        DataValue::List(value) => {
            let list_copy = value.borrow().iter().
                    map(|data_object| op_deep_copy(interpreter, data_object, pos)).
                    collect::<Option<_>>()?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_list(list_copy)
            }).unwrap()))
        },

        DataValue::Struct(value) => {
            if value.is_definition() {
                let member_names = value.member_names();
                let members = member_names.iter().
                        zip(value.type_constraints()).
                        map(|(name, type_constraint)|
                                (name.deref(), type_constraint.map(|type_constraint|
                                        *type_constraint))).
                        collect::<Vec<_>>();

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_struct(Rc::new(StructObject::new_definition(&members)))
                }).unwrap()))
            }else {
                let base_definition = value.base_definition()?;

                let member_values_copy = value.member_names().into_iter().
                        map(|member| {
                            let member = value.get_member(&member);
                            match member {
                                Ok(member) => Ok(op_deep_copy(interpreter, &member, pos)),
                                Err(e) => Err(e),
                            }
                        }).
                        collect::<Result<Option<Box<_>>, _>>();
                let member_values_copy = match member_values_copy {
                    Ok(member_values_copy) => member_values_copy?,
                    Err(e) => {
                        return Some(interpreter.set_errno_error_object(
                            InterpretingError::IncompatibleDataType,
                            Some(e.message()),
                            pos,
                        ));
                    },
                };

                let struct_copy = StructObject::new_instance(
                    base_definition,
                    &member_values_copy,
                );
                let struct_copy = match struct_copy {
                    Ok(struct_copy) => struct_copy,
                    Err(e) => {
                        return Some(interpreter.set_errno_error_object(
                            InterpretingError::IncompatibleDataType,
                            Some(e.message()),
                            pos,
                        ));
                    },
                };

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_struct(Rc::new(struct_copy))
                }).unwrap()))
            }
        },

        DataValue::Object(_) => None,

        _ => Some(DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_data(&operand.borrow())
        }).unwrap())),
    }
}
/**
 * For "|||"
 */
pub fn op_concat(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "concat",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            Some(DataObjectRef::new(DataObject::new_text(format!(
                "{left_value}{}",
                conversions::to_text(interpreter, right_side_operand, pos),
            ))))
        },

        DataValue::Long(left_value) => {
            Some(DataObjectRef::new(DataObject::new_text(format!(
                "{left_value}{}",
                conversions::to_text(interpreter, right_side_operand, pos),
            ))))
        },

        DataValue::Float(left_value) => {
            Some(DataObjectRef::new(DataObject::new_text(format!(
                "{left_value}{}",
                conversions::to_text(interpreter, right_side_operand, pos),
            ))))
        },

        DataValue::Double(left_value) => {
            Some(DataObjectRef::new(DataObject::new_text(format!(
                "{left_value}{}",
                conversions::to_text(interpreter, right_side_operand, pos),
            ))))
        },

        DataValue::Char(left_value) => {
            Some(DataObjectRef::new(DataObject::new_text(format!(
                "{left_value}{}",
                conversions::to_text(interpreter, right_side_operand, pos),
            ))))
        },

        DataValue::Text(left_value) => {
            Some(DataObjectRef::new(DataObject::new_text(format!(
                "{left_value}{}",
                conversions::to_text(interpreter, right_side_operand, pos),
            ))))
        },

        DataValue::ByteBuffer(left_value) => {
            let right_value = right_side_operand.byte_buffer_value()?;

            let left_len = left_value.borrow().len();
            let mut new_byte_buf = vec![0; left_len + right_value.borrow().len()];
            new_byte_buf[..left_len].copy_from_slice(&left_value.borrow());
            new_byte_buf[left_len..].copy_from_slice(&right_value.borrow());

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_byte_buffer(new_byte_buf.into_boxed_slice())
            }).unwrap()))
        },

        DataValue::Array(left_value) => {
            if let Some(right_value) = right_side_operand.array_value() {
                let mut new_array = Vec::with_capacity(left_value.borrow().len() + right_value.borrow().len());
                new_array.extend_from_slice(&left_value.borrow());
                new_array.extend_from_slice(&right_value.borrow());

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_array(new_array.into_boxed_slice())
                }).unwrap()))
            }else if let Some(right_value) = right_side_operand.list_value() {
                let mut new_array = Vec::with_capacity(left_value.borrow().len() + right_value.borrow().len());
                new_array.extend_from_slice(&left_value.borrow());
                {
                    let right_value = right_value.borrow();
                    let right_value_slices = right_value.as_slices();
                    new_array.extend_from_slice(right_value_slices.0);
                    new_array.extend_from_slice(right_value_slices.1);
                }

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_array(new_array.into_boxed_slice())
                }).unwrap()))
            }else {
                None
            }
        },

        DataValue::List(left_value) => {
            if let Some(right_value) = right_side_operand.array_value() {
                let mut new_array = VecDeque::with_capacity(left_value.borrow().len() + right_value.borrow().len());
                new_array.extend(left_value.borrow().iter().cloned());
                new_array.extend(right_value.borrow().iter().cloned());

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_list(new_array)
                }).unwrap()))
            }else if let Some(right_value) = right_side_operand.list_value() {
                let mut new_array = VecDeque::with_capacity(left_value.borrow().len() + right_value.borrow().len());
                new_array.extend(left_value.borrow().iter().cloned());
                new_array.extend(right_value.borrow().iter().cloned());

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_list(new_array)
                }).unwrap()))
            }else {
                None
            }
        },

        DataValue::FunctionPointer(left_value) => {
            let right_value = right_side_operand.function_pointer_value()?;

            let function_name = format!("<concat-func({left_value}, {right_value})>");

            let left_side_operand = left_side_operand.clone();
            let right_side_operand = right_side_operand.clone();
            let concat_func = {
                let left_side_operand = left_side_operand.clone();
                let right_side_operand = right_side_operand.clone();

                move |interpreter: &mut Interpreter, args: Vec<DataObjectRef>| -> OptionDataObjectRef {
                    let ret_a = interpreter.call_function_pointer(
                        &left_value,
                        left_side_operand.variable_name().as_deref(),
                        &args,
                        CodePosition::EMPTY,
                    );

                    interpreter.call_function_pointer(
                        &right_value,
                        right_side_operand.variable_name().as_deref(),
                        &[utils::none_to_lang_void(ret_a)],
                        CodePosition::EMPTY,
                    )
                }
            };
            let func = FunctionPointerObject::from(crate::lang_func!(
                concat_func,
                vec![
                    Box::new(left_side_operand),
                    Box::new(right_side_operand),
                ],
                crate::lang_func_metadata!(
                    name="concat-func",
                    parameter(
                        name="&args",
                        parameter_type(raw_var_args),
                    ),
                ),
            )).copy_with_function_name(&function_name);

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(func))
            }).unwrap()))
        },

        _ => None,
    }
}
/**
 * For "&lt;=&gt;"
 */
pub fn op_spaceship(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    if is_less_than(interpreter, left_side_operand, right_side_operand, pos) {
        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_int(-1)
        }).unwrap()))
    }else if is_equals(interpreter, left_side_operand, right_side_operand, pos) {
        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_int(0)
        }).unwrap()))
    }else if is_greater_than(interpreter, left_side_operand, right_side_operand, pos) {
        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_int(1)
        }).unwrap()))
    }else {
        Some(DataObjectRef::new(DataObject::new()))
    }
}

//Math operation functions
/**
 * For "+|"
 */
pub fn op_inc(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "inc",
        operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match operand.data_value() {
        DataValue::Int(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value.wrapping_add(1))
            }).unwrap()))
        },

        DataValue::Long(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_long(value.wrapping_add(1))
            }).unwrap()))
        },

        DataValue::Float(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_float(value + 1.0)
            }).unwrap()))
        },

        DataValue::Double(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_double(value + 1.0)
            }).unwrap()))
        },

        DataValue::Char(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char(char::from_u32((value as u32).wrapping_add(1)).unwrap_or('\u{FFFD}'))
            }).unwrap()))
        },

        DataValue::FunctionPointer(value) => {
            let function_name = format!("<auto-unpack-func({value}>");

            let operand = operand.clone();
            let auto_unpack_func = {
                let operand = operand.clone();

                move |interpreter: &mut Interpreter, array_object: DataObjectRef| -> OptionDataObjectRef {
                    let argument_list = array_object.array_value().unwrap().borrow().iter().
                            map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                                data_object.set_data(&ele.borrow())
                            }).unwrap())).
                            collect::<Box<_>>();

                    interpreter.call_function_pointer(
                        &value,
                        operand.variable_name().as_deref(),
                        &utils::separate_arguments_with_argument_separators(&argument_list),
                        CodePosition::EMPTY,
                    )
                }
            };
            let func = FunctionPointerObject::from(crate::lang_func!(
                auto_unpack_func,
                vec![
                    Box::new(operand),
                ],
                crate::lang_func_metadata!(
                    name="auto-unpack-func",
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                ),
            )).copy_with_function_name(&function_name);

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(func))
            }).unwrap()))
        },

        _ => None,
    }
}
/**
 * For "-|"
 */
pub fn op_dec(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "dec",
        operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match operand.data_value() {
        DataValue::Int(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value.wrapping_sub(1))
            }).unwrap()))
        },

        DataValue::Long(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_long(value.wrapping_sub(1))
            }).unwrap()))
        },

        DataValue::Float(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_float(value - 1.0)
            }).unwrap()))
        },

        DataValue::Double(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_double(value - 1.0)
            }).unwrap()))
        },

        DataValue::Char(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char(char::from_u32((value as u32).wrapping_sub(1)).unwrap_or('\u{FFFD}'))
            }).unwrap()))
        },

        DataValue::FunctionPointer(value) => {
            let function_name = format!("<auto-pack-func({value}>");

            let operand = operand.clone();
            let auto_pack_func = {
                let operand = operand.clone();

                move |interpreter: &mut Interpreter, args: Vec<DataObjectRef>| -> OptionDataObjectRef {
                    let array = args.iter().
                            map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                                data_object.set_data(&ele.borrow())
                            }).unwrap())).
                            collect::<Box<_>>();

                    let array_value = DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_array(array)
                    }).unwrap());

                    interpreter.call_function_pointer(
                        &value,
                        operand.variable_name().as_deref(),
                        &[array_value],
                        CodePosition::EMPTY,
                    )
                }
            };
            let func = FunctionPointerObject::from(crate::lang_func!(
                auto_pack_func,
                vec![
                    Box::new(operand),
                ],
                crate::lang_func_metadata!(
                    name="auto-pack-func",
                    parameter(
                        name="&args",
                        parameter_type(var_args),
                    ),
                ),
            )).copy_with_function_name(&function_name);

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(func))
            }).unwrap()))
        },

        _ => None,
    }
}
/**
 * For "+"
 */
pub fn op_pos(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "pos",
        operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
        data_object.set_data(&operand.borrow())
    }).unwrap()))
}
/**
 * For "-"
 */
pub fn op_inv(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "inv",
        operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match operand.data_value() {
        DataValue::Int(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value.wrapping_neg())
            }).unwrap()))
        },

        DataValue::Long(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_long(value.wrapping_neg())
            }).unwrap()))
        },

        DataValue::Float(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_float(-value)
            }).unwrap()))
        },

        DataValue::Double(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_double(-value)
            }).unwrap()))
        },

        DataValue::Text(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_text(value.chars().rev().collect::<Box<_>>())
            }).unwrap()))
        },

        DataValue::ByteBuffer(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_byte_buffer(value.borrow().iter().rev().copied().collect::<Box<_>>())
            }).unwrap()))
        },

        DataValue::Array(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(value.borrow().iter().rev().cloned().collect::<Box<_>>())
            }).unwrap()))
        },

        DataValue::List(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_list(value.borrow().iter().rev().cloned().collect::<VecDeque<_>>())
            }).unwrap()))
        },

        _ => None,
    }
}
/**
 * For "+"
 */
pub fn op_add(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "add",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value.wrapping_add(right_value))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as i64).wrapping_add(right_value))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 + right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 + right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value.wrapping_add(right_value as i32))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_add(right_value as i64))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_add(right_value))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 + right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 + right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_add(right_value as i64))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Float(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value + right_value as f32)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value + right_value as f32)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value + right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 + right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value + right_value as i32 as f32)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Double(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value + right_value as f64)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value + right_value as f64)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value + right_value as f64)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value + right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value + right_value as i32 as f64)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Char(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int((left_value as i32).wrapping_add(right_value))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as i64).wrapping_add(right_value))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as i32 as f32 + right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as i32 as f64 + right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int((left_value as i32).wrapping_add(right_value as i32))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Text(left_value) => {
            Some(DataObjectRef::new(DataObject::new_text(format!(
                "{left_value}{}",
                conversions::to_text(interpreter, right_side_operand, pos),
            ))))
        },

        DataValue::Array(left_value) => {
            let mut arr_new = Vec::with_capacity(left_value.borrow().len() + 1);
            arr_new.extend_from_slice(&left_value.borrow());
            arr_new.push(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&right_side_operand.borrow())
            }).unwrap()));

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(arr_new.into_boxed_slice())
            }).unwrap()))
        },

        DataValue::List(left_value) => {
            let mut arr_new = VecDeque::with_capacity(left_value.borrow().len() + 1);
            arr_new.extend(left_value.borrow().iter().cloned());
            arr_new.push_back(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&right_side_operand.borrow())
            }).unwrap()));

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_list(arr_new)
            }).unwrap()))
        },

        DataValue::FunctionPointer(left_value) => {
            let right_value = right_side_operand.function_pointer_value()?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_function_pointer(Rc::new(left_value.
                        copy_with_added_functions(&right_value)))
            }).unwrap()))
        },

        _ => None,
    }
}
/**
 * For "-"
 */
pub fn op_sub(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "sub",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value.wrapping_sub(right_value))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as i64).wrapping_sub(right_value))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 - right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 - right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value.wrapping_sub(right_value as i32))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_sub(right_value as i64))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_sub(right_value))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 - right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 - right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_sub(right_value as i64))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Float(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value - right_value as f32)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value - right_value as f32)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value - right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 - right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value - right_value as i32 as f32)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Double(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value - right_value as f64)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value - right_value as f64)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value - right_value as f64)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value - right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value - right_value as i32 as f64)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Char(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int((left_value as i32).wrapping_sub(right_value))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as i64).wrapping_sub(right_value))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as i32 as f32 - right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as i32 as f64 - right_value)
                    }).unwrap()))
                },

                DataValue::Char(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int((left_value as i32).wrapping_sub(right_value as i32))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "*"
 */
pub fn op_mul(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "mul",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value.wrapping_mul(right_value))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as i64).wrapping_mul(right_value))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 * right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 * right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_mul(right_value as i64))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_mul(right_value))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 * right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 * right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Float(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value * right_value as f32)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value * right_value as f32)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value * right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 * right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Double(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value * right_value as f64)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value * right_value as f64)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value * right_value as f64)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value * right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Text(left_value) => {
            let right_value = right_side_operand.int_value()?;
            if right_value < 0 {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Integer value must be larger than or equals to 0"),
                    pos,
                ));
            }

            let mut builder = String::with_capacity(left_value.len() * right_value as usize);
            for _ in 0..right_value {
                builder += &left_value;
            }

            Some(DataObjectRef::new(DataObject::new_text(builder)))
        },

        _ => None,
    }
}
/**
 * For "**"
 */
pub fn op_pow(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "pow",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    let pow_result = if right_value >= 0 {
                        left_value.checked_pow(right_value as u32)
                    }else {
                        None
                    };

                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        if let Some(pow_result) = pow_result {
                            data_object.set_int(pow_result)
                        }else {
                            data_object.set_double((left_value as f64).powf(right_value as f64))
                        }
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Float(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double((left_value as f64).powf(right_value))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Double(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value.powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value.powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value.powf(right_value as f64))
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value.powf(right_value))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::FunctionPointer(left_value) => {
            let count = right_side_operand.int_value()?;
            if count < 0 {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Number must not be less than 0!"),
                    pos,
                ));
            }

            let function_name = format!("<{left_value} ** {count}>");

            if count == 0 {
                let pow_func = {
                    move |_: &mut Interpreter, _: Vec<DataObjectRef>| -> DataObjectRef {
                        DataObjectRef::new(DataObject::new_void())
                    }
                };
                let func = FunctionPointerObject::from(crate::lang_func!(
                    pow_func,
                    crate::lang_func_metadata!(
                        name="pow-func",
                        parameter(
                            name="&args",
                            parameter_type(raw_var_args),
                        ),
                    ),
                )).copy_with_function_name(&function_name);

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_function_pointer(Rc::new(func))
                }).unwrap()))
            }else {
                let left_side_operand = left_side_operand.clone();

                let pow_func = {
                    let left_side_operand = left_side_operand.clone();

                    move |interpreter: &mut Interpreter, args: Vec<DataObjectRef>| -> DataObjectRef {
                        let mut ret = utils::none_to_lang_void(interpreter.call_function_pointer(
                            &left_value,
                            left_side_operand.variable_name().as_deref(),
                            &args,
                            CodePosition::EMPTY,
                        ));

                        for _ in 1..count {
                            ret = utils::none_to_lang_void(interpreter.call_function_pointer(
                                &left_value,
                                left_side_operand.variable_name().as_deref(),
                                &[ret],
                                CodePosition::EMPTY,
                            ));
                        }

                        ret
                    }
                };
                let func = FunctionPointerObject::from(crate::lang_func!(
                    pow_func,
                    vec![
                        Box::new(count),
                        Box::new(left_side_operand),
                    ],
                    crate::lang_func_metadata!(
                        name="pow-func",
                        parameter(
                            name="&args",
                            parameter_type(raw_var_args),
                        ),
                    ),
                )).copy_with_function_name(&function_name);

                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_function_pointer(Rc::new(func))
                }).unwrap()))
            }
        },

        _ => None,
    }
}
/**
 * For "/"
 */
pub fn op_div(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "div",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(left_value as f64 / 0.0)
                        }).unwrap()))
                    }else if left_value.wrapping_rem(right_value) != 0 {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(left_value as f64 / right_value as f64)
                        }).unwrap()))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_int(left_value.wrapping_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(left_value as f64 / 0.0)
                        }).unwrap()))
                    }else if (left_value as i64).wrapping_rem(right_value) != 0 {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(left_value as f64 / right_value as f64)
                        }).unwrap()))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long((left_value as i64).wrapping_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 / right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 / right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(left_value as f64 / 0.0)
                        }).unwrap()))
                    }else if left_value.wrapping_rem(right_value as i64) != 0 {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(left_value as f64 / right_value as f64)
                        }).unwrap()))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_div(right_value as i64))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(left_value as f64 / 0.0)
                        }).unwrap()))
                    }else if left_value.wrapping_rem(right_value) != 0 {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(left_value as f64 / right_value as f64)
                        }).unwrap()))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 / right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 / right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Float(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value / right_value as f32)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value / right_value as f32)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value / right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 / right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Double(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value / right_value as f64)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value / right_value as f64)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value / right_value as f64)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value / right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "~/"
 */
pub fn op_trunc_div(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "truncDiv",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_int(left_value.wrapping_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long((left_value as i64).wrapping_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value as f32 / right_value;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float(tmp)
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value as f64 / right_value;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(tmp)
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_div(right_value as i64))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value as f32 / right_value;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float(tmp)
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value as f64 / right_value;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(tmp)
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        DataValue::Float(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value / right_value as f32;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float(tmp)
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value / right_value as f32;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float(tmp)
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value / right_value;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float(tmp)
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value as f64 / right_value;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(tmp)
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        DataValue::Double(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value / right_value as f64;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(tmp)
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value / right_value as f64;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(tmp)
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value / right_value as f64;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(tmp)
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        let tmp = left_value / right_value;
                        let tmp = if tmp > 0.0 {
                            tmp.floor()
                        }else {
                            tmp.ceil()
                        };

                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double(tmp)
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "//"
 */
pub fn op_floor_div(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "floorDiv",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_int(left_value.wrapping_floor_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long((left_value as i64).wrapping_floor_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value as f32 / right_value).floor())
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value as f64 / right_value).floor())
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_floor_div(right_value as i64))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_floor_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value as f32 / right_value).floor())
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value as f64 / right_value).floor())
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        DataValue::Float(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value / right_value as f32).floor())
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value / right_value as f32).floor())
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value / right_value).floor())
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value as f64 / right_value).floor())
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        DataValue::Double(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value / right_value as f64).floor())
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value / right_value as f64).floor())
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value / right_value as f64).floor())
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value / right_value).floor())
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "^/"
 */
pub fn op_ceil_div(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "ceilDiv",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_int(left_value.wrapping_ceil_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long((left_value as i64).wrapping_ceil_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value as f32 / right_value).ceil())
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value as f64 / right_value).ceil())
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_ceil_div(right_value as i64))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_ceil_div(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value as f32 / right_value).ceil())
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value as f64 / right_value).ceil())
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        DataValue::Float(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value / right_value as f32).ceil())
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value / right_value as f32).ceil())
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_float((left_value / right_value).ceil())
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value as f64 / right_value).ceil())
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        DataValue::Double(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value / right_value as f64).ceil())
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value / right_value as f64).ceil())
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value / right_value as f64).ceil())
                        }).unwrap()))
                    }
                },

                DataValue::Double(right_value) => {
                    if right_value == 0.0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_double((left_value / right_value).ceil())
                        }).unwrap()))
                    }
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "%"
 */
pub fn op_mod(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "mod",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_int(left_value.wrapping_rem(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long((left_value as i64).wrapping_rem(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 % right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 % right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_rem(right_value as i64))
                        }).unwrap()))
                    }
                },

                DataValue::Long(right_value) => {
                    if right_value == 0 {
                        Some(interpreter.set_errno_error_object(
                            InterpretingError::DivByZero,
                            None,
                            pos,
                        ))
                    }else {
                        Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_long(left_value.wrapping_rem(right_value))
                        }).unwrap()))
                    }
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value as f32 % right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 % right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Float(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value % right_value as f32)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value % right_value as f32)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_float(left_value % right_value)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value as f64 % right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Double(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value % right_value as f64)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value % right_value as f64)
                    }).unwrap()))
                },

                DataValue::Float(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value % right_value as f64)
                    }).unwrap()))
                },

                DataValue::Double(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_double(left_value % right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Text(left_value) => {
            let right_value = right_side_operand.array_value()?.borrow().clone();

            Some(interpreter.format_text(&left_value, &right_value))
        },

        _ => None,
    }
}
/**
 * For "&amp;"
 */
pub fn op_and(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "and",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value & right_value)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value as i64 & right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value & right_value as i64)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value & right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "|"
 */
pub fn op_or(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "or",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    if let Some(right_value) = right_side_operand.function_pointer_value() {
        return interpreter.call_function_pointer(
            &right_value,
            right_side_operand.variable_name().as_deref(),
            &[left_side_operand.clone()],
            pos
        );
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value | right_value)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value as i64 | right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value | right_value as i64)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value | right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "^"
 */
pub fn op_xor(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "xor",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value ^ right_value)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value as i64 ^ right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value ^ right_value as i64)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value ^ right_value)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "~"
 */
pub fn op_not(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "not",
        operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match operand.data_value() {
        DataValue::Int(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(!value)
            }).unwrap()))
        },

        DataValue::Long(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_long(!value)
            }).unwrap()))
        },

        _ => None,
    }
}
/**
 * For "&lt;&lt;"
 */
pub fn op_lshift(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "lshift",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value.wrapping_shl(right_value as u32))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as i64).wrapping_shl(right_value as u32))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_shl(right_value as u32))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_shl(right_value as u32))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "&gt;&gt;"
 */
pub fn op_rshift(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "rshift",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    if let Some(right_value) = right_side_operand.function_pointer_value() {
        return interpreter.call_function_pointer(
            &right_value,
            right_side_operand.variable_name().as_deref(),
            &[left_side_operand.clone()],
            pos
        );
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int(left_value.wrapping_shr(right_value as u32))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as i64).wrapping_shr(right_value as u32))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_shr(right_value as u32))
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long(left_value.wrapping_shr(right_value as u32))
                    }).unwrap()))
                },

                _ => None,
            }
        },

        _ => None,
    }
}
/**
 * For "&gt;&gt;&gt;"
 */
pub fn op_rzshift(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "rzshift",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    if let Some(right_value) = right_side_operand.function_pointer_value() {
        if let Some(left_value) = left_side_operand.array_value() {
            return interpreter.call_function_pointer(
                &right_value,
                right_side_operand.variable_name().as_deref(),
                &utils::separate_arguments_with_argument_separators(&left_value.borrow()),
                pos
            );
        }
    }

    match left_side_operand.data_value() {
        DataValue::Int(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_int((left_value as u32).wrapping_shr(right_value as u32) as i32)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as i64 as u64).wrapping_shr(right_value as u32) as i64)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        DataValue::Long(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Int(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as u64).wrapping_shr(right_value as u32) as i64)
                    }).unwrap()))
                },

                DataValue::Long(right_value) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_long((left_value as u64).wrapping_shr(right_value as u32) as i64)
                    }).unwrap()))
                },

                _ => None,
            }
        },

        _ => None,
    }
}

//All operation functions
pub fn op_cast(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let type_value = left_side_operand.type_value()?;

    match type_value {
        DataType::TEXT => {
            let value = conversions::to_text(interpreter, right_side_operand, pos);

            Some(DataObjectRef::new(DataObject::new_text(value)))
        },

        DataType::CHAR => {
            let value = conversions::to_char(interpreter, right_side_operand, pos)?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char(value)
            }).unwrap()))
        },

        DataType::INT => {
            let value = conversions::to_int(interpreter, right_side_operand, pos)?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value)
            }).unwrap()))
        },

        DataType::LONG => {
            let value = conversions::to_long(interpreter, right_side_operand, pos)?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_long(value)
            }).unwrap()))
        },

        DataType::FLOAT => {
            let value = conversions::to_float(interpreter, right_side_operand, pos)?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_float(value)
            }).unwrap()))
        },

        DataType::DOUBLE => {
            let value = conversions::to_double(interpreter, right_side_operand, pos)?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_double(value)
            }).unwrap()))
        },

        DataType::BYTE_BUFFER => {
            let value = conversions::to_byte_buffer(interpreter, right_side_operand, pos)?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_byte_buffer(value)
            }).unwrap()))
        },

        DataType::ARRAY => {
            let value = conversions::to_array(interpreter, right_side_operand, pos)?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(value)
            }).unwrap()))
        },

        DataType::LIST => {
            let value = conversions::to_list(interpreter, right_side_operand, pos)?;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_list(value)
            }).unwrap()))
        },

        _ => None,
    }
}
/**
 * For "[...]"
 */
pub fn op_get_item(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_2_arg(
        interpreter,
        "getItem",
        false,
        left_side_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::ByteBuffer(left_value) => {
            let right_value = right_side_operand.int_value()? as isize;

            let len = left_value.borrow().len();
            let index = if right_value < 0 {
                right_value.wrapping_add(len as isize)
            }else {
                right_value
            };

            if index < 0 || index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    None,
                    pos,
                ));
            }

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(left_value.borrow()[index as usize] as i8 as i32)
            }).unwrap()))
        },

        DataValue::Array(left_value) => {
            let right_value = right_side_operand.int_value()? as isize;

            let len = left_value.borrow().len();
            let index = if right_value < 0 {
                right_value.wrapping_add(len as isize)
            }else {
                right_value
            };

            if index < 0 || index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    None,
                    pos,
                ));
            }

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&left_value.borrow()[index as usize].borrow())
            }).unwrap()))
        },

        DataValue::List(left_value) => {
            let right_value = right_side_operand.int_value()? as isize;

            let len = left_value.borrow().len();
            let index = if right_value < 0 {
                right_value.wrapping_add(len as isize)
            }else {
                right_value
            };

            if index < 0 || index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    None,
                    pos,
                ));
            }

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&left_value.borrow()[index as usize].borrow())
            }).unwrap()))
        },

        DataValue::Text(left_value) => {
            let right_value = right_side_operand.int_value()? as isize;

            let len = left_value.chars().count();
            let index = if right_value < 0 {
                right_value.wrapping_add(len as isize)
            }else {
                right_value
            };

            if index < 0 || index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    None,
                    pos,
                ));
            }

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char(left_value.chars().nth(index as usize).unwrap())
            }).unwrap()))
        },

        DataValue::Char(left_value) => {
            let right_value = right_side_operand.int_value()? as isize;

            let index = if right_value < 0 {
                right_value.wrapping_add(1)
            }else {
                right_value
            };

            if index < 0 {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    None,
                    pos,
                ));
            }

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_char(left_value)
            }).unwrap()))
        },

        DataValue::Struct(left_value) => {
            let right_value = right_side_operand.text_value()?;

            let ret = left_value.get_member(&right_value);
            match ret {
                Ok(ret) => {
                    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_data(&ret.borrow())
                    }).unwrap()))
                },

                Err(e) => {
                    Some(interpreter.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(e.message()),
                        pos,
                    ))
                },
            }
        },

        _ => None,
    }
}
/**
 * For "?.[...]"
 */
pub fn op_optional_get_item(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    if matches!(left_side_operand.data_type(), DataType::NULL | DataType::VOID) {
        Some(DataObjectRef::new(DataObject::new_void()))
    }else {
        op_get_item(interpreter, left_side_operand, right_side_operand, pos)
    }
}
/**
 * For "[...:...]"
 */
pub fn op_slice(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    middle_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_3_arg(
        interpreter,
        "slice",
        left_side_operand,
        middle_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::ByteBuffer(left_value) => {
            let len = left_value.borrow().len();

            let middle_value = middle_operand.int_value().
                    or_else(|| (middle_operand.data_type() == DataType::VOID).then_some(0))? as isize;

            let right_value = right_side_operand.int_value().map(|i| i as isize).
                    or_else(|| (right_side_operand.data_type() == DataType::VOID).then_some(len as isize))?;

            let from_index = if middle_value < 0 {
                middle_value.wrapping_add(len as isize)
            }else {
                middle_value
            };

            let to_index = if right_value < 0 {
                right_value.wrapping_add(len as isize)
            }else {
                right_value
            };

            if from_index < 0 || from_index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice from index"),
                    pos,
                ));
            }

            if to_index < 0 || to_index as usize > len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice to index"),
                    pos,
                ));
            }

            if to_index < from_index {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("to index is less than from index"),
                    pos,
                ));
            }

            let from_index = from_index as usize;
            let to_index = to_index as usize;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_byte_buffer(Box::from(&left_value.borrow()[from_index..to_index]))
            }).unwrap()))
        },

        DataValue::Array(left_value) => {
            let len = left_value.borrow().len();

            let middle_value = middle_operand.int_value().
                    or_else(|| (middle_operand.data_type() == DataType::VOID).then_some(0))? as isize;

            let right_value = right_side_operand.int_value().map(|i| i as isize).
                    or_else(|| (right_side_operand.data_type() == DataType::VOID).then_some(len as isize))?;

            let from_index = if middle_value < 0 {
                middle_value.wrapping_add(len as isize)
            }else {
                middle_value
            };

            let to_index = if right_value < 0 {
                right_value.wrapping_add(len as isize)
            }else {
                right_value
            };

            if from_index < 0 || from_index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice from index"),
                    pos,
                ));
            }

            if to_index < 0 || to_index as usize > len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice to index"),
                    pos,
                ));
            }

            if to_index < from_index {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("to index is less than from index"),
                    pos,
                ));
            }

            let from_index = from_index as usize;
            let to_index = to_index as usize;

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(Box::from(&left_value.borrow()[from_index..to_index]))
            }).unwrap()))
        },

        DataValue::List(left_value) => {
            let len = left_value.borrow().len();

            let middle_value = middle_operand.int_value().
                    or_else(|| (middle_operand.data_type() == DataType::VOID).then_some(0))? as isize;

            let right_value = right_side_operand.int_value().map(|i| i as isize).
                    or_else(|| (right_side_operand.data_type() == DataType::VOID).then_some(len as isize))?;

            let from_index = if middle_value < 0 {
                middle_value.wrapping_add(len as isize)
            }else {
                middle_value
            };

            let to_index = if right_value < 0 {
                right_value.wrapping_add(len as isize)
            }else {
                right_value
            };

            if from_index < 0 || from_index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice from index"),
                    pos,
                ));
            }

            if to_index < 0 || to_index as usize > len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice to index"),
                    pos,
                ));
            }

            if to_index < from_index {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("to index is less than from index"),
                    pos,
                ));
            }

            let from_index = from_index as usize;
            let to_index = to_index as usize;

            let list = left_value.borrow().iter().
                    take(to_index).
                    skip(from_index).
                    cloned().
                    collect::<VecDeque<_>>();

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_list(list)
            }).unwrap()))
        },

        DataValue::Text(left_value) => {
            let len = left_value.chars().count();

            let middle_value = middle_operand.int_value().
                    or_else(|| (middle_operand.data_type() == DataType::VOID).then_some(0))? as isize;

            let right_value = right_side_operand.int_value().map(|i| i as isize).
                    or_else(|| (right_side_operand.data_type() == DataType::VOID).then_some(len as isize))?;

            let from_index = if middle_value < 0 {
                middle_value.wrapping_add(len as isize)
            }else {
                middle_value
            };

            let to_index = if right_value < 0 {
                right_value.wrapping_add(len as isize)
            }else {
                right_value
            };

            if from_index < 0 || from_index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice from index"),
                    pos,
                ));
            }

            if to_index < 0 || to_index as usize > len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice to index"),
                    pos,
                ));
            }

            if to_index < from_index {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("to index is less than from index"),
                    pos,
                ));
            }

            let from_index = from_index as usize;
            let to_index = to_index as usize;

            let text = left_value.chars().
                    take(to_index).
                    skip(from_index).
                    collect::<Box<str>>();

            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_text(text)
            }).unwrap()))
        },

        DataValue::Char(left_value) => {
            let middle_value = middle_operand.int_value().
                    or_else(|| (middle_operand.data_type() == DataType::VOID).then_some(0))? as isize;

            let right_value = right_side_operand.int_value().map(|i| i as isize).
                    or_else(|| (right_side_operand.data_type() == DataType::VOID).then_some(1))?;

            let from_index = if middle_value < 0 {
                middle_value.wrapping_add(1)
            }else {
                middle_value
            };

            let to_index = if right_value < 0 {
                right_value.wrapping_add(1)
            }else {
                right_value
            };

            if from_index != 0 {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice from index"),
                    pos,
                ));
            }

            if to_index != 0 && to_index != 1 {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    Some("For slice to index"),
                    pos,
                ));
            }

            if to_index == 0 {
                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_text("")
                }).unwrap()))
            }else {
                Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_char(left_value)
                }).unwrap()))
            }
        },

        _ => None,
    }
}
/**
 * For "?.[...:...]"
 */
pub fn op_optional_slice(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    middle_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    if matches!(left_side_operand.data_type(), DataType::NULL | DataType::VOID) {
        Some(DataObjectRef::new(DataObject::new_void()))
    }else {
        op_slice(interpreter, left_side_operand, middle_operand, right_side_operand, pos)
    }
}
pub fn op_set_item(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    middle_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_3_arg(
        interpreter,
        "setItem",
        left_side_operand,
        middle_operand,
        right_side_operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match left_side_operand.data_value() {
        DataValue::ByteBuffer(left_value) => {
            let middle_value = middle_operand.int_value()? as isize;

            let len = left_value.borrow().len();
            let index = if middle_value < 0 {
                middle_value.wrapping_add(len as isize)
            }else {
                middle_value
            };

            if index < 0 || index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    None,
                    pos,
                ));
            }

            let value_number = conversions::to_number(interpreter, right_side_operand, pos);
            let Some(value_number) = value_number else {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::NoNum,
                    None,
                    pos,
                ));
            };
            let value = value_number.int_value() as u8;

            left_value.borrow_mut()[index as usize] = value;

            Some(DataObjectRef::new(DataObject::new_void()))
        },

        DataValue::Array(left_value) => {
            let middle_value = middle_operand.int_value()? as isize;

            let len = left_value.borrow().len();
            let index = if middle_value < 0 {
                middle_value.wrapping_add(len as isize)
            }else {
                middle_value
            };

            if index < 0 || index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    None,
                    pos,
                ));
            }

            let value = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&right_side_operand.borrow())
            }).unwrap());

            left_value.borrow_mut()[index as usize] = value;

            Some(DataObjectRef::new(DataObject::new_void()))
        },

        DataValue::List(left_value) => {
            let middle_value = middle_operand.int_value()? as isize;

            let len = left_value.borrow().len();
            let index = if middle_value < 0 {
                middle_value.wrapping_add(len as isize)
            }else {
                middle_value
            };

            if index < 0 || index as usize >= len {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IndexOutOfBounds,
                    None,
                    pos,
                ));
            }

            let value = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&right_side_operand.borrow())
            }).unwrap());

            left_value.borrow_mut()[index as usize] = value;

            Some(DataObjectRef::new(DataObject::new_void()))
        },

        DataValue::Struct(left_value) => {
            let middle_value = middle_operand.text_value()?;

            let ret = left_value.set_member(&middle_value, &right_side_operand.borrow());
            if let Err(e) = ret {
                Some(interpreter.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(e.message()),
                    pos,
                ))
            }else {
                Some(DataObjectRef::new(DataObject::new_void()))
            }
        },

        _ => None,
    }
}

//Special operator functions
/**
 * For "func.abs()"
 */
pub fn op_abs(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "abs",
        operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match operand.data_value() {
        DataValue::Int(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_int(value.wrapping_abs())
            }).unwrap()))
        },

        DataValue::Long(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_long(value.wrapping_abs())
            }).unwrap()))
        },

        DataValue::Float(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_float(value.abs())
            }).unwrap()))
        },

        DataValue::Double(value) => {
            Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_double(value.abs())
            }).unwrap()))
        },

        _ => None,
    }
}
/**
 * For "func.iter()"
 */
pub fn op_iter(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "iter",
        operand,
        pos,
    );
    if let Some(ret) = ret {
        return Some(ret);
    }

    match operand.data_value() {
        DataValue::ByteBuffer(_) |
        DataValue::Array(_) |
        DataValue::List(_) |
        DataValue::Struct(_) |
        DataValue::Text(_) => {
            let basic_iterator_class = interpreter.standard_types.get("&BasicIterator").unwrap().object_value().unwrap();

            Some(interpreter.call_constructor(&basic_iterator_class, &[operand.clone()], pos))
        },

        _ => None,
    }
}
/**
 * For "func.hasNext()"
 */
pub fn op_has_next(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method_1_arg(
        interpreter,
        "hasNext",
        operand,
        pos,
    )?;

    Some(DataObjectRef::new(DataObject::with_update(|data_object| {
        data_object.set_bool(conversions::to_bool(interpreter, &ret, pos))
    }).unwrap()))
}
/**
 * For "func.next()"
 */
pub fn op_next(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    call_operator_method_1_arg(
        interpreter,
        "next",
        operand,
        pos,
    )
}

/**
 * For "...(...)"
 */
pub fn op_call(
    interpreter: &mut Interpreter,
    callee: &DataObjectRef,
    argument_list: &[DataObjectRef],
    pos: CodePosition,
) -> OptionDataObjectRef {
    let ret = call_operator_method(interpreter, callee,"op:call", argument_list, pos);
    if let Some(ret) = ret {
        return Some(ret);
    }

    if let Some(function_value) = callee.function_pointer_value() {
        return interpreter.call_function_pointer(
            &function_value,
            callee.variable_name().as_deref(),
            argument_list,
            pos,
        );
    }else if let Some(type_value) = callee.type_value() {
        let combined_argument_list = utils::combine_arguments_without_argument_separators(
            argument_list, interpreter, pos,
        );

        if combined_argument_list.is_empty() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArgCount,
                Some("Not enough arguments for TYPE casting (1 needed)"),
                pos,
            ));
        }
        if combined_argument_list.len() > 1 {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArgCount,
                Some("Too many arguments for TYPE casting (1 needed)"),
                pos,
            ));
        }

        let arg = &combined_argument_list[0];

        let output = op_cast(interpreter, callee, arg, pos);
        if output.is_none() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::IncompatibleDataType,
                Some(&format!(
                    "Data type \"{:?}\" can not be casted to \"{:?}\"!",
                    arg.data_type(),
                    type_value,
                )),
                pos,
            ));
        }

        return output;
    }else if let Some(struct_value) = callee.struct_value() {
        if struct_value.is_definition() {
            let combined_argument_list = utils::combine_arguments_without_argument_separators(
                argument_list, interpreter, pos,
            );

            let member_names = struct_value.member_names();
            if combined_argument_list.len() != member_names.len() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The argument count is not equals to the count of member names ({})",
                        member_names.len(),
                    )),
                    pos,
                ));
            }

            let ret = DataObject::with_update(|data_object| {
                data_object.set_struct(Rc::new(StructObject::new_instance(
                    struct_value,
                    &combined_argument_list,
                )?))
            });
            return match ret {
                Ok(ret) => Some(DataObjectRef::new(ret)),

                Err(e) => {
                    Some(interpreter.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(e.message()),
                        pos,
                    ))
                },
            };
        }
    }else if let Some(object_value) = callee.object_value() {
        let is_class = object_value.borrow().is_class();
        if is_class {
            let created_object = LangObject::new_object(&object_value).unwrap();

            let constructors = created_object.borrow().constructors();

            let ret = interpreter.call_function_pointer(
                &constructors,
                constructors.function_name(),
                argument_list,
                pos,
            ).unwrap_or_else(|| DataObjectRef::new(DataObject::new_void()));

            if ret.data_type() != DataType::VOID {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidAstNode,
                    Some("Invalid constructor implementation: VOID must be returned"),
                    pos,
                ));
            }

            let ret = created_object.borrow_mut().post_construct();
            if let Err(e) = ret {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(&format!(
                        "Invalid constructor implementation (Some members have invalid types): {}",
                        e.message(),
                    )),
                    pos,
                ));
            };

            return Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_object(created_object)
            }).unwrap()));
        }
    }

    None
}

//Comparison functions
/**
 * For "=="
 */
pub fn is_equals(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> bool {
    let ret = call_operator_method_2_arg(
        interpreter,
        "isEquals",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );

    if let Some(ret) = ret {
        return conversions::to_bool(interpreter, &ret, pos);
    }

    if ptr::eq(
        left_side_operand.borrow().deref(),
        right_side_operand.borrow().deref(),
    ) {
        return true;
    }

    let number = conversions::to_number(interpreter, right_side_operand, pos);

    match left_side_operand.data_value() {
        DataValue::Text(left_value) => {
            if let Some(right_value) = right_side_operand.text_value() {
                return *left_value == *right_value;
            }

            if left_value.chars().count() == 1 {
                if let Some(right_value) = right_side_operand.char_value() {
                    return left_value.chars().next().unwrap() == right_value;
                }
            }

            number.is_some_and(|_| is_equals(interpreter, right_side_operand, left_side_operand, pos))
        },

        DataValue::Char(left_value) => {
            if let Some(right_value) = right_side_operand.text_value() {
                if right_value.chars().count() == 1 {
                    return left_value == right_value.chars().next().unwrap();
                }
            }

            number.is_some_and(|number| left_value as i32 == number.int_value())
        },

        DataValue::Int(left_value) => {
            number.is_some_and(|number| left_value == number.int_value())
        },

        DataValue::Long(left_value) => {
            number.is_some_and(|number| left_value == number.long_value())
        },

        DataValue::Float(left_value) => {
            number.is_some_and(|number| left_value == number.float_value())
        },

        DataValue::Double(left_value) => {
            number.is_some_and(|number| left_value == number.double_value())
        },

        DataValue::ByteBuffer(left_value) => {
            if let Some(right_value) = right_side_operand.byte_buffer_value() {
                return **left_value.borrow() == **right_value.borrow();
            }

            number.is_some_and(|number| left_value.borrow().len() == number.int_value() as usize)
        },

        DataValue::Array(left_value) => {
            if let Some(right_value) = right_side_operand.array_value() {
                if left_value.borrow().len() != right_value.borrow().len() {
                    return false;
                }

                let left_value = left_value.borrow().clone();
                let right_value = right_value.borrow().clone();
                for (left_value, right_value) in left_value.iter().
                        zip(right_value.iter()) {
                    if !is_equals(interpreter, left_value, right_value, pos) {
                        return false;
                    }
                }

                return true;
            }

            if let Some(right_value) = right_side_operand.list_value() {
                if left_value.borrow().len() != right_value.borrow().len() {
                    return false;
                }

                let left_value = left_value.borrow().clone();
                let right_value = right_value.borrow().clone();
                for (left_value, right_value) in left_value.iter().
                        zip(right_value.iter()) {
                    if !is_equals(interpreter, left_value, right_value, pos) {
                        return false;
                    }
                }

                return true;
            }

            number.is_some_and(|number| left_value.borrow().len() == number.int_value() as usize)
        },

        DataValue::List(left_value) => {
            if let Some(right_value) = right_side_operand.list_value() {
                if left_value.borrow().len() != right_value.borrow().len() {
                    return false;
                }

                let left_value = left_value.borrow().clone();
                let right_value = right_value.borrow().clone();
                for (left_value, right_value) in left_value.iter().
                        zip(right_value.iter()) {
                    if !is_equals(interpreter, left_value, right_value, pos) {
                        return false;
                    }
                }

                return true;
            }

            if let Some(right_value) = right_side_operand.array_value() {
                if left_value.borrow().len() != right_value.borrow().len() {
                    return false;
                }

                let left_value = left_value.borrow().clone();
                let right_value = right_value.borrow().clone();
                for (left_value, right_value) in left_value.iter().
                        zip(right_value.iter()) {
                    if !is_equals(interpreter, left_value, right_value, pos) {
                        return false;
                    }
                }

                return true;
            }

            number.is_some_and(|number| left_value.borrow().len() == number.int_value() as usize)
        },

        DataValue::Struct(left_value) => {
            if let Some(right_value) = right_side_operand.struct_value() {
                if left_value.is_definition() != right_value.is_definition() {
                    return false;
                }

                let left_member_names = left_value.member_names();
                let right_member_names = right_value.member_names();
                if left_member_names.len() != right_member_names.len() {
                    return false;
                }

                for (left_member_name, right_member_name) in left_member_names.into_iter().
                        zip(right_member_names) {
                    if left_member_name != right_member_name || (!left_value.is_definition() &&
                            !is_equals(
                                interpreter,
                                &left_value.get_member(&left_member_name).unwrap(),
                                &right_value.get_member(&right_member_name).unwrap(),
                                pos,
                            )) {
                        return false;
                    }
                }

                return true;
            }

            number.is_some_and(|number| left_value.member_names().len() == number.int_value() as usize)
        },

        DataValue::Object(left_value) => {
            right_side_operand.object_value().is_some_and(|right_value| {
                //Check for same reference only (For classes and objects if "op:isEquals()" is not defined)
                ptr::eq(left_value.borrow().deref(), right_value.borrow().deref())
            })
        },

        DataValue::VarPointer(left_value) => {
            right_side_operand.var_pointer_value().is_some_and(|right_value| {
                //Check for same reference only
                ptr::eq(left_value.borrow().deref(), right_value.borrow().deref())
            })
        },

        DataValue::FunctionPointer(left_value) => {
            right_side_operand.function_pointer_value().is_some_and(|right_value| {
                left_value.is_equals(&right_value, interpreter, pos)
            })
        },

        DataValue::Error(left_value) => {
            match right_side_operand.data_value() {
                DataValue::Text(right_value) => {
                    if let Some(number) = number {
                        left_value.err().error_code() == number.int_value()
                    }else {
                        *left_value.err().error_text() == *right_value
                    }
                },

                DataValue::Char(_) |
                DataValue::Int(_) |
                DataValue::Long(_) |
                DataValue::Float(_) |
                DataValue::Double(_) |
                DataValue::ByteBuffer(_) |
                DataValue::Array(_) |
                DataValue::List(_) |
                DataValue::Struct(_) => {
                    number.is_some_and(|number| left_value.err().error_code() == number.int_value())
                },

                DataValue::Error(right_value) => {
                    left_value == right_value
                },

                _ => false,
            }
        },

        DataValue::Type(left_value) => {
            if let Some(right_value) = right_side_operand.type_value() {
                left_value == right_value
            }else {
                left_value == right_side_operand.data_type()
            }
        },

        DataValue::Null |
        DataValue::Void |
        DataValue::ArgumentSeparator(_) => {
            left_side_operand.data_type() == right_side_operand.data_type()
        },
    }
}
/**
 * For "==="
 */
pub fn is_strict_equals(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> bool {
    let ret = call_operator_method_2_arg(
        interpreter,
        "isStrictEquals",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );

    if let Some(ret) = ret {
        return conversions::to_bool(interpreter, &ret, pos);
    }

    if ptr::eq(
        left_side_operand.borrow().deref(),
        right_side_operand.borrow().deref(),
    ) {
        return true;
    }

    if left_side_operand.data_type() != right_side_operand.data_type() {
        return false;
    }

    match left_side_operand.data_value() {
        DataValue::Text(left_value) => {
            let right_value = right_side_operand.text_value().unwrap();

            *left_value == *right_value
        },

        DataValue::Char(left_value) => {
            let right_value = right_side_operand.char_value().unwrap();

            left_value == right_value
        },

        DataValue::Int(left_value) => {
            let right_value = right_side_operand.int_value().unwrap();

            left_value == right_value
        },

        DataValue::Long(left_value) => {
            let right_value = right_side_operand.long_value().unwrap();

            left_value == right_value
        },

        DataValue::Float(left_value) => {
            let right_value = right_side_operand.float_value().unwrap();

            left_value == right_value
        },

        DataValue::Double(left_value) => {
            let right_value = right_side_operand.double_value().unwrap();

            left_value == right_value
        },

        DataValue::ByteBuffer(left_value) => {
            let right_value = right_side_operand.byte_buffer_value().unwrap();
            let right_value = right_value.borrow();

            **left_value.borrow() == **right_value
        },

        DataValue::Array(left_value) => {
            let right_value = right_side_operand.array_value().unwrap();

            if left_value.borrow().len() != right_value.borrow().len() {
                return false;
            }

            let left_value = left_value.borrow().clone();
            let right_value = right_value.borrow().clone();
            for (left_value, right_value) in left_value.iter().
                    zip(right_value.iter()) {
                if !is_strict_equals(interpreter, left_value, right_value, pos) {
                    return false;
                }
            }

            true
        },

        DataValue::List(left_value) => {
            let right_value = right_side_operand.list_value().unwrap();

            if left_value.borrow().len() != right_value.borrow().len() {
                return false;
            }

            let left_value = left_value.borrow().clone();
            let right_value = right_value.borrow().clone();
            for (left_value, right_value) in left_value.iter().
                    zip(right_value.iter()) {
                if !is_strict_equals(interpreter, left_value, right_value, pos) {
                    return false;
                }
            }

            true
        },

        DataValue::Struct(left_value) => {
            let right_value = right_side_operand.struct_value().unwrap();

            if left_value.is_definition() != right_value.is_definition() {
                return false;
            }

            let left_member_names = left_value.member_names();
            let right_member_names = right_value.member_names();
            if left_member_names.len() != right_member_names.len() {
                return false;
            }

            for (left_member_name, right_member_name) in left_member_names.into_iter().
                    zip(right_member_names) {
                if left_member_name != right_member_name || (!left_value.is_definition() &&
                        !is_strict_equals(
                            interpreter,
                            &left_value.get_member(&left_member_name).unwrap(),
                            &right_value.get_member(&right_member_name).unwrap(),
                            pos,
                        )) {
                    return false;
                }
            }

            true
        },

        DataValue::Object(left_value) => {
            let right_value = right_side_operand.object_value().unwrap();
            let right_value = right_value.borrow();

            //Check for same reference only (For classes and objects if "op:isStrictEquals()" is not defined)
            ptr::eq(left_value.borrow().deref(), right_value.deref())
        },

        DataValue::VarPointer(left_value) => {
            let right_value = right_side_operand.var_pointer_value().unwrap();
            let right_value = right_value.borrow();

            //Check for same reference only
            ptr::eq(left_value.borrow().deref(), right_value.deref())
        },

        DataValue::FunctionPointer(left_value) => {
            let right_value = right_side_operand.function_pointer_value().unwrap();

            left_value.is_equals(&right_value, interpreter, pos)
        },

        DataValue::Error(left_value) => {
            let right_value = right_side_operand.error_value().unwrap();

            left_value == right_value
        },

        DataValue::Type(left_value) => {
            let right_value = right_side_operand.type_value().unwrap();

            left_value == right_value
        },

        DataValue::Null |
        DataValue::Void |
        DataValue::ArgumentSeparator(_) => {
            left_side_operand.data_type() == right_side_operand.data_type()
        },
    }
}
/**
 * For "&lt;"
 */
pub fn is_less_than(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> bool {
    let ret = call_operator_method_2_arg(
        interpreter,
        "isLessThan",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );

    if let Some(ret) = ret {
        return conversions::to_bool(interpreter, &ret, pos);
    }

    if ptr::eq(
        left_side_operand.borrow().deref(),
        right_side_operand.borrow().deref(),
    ) {
        return false;
    }

    let number = conversions::to_number(interpreter, right_side_operand, pos).
            map(|number| DataObject::with_update(|data_object| {
                data_object.set_number(number)
            }).unwrap()).unwrap_or_default();

    match left_side_operand.data_value() {
        DataValue::Text(left_value) => {
            if let Some(right_value) = right_side_operand.text_value() {
                return *left_value < *right_value;
            }

            if left_value.chars().count() == 1 {
                if let Some(right_value) = right_side_operand.char_value() {
                    return left_value.chars().next().unwrap() < right_value;
                }
            }

            let this_number = conversions::to_number(interpreter, left_side_operand, pos);
            let Some(this_number) = this_number else {
                return false;
            };

            match number.data_value().clone() {
                DataValue::Int(number) => this_number.int_value() < number,
                DataValue::Long(number) => this_number.long_value() < number,
                DataValue::Float(number) => this_number.float_value() < number,
                DataValue::Double(number) => this_number.double_value() < number,

                _ => false
            }
        },

        DataValue::Char(left_value) => {
            if let Some(right_value) = right_side_operand.text_value() {
                if right_value.chars().count() == 1 {
                    return left_value < right_value.chars().next().unwrap();
                }
            }

            match number.data_value().clone() {
                DataValue::Int(number) => (left_value as i32) < number,
                DataValue::Long(number) => (left_value as i64) < number,
                DataValue::Float(number) => (left_value as i32 as f32) < number,
                DataValue::Double(number) => (left_value as i32 as f64) < number,

                _ => false
            }
        },

        DataValue::Int(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => left_value < number,
                DataValue::Long(number) => (left_value as i64) < number,
                DataValue::Float(number) => (left_value as f32) < number,
                DataValue::Double(number) => (left_value as f64) < number,

                _ => false
            }
        },

        DataValue::Long(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => left_value < number as i64,
                DataValue::Long(number) => left_value < number,
                DataValue::Float(number) => (left_value as f32) < number,
                DataValue::Double(number) => (left_value as f64) < number,

                _ => false
            }
        },

        DataValue::Float(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => left_value < number as f32,
                DataValue::Long(number) => left_value < number as f32,
                DataValue::Float(number) => left_value < number,
                DataValue::Double(number) => (left_value as f64) < number,

                _ => false
            }
        },

        DataValue::Double(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => left_value < number as f64,
                DataValue::Long(number) => left_value < number as f64,
                DataValue::Float(number) => left_value < number as f64,
                DataValue::Double(number) => left_value < number,

                _ => false
            }
        },

        DataValue::ByteBuffer(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => (left_value.borrow().len() as i32) < number,
                DataValue::Long(number) => (left_value.borrow().len() as i32 as i64) < number,
                DataValue::Float(number) => (left_value.borrow().len() as i32 as f32) < number,
                DataValue::Double(number) => (left_value.borrow().len() as i32 as f64) < number,

                _ => false
            }
        },

        DataValue::Array(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => (left_value.borrow().len() as i32) < number,
                DataValue::Long(number) => (left_value.borrow().len() as i32 as i64) < number,
                DataValue::Float(number) => (left_value.borrow().len() as i32 as f32) < number,
                DataValue::Double(number) => (left_value.borrow().len() as i32 as f64) < number,

                _ => false
            }
        },

        DataValue::List(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => (left_value.borrow().len() as i32) < number,
                DataValue::Long(number) => (left_value.borrow().len() as i32 as i64) < number,
                DataValue::Float(number) => (left_value.borrow().len() as i32 as f32) < number,
                DataValue::Double(number) => (left_value.borrow().len() as i32 as f64) < number,

                _ => false
            }
        },

        DataValue::Error(left_value) => {
            if let Some(right_value) = right_side_operand.text_value() {
                return *left_value.err().error_text() < *right_value;
            }

            match number.data_value().clone() {
                DataValue::Int(number) => left_value.err().error_code() < number,
                DataValue::Long(number) => (left_value.err().error_code() as i64) < number,
                DataValue::Float(number) => (left_value.err().error_code() as f32) < number,
                DataValue::Double(number) => (left_value.err().error_code() as f64) < number,

                _ => false
            }
        },

        DataValue::Struct(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => (left_value.member_names().len() as i32) < number,
                DataValue::Long(number) => (left_value.member_names().len() as i32 as i64) < number,
                DataValue::Float(number) => (left_value.member_names().len() as i32 as f32) < number,
                DataValue::Double(number) => (left_value.member_names().len() as i32 as f64) < number,

                _ => false
            }
        },

        _ => false,
    }
}

pub fn is_greater_than(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> bool {
    let ret = call_operator_method_2_arg(
        interpreter,
        "isGreaterThan",
        true,
        left_side_operand,
        right_side_operand,
        pos,
    );

    if let Some(ret) = ret {
        return conversions::to_bool(interpreter, &ret, pos);
    }

    if ptr::eq(
        left_side_operand.borrow().deref(),
        right_side_operand.borrow().deref(),
    ) {
        return false;
    }

    let number = conversions::to_number(interpreter, right_side_operand, pos).
            map(|number| DataObject::with_update(|data_object| {
                data_object.set_number(number)
            }).unwrap()).unwrap_or_default();

    match left_side_operand.data_value() {
        DataValue::Text(left_value) => {
            if let Some(right_value) = right_side_operand.text_value() {
                return *left_value > *right_value;
            }

            if left_value.chars().count() == 1 {
                if let Some(right_value) = right_side_operand.char_value() {
                    return left_value.chars().next().unwrap() > right_value;
                }
            }

            let this_number = conversions::to_number(interpreter, left_side_operand, pos);
            let Some(this_number) = this_number else {
                return false;
            };

            match number.data_value().clone() {
                DataValue::Int(number) => this_number.int_value() > number,
                DataValue::Long(number) => this_number.long_value() > number,
                DataValue::Float(number) => this_number.float_value() > number,
                DataValue::Double(number) => this_number.double_value() > number,

                _ => false
            }
        },

        DataValue::Char(left_value) => {
            if let Some(right_value) = right_side_operand.text_value() {
                if right_value.chars().count() == 1 {
                    return left_value > right_value.chars().next().unwrap();
                }
            }

            match number.data_value().clone() {
                DataValue::Int(number) => (left_value as i32) > number,
                DataValue::Long(number) => (left_value as i64) > number,
                DataValue::Float(number) => (left_value as i32 as f32) > number,
                DataValue::Double(number) => (left_value as i32 as f64) > number,

                _ => false
            }
        },

        DataValue::Int(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => left_value > number,
                DataValue::Long(number) => (left_value as i64) > number,
                DataValue::Float(number) => (left_value as f32) > number,
                DataValue::Double(number) => (left_value as f64) > number,

                _ => false
            }
        },

        DataValue::Long(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => left_value > number as i64,
                DataValue::Long(number) => left_value > number,
                DataValue::Float(number) => (left_value as f32) > number,
                DataValue::Double(number) => (left_value as f64) > number,

                _ => false
            }
        },

        DataValue::Float(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => left_value > number as f32,
                DataValue::Long(number) => left_value > number as f32,
                DataValue::Float(number) => left_value > number,
                DataValue::Double(number) => (left_value as f64) > number,

                _ => false
            }
        },

        DataValue::Double(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => left_value > number as f64,
                DataValue::Long(number) => left_value > number as f64,
                DataValue::Float(number) => left_value > number as f64,
                DataValue::Double(number) => left_value > number,

                _ => false
            }
        },

        DataValue::ByteBuffer(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => (left_value.borrow().len() as i32) > number,
                DataValue::Long(number) => (left_value.borrow().len() as i32 as i64) > number,
                DataValue::Float(number) => (left_value.borrow().len() as i32 as f32) > number,
                DataValue::Double(number) => (left_value.borrow().len() as i32 as f64) > number,

                _ => false
            }
        },

        DataValue::Array(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => (left_value.borrow().len() as i32) > number,
                DataValue::Long(number) => (left_value.borrow().len() as i32 as i64) > number,
                DataValue::Float(number) => (left_value.borrow().len() as i32 as f32) > number,
                DataValue::Double(number) => (left_value.borrow().len() as i32 as f64) > number,

                _ => false
            }
        },

        DataValue::List(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => (left_value.borrow().len() as i32) > number,
                DataValue::Long(number) => (left_value.borrow().len() as i32 as i64) > number,
                DataValue::Float(number) => (left_value.borrow().len() as i32 as f32) > number,
                DataValue::Double(number) => (left_value.borrow().len() as i32 as f64) > number,

                _ => false
            }
        },

        DataValue::Error(left_value) => {
            if let Some(right_value) = right_side_operand.text_value() {
                return *left_value.err().error_text() > *right_value;
            }

            match number.data_value().clone() {
                DataValue::Int(number) => left_value.err().error_code() > number,
                DataValue::Long(number) => (left_value.err().error_code() as i64) > number,
                DataValue::Float(number) => (left_value.err().error_code() as f32) > number,
                DataValue::Double(number) => (left_value.err().error_code() as f64) > number,

                _ => false
            }
        },

        DataValue::Struct(left_value) => {
            match number.data_value().clone() {
                DataValue::Int(number) => (left_value.member_names().len() as i32) > number,
                DataValue::Long(number) => (left_value.member_names().len() as i32 as i64) > number,
                DataValue::Float(number) => (left_value.member_names().len() as i32 as f32) > number,
                DataValue::Double(number) => (left_value.member_names().len() as i32 as f64) > number,

                _ => false
            }
        },

        _ => false,
    }
}
/**
 * For "&lt;="
 */
pub fn is_less_than_or_equals(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> bool {
    is_less_than(interpreter, left_side_operand, right_side_operand, pos) ||
            is_equals(interpreter, left_side_operand, right_side_operand, pos)
}
/**
 * For "&gt;="
 */
pub fn is_greater_than_or_equals(
    interpreter: &mut Interpreter,
    left_side_operand: &DataObjectRef,
    right_side_operand: &DataObjectRef,
    pos: CodePosition,
) -> bool {
    is_greater_than(interpreter, left_side_operand, right_side_operand, pos) ||
            is_equals(interpreter, left_side_operand, right_side_operand, pos)
}
