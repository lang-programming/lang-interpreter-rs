use std::collections::VecDeque;
use std::ops::Deref;
use std::str::FromStr;
use gc::{Gc, GcCell};
use crate::interpreter::data::{DataObject, DataObjectRef, DataValue, Number, OptionDataObjectRef, StructObject};
use crate::interpreter::Interpreter;
use crate::lexer::CodePosition;

const MAX_TO_TEXT_RECURSION_DEPTH: usize = 10;

fn call_conversion_method(
    interpreter: &mut Interpreter,
    conversion_name: &str,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> OptionDataObjectRef {
    let method_name = &*("to:".to_string() + conversion_name);

    let operand = operand.object_value()?;

    if operand.borrow().is_class() {
        return None;
    }

    let method = operand.borrow().methods().get(method_name)?.clone();

    let ret = interpreter.call_function_pointer(&method, Some(method_name), &[], pos);

    Some(ret.unwrap_or_else(|| DataObjectRef::new(DataObject::new_void())))
}

//DataType conversion methods
pub fn convert_byte_buffer_to_text(
    operand: Gc<GcCell<Box<[u8]>>>,
) -> String {
    let mut builder = String::new();

    if operand.borrow().is_empty() {
        builder += "<Empty ByteBuffer>";
    }else {
        const HEX_DIGITS: &str = "0123456789ABCDEF";

        builder += "0x";
        for b in operand.borrow().iter() {
            builder.push(HEX_DIGITS.as_bytes()[((b.wrapping_shr(4)) & 0xF) as usize] as char);
            builder.push(HEX_DIGITS.as_bytes()[(b & 0xF) as usize] as char);
        }
    }

    builder
}

fn convert_to_text_max_recursion(ele: &DataObjectRef) -> String {
    if let Some(array_value) = ele.array_value() {
        format!("<Array[{}]>", array_value.borrow().len())
    }else if let Some(list_value) = ele.list_value() {
        format!("<List[{}]>", list_value.borrow().len())
    }else if let Some(struct_value) = ele.struct_value() {
        if struct_value.is_definition() {
            "<Struct[Definition]>".to_string()
        }else {
            "<Struct[Instance]>".to_string()
        }
    }else if let Some(object_value) = ele.object_value() {
        if object_value.borrow().is_class() {
            "<Class>".to_string()
        }else {
            "<Object>".to_string()
        }
    }else {
        "...".to_string()
    }
}

fn convert_array_to_text(
    interpreter: &mut Interpreter,
    operand: Gc<GcCell<Box<[DataObjectRef]>>>,
    recursion_step: usize,
    pos: CodePosition,
) -> String {
    let mut builder = String::new();
    builder += "[";

    let is_empty = operand.borrow().is_empty();
    if !is_empty {
        let ele_copy = operand.borrow().clone();
        for ele in ele_copy {
            builder += &to_text_internal(interpreter, &ele, recursion_step - 1, pos);
            builder += ", ";
        }

        builder = builder[..builder.len() - 2].to_string();
    }

    builder += "]";

    builder
}

fn convert_list_to_text(
    interpreter: &mut Interpreter,
    operand: Gc<GcCell<VecDeque<DataObjectRef>>>,
    recursion_step: usize,
    pos: CodePosition,
) -> String {
    let mut builder = String::new();
    builder += "[";

    let is_empty = operand.borrow().is_empty();
    if !is_empty {
        let ele_copy = operand.borrow().clone();
        for ele in ele_copy {
            builder += &to_text_internal(interpreter, &ele, recursion_step - 1, pos);
            builder += ", ";
        }

        builder = builder[..builder.len() - 2].to_string();
    }

    builder += "]";

    builder
}

fn convert_struct_to_text(
    interpreter: &mut Interpreter,
    operand: Gc<StructObject>,
    recursion_step: usize,
    pos: CodePosition,
) -> String {
    let mut builder = String::new();
    builder += "{";

    let member_names = operand.member_names();
    if !member_names.is_empty() {
        if operand.is_definition() {
            for member_name in member_names {
                builder += &member_name;
                builder += ", ";
            }
        }else {
            for member_name in member_names {
                builder += &member_name;
                builder += ": ";
                let member = operand.get_member(&member_name).unwrap();
                builder += &to_text_internal(interpreter, &member, recursion_step - 1, pos);
                builder += ", ";
            }
        }

        builder = builder[..builder.len() - 2].to_string();
    }

    builder += "}";

    builder
}

pub fn to_text_internal(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    recursion_step: usize,
    pos: CodePosition,
) -> String {
    let ret = call_conversion_method(interpreter, "text", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    if recursion_step == 0 {
        return convert_to_text_max_recursion(&operand);
    }

    match &operand.data_value() {
        DataValue::Text(value) |
        DataValue::ArgumentSeparator(value) => {
            value.to_string()
        },

        DataValue::ByteBuffer(value) => {
            convert_byte_buffer_to_text(value.clone())
        },

        DataValue::Array(value) => {
            convert_array_to_text(interpreter, value.clone(), recursion_step, pos)
        },

        DataValue::List(value) => {
            convert_list_to_text(interpreter, value.clone(), recursion_step, pos)
        },

        DataValue::VarPointer(value) => {
            format!(
                "-->{{{}}}", to_text_internal(interpreter, value, recursion_step - 1, pos),
            )
        },

        DataValue::FunctionPointer(value) => {
            if let Some(variable_name) = operand.variable_name() {
                variable_name.to_string()
            }else {
                value.to_string()
            }
        },

        DataValue::Struct(value) => {
            convert_struct_to_text(interpreter, value.clone(), recursion_step, pos)
        },

        DataValue::Object(value) => {
            value.borrow().to_string()
        },

        DataValue::Void => {
            String::new()
        },

        DataValue::Null => {
            "null".to_string()
        },

        DataValue::Int(value) => {
            value.to_string()
        },

        DataValue::Long(value) => {
            value.to_string()
        },

        DataValue::Float(value) => {
            if value.is_nan() {
                "NaN".to_string()
            }else if value.is_infinite() {
                (if *value == f32::NEG_INFINITY { "-" } else { "" }).to_string() + "Infinity"
            }else {
                let ret = value.to_string();
                if ret.contains(".") {
                    ret
                } else {
                    format!("{ret}.0")
                }
            }
        },

        DataValue::Double(value) => {
            if value.is_nan() {
                "NaN".to_string()
            }else if value.is_infinite() {
                (if *value == f64::NEG_INFINITY { "-" } else { "" }).to_string() + "Infinity"
            }else {
                let ret = value.to_string();
                if ret.contains(".") {
                    ret
                } else {
                    format!("{ret}.0")
                }
            }
        },

        DataValue::Char(value) => {
            value.to_string()
        },

        DataValue::Error(value) => {
            value.to_string()
        },

        DataValue::Type(value) => {
            format!("{value:?}")
        },
    }
}
pub fn to_text(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Box<str> {
    Box::from(to_text_internal(interpreter, operand, MAX_TO_TEXT_RECURSION_DEPTH, pos))
}
pub fn to_char(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Option<char> {
    let ret = call_conversion_method(interpreter, "char", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match operand.data_value() {
        DataValue::Int(value) => {
            let code_point = value as u32;

            Some(char::from_u32(code_point).unwrap_or('\u{FFFD}'))
        },

        DataValue::Long(value) => {
            let code_point = value as u32;

            Some(char::from_u32(code_point).unwrap_or('\u{FFFD}'))
        },

        DataValue::Float(value) => {
            let code_point = value as u32;

            Some(char::from_u32(code_point).unwrap_or('\u{FFFD}'))
        },

        DataValue::Double(value) => {
            let code_point = value as u32;

            Some(char::from_u32(code_point).unwrap_or('\u{FFFD}'))
        },

        DataValue::Char(value) => {
            Some(value)
        },

        _ => None,
    }
}
pub fn to_int(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Option<i32> {
    let ret = call_conversion_method(interpreter, "int", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match &operand.data_value() {
        DataValue::Text(value) => {
            if value.trim().len() == value.len() {
                i32::from_str(value).ok()
            }else {
                None
            }
        },

        DataValue::Char(value) => {
            Some(*value as i32)
        },

        DataValue::Int(value) => {
            Some(*value)
        },

        DataValue::Long(value) => {
            Some(*value as i32)
        },

        DataValue::Float(value) => {
            Some(*value as i32)
        },

        DataValue::Double(value) => {
            Some(*value as i32)
        },

        DataValue::Error(value) => {
            Some(value.err().error_code())
        },

        DataValue::ByteBuffer(value) => {
            Some(value.borrow().len() as i32)
        },

        DataValue::Array(value) => {
            Some(value.borrow().len() as i32)
        },

        DataValue::List(value) => {
            Some(value.borrow().len() as i32)
        },

        DataValue::Struct(value) => {
            Some(value.member_names().len() as i32)
        },

        _ => None,
    }
}
pub fn to_long(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Option<i64> {
    let ret = call_conversion_method(interpreter, "long", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match &operand.data_value() {
        DataValue::Text(value) => {
            if value.trim().len() == value.len() {
                i64::from_str(value).ok()
            }else {
                None
            }
        },

        DataValue::Char(value) => {
            Some(*value as i64)
        },

        DataValue::Int(value) => {
            Some(*value as i64)
        },

        DataValue::Long(value) => {
            Some(*value)
        },

        DataValue::Float(value) => {
            Some(*value as i64)
        },

        DataValue::Double(value) => {
            Some(*value as i64)
        },

        DataValue::Error(value) => {
            Some(value.err().error_code() as i64)
        },

        DataValue::ByteBuffer(value) => {
            Some(value.borrow().len() as i64)
        },

        DataValue::Array(value) => {
            Some(value.borrow().len() as i64)
        },

        DataValue::List(value) => {
            Some(value.borrow().len() as i64)
        },

        DataValue::Struct(value) => {
            Some(value.member_names().len() as i64)
        },

        _ => None,
    }
}
pub fn to_float(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Option<f32> {
    let ret = call_conversion_method(interpreter, "float", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match &operand.data_value() {
        DataValue::Text(value) => {
            if value.trim().len() == value.len() {
                //Do not allow: NaN, Infinity, xX
                if value.contains("N") || value.contains("n") || value.contains("I") ||
                        value.contains("i") || value.contains("x") || value.contains("X") {
                    return None;
                }

                f32::from_str(value).ok()
            }else {
                None
            }
        },

        DataValue::Char(value) => {
            Some(*value as i32 as f32)
        },

        DataValue::Int(value) => {
            Some(*value as f32)
        },

        DataValue::Long(value) => {
            Some(*value as f32)
        },

        DataValue::Float(value) => {
            Some(*value)
        },

        DataValue::Double(value) => {
            Some(*value as f32)
        },

        DataValue::Error(value) => {
            Some(value.err().error_code() as f32)
        },

        DataValue::ByteBuffer(value) => {
            Some(value.borrow().len() as f32)
        },

        DataValue::Array(value) => {
            Some(value.borrow().len() as f32)
        },

        DataValue::List(value) => {
            Some(value.borrow().len() as f32)
        },

        DataValue::Struct(value) => {
            Some(value.member_names().len() as f32)
        },

        _ => None,
    }
}
pub fn to_double(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Option<f64> {
    let ret = call_conversion_method(interpreter, "double", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match &operand.data_value() {
        DataValue::Text(value) => {
            if value.trim().len() == value.len() {
                //Do not allow: NaN, Infinity, xX
                if value.contains("N") || value.contains("n") || value.contains("I") ||
                        value.contains("i") || value.contains("x") || value.contains("X") {
                    return None;
                }

                f64::from_str(value).ok()
            }else {
                None
            }
        },

        DataValue::Char(value) => {
            Some(*value as i32 as f64)
        },

        DataValue::Int(value) => {
            Some(*value as f64)
        },

        DataValue::Long(value) => {
            Some(*value as f64)
        },

        DataValue::Float(value) => {
            Some(*value as f64)
        },

        DataValue::Double(value) => {
            Some(*value)
        },

        DataValue::Error(value) => {
            Some(value.err().error_code() as f64)
        },

        DataValue::ByteBuffer(value) => {
            Some(value.borrow().len() as f64)
        },

        DataValue::Array(value) => {
            Some(value.borrow().len() as f64)
        },

        DataValue::List(value) => {
            Some(value.borrow().len() as f64)
        },

        DataValue::Struct(value) => {
            Some(value.member_names().len() as f64)
        },

        _ => None,
    }
}
pub fn to_byte_buffer(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Option<Box<[u8]>> {
    let ret = call_conversion_method(interpreter, "byte_buffer", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match &operand.data_value() {
        DataValue::ByteBuffer(value) => {
            Some(value.borrow().clone())
        },

        _ => None,
    }
}
pub fn to_array(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Option<Box<[DataObjectRef]>> {
    let ret = call_conversion_method(interpreter, "array", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match &operand.data_value() {
        DataValue::Array(value) => {
            Some(value.borrow().clone())
        },

        DataValue::List(value) => {
            Some(value.borrow().clone().into_iter().collect())
        },

        DataValue::Struct(value) => {
            let member_names = value.member_names();
            let members = member_names.into_iter().
                    map(|member_name| value.get_member(&member_name)).
                    collect::<Result<_, _>>();

            members.ok()
        },

        _ => None,
    }
}
pub fn to_list(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Option<VecDeque<DataObjectRef>> {
    let ret = call_conversion_method(interpreter, "list", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match &operand.data_value() {
        DataValue::Array(value) => {
            Some(VecDeque::from(Vec::from(value.borrow().deref().clone())))
        },

        DataValue::List(value) => {
            Some(value.borrow().clone())
        },

        DataValue::Struct(value) => {
            let member_names = value.member_names();
            let members = member_names.into_iter().
                    map(|member_name| value.get_member(&member_name)).
                    collect::<Result<Vec<_>, _>>();

            members.ok().map(VecDeque::from)
        },

        _ => None,
    }
}

//Special conversion methods
pub fn to_bool(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> bool {
    let ret = call_conversion_method(interpreter, "bool", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match &operand.data_value() {
        DataValue::Text(value) => {
            !value.is_empty()
        },

        DataValue::Char(value) => {
            *value != '\0'
        },

        DataValue::Int(value) => {
            *value != 0
        },

        DataValue::Long(value) => {
            *value != 0
        },

        DataValue::Float(value) => {
            *value != 0.0
        },

        DataValue::Double(value) => {
            *value != 0.0
        },

        DataValue::ByteBuffer(value) => {
            !value.borrow().is_empty()
        },

        DataValue::Array(value) => {
            !value.borrow().is_empty()
        },

        DataValue::List(value) => {
            !value.borrow().is_empty()
        },

        DataValue::Struct(value) => {
            !value.member_names().is_empty()
        },

        DataValue::Error(value) => {
            value.err().error_code() != 0
        },

        DataValue::VarPointer(_) |
        DataValue::FunctionPointer(_) |
        DataValue::Type(_) |
        DataValue::Object(_) => {
            true
        },

        DataValue::Null |
        DataValue::Void |
        DataValue::ArgumentSeparator(_) => {
            false
        },
    }
}

pub fn to_number(
    interpreter: &mut Interpreter,
    operand: &DataObjectRef,
    pos: CodePosition,
) -> Option<Number> {
    let ret = call_conversion_method(interpreter, "number", operand, pos);
    let operand = match ret {
        Some(ret) => ret,

        None => operand.clone(),
    };

    match &operand.data_value() {
        DataValue::Text(value) => {
            if value.trim().len() == value.len() {
                //Do not allow: NaN, Infinity, xX
                if value.contains("N") || value.contains("n") || value.contains("I") ||
                        value.contains("i") || value.contains("x") || value.contains("X") {
                    return None;
                }

                //INT
                let ret = i32::from_str(value).ok();
                if let Some(ret) = ret {
                    return Some(ret.into())
                }

                //LONG
                let ret = if value.ends_with("l") || value.ends_with("L") {
                    i64::from_str(&value[..value.len() - 1]).ok()
                }else {
                    i64::from_str(value).ok()
                };
                if let Some(ret) = ret {
                    return Some(ret.into())
                }

                //FLOAT
                if value.ends_with("f") || value.ends_with("F") {
                    let ret = f32::from_str(&value[..value.len() - 1]).ok();
                    if let Some(ret) = ret {
                        return Some(ret.into())
                    }
                }

                //DOUBLE
                f64::from_str(value).ok().map(Number::from)
            }else {
                None
            }
        },

        DataValue::Char(value) => {
            Some((*value as i32).into())
        },

        DataValue::Int(value) => {
            Some((*value).into())
        },

        DataValue::Long(value) => {
            Some((*value).into())
        },

        DataValue::Float(value) => {
            Some((*value).into())
        },

        DataValue::Double(value) => {
            Some((*value).into())
        },

        DataValue::Error(value) => {
            Some(value.err().error_code().into())
        },

        DataValue::ByteBuffer(value) => {
            Some((value.borrow().len() as i32).into())
        },

        DataValue::Array(value) => {
            Some((value.borrow().len() as i32).into())
        },

        DataValue::List(value) => {
            Some((value.borrow().len() as i32).into())
        },

        DataValue::Struct(value) => {
            Some((value.member_names().len() as i32).into())
        },

        _ => None,
    }
}
