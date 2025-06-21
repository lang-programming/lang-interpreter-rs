use std::cell::RefCell;
use std::cmp::Ordering;
use std::rc::Rc;
use gc::Gc;
use crate::interpreter::data::function::{Function, FunctionMetadata, FunctionPointerObject};
use crate::interpreter::{conversions, operators, Interpreter, InterpretingError};
use crate::interpreter::data::{DataObject, DataObjectRef, DataType, OptionDataObjectRef};
use crate::lexer::CodePosition;
use crate::utils;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            array_create_function,
            crate::lang_func_metadata!(
                name="arrayCreate",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="$length",
                    parameter_type(number),
                ),
            ),
        ));
    fn array_create_function(
        interpreter: &mut Interpreter,
        length_number: DataObjectRef,
    ) -> DataObjectRef {
        let length_number = length_number.number_value().unwrap();
        let length = length_number.int_value();
        if length < 0 {
            return interpreter.set_errno_error_object_error_only(InterpretingError::NegativeArrayLen);
        }

        let arr = (0..length).
                map(|_| DataObjectRef::new(DataObject::new())).
                collect::<Box<_>>();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(arr)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_of_function,
            crate::lang_func_metadata!(
                name="arrayOf",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&elements",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn array_of_function(
        _: &mut Interpreter,
        elements: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let arr = elements.iter().
                map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap())).
                collect::<Box<_>>();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(arr)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_generate_from_function,
            crate::lang_func_metadata!(
                name="arrayGenerateFrom",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="fp.func",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"],
                    ),
                ),
                parameter(
                    name="$count",
                    parameter_type(number),
                ),
            ),
        ));
    fn array_generate_from_function(
        interpreter: &mut Interpreter,
        func_pointer_object: DataObjectRef,
        count_number: DataObjectRef,
    ) -> DataObjectRef {
        let count_number = count_number.number_value().unwrap();
        let count = count_number.int_value();
        if count < 0 {
            return interpreter.set_errno_error_object_error_only(InterpretingError::NegativeArrayLen);
        }

        let function_pointer = func_pointer_object.function_pointer_value().unwrap();

        let arr = (0..count).
                map(|i| {
                    utils::none_to_lang_void(interpreter.call_function_pointer(
                        &function_pointer,
                        func_pointer_object.variable_name().as_deref(),
                        &[
                            DataObjectRef::new(DataObject::new_number(i)),
                        ],
                        CodePosition::EMPTY,
                    ))
                }).
                collect::<Box<_>>();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(arr)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_zip_function,
            crate::lang_func_metadata!(
                name="arrayZip",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&arrays",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                   parameter_type(var_args),
                ),
            ),
        ));
    fn array_zip_function(
        interpreter: &mut Interpreter,
        arrays: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut len = 0;
        for (i, len_test) in arrays.iter().
                map(|array| array.array_value().unwrap().borrow().len()).
                enumerate() {
            if i == 0 {
                len = len_test;

                continue;
            }

            if len != len_test {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The size of argument {} (for var args parameter \"&arrays\") must be {}",
                        i + 1,
                        len,
                    )),
                    CodePosition::EMPTY,
                );
            }
        }

        let zipped_array = (0..len).
                map(|i| {
                    let arr = arrays.iter().
                            map(|array| array.array_value().unwrap()).
                            map(|array| {
                                DataObjectRef::new(DataObject::with_update(|data_object| {
                                    data_object.set_data(&array.borrow()[i].borrow())
                                }).unwrap())
                            }).collect();

                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_array(arr)
                    }).unwrap())
                }).collect();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(zipped_array)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_set_all_single_value_function,
            crate::lang_func_metadata!(
                name="arraySetAll",
                has_info=true,
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_set_all_single_value_function(
        _: &mut Interpreter,
        array_object: DataObjectRef,
        value_object: DataObjectRef,
    ) {
        let arr = array_object.array_value().unwrap();

        let mut arr = arr.borrow_mut();
        arr.iter_mut().for_each(|ele| {
            *ele = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&value_object.borrow())
            }).unwrap());
        });
    }

    functions.push(crate::lang_func!(
            array_set_all_var_args_value_function,
            crate::lang_func_metadata!(
                name="arraySetAll",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="&values",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn array_set_all_var_args_value_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        values: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let arr = array_object.array_value().unwrap();

        let mut arr = arr.borrow_mut();

        if values.len() < arr.len() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArgCount,
                Some(&format!(
                    "The var args argument (\"&values\") has not enough values ({} needed)",
                    arr.len(),
                )),
                CodePosition::EMPTY,
            ));
        }
        if values.len() > arr.len() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArgCount,
                Some(&format!(
                    "The var args argument (\"&values\") has too many values ({} needed)",
                    arr.len(),
                )),
                CodePosition::EMPTY,
            ));
        }

        arr.iter_mut().zip(values.iter()).for_each(|(ele, value)| {
            *ele = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&value.borrow())
            }).unwrap());
        });

        None
    }

    functions.push(crate::lang_func!(
            array_get_all_function,
            crate::lang_func_metadata!(
                name="arrayGetAll",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
            ),
        ));
    fn array_get_all_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let text = arr.borrow().iter().
                map(|ele| conversions::to_text(interpreter, ele, CodePosition::EMPTY)).
                collect::<Vec<_>>().join(", ");

        DataObjectRef::new(DataObject::new_text(text))
    }

    functions.push(crate::lang_func!(
            array_read_function,
            crate::lang_func_metadata!(
                name="arrayRead",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="&pointers",
                    parameter_type(var_args),
                    type_constraint(
                        allowed=["VAR_POINTER"]
                    ),
                ),
            ),
        ));
    fn array_read_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        pointers: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let arr = array_object.array_value().unwrap();

        if pointers.len() < arr.borrow().len() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArgCount,
                Some(&format!(
                    "The var args argument (\"&pointers\") has not enough values ({} needed)",
                    arr.borrow().len(),
                )),
                CodePosition::EMPTY,
            ));
        }

        if pointers.len() > arr.borrow().len() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArgCount,
                Some(&format!(
                    "The var args argument (\"&pointers\") has too many values ({} needed)",
                    arr.borrow().len(),
                )),
                CodePosition::EMPTY,
            ));
        }

        let arr = arr.borrow();

        for (i, (ele, pointer)) in arr.iter().
                zip(pointers.iter()).
                enumerate() {
            let dereferenced_pointer = pointer.var_pointer_value().unwrap();
            if dereferenced_pointer.is_final_data() || dereferenced_pointer.is_lang_var() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FinalVarChange,
                    Some(&format!(
                        "For the dereferenced pointer (argument {}) for var args parameter (\"&pointers\")",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                ));
            }


            if !dereferenced_pointer.type_constraint().is_type_allowed(ele.data_type()) {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::FinalVarChange,
                    Some(&format!(
                        "For the dereferenced pointer (argument {}) for var args parameter (\"&pointers\") does not allow the type {}",
                        i + 1,
                        ele.data_type(),
                    )),
                    CodePosition::EMPTY,
                ));
            }

            dereferenced_pointer.borrow_mut().set_data(&ele.borrow()).unwrap();
        }

        None
    }

    functions.push(crate::lang_func!(
            array_fill_function,
            crate::lang_func_metadata!(
                name="arrayFill",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_fill_function(
        _: &mut Interpreter,
        array_object: DataObjectRef,
        value_object: DataObjectRef,
    ) {
        let arr = array_object.array_value().unwrap();

        let mut arr = arr.borrow_mut();
        arr.iter_mut().for_each(|ele| {
            *ele = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&value_object.borrow())
            }).unwrap());
        });
    }

    functions.push(crate::lang_func!(
            array_fill_from_function,
            crate::lang_func_metadata!(
                name="arrayFillFrom",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$startIndex",
                    parameter_type(number),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_fill_from_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        start_index_number: DataObjectRef,
        value_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let start_index_number = start_index_number.number_value().unwrap();
        let start_index = start_index_number.int_value();

        let arr = array_object.array_value().unwrap();

        let start_index = utils::wrap_index(start_index, arr.borrow().len());
        let Some(start_index) = start_index else {
            return Some(interpreter.set_errno_error_object_error_only(InterpretingError::IndexOutOfBounds));
        };

        let mut arr = arr.borrow_mut();
        arr.iter_mut().skip(start_index).for_each(|ele| {
            *ele = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&value_object.borrow())
            }).unwrap());
        });

        None
    }

    functions.push(crate::lang_func!(
            array_fill_to_function,
            crate::lang_func_metadata!(
                name="arrayFillTo",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$endIndex",
                    parameter_type(number),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_fill_to_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        end_index_number: DataObjectRef,
        value_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let end_index_number = end_index_number.number_value().unwrap();
        let end_index = end_index_number.int_value();

        let arr = array_object.array_value().unwrap();

        let end_index = utils::wrap_index(end_index, arr.borrow().len());
        let Some(end_index) = end_index else {
            return Some(interpreter.set_errno_error_object_error_only(InterpretingError::IndexOutOfBounds));
        };

        let mut arr = arr.borrow_mut();
        arr.iter_mut().take(end_index + 1).for_each(|ele| {
            *ele = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&value_object.borrow())
            }).unwrap());
        });

        None
    }

    functions.push(crate::lang_func!(
            array_count_of_function,
            crate::lang_func_metadata!(
                name="arrayCountOf",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_count_of_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let arr = arr.borrow();
        let count = arr.iter().filter(|ele| {
            operators::is_strict_equals(interpreter, ele, &value_object, CodePosition::EMPTY)
        }).count();

        DataObjectRef::new(DataObject::new_number(count as i32))
    }

    functions.push(crate::lang_func!(
            array_index_of_function,
            crate::lang_func_metadata!(
                name="arrayIndexOf",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_index_of_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let arr = arr.borrow();

        let mut index = -1;

        for (i, ele) in arr.iter().
                enumerate() {
            if operators::is_strict_equals(interpreter, ele, &value_object, CodePosition::EMPTY) {
                index = i as i32;
                break;
            }
        }

        DataObjectRef::new(DataObject::new_number(index))
    }

    functions.push(crate::lang_func!(
            array_last_index_of_function,
            crate::lang_func_metadata!(
                name="arrayLastIndexOf",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_last_index_of_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let arr = arr.borrow();

        let mut index = -1;

        for (i, ele) in arr.iter().
                enumerate().rev() {
            if operators::is_strict_equals(interpreter, ele, &value_object, CodePosition::EMPTY) {
                index = i as i32;
                break;
            }
        }

        DataObjectRef::new(DataObject::new_number(index))
    }

    functions.push(crate::lang_func!(
            array_count_like_function,
            crate::lang_func_metadata!(
                name="arrayCountLike",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_count_like_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let arr = arr.borrow();
        let count = arr.iter().filter(|ele| {
            operators::is_equals(interpreter, ele, &value_object, CodePosition::EMPTY)
        }).count();

        DataObjectRef::new(DataObject::new_number(count as i32))
    }

    functions.push(crate::lang_func!(
            array_index_like_function,
            crate::lang_func_metadata!(
                name="arrayIndexLike",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_index_like_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let arr = arr.borrow();

        let mut index = -1;

        for (i, ele) in arr.iter().
                enumerate() {
            if operators::is_equals(interpreter, ele, &value_object, CodePosition::EMPTY) {
                index = i as i32;
                break;
            }
        }

        DataObjectRef::new(DataObject::new_number(index))
    }

    functions.push(crate::lang_func!(
            array_last_index_like_function,
            crate::lang_func_metadata!(
                name="arrayLastIndexLike",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn array_last_index_like_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let arr = arr.borrow();

        let mut index = -1;

        for (i, ele) in arr.iter().
                enumerate().rev() {
            if operators::is_equals(interpreter, ele, &value_object, CodePosition::EMPTY) {
                index = i as i32;
                break;
            }
        }

        DataObjectRef::new(DataObject::new_number(index))
    }

    functions.push(crate::lang_func!(
            array_distinct_values_of_function,
            crate::lang_func_metadata!(
                name="arrayDistinctValuesOf",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
            ),
        ));
    fn array_distinct_values_of_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let mut distinct_values = Vec::new();

        let arr = arr.borrow();

        for ele in arr.iter() {
            let mut flag = true;
            for distinct_ele in distinct_values.iter() {
                if operators::is_strict_equals(interpreter, ele, distinct_ele, CodePosition::EMPTY) {
                    flag = false;
                    break;
                }
            }

            if flag {
                distinct_values.push(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap()));
            }
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(distinct_values.into_boxed_slice())
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_distinct_values_like_function,
            crate::lang_func_metadata!(
                name="arrayDistinctValuesLike",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
            ),
        ));
    fn array_distinct_values_like_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let mut distinct_values = Vec::new();

        let arr = arr.borrow();

        for ele in arr.iter() {
            let mut flag = true;
            for distinct_ele in distinct_values.iter() {
                if operators::is_equals(interpreter, ele, distinct_ele, CodePosition::EMPTY) {
                    flag = false;
                    break;
                }
            }

            if flag {
                distinct_values.push(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap()));
            }
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(distinct_values.into_boxed_slice())
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_sorted_function,
            crate::lang_func_metadata!(
                name="arraySorted",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="fp.comparator",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"]
                    ),
                ),
            ),
        ));
    fn array_sorted_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        comparator_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let mut sorted_arr = arr.borrow().iter().
                map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap())).
                collect::<Box<_>>();

        sorted_arr.sort_by(|a, b| {
            let ret = interpreter.call_function_pointer(
                &comparator_object.function_pointer_value().unwrap(),
                comparator_object.variable_name().as_deref(),
                &utils::separate_arguments_with_argument_separators(
                    &[a.clone(), b.clone()],
                ),
                CodePosition::EMPTY,
            );
            let ret = conversions::to_number(
                interpreter,
                &utils::none_to_lang_void(ret),
                CodePosition::EMPTY,
            );

            let Some(ret) = ret else {
                interpreter.set_errno(
                    InterpretingError::NoNum,
                    Some("The value returned by Argument 2 (\"fp.comparator\") must be a number."),
                    CodePosition::EMPTY,
                );

                return Ordering::Equal;
            };

            let ret = ret.int_value();
            match ret {
                0 => Ordering::Equal,
                1.. => Ordering::Greater,
                ..=-1 => Ordering::Less,
            }
        });

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(sorted_arr)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_filtered_function,
            crate::lang_func_metadata!(
                name="arrayFiltered",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="fp.filter",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"]
                    ),
                ),
            ),
        ));
    fn array_filtered_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        filter_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let filtered_arr = arr.borrow().iter().
                map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap())).
                filter(|ele| {
                    let ret = utils::none_to_lang_void(interpreter.call_function_pointer(
                        &filter_object.function_pointer_value().unwrap(),
                        filter_object.variable_name().as_deref(),
                        &[ele.clone()],
                        CodePosition::EMPTY,
                    ));

                    conversions::to_bool(interpreter, &ret, CodePosition::EMPTY)
                }).
                collect::<Box<_>>();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(filtered_arr)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_filtered_count_function,
            crate::lang_func_metadata!(
                name="arrayFilteredCount",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="fp.filter",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"]
                    ),
                ),
            ),
        ));
    fn array_filtered_count_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        filter_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let count = arr.borrow().iter().
                map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap())).
                filter(|ele| {
                    let ret = utils::none_to_lang_void(interpreter.call_function_pointer(
                        &filter_object.function_pointer_value().unwrap(),
                        filter_object.variable_name().as_deref(),
                        &[ele.clone()],
                        CodePosition::EMPTY,
                    ));

                    conversions::to_bool(interpreter, &ret, CodePosition::EMPTY)
                }).
                count();

        DataObjectRef::new(DataObject::new_number(count as i32))
    }

    functions.push(crate::lang_func!(
            array_map_function,
            crate::lang_func_metadata!(
                name="arrayMap",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="fp.map",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"]
                    ),
                ),
            ),
        ));
    fn array_map_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        map_object: DataObjectRef,
    ) {
        let arr = array_object.array_value().unwrap();

        for ele in arr.borrow_mut().iter_mut() {
            *ele = utils::none_to_lang_void(interpreter.call_function_pointer(
                &map_object.function_pointer_value().unwrap(),
                map_object.variable_name().as_deref(),
                &[ele.clone()],
                CodePosition::EMPTY,
            ));
        }
    }

    functions.push(crate::lang_func!(
            array_map_to_new_function,
            crate::lang_func_metadata!(
                name="arrayMapToNew",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="fp.map",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"]
                    ),
                ),
            ),
        ));
    fn array_map_to_new_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        map_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let mut new_arr = Vec::with_capacity(arr.borrow().len());

        for ele in arr.borrow_mut().iter() {
            new_arr.push(utils::none_to_lang_void(interpreter.call_function_pointer(
                &map_object.function_pointer_value().unwrap(),
                map_object.variable_name().as_deref(),
                &[ele.clone()],
                CodePosition::EMPTY,
            )));
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(new_arr.into_boxed_slice())
        }).unwrap())
    }

    {
        functions.push(crate::lang_func!(
                array_map_to_new_without_initial_value_function,
                crate::lang_func_metadata!(
                    name="arrayMapToOne",
                    info="Alias for \"func.arrayReduce()\"",
                    has_info=true,
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="fp.combine",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                ),
            ));
        fn array_map_to_new_without_initial_value_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            array_reduce_without_initial_value_function(interpreter, array_object, combine_object)
        }

        functions.push(crate::lang_func!(
                array_map_to_new_with_initial_value_function,
                crate::lang_func_metadata!(
                    name="arrayMapToOne",
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="$initialValue",
                    ),
                    parameter(
                        name="fp.combine",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                ),
            ));
        fn array_map_to_new_with_initial_value_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            initial_value_object: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            array_reduce_with_initial_value_function(interpreter, array_object, initial_value_object, combine_object)
        }

        functions.push(crate::lang_func!(
                array_reduce_without_initial_value_function,
                crate::lang_func_metadata!(
                    name="arrayReduce",
                    has_info=true,
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="fp.combine",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                ),
            ));
        fn array_reduce_without_initial_value_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            array_reduce_internal_function(interpreter, array_object, None, combine_object)
        }

        functions.push(crate::lang_func!(
                array_reduce_with_initial_value_function,
                crate::lang_func_metadata!(
                    name="arrayReduce",
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="$initialValue",
                    ),
                    parameter(
                        name="fp.combine",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                ),
            ));
        fn array_reduce_with_initial_value_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            initial_value_object: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            array_reduce_internal_function(interpreter, array_object, Some(initial_value_object), combine_object)
        }

        fn array_reduce_internal_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            initial_value_object: OptionDataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            let arr = array_object.array_value().unwrap();
            let arr = arr.borrow();

            let mut current_value_object = initial_value_object;

            for ele in arr.iter() {
                if let Some(current_value) = current_value_object {
                    current_value_object = Some(utils::none_to_lang_void(interpreter.call_function_pointer(
                        &combine_object.function_pointer_value().unwrap(),
                        combine_object.variable_name().as_deref(),
                        &utils::separate_arguments_with_argument_separators(&[
                            current_value,
                            ele.clone(),
                        ]),
                        CodePosition::EMPTY,
                    )));
                }else {
                    //Set first element as currentValue if no initial value was provided

                    current_value_object = Some(ele.clone());

                    continue;
                }
            }

            utils::none_to_lang_void(current_value_object)
        }
    }

    {
        functions.push(crate::lang_func!(
                array_reduce_column_without_initial_value_function,
                crate::lang_func_metadata!(
                    name="arrayReduceColumn",
                    has_info=true,
                    return_type_constraint(
                        allowed=["ARRAY"]
                    ),
                    parameter(
                        name="&arrays",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="fp.combine",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                ),
            ));
        fn array_reduce_column_without_initial_value_function(
            interpreter: &mut Interpreter,
            array_objects: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            array_reduce_internal_function(interpreter, array_objects, None, combine_object)
        }

        functions.push(crate::lang_func!(
                array_reduce_column_with_initial_value_function,
                crate::lang_func_metadata!(
                    name="arrayReduceColumn",
                    return_type_constraint(
                        allowed=["ARRAY"]
                    ),
                    parameter(
                        name="&arrays",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="$initialValue",
                    ),
                    parameter(
                        name="fp.combine",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                ),
            ));
        fn array_reduce_column_with_initial_value_function(
            interpreter: &mut Interpreter,
            array_objects: DataObjectRef,
            initial_value_object: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            array_reduce_internal_function(interpreter, array_objects, Some(initial_value_object), combine_object)
        }

        fn array_reduce_internal_function(
            interpreter: &mut Interpreter,
            array_objects: DataObjectRef,
            initial_value_object: OptionDataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            let array_of_arrays = array_objects.array_value().unwrap();

            let mut arrays = Vec::with_capacity(array_of_arrays.borrow().len());

            let mut len = 0;
            for (i, arg) in array_of_arrays.borrow().iter().
                    enumerate() {
                let Some(arr) = arg.array_value() else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The element at index {i} of argument 1 (\"&arrays\") must be of type {}",
                            DataType::ARRAY,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                let len_test = arr.borrow().len();

                arrays.push(arr);

                if i == 0 {
                    len = len_test;

                    continue;
                }

                if len != len_test {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The length of the array at index {i} of argument 1 (\"&arrays\") must be {}",
                            len,
                        )),
                        CodePosition::EMPTY,
                    );
                }
            }

            if arrays.is_empty() {
                return DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_array(Box::from([]))
                }).unwrap())
            }

            let mut reduced_arrays = Vec::with_capacity(arrays.len());
            for i in 0..len {
                let mut current_value_object = initial_value_object.clone();

                for arr in arrays.iter() {
                    let ele = arr.borrow()[i].clone();

                    if let Some(current_value) = current_value_object {
                        current_value_object = Some(utils::none_to_lang_void(interpreter.call_function_pointer(
                            &combine_object.function_pointer_value().unwrap(),
                            combine_object.variable_name().as_deref(),
                            &utils::separate_arguments_with_argument_separators(&[
                                current_value,
                                ele.clone(),
                            ]),
                            CodePosition::EMPTY,
                        )));
                    } else {
                        //Set first element as currentValue if no initial value was provided

                        current_value_object = Some(ele.clone());

                        continue;
                    }
                }

                reduced_arrays.push(utils::none_to_lang_void(current_value_object));
            }

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(reduced_arrays.into_boxed_slice())
            }).unwrap())
        }
    }

    {
        functions.push(crate::lang_func!(
                array_for_each_without_breakable_function,
                crate::lang_func_metadata!(
                    name="arrayForEach",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="fp.func",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                ),
            ));
        fn array_for_each_without_breakable_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            function_object: DataObjectRef,
        ) {
            array_for_each_internal_function(interpreter, array_object, function_object, false);
        }

        functions.push(crate::lang_func!(
                array_for_each_with_breakable_function,
                crate::lang_func_metadata!(
                    name="arrayForEach",
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="fp.func",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                    parameter(
                        name="$breakable",
                        parameter_type(boolean),
                    ),
                ),
            ));
        fn array_for_each_with_breakable_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            function_object: DataObjectRef,
            breakable_object: DataObjectRef,
        ) {
            let breakable = conversions::to_bool(interpreter, &breakable_object, CodePosition::EMPTY);

            array_for_each_internal_function(interpreter, array_object, function_object, breakable);
        }

        fn array_for_each_internal_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            function_object: DataObjectRef,
            breakable: bool,
        ) {
            let arr = array_object.array_value().unwrap();

            if breakable {
                let should_break = Rc::new(RefCell::new(false));

                let break_func = {
                    let break_func = {
                        let should_break = should_break.clone();
                        move |_: &mut Interpreter| {
                            *should_break.borrow_mut() = true;
                        }
                    };
                    let func = FunctionPointerObject::from(crate::lang_func!(
                            break_func,
                            crate::lang_func_metadata!(
                                name="break",
                                return_type_constraint(
                                    allowed=["VOID"]
                                ),
                            ),
                        ));

                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_function_pointer(Gc::new(func))
                    }).unwrap())
                };

                for ele in arr.borrow().iter() {
                    interpreter.call_function_pointer(
                        &function_object.function_pointer_value().unwrap(),
                        function_object.variable_name().as_deref(),
                        &utils::separate_arguments_with_argument_separators(&[
                            ele.clone(),
                            break_func.clone(),
                        ]),
                        CodePosition::EMPTY,
                    );

                    if *should_break.borrow() {
                        break;
                    }
                }
            }else {
                for ele in arr.borrow().iter() {
                    interpreter.call_function_pointer(
                        &function_object.function_pointer_value().unwrap(),
                        function_object.variable_name().as_deref(),
                        &[ele.clone()],
                        CodePosition::EMPTY,
                    );
                }
            }
        }
    }

    {
        functions.push(crate::lang_func!(
                array_enumerate_without_breakable_function,
                crate::lang_func_metadata!(
                    name="arrayEnumerate",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="fp.func",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                ),
            ));
        fn array_enumerate_without_breakable_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            function_object: DataObjectRef,
        ) {
            array_enumerate_internal_function(interpreter, array_object, function_object, false);
        }

        functions.push(crate::lang_func!(
                array_enumerate_with_breakable_function,
                crate::lang_func_metadata!(
                    name="arrayEnumerate",
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="fp.func",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                    parameter(
                        name="$breakable",
                        parameter_type(boolean),
                    ),
                ),
            ));
        fn array_enumerate_with_breakable_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            function_object: DataObjectRef,
            breakable_object: DataObjectRef,
        ) {
            let breakable = conversions::to_bool(interpreter, &breakable_object, CodePosition::EMPTY);

            array_enumerate_internal_function(interpreter, array_object, function_object, breakable);
        }

        fn array_enumerate_internal_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            function_object: DataObjectRef,
            breakable: bool,
        ) {
            let arr = array_object.array_value().unwrap();

            if breakable {
                let should_break = Rc::new(RefCell::new(false));

                let break_func = {
                    let break_func = {
                        let should_break = should_break.clone();
                        move |_: &mut Interpreter| {
                            *should_break.borrow_mut() = true;
                        }
                    };
                    let func = FunctionPointerObject::from(crate::lang_func!(
                            break_func,
                            crate::lang_func_metadata!(
                                name="break",
                                return_type_constraint(
                                    allowed=["VOID"]
                                ),
                            ),
                        ));

                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_function_pointer(Gc::new(func))
                    }).unwrap())
                };

                for (i, ele) in arr.borrow().iter().
                        enumerate() {
                    interpreter.call_function_pointer(
                        &function_object.function_pointer_value().unwrap(),
                        function_object.variable_name().as_deref(),
                        &utils::separate_arguments_with_argument_separators(&[
                            DataObjectRef::new(DataObject::new_number(i as i32)),
                            ele.clone(),
                            break_func.clone(),
                        ]),
                        CodePosition::EMPTY,
                    );

                    if *should_break.borrow() {
                        break;
                    }
                }
            }else {
                for (i, ele) in arr.borrow().iter().
                        enumerate() {
                    interpreter.call_function_pointer(
                        &function_object.function_pointer_value().unwrap(),
                        function_object.variable_name().as_deref(),
                        &utils::separate_arguments_with_argument_separators(&[
                            DataObjectRef::new(DataObject::new_number(i as i32)),
                            ele.clone(),
                        ]),
                        CodePosition::EMPTY,
                    );
                }
            }
        }
    }

    functions.push(crate::lang_func!(
            array_all_match_function,
            crate::lang_func_metadata!(
                name="arrayAllMatch",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="fp.predicate",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"]
                    ),
                ),
            ),
        ));
    fn array_all_match_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        predicate_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let ret = arr.borrow().iter().all(|ele| {
            let ret =  utils::none_to_lang_void(interpreter.call_function_pointer(
                &predicate_object.function_pointer_value().unwrap(),
                predicate_object.variable_name().as_deref(),
                &[ele.clone()],
                CodePosition::EMPTY,
            ));

            conversions::to_bool(interpreter, &ret, CodePosition::EMPTY)
        });

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(ret)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_any_match_function,
            crate::lang_func_metadata!(
                name="arrayAnyMatch",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="fp.predicate",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"]
                    ),
                ),
            ),
        ));
    fn array_any_match_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        predicate_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let ret = arr.borrow().iter().any(|ele| {
            let ret =  utils::none_to_lang_void(interpreter.call_function_pointer(
                &predicate_object.function_pointer_value().unwrap(),
                predicate_object.variable_name().as_deref(),
                &[ele.clone()],
                CodePosition::EMPTY,
            ));

            conversions::to_bool(interpreter, &ret, CodePosition::EMPTY)
        });

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(ret)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_none_match_function,
            crate::lang_func_metadata!(
                name="arrayNoneMatch",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
                parameter(
                    name="fp.predicate",
                    type_constraint(
                        allowed=["FUNCTION_POINTER"]
                    ),
                ),
            ),
        ));
    fn array_none_match_function(
        interpreter: &mut Interpreter,
        array_object: DataObjectRef,
        predicate_object: DataObjectRef,
    ) -> DataObjectRef {
        let arr = array_object.array_value().unwrap();

        let ret = arr.borrow().iter().any(|ele| {
            let ret =  utils::none_to_lang_void(interpreter.call_function_pointer(
                &predicate_object.function_pointer_value().unwrap(),
                predicate_object.variable_name().as_deref(),
                &[ele.clone()],
                CodePosition::EMPTY,
            ));

            conversions::to_bool(interpreter, &ret, CodePosition::EMPTY)
        });

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_bool(!ret) //Bool must be negated for none (!any => none)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            array_combine_function,
            crate::lang_func_metadata!(
                name="arrayCombine",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&arrays",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                   parameter_type(var_args),
                ),
            ),
        ));
    fn array_combine_function(
        _: &mut Interpreter,
        arrays: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let combined_array = arrays.iter().
                flat_map(|ele| {
                    let arr = ele.array_value().unwrap().borrow().clone();

                    arr
                }).collect();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_array(combined_array)
        }).unwrap())
    }

    {
        functions.push(crate::lang_func!(
                array_permutations_without_r_function,
                crate::lang_func_metadata!(
                    name="arrayPermutations",
                    has_info=true,
                    return_type_constraint(
                        allowed=["ARRAY"]
                    ),
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                ),
            ));
        fn array_permutations_without_r_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
        ) -> DataObjectRef {
            let count = array_object.array_value().unwrap().borrow().len() as i32;

            array_permutations_internal_function(interpreter, array_object, count)
        }

        functions.push(crate::lang_func!(
                array_permutations_with_r_function,
                crate::lang_func_metadata!(
                    name="arrayPermutations",
                    return_type_constraint(
                        allowed=["ARRAY"]
                    ),
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="$r",
                        info="The amount of selected items per permutation",
                        parameter_type(number),
                    ),
                ),
            ));
        fn array_permutations_with_r_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            count_number: DataObjectRef,
        ) -> DataObjectRef {
            let count = count_number.number_value().unwrap().int_value();

            array_permutations_internal_function(interpreter, array_object, count)
        }

        fn array_permutations_internal_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            count: i32,
        ) -> DataObjectRef {
            let arr = array_object.array_value().unwrap();

            let arr = arr.borrow();

            if count < 0 {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 2 (\"$count\") must be >= 0!"),
                    CodePosition::EMPTY,
                );
            }

            if count as usize > arr.len() {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "Argument 2 (\"$count\") must be <= {}!",
                        arr.len(),
                    )),
                    CodePosition::EMPTY,
                );
            }

            if arr.len() == 0 || count == 0 {
                return DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_array(Box::from([]))
                }).unwrap());
            }

            let mut permutations = Vec::new();
            let mut indices = Vec::with_capacity(count as usize);
            for i in 0..count {
                indices.push(i as isize);
            }

            let mut current_permutation_index = count as usize - 1;

            'outer:
            loop {
                let mut permutation_arr = Vec::with_capacity(count as usize);
                for i in 0..count {
                    permutation_arr.push(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_data(&arr[indices[i as usize] as usize].borrow())
                    }).unwrap()));
                }
                permutations.push(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_array(permutation_arr.into_boxed_slice())
                }).unwrap()));

                let mut used_indices = Vec::new();
                for index in indices.iter().take(current_permutation_index).copied() {
                    used_indices.push(index);
                }

                while current_permutation_index < count as usize {
                    let mut index = indices[current_permutation_index] + 1;
                    while used_indices.contains(&index) {
                        index += 1;
                    }

                    if index as usize == arr.len() {
                        if !used_indices.is_empty() {
                            used_indices.remove(used_indices.len() - 1);
                        }

                        indices[current_permutation_index] = -1;
                        if current_permutation_index < 1 {
                            break 'outer;
                        }

                        current_permutation_index -= 1;

                        continue;
                    }

                    indices[current_permutation_index] = index;

                    used_indices.push(index);

                    current_permutation_index += 1;
                }
                current_permutation_index = count as usize - 1;
            }

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_array(permutations.into_boxed_slice())
            }).unwrap())
        }
    }

    {
        functions.push(crate::lang_func!(
                array_permutations_for_each_without_r_function,
                crate::lang_func_metadata!(
                    name="arrayPermutationsForEach",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="fp.func",
                        info="If the value returned by fp.func evaluates to true, this function will stop the execution early.",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                ),
            ));
        fn array_permutations_for_each_without_r_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            function_object: DataObjectRef,
        ) -> OptionDataObjectRef {
            let count = array_object.array_value().unwrap().borrow().len() as i32;

            array_permutations_for_each_internal_function(interpreter, array_object, function_object, count)
        }

        functions.push(crate::lang_func!(
                array_permutations_for_each_with_r_function,
                crate::lang_func_metadata!(
                    name="arrayPermutationsForEach",
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&array",
                        type_constraint(
                            allowed=["ARRAY"]
                        ),
                    ),
                    parameter(
                        name="fp.func",
                        info="If the value returned by fp.func evaluates to true, this function will stop the execution early.",
                        type_constraint(
                            allowed=["FUNCTION_POINTER"]
                        ),
                    ),
                    parameter(
                        name="$r",
                        info="The amount of selected items per permutation",
                        parameter_type(number),
                    ),
                ),
            ));
        fn array_permutations_for_each_with_r_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            function_object: DataObjectRef,
            count_number: DataObjectRef,
        ) -> OptionDataObjectRef {
            let count = count_number.number_value().unwrap().int_value();

            array_permutations_for_each_internal_function(interpreter, array_object, function_object, count)
        }

        fn array_permutations_for_each_internal_function(
            interpreter: &mut Interpreter,
            array_object: DataObjectRef,
            function_object: DataObjectRef,
            count: i32,
        ) -> OptionDataObjectRef {
            let arr = array_object.array_value().unwrap();

            let arr = arr.borrow();

            if count < 0 {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some("Argument 2 (\"$count\") must be >= 0!"),
                    CodePosition::EMPTY,
                ));
            }

            if count as usize > arr.len() {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "Argument 2 (\"$count\") must be <= {}!",
                        arr.len(),
                    )),
                    CodePosition::EMPTY,
                ));
            }

            if arr.len() == 0 || count == 0 {
                return Some(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_array(Box::from([]))
                }).unwrap()));
            }

            let mut indices = Vec::with_capacity(count as usize);
            for i in 0..count {
                indices.push(i as isize);
            }

            let mut current_permutation_index = count as usize - 1;

            let mut permutation_number = 0;

            'outer:
            loop {
                let mut permutation_arr = Vec::with_capacity(count as usize);
                for i in 0..count {
                    permutation_arr.push(DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_data(&arr[indices[i as usize] as usize].borrow())
                    }).unwrap()));
                }

                let ret = utils::none_to_lang_void(interpreter.call_function_pointer(
                    &function_object.function_pointer_value().unwrap(),
                    function_object.variable_name().as_deref(),
                    &utils::separate_arguments_with_argument_separators(&[
                        DataObjectRef::new(DataObject::with_update(|data_object| {
                            data_object.set_array(permutation_arr.into_boxed_slice())
                        }).unwrap()),
                        DataObjectRef::new(DataObject::new_number(permutation_number)),
                    ]),
                    CodePosition::EMPTY,
                ));
                if conversions::to_bool(interpreter, &ret, CodePosition::EMPTY) {
                    return None;
                }
                permutation_number += 1;

                let mut used_indices = Vec::new();
                for index in indices.iter().take(current_permutation_index).copied() {
                    used_indices.push(index);
                }

                while current_permutation_index < count as usize {
                    let mut index = indices[current_permutation_index] + 1;
                    while used_indices.contains(&index) {
                        index += 1;
                    }

                    if index as usize == arr.len() {
                        if !used_indices.is_empty() {
                            used_indices.remove(used_indices.len() - 1);
                        }

                        indices[current_permutation_index] = -1;
                        if current_permutation_index < 1 {
                            break 'outer;
                        }

                        current_permutation_index -= 1;

                        continue;
                    }

                    indices[current_permutation_index] = index;

                    used_indices.push(index);

                    current_permutation_index += 1;
                }
                current_permutation_index = count as usize - 1;
            }

            None
        }
    }

    functions.push(crate::lang_func!(
            array_reset_function,
            crate::lang_func_metadata!(
                name="arrayReset",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&array",
                    type_constraint(
                        allowed=["ARRAY"]
                    ),
                ),
            ),
        ));
    fn array_reset_function(
        _: &mut Interpreter,
        array_object: DataObjectRef,
    ) {
        let arr = array_object.array_value().unwrap();

        let mut arr = arr.borrow_mut();
        arr.iter_mut().for_each(|ele| {
            *ele = DataObjectRef::new(DataObject::new());
        });
    }
}
