use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::rc::Rc;
use gc::Gc;
use crate::interpreter::data::function::{Function, FunctionMetadata, FunctionPointerObject};
use crate::interpreter::{conversions, operators, Interpreter, InterpretingError};
use crate::interpreter::data::{DataObject, DataObjectRef, DataType, OptionDataObjectRef};
use crate::lexer::CodePosition;
use crate::utils;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            list_create_function,
            crate::lang_func_metadata!(
                name="listCreate",
                return_type_constraint(
                    allowed=["LIST"],
                ),
            ),
        ));
    fn list_create_function(
        _: &mut Interpreter,
    ) -> DataObjectRef {
        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_list(VecDeque::new())
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            list_of_function,
            crate::lang_func_metadata!(
                name="listOf",
                return_type_constraint(
                    allowed=["LIST"],
                ),
                parameter(
                    name="&elements",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn list_of_function(
        _: &mut Interpreter,
        elements: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let list = elements.iter().
                map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap())).
                collect::<VecDeque<_>>();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_list(list)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            list_generate_from_function,
            crate::lang_func_metadata!(
                name="listGenerateFrom",
                return_type_constraint(
                    allowed=["LIST"],
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
    fn list_generate_from_function(
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

        let list = (0..count).
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
                collect::<VecDeque<_>>();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_list(list)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            list_zip_function,
            crate::lang_func_metadata!(
                name="listZip",
                return_type_constraint(
                    allowed=["LIST"],
                ),
                parameter(
                    name="&lists",
                    type_constraint(
                        allowed=["LIST"],
                    ),
                   parameter_type(var_args),
                ),
            ),
        ));
    fn list_zip_function(
        interpreter: &mut Interpreter,
        lists: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut len = 0;
        for (i, len_test) in lists.iter().
                map(|array| array.list_value().unwrap().borrow().len()).
                enumerate() {
            if i == 0 {
                len = len_test;

                continue;
            }

            if len != len_test {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The size of argument {} (for var args parameter \"&lists\") must be {}",
                        i + 1,
                        len,
                    )),
                    CodePosition::EMPTY,
                );
            }
        }

        let zipped_list = (0..len).
                map(|i| {
                    let list = lists.iter().
                            map(|array| array.array_value().unwrap()).
                            map(|array| {
                                DataObjectRef::new(DataObject::with_update(|data_object| {
                                    data_object.set_data(&array.borrow()[i].borrow())
                                }).unwrap())
                            }).collect();

                    DataObjectRef::new(DataObject::with_update(|data_object| {
                        data_object.set_list(list)
                    }).unwrap())
                }).collect();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_list(zipped_list)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            list_add_function,
            crate::lang_func_metadata!(
                name="listAdd",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_add_function(
        _: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) {
        let list = list_object.list_value().unwrap();

        let mut list = list.borrow_mut();
        list.push_back(DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_data(&value_object.borrow())
        }).unwrap()));
    }

    functions.push(crate::lang_func!(
            list_shift_function,
            crate::lang_func_metadata!(
                name="listShift",
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
            ),
        ));
    fn list_shift_function(
        _: &mut Interpreter,
        list_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let list = list_object.list_value().unwrap();

        let mut list = list.borrow_mut();

        list.pop_front()
    }

    functions.push(crate::lang_func!(
            list_unshift_function,
            crate::lang_func_metadata!(
                name="listUnshift",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_unshift_function(
        _: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) {
        let list = list_object.list_value().unwrap();

        let mut list = list.borrow_mut();
        list.push_front(DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_data(&value_object.borrow())
        }).unwrap()));
    }

    functions.push(crate::lang_func!(
            list_peek_first_function,
            crate::lang_func_metadata!(
                name="listPeekFirst",
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
            ),
        ));
    fn list_peek_first_function(
        _: &mut Interpreter,
        list_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let list = list_object.list_value().unwrap();

        let list = list.borrow();

        list.front().cloned()
    }

    functions.push(crate::lang_func!(
            list_pop_function,
            crate::lang_func_metadata!(
                name="listPop",
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
            ),
        ));
    fn list_pop_function(
        _: &mut Interpreter,
        list_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let list = list_object.list_value().unwrap();

        let mut list = list.borrow_mut();

        list.pop_back()
    }

    functions.push(crate::lang_func!(
            list_push_function,
            crate::lang_func_metadata!(
                name="listPush",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_push_function(
        _: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) {
        let list = list_object.list_value().unwrap();

        let mut list = list.borrow_mut();
        list.push_back(DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_data(&value_object.borrow())
        }).unwrap()));
    }

    functions.push(crate::lang_func!(
            list_peek_last_function,
            crate::lang_func_metadata!(
                name="listPeekLast",
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
            ),
        ));
    fn list_peek_last_function(
        _: &mut Interpreter,
        list_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let list = list_object.list_value().unwrap();

        let list = list.borrow();

        list.back().cloned()
    }

    functions.push(crate::lang_func!(
            list_remove_function,
            crate::lang_func_metadata!(
                name="listRemove",
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_remove_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let list = list_object.list_value().unwrap();

        let len = list.borrow().len();
        for i in 0..len {
            let data_object = list.borrow()[i].clone();
            if operators::is_strict_equals(interpreter, &data_object, &value_object, CodePosition::EMPTY) {
                list.borrow_mut().remove(i);

                return Some(data_object);
            }
        }

        None
    }

    functions.push(crate::lang_func!(
            list_remove_like_function,
            crate::lang_func_metadata!(
                name="listRemoveLike",
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_remove_like_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let list = list_object.list_value().unwrap();

        let len = list.borrow().len();
        for i in 0..len {
            let data_object = list.borrow()[i].clone();
            if operators::is_equals(interpreter, &data_object, &value_object, CodePosition::EMPTY) {
                list.borrow_mut().remove(i);

                return Some(data_object);
            }
        }

        None
    }

    functions.push(crate::lang_func!(
            list_remove_at_function,
            crate::lang_func_metadata!(
                name="listRemoveAt",
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$index",
                    parameter_type(number),
                ),
            ),
        ));
    fn list_remove_at_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        index_number: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let index_number = index_number.number_value().unwrap();
        let index = index_number.int_value();

        let mut list = list.borrow_mut();

        let index = utils::wrap_index(index, list.len());
        let Some(index) = index else {
            return interpreter.set_errno_error_object_error_only(InterpretingError::IndexOutOfBounds);
        };

        list.remove(index).unwrap()
    }

    functions.push(crate::lang_func!(
            list_get_all_function,
            crate::lang_func_metadata!(
                name="listGetAll",
                return_type_constraint(
                    allowed=["TEXT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
            ),
        ));
    fn list_get_all_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let text = list.borrow().iter().
                map(|ele| conversions::to_text(interpreter, ele, CodePosition::EMPTY)).
                collect::<Vec<_>>().join(", ");

        DataObjectRef::new(DataObject::new_text(text))
    }

    functions.push(crate::lang_func!(
            list_fill_function,
            crate::lang_func_metadata!(
                name="listFill",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_fill_function(
        _: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) {
        let list = list_object.list_value().unwrap();

        let mut list = list.borrow_mut();
        list.iter_mut().for_each(|ele| {
            *ele = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&value_object.borrow())
            }).unwrap());
        });
    }

    functions.push(crate::lang_func!(
            list_fill_from_function,
            crate::lang_func_metadata!(
                name="listFillFrom",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_fill_from_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        start_index_number: DataObjectRef,
        value_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let start_index_number = start_index_number.number_value().unwrap();
        let start_index = start_index_number.int_value();

        let list = list_object.list_value().unwrap();

        let start_index = utils::wrap_index(start_index, list.borrow().len());
        let Some(start_index) = start_index else {
            return Some(interpreter.set_errno_error_object_error_only(InterpretingError::IndexOutOfBounds));
        };

        let mut list = list.borrow_mut();
        list.iter_mut().skip(start_index).for_each(|ele| {
            *ele = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&value_object.borrow())
            }).unwrap());
        });

        None
    }

    functions.push(crate::lang_func!(
            list_fill_to_function,
            crate::lang_func_metadata!(
                name="listFillTo",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_fill_to_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        end_index_number: DataObjectRef,
        value_object: DataObjectRef,
    ) -> OptionDataObjectRef {
        let end_index_number = end_index_number.number_value().unwrap();
        let end_index = end_index_number.int_value();

        let list = list_object.list_value().unwrap();

        let end_index = utils::wrap_index(end_index, list.borrow().len());
        let Some(end_index) = end_index else {
            return Some(interpreter.set_errno_error_object_error_only(InterpretingError::IndexOutOfBounds));
        };

        let mut list = list.borrow_mut();
        list.iter_mut().take(end_index + 1).for_each(|ele| {
            *ele = DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_data(&value_object.borrow())
            }).unwrap());
        });

        None
    }

    functions.push(crate::lang_func!(
            list_count_of_function,
            crate::lang_func_metadata!(
                name="listCountOf",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_count_of_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap().borrow().clone();

        let count = list.iter().
                filter(|ele| operators::is_strict_equals(
                    interpreter, ele, &value_object, CodePosition::EMPTY,
                )).count();

        DataObjectRef::new(DataObject::new_number(count as i32))
    }

    functions.push(crate::lang_func!(
            list_index_of_function,
            crate::lang_func_metadata!(
                name="listIndexOf",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_index_of_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let list = list.borrow();

        let mut index = -1;

        for (i, ele) in list.iter().
                enumerate() {
            if operators::is_strict_equals(interpreter, ele, &value_object, CodePosition::EMPTY) {
                index = i as i32;
                break;
            }
        }

        DataObjectRef::new(DataObject::new_number(index))
    }

    functions.push(crate::lang_func!(
            list_last_index_of_function,
            crate::lang_func_metadata!(
                name="listLastIndexOf",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_last_index_of_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let list = list.borrow();

        let mut index = -1;

        for (i, ele) in list.iter().
                enumerate().rev() {
            if operators::is_strict_equals(interpreter, ele, &value_object, CodePosition::EMPTY) {
                index = i as i32;
                break;
            }
        }

        DataObjectRef::new(DataObject::new_number(index))
    }

    functions.push(crate::lang_func!(
            list_count_like_function,
            crate::lang_func_metadata!(
                name="listCountLike",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_count_like_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap().borrow().clone();

        let count = list.iter().
                filter(|ele| operators::is_equals(
                    interpreter, ele, &value_object, CodePosition::EMPTY,
                )).count();

        DataObjectRef::new(DataObject::new_number(count as i32))
    }

    functions.push(crate::lang_func!(
            list_index_like_function,
            crate::lang_func_metadata!(
                name="listIndexLike",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_index_like_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let list = list.borrow();

        let mut index = -1;

        for (i, ele) in list.iter().
                enumerate() {
            if operators::is_equals(interpreter, ele, &value_object, CodePosition::EMPTY) {
                index = i as i32;
                break;
            }
        }

        DataObjectRef::new(DataObject::new_number(index))
    }

    functions.push(crate::lang_func!(
            list_last_index_like_function,
            crate::lang_func_metadata!(
                name="listLastIndexLike",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
                parameter(
                    name="$value",
                ),
            ),
        ));
    fn list_last_index_like_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        value_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let list = list.borrow();

        let mut index = -1;

        for (i, ele) in list.iter().
                enumerate().rev() {
            if operators::is_equals(interpreter, ele, &value_object, CodePosition::EMPTY) {
                index = i as i32;
                break;
            }
        }

        DataObjectRef::new(DataObject::new_number(index))
    }

    functions.push(crate::lang_func!(
            list_distinct_values_of_function,
            crate::lang_func_metadata!(
                name="listDistinctValuesOf",
                return_type_constraint(
                    allowed=["LIST"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
            ),
        ));
    fn list_distinct_values_of_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let mut distinct_values = VecDeque::new();

        let list = list.borrow();

        for ele in list.iter() {
            let mut flag = true;
            for distinct_ele in distinct_values.iter() {
                if operators::is_strict_equals(interpreter, ele, distinct_ele, CodePosition::EMPTY) {
                    flag = false;
                    break;
                }
            }

            if flag {
                distinct_values.push_back(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap()));
            }
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_list(distinct_values)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            list_distinct_values_like_function,
            crate::lang_func_metadata!(
                name="listDistinctValuesLike",
                return_type_constraint(
                    allowed=["LIST"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
            ),
        ));
    fn list_distinct_values_like_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let mut distinct_values = VecDeque::new();

        let list = list.borrow();

        for ele in list.iter() {
            let mut flag = true;
            for distinct_ele in distinct_values.iter() {
                if operators::is_equals(interpreter, ele, distinct_ele, CodePosition::EMPTY) {
                    flag = false;
                    break;
                }
            }

            if flag {
                distinct_values.push_back(DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap()));
            }
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_list(distinct_values)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            list_sorted_function,
            crate::lang_func_metadata!(
                name="listSorted",
                return_type_constraint(
                    allowed=["LIST"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_sorted_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        comparator_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let mut sorted_list = list.borrow().iter().
                map(|ele| DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_data(&ele.borrow())
                }).unwrap())).
                collect::<Vec<_>>();

        sorted_list.sort_by(|a, b| {
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
            data_object.set_list(VecDeque::from(sorted_list))
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            list_filtered_function,
            crate::lang_func_metadata!(
                name="listFiltered",
                return_type_constraint(
                    allowed=["LIST"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_filtered_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        filter_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let filtered_list = list.borrow().iter().
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
                collect::<VecDeque<_>>();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_list(filtered_list)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            list_filtered_count_function,
            crate::lang_func_metadata!(
                name="listFilteredCount",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_filtered_count_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        filter_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let count = list.borrow().iter().
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
            list_map_function,
            crate::lang_func_metadata!(
                name="listMap",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_map_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        map_object: DataObjectRef,
    ) {
        let list = list_object.list_value().unwrap();

        for ele in list.borrow_mut().iter_mut() {
            *ele = utils::none_to_lang_void(interpreter.call_function_pointer(
                &map_object.function_pointer_value().unwrap(),
                map_object.variable_name().as_deref(),
                &[ele.clone()],
                CodePosition::EMPTY,
            ));
        }
    }

    functions.push(crate::lang_func!(
            list_map_to_new_function,
            crate::lang_func_metadata!(
                name="listMapToNew",
                return_type_constraint(
                    allowed=["ARRAY"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_map_to_new_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        map_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let mut new_list = VecDeque::with_capacity(list.borrow().len());

        for ele in list.borrow_mut().iter() {
            new_list.push_front(utils::none_to_lang_void(interpreter.call_function_pointer(
                &map_object.function_pointer_value().unwrap(),
                map_object.variable_name().as_deref(),
                &[ele.clone()],
                CodePosition::EMPTY,
            )));
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_list(new_list)
        }).unwrap())
    }

    {
        functions.push(crate::lang_func!(
                list_reduce_without_initial_value_function,
                crate::lang_func_metadata!(
                    name="listReduce",
                    has_info=true,
                    parameter(
                        name="&list",
                        type_constraint(
                            allowed=["LIST"]
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
        fn list_reduce_without_initial_value_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            list_reduce_internal_function(interpreter, list_object, None, combine_object)
        }

        functions.push(crate::lang_func!(
                list_reduce_with_initial_value_function,
                crate::lang_func_metadata!(
                    name="listReduce",
                    parameter(
                        name="&list",
                        type_constraint(
                            allowed=["LIST"]
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
        fn list_reduce_with_initial_value_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            initial_value_object: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            list_reduce_internal_function(interpreter, list_object, Some(initial_value_object), combine_object)
        }

        fn list_reduce_internal_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            initial_value_object: OptionDataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            let list = list_object.list_value().unwrap();
            let list = list.borrow();

            let mut current_value_object = initial_value_object;

            for ele in list.iter() {
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
                list_reduce_column_without_initial_value_function,
                crate::lang_func_metadata!(
                    name="listReduceColumn",
                    has_info=true,
                    return_type_constraint(
                        allowed=["LIST"]
                    ),
                    parameter(
                        name="&lists",
                        type_constraint(
                            allowed=["LIST"]
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
        fn list_reduce_column_without_initial_value_function(
            interpreter: &mut Interpreter,
            list_objects: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            list_reduce_internal_function(interpreter, list_objects, None, combine_object)
        }

        functions.push(crate::lang_func!(
                list_reduce_column_with_initial_value_function,
                crate::lang_func_metadata!(
                    name="listReduceColumn",
                    return_type_constraint(
                        allowed=["LIST"]
                    ),
                    parameter(
                        name="&lists",
                        type_constraint(
                            allowed=["LIST"]
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
        fn list_reduce_column_with_initial_value_function(
            interpreter: &mut Interpreter,
            list_objects: DataObjectRef,
            initial_value_object: DataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            list_reduce_internal_function(interpreter, list_objects, Some(initial_value_object), combine_object)
        }

        fn list_reduce_internal_function(
            interpreter: &mut Interpreter,
            list_objects: DataObjectRef,
            initial_value_object: OptionDataObjectRef,
            combine_object: DataObjectRef,
        ) -> DataObjectRef {
            let list_of_lists = list_objects.list_value().unwrap();

            let mut lists = VecDeque::with_capacity(list_of_lists.borrow().len());

            let mut len = 0;
            for (i, arg) in list_of_lists.borrow().iter().
                    enumerate() {
                let Some(list) = arg.list_value() else {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The element at index {i} of argument 1 (\"&lists\") must be of type {}",
                            DataType::LIST,
                        )),
                        CodePosition::EMPTY,
                    );
                };

                let len_test = list.borrow().len();

                lists.push_back(list);

                if i == 0 {
                    len = len_test;

                    continue;
                }

                if len != len_test {
                    return interpreter.set_errno_error_object(
                        InterpretingError::InvalidArguments,
                        Some(&format!(
                            "The length of the array at index {i} of argument 1 (\"&lists\") must be {}",
                            len,
                        )),
                        CodePosition::EMPTY,
                    );
                }
            }

            if lists.is_empty() {
                return DataObjectRef::new(DataObject::with_update(|data_object| {
                    data_object.set_list(VecDeque::new())
                }).unwrap())
            }

            let mut reduced_lists = VecDeque::with_capacity(lists.len());
            for i in 0..len {
                let mut current_value_object = initial_value_object.clone();

                for list in lists.iter() {
                    let ele = list.borrow()[i].clone();

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

                reduced_lists.push_back(utils::none_to_lang_void(current_value_object));
            }

            DataObjectRef::new(DataObject::with_update(|data_object| {
                data_object.set_list(reduced_lists)
            }).unwrap())
        }
    }

    {
        functions.push(crate::lang_func!(
                list_for_each_without_breakable_function,
                crate::lang_func_metadata!(
                    name="listForEach",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&list",
                        type_constraint(
                            allowed=["LIST"]
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
        fn list_for_each_without_breakable_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            function_object: DataObjectRef,
        ) {
            list_for_each_internal_function(interpreter, list_object, function_object, false);
        }

        functions.push(crate::lang_func!(
                list_for_each_with_breakable_function,
                crate::lang_func_metadata!(
                    name="listForEach",
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&list",
                        type_constraint(
                            allowed=["LIST"]
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
        fn list_for_each_with_breakable_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            function_object: DataObjectRef,
            breakable_object: DataObjectRef,
        ) {
            let breakable = conversions::to_bool(interpreter, &breakable_object, CodePosition::EMPTY);

            list_for_each_internal_function(interpreter, list_object, function_object, breakable);
        }

        fn list_for_each_internal_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            function_object: DataObjectRef,
            breakable: bool,
        ) {
            let list = list_object.list_value().unwrap();

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

                for ele in list.borrow().iter() {
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
                for ele in list.borrow().iter() {
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
                list_enumerate_without_breakable_function,
                crate::lang_func_metadata!(
                    name="listEnumerate",
                    has_info=true,
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&list",
                        type_constraint(
                            allowed=["LIST"]
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
        fn list_enumerate_without_breakable_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            function_object: DataObjectRef,
        ) {
            list_enumerate_internal_function(interpreter, list_object, function_object, false);
        }

        functions.push(crate::lang_func!(
                list_enumerate_with_breakable_function,
                crate::lang_func_metadata!(
                    name="listEnumerate",
                    return_type_constraint(
                        allowed=["VOID"]
                    ),
                    parameter(
                        name="&list",
                        type_constraint(
                            allowed=["LIST"]
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
        fn list_enumerate_with_breakable_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            function_object: DataObjectRef,
            breakable_object: DataObjectRef,
        ) {
            let breakable = conversions::to_bool(interpreter, &breakable_object, CodePosition::EMPTY);

            list_enumerate_internal_function(interpreter, list_object, function_object, breakable);
        }

        fn list_enumerate_internal_function(
            interpreter: &mut Interpreter,
            list_object: DataObjectRef,
            function_object: DataObjectRef,
            breakable: bool,
        ) {
            let list = list_object.list_value().unwrap();

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

                for (i, ele) in list.borrow().iter().
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
                for (i, ele) in list.borrow().iter().
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
            list_all_match_function,
            crate::lang_func_metadata!(
                name="listAllMatch",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_all_match_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        predicate_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let ret = list.borrow().iter().all(|ele| {
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
            list_any_match_function,
            crate::lang_func_metadata!(
                name="ListAnyMatch",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_any_match_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        predicate_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let ret = list.borrow().iter().any(|ele| {
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
            list_none_match_function,
            crate::lang_func_metadata!(
                name="listNoneMatch",
                return_type_constraint(
                    allowed=["INT"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
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
    fn list_none_match_function(
        interpreter: &mut Interpreter,
        list_object: DataObjectRef,
        predicate_object: DataObjectRef,
    ) -> DataObjectRef {
        let list = list_object.list_value().unwrap();

        let ret = list.borrow().iter().any(|ele| {
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
            list_combine_function,
            crate::lang_func_metadata!(
                name="listCombine",
                return_type_constraint(
                    allowed=["LIST"],
                ),
                parameter(
                    name="&lists",
                    type_constraint(
                        allowed=["LIST"],
                    ),
                   parameter_type(var_args),
                ),
            ),
        ));
    fn list_combine_function(
        _: &mut Interpreter,
        lists: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let combined_list = lists.iter().
                flat_map(|ele| {
                    let arr = ele.list_value().unwrap().borrow().clone();

                    arr
                }).collect();

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_list(combined_list)
        }).unwrap())
    }

    functions.push(crate::lang_func!(
            list_clear,
            crate::lang_func_metadata!(
                name="list_clear",
                return_type_constraint(
                    allowed=["VOID"],
                ),
                parameter(
                    name="&list",
                    type_constraint(
                        allowed=["LIST"]
                    ),
                ),
            ),
        ));
    fn list_clear(
        _: &mut Interpreter,
        list_object: DataObjectRef,
    ) {
        let list = list_object.list_value().unwrap();

        list.borrow_mut().clear();
    }
}
