pub mod native;

use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Formatter, Write as _};
use std::ops::Deref;
use std::ptr;
use gc::{Finalize, Gc, Trace};
use crate::interpreter::{conversions, data, operators, Interpreter, InterpretingError};
use crate::interpreter::data::{DataObject, DataObjectRef, DataType, DataTypeConstraint, DataTypeConstraintError, LangObjectRef, OptionDataObjectRef, OptionLangObjectRef, Visibility};
use crate::interpreter::data::function::native::NativeFunction;
use crate::lexer::CodePosition;
use crate::parser::ast::AST;
use crate::utils;

#[derive(Debug)]
pub struct NormalFunction {
    argument_pos_list: Vec<CodePosition>,
    function_body: AST,

    /**
     * If langPath is set, the Lang path from the stack frame element which is created for the function call will be overridden
     */
    lang_path: Option<Box<str>>,

    /**
     * If langFile or langPath is set, the Lang file from the stack frame element which is created for the function call will be overridden<br>
     * This behavior allows for keeping the "&lt;shell&gt;" special case - when the Lang file is None - if a function within a stack frame element where the Lang file is null is
     * called from within a stack frame element where Lang file is not None.
     */
    lang_file: Option<Box<str>>,
}

impl NormalFunction {
    pub(crate) fn new(
        argument_pos_list: Vec<CodePosition>,
        function_body: AST,

        lang_path: Option<&str>,
        lang_file: Option<&str>,
    ) -> Self {
        Self {
            argument_pos_list,
            function_body,

            lang_path: lang_path.map(Box::from),
            lang_file: lang_file.map(Box::from),
        }
    }

    pub fn is_equals(&self, other: &Self, _interpreter: &mut Interpreter, _pos: CodePosition) -> bool {
        self.function_body == other.function_body
    }

    pub fn is_strict_equals(&self, other: &Self, _interpreter: &mut Interpreter, _pos: CodePosition) -> bool {
        self.function_body == other.function_body
    }

    pub fn argument_pos_list(&self) -> &[CodePosition] {
        &self.argument_pos_list
    }

    pub fn function_body(&self) -> &AST {
        &self.function_body
    }

    pub fn lang_path(&self) -> Option<&str> {
        self.lang_path.as_deref()
    }

    pub fn lang_file(&self) -> Option<&str> {
        self.lang_file.as_deref()
    }
}

#[derive(Debug, Trace, Finalize)]
pub enum FunctionData {
    //SAFETY: There are no GC reference inside NormalFunction
    Normal(#[unsafe_ignore_trace] NormalFunction),
    Native(NativeFunction),
}

impl FunctionData {
    pub fn is_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        match (self, other) {
            (FunctionData::Normal(s), FunctionData::Normal(o)) => s.is_equals(o, interpreter, pos),
            (FunctionData::Native(s), FunctionData::Native(o)) => s.is_equals(o, interpreter, pos),

            _ => false,
        }
    }

    pub fn is_strict_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        match (self, other) {
            (FunctionData::Normal(s), FunctionData::Normal(o)) => s.is_strict_equals(o, interpreter, pos),
            (FunctionData::Native(s), FunctionData::Native(o)) => s.is_strict_equals(o, interpreter, pos),

            _ => false,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct Function {
    function: Gc<FunctionData>,

    //SAFETY: There are no GC reference inside Parameter
    #[unsafe_ignore_trace]
    parameter_list: Vec<Parameter>,

    //SAFETY: There are no GC reference inside VarArgsType
    #[unsafe_ignore_trace]
    var_args_parameter: Option<(usize, VarArgsType)>,

    //SAFETY: There are no GC reference inside DataTypeConstraint
    #[unsafe_ignore_trace]
    return_value_type_constraint: Option<Box<DataTypeConstraint>>,

    combinator_function_call_count: Option<usize>,
    combinator_function_provided_arguments: Vec<DataObjectRef>,

    function_name: Option<Box<str>>,
}

impl Function {
    pub fn new_normal(
        function: NormalFunction,
        metadata: &FunctionMetadata,
    ) -> Self {
        if function.argument_pos_list.len() != metadata.parameter_list.len() {
            panic!("Normal function parameter count does not match parameter count of function metadata");
        }

        Self::new(Gc::new(FunctionData::Normal(function)), metadata)
    }

    pub fn new_native(
        function: NativeFunction,
        metadata: &FunctionMetadata,
    ) -> Self {
        if function.lang_parameter_count() != metadata.parameter_list.len() {
            panic!("Native function parameter count (without interpreter and without this object parameter) does not match parameter count of function metadata");
        }

        Self::new(Gc::new(FunctionData::Native(function)), metadata)
    }

    fn new(
        function: Gc<FunctionData>,
        metadata: &FunctionMetadata,
    ) -> Self {
        let mut var_args_parameter = None;
        for (i, parameter) in metadata.parameter_list.iter().enumerate() {
            match parameter.parameter_type {
                ParameterType::VarArgs => {
                    if var_args_parameter.is_some() {
                        panic!("There can only be one var args parameter");
                    }

                    if parameter.parameter_name.starts_with("$") {
                        if metadata.combinator_function {
                            panic!("Text var args can not be used in combinator function");
                        }

                        var_args_parameter = Some((i, VarArgsType::Text));
                    }else {
                        var_args_parameter = Some((i, VarArgsType::Normal));
                    }
                },

                ParameterType::RawVarArgs => {
                    if var_args_parameter.is_some() {
                        panic!("There can only be one var args parameter");

                    }

                    if metadata.combinator_function {
                        panic!("Raw var args can not be used in combinator function");
                    }

                    var_args_parameter = Some((i, VarArgsType::Raw));
                },

                _ => {},
            }
        }

        if matches!(var_args_parameter, Some((_, VarArgsType::Raw))) && metadata.parameter_list.len() != 1 {
            panic!("If @RawVarArgs is used there must be exactly two parameters: (Interpreter, (Vec<DataObjectRef>,))");
        }

        Self {
            function,

            parameter_list: metadata.parameter_list.iter().cloned().map(Into::into).collect(),

            var_args_parameter,

            return_value_type_constraint: metadata.return_value_type_constraint.clone().map(Box::new),

            combinator_function_call_count: metadata.combinator_function.then_some(0),
            combinator_function_provided_arguments: Vec::with_capacity(0),

            function_name: metadata.function_name.clone(),
        }
    }

    pub fn call_native_func(
        &self,
        interpreter: &mut Interpreter,
        this_object_with_super_level: Option<(LangObjectRef, usize)>,
        mut argument_list: Vec<DataObjectRef>,
        mut combined_argument_list: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let FunctionData::Native(function) = self.function.as_ref() else {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidFuncPtr,
                Some("Function call of invalid FP"),
                CodePosition::EMPTY,
            ));
        };

        if function.is_method() && this_object_with_super_level.is_none() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("This-object is not bound for native function for LangObject"),
                CodePosition::EMPTY,
            ));
        }

        if !function.is_method() && this_object_with_super_level.is_some() {
            return Some(interpreter.set_errno_error_object(
                InterpretingError::InvalidArguments,
                Some("This-object is bound for native function"),
                CodePosition::EMPTY,
            ));
        }

        let arg_cnt = self.parameter_list.len();

        if self.combinator_function_call_count.is_some() {
            let mut new_combined_argument_list = self.combinator_function_provided_arguments().to_vec();

            new_combined_argument_list.append(&mut combined_argument_list.into_iter().
                    map(|data_object| {
                        let mut new_data_object = DataObject::new();
                        new_data_object.set_data(&data_object.borrow()).unwrap();

                        DataObjectRef::new(new_data_object)
                    }).collect::<Vec<_>>());

            combined_argument_list = new_combined_argument_list;
        }

        if self.var_args_parameter.is_some() {
            //Infinite combinator functions (= Combinator functions with var args argument) must be called exactly two times
            if self.combinator_function_call_count.is_none_or(|call_cnt| call_cnt > 0) &&
                    combined_argument_list.len() < arg_cnt - 1 {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArgCount,
                    Some(&format!("Not enough arguments (at least {arg_cnt} needed)")),
                    CodePosition::EMPTY,
                ));
            }
        }else {
            if self.combinator_function_call_count.is_none() && combined_argument_list.len() < arg_cnt {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArgCount,
                    Some(&format!("Not enough arguments ({arg_cnt} needed)")),
                    CodePosition::EMPTY,
                ));
            }

            if combined_argument_list.len() > arg_cnt {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArgCount,
                    Some(&format!("Too many arguments ({arg_cnt} needed)")),
                    CodePosition::EMPTY,
                ));
            }
        }

        let mut native_function_args = Vec::with_capacity(arg_cnt);
        let mut argument_index = 0;
        'outer:
        for i in 0..arg_cnt {
            if let Some(call_cnt) = self.combinator_function_call_count {
                if argument_index >= combined_argument_list.len() &&
                        (self.var_args_parameter.is_none() || call_cnt == 0) {
                    return Some(self.combinator_call(
                        this_object_with_super_level,
                        combined_argument_list,
                    ));
                }
            }

            let parameter = &self.parameter_list[i];

            let variable_name = &parameter.parameter_name;

            let is_ignore_type_check = matches!(parameter.parameter_type,
                ParameterType::CallByPointer | ParameterType::VarArgs | ParameterType::RawVarArgs);

            if !is_ignore_type_check && !parameter.type_constraint.
                    is_type_allowed(combined_argument_list[argument_index].data_type()) {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The type of argument {} (\"{}\") must be one of [{}]",
                        argument_index + 1,
                        variable_name,
                        parameter.type_constraint.allowed_types().iter().
                                map(|data_type| data_type.to_string()).
                                collect::<Vec<_>>().
                                join(", "),
                    )),
                    CodePosition::EMPTY,
                ));
            }

            let number_data_object = if matches!(parameter.parameter_type, ParameterType::Number) {
                let number = conversions::to_number(
                    interpreter,
                    &combined_argument_list[argument_index],
                    CodePosition::EMPTY,
                );

                if number.is_none() {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::NoNum,
                        Some(&format!(
                            "Argument {} (\"{}\") must be a number",
                            argument_index + 1,
                            variable_name,
                        )),
                        CodePosition::EMPTY,
                    ));
                }

                number.map(|number| {
                    let mut number_data_object = DataObject::new();
                    number_data_object.set_number(number).unwrap();

                    DataObjectRef::new(number_data_object)
                })
            }else {
                None
            };

            if matches!(parameter.parameter_type, ParameterType::Callable) &&
                    !utils::is_callable(&combined_argument_list[argument_index]) {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "Argument {} (\"{}\") must be callable",
                        argument_index + 1,
                        variable_name,
                    )),
                    CodePosition::EMPTY,
                ));
            }

            let ret = 'error: {
                match parameter.parameter_type {
                    ParameterType::RawVarArgs => {
                        native_function_args.append(&mut argument_list);

                        //There can only be one parameter if raw var args is used
                        break 'outer;
                    },

                    ParameterType::VarArgs => {
                        //Infinite combinator functions (= Combinator functions with var args argument) must be called exactly two times
                        if self.combinator_function_call_count.is_some_and(|call_cnt| call_cnt == 0) {
                            return Some(self.combinator_call(
                                this_object_with_super_level,
                                combined_argument_list,
                            ));
                        }

                        let mut var_args_argument_list = combined_argument_list[i..combined_argument_list.len() + i + 1 - arg_cnt].iter().
                                map(|data_object| {
                                    let mut new_data_object = DataObject::new();
                                    new_data_object.set_data(&data_object.borrow()).unwrap();

                                    DataObjectRef::new(new_data_object)
                                }).collect::<Vec<_>>();

                        if !matches!(self.var_args_parameter, Some((_, VarArgsType::Text))) {
                            for (j, arg) in var_args_argument_list.iter().
                                    enumerate() {
                                if !parameter.type_constraint.is_type_allowed(arg.data_type()) {
                                    return Some(interpreter.set_errno_error_object(
                                        InterpretingError::InvalidArguments,
                                        Some(&format!(
                                            "The type of argument {} (for var args parameter \"{}\") must be one of [{}]",
                                            i + j + 1,
                                            variable_name,
                                            parameter.type_constraint.allowed_types().iter().
                                                    map(|data_type| data_type.to_string()).
                                                    collect::<Vec<_>>().
                                                    join(", "),
                                        )),
                                        CodePosition::EMPTY,
                                    ));
                                }
                            }
                        }

                        if matches!(self.var_args_parameter, Some((_, VarArgsType::Text))) {
                            let mut argument_list_copy = VecDeque::from(argument_list.clone());

                            //Remove leading arguments
                            for _ in 0..i {
                                for _ in 0..argument_list_copy.len() {
                                    if argument_list_copy.pop_front().
                                            is_some_and(|val| val.
                                                    data_type() == DataType::ARGUMENT_SEPARATOR) {
                                        break;
                                    }
                                }
                            }

                            //Remove trailing arguments
                            for _ in 0..arg_cnt - i - 1 {
                                for k in (0..argument_list_copy.len()).rev() {
                                    if argument_list_copy.remove(k).
                                            is_some_and(|val| val.
                                                    data_type() == DataType::ARGUMENT_SEPARATOR) {
                                        break;
                                    }
                                }
                            }

                            let combined_arguments = utils::combine_data_objects(
                                argument_list_copy.make_contiguous(),
                                interpreter,
                                CodePosition::EMPTY,
                            );

                            let text = combined_arguments.map(|arg| conversions::to_text(
                                interpreter,
                                &arg,
                                CodePosition::EMPTY,
                            )).unwrap_or_default();

                            let mut argument = DataObject::new_text(text);
                            let ret = argument.set_variable_name(
                                Some(variable_name),
                            );
                            if ret.is_err() {
                                break 'error ret.map(|_| ());
                            }

                            native_function_args.push(DataObjectRef::new(argument));
                        }else {
                            native_function_args.append(&mut var_args_argument_list);
                        }

                        //Not "+1", because argumentIndex will be incremented at the end of the for loop
                        argument_index = (combined_argument_list.len() + i).wrapping_sub(arg_cnt);

                        Ok(())
                    },

                    ParameterType::CallByPointer => {
                        let mut argument = DataObject::new();
                        let ret = argument.set_variable_name(
                            Some(variable_name),
                        );
                        if ret.is_err() {
                            break 'error ret.map(|_| ());
                        }

                        let ret = argument.set_var_pointer(
                            combined_argument_list[argument_index].clone(),
                        );
                        if ret.is_err() {
                            break 'error ret.map(|_| ());
                        }

                        let ret = argument.set_type_constraint(
                            parameter.type_constraint.clone(),
                        );
                        if ret.is_err() {
                            break 'error ret.map(|_| ());
                        }

                        native_function_args.push(DataObjectRef::new(argument));

                        Ok(())
                    },

                    _ => {
                        let mut argument = DataObject::new();

                        let ret = if let Some(number_data_object) = &number_data_object {
                            argument.set_data(&number_data_object.borrow())
                        }else {
                            argument.set_data(&combined_argument_list[argument_index].borrow())
                        };
                        if ret.is_err() {
                            break 'error ret.map(|_| ());
                        }

                        let ret = argument.set_variable_name(
                            Some(variable_name),
                        );
                        if ret.is_err() {
                            break 'error ret.map(|_| ());
                        }

                        let ret = argument.set_type_constraint(
                            parameter.type_constraint.clone(),
                        );
                        if ret.is_err() {
                            break 'error ret.map(|_| ());
                        }

                        native_function_args.push(DataObjectRef::new(argument));

                        Ok(())
                    },
                }
            };
            if let Err(e) = ret {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::IncompatibleDataType,
                    Some(&e.message),
                    CodePosition::EMPTY,
                ));
            }

            argument_index = argument_index.wrapping_add(1);
        }

        let ret = function.function_body().lang_call(
            interpreter,
            this_object_with_super_level.map(|(this_object, _)| this_object),
            native_function_args,
        );

        if let Err(ret) = ret {
            //TODO print stack trace if enabled

            return Some(interpreter.set_errno_error_object(
                InterpretingError::SystemError,
                Some(&format!("Native Error: {}", ret)),
                CodePosition::EMPTY,
            ));
        };

        let ret = ret.unwrap();

        if let Some(type_constraint) = &self.return_value_type_constraint {
            if !interpreter.is_thrown_value() {
                //Thrown values are always allowed

                let ret = utils::none_to_lang_void(ret.clone());
                if !type_constraint.is_type_allowed(ret.data_type()) {
                    return Some(interpreter.set_errno_error_object(
                        InterpretingError::IncompatibleDataType,
                        Some(&format!("Invalid return value type \"{}\"", ret.data_type())),
                        CodePosition::EMPTY,
                    ));
                }
            }
        }

        ret
    }

    pub(crate) fn combinator_call(
        &self,
        this_object_with_super_level: Option<(LangObjectRef, usize)>,
        combined_argument_list: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let function_name = combined_argument_list.iter().
                map(|data_object| {
                    let Some(func) = data_object.function_pointer_value() else {
                        return "<arg>".to_string();
                    };

                    if let Some(function_name) = &func.function_name {
                        function_name.to_string()
                    }else {
                        data_object.variable_name().map(|str| str.to_string()).unwrap_or("null".to_string())
                    }
                }).collect::<Vec<_>>().join(", ");

        let function_name = "<".to_string() + self.var_args_parameter.map_or("", |_| "inf-") +
                self.function_name.as_deref().unwrap_or("null") + "-func(" + &function_name + ")>";

        let function = Function {
            function: Gc::clone(&self.function),

            parameter_list: self.parameter_list.clone(),

            var_args_parameter: self.var_args_parameter,

            return_value_type_constraint: self.return_value_type_constraint.clone(),

            combinator_function_call_count: self.combinator_function_call_count.map(|cnt| cnt + 1),
            combinator_function_provided_arguments: combined_argument_list,

            function_name: self.function_name.clone(),
        };

        let mut fp = FunctionPointerObject::new_with_function_name(&function_name, function);

        if let Some((this_object, super_level)) = this_object_with_super_level {
            fp = FunctionPointerObject::copy_with_this_object(&fp, this_object).unwrap().
                    copy_with_mapped_functions(|function|
                            InternalFunction::copy_with_super_level(function, super_level));
        }

        DataObjectRef::new(DataObject::with_update(|data_object| {
            data_object.set_function_pointer(Gc::new(fp))
        }).unwrap())
    }

    pub fn is_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        match (self.var_args_parameter, other.var_args_parameter) {
            (Some((s_index, s_var_args_type)), Some((o_index, o_var_args_type))) => {
                if s_index != o_index || (matches!(s_var_args_type, VarArgsType::Text) != matches!(o_var_args_type, VarArgsType::Text)) {
                    return false;
                }
            },

            (None, None) => {},

            _ => {
                return false;
            },
        }

        self.function.is_equals(other.function.as_ref(), interpreter, pos) &&
                self.parameter_list.len() == other.parameter_list.len() &&
                self.parameter_list.iter().zip(other.parameter_list.iter()).
                        all(|(s, o)|s.is_equals(o, interpreter, pos)) &&
                self.combinator_function_provided_arguments.len() == other.combinator_function_provided_arguments.len() &&
                self.combinator_function_provided_arguments.iter().zip(other.combinator_function_provided_arguments.iter()).
                        all(|(s, o)|operators::is_equals(interpreter, s, o, pos))
    }

    pub fn is_strict_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        match (self.var_args_parameter, other.var_args_parameter) {
            (Some((s_index, s_var_args_type)), Some((o_index, o_var_args_type))) => {
                if s_index != o_index || (matches!(s_var_args_type, VarArgsType::Text) != matches!(o_var_args_type, VarArgsType::Text)) {
                    return false;
                }
            },

            (None, None) => {},

            _ => {
                return false;
            },
        }

        self.function.is_strict_equals(other.function.as_ref(), interpreter, pos) &&
                self.parameter_list.len() == other.parameter_list.len() &&
                self.parameter_list.iter().zip(other.parameter_list.iter()).
                        all(|(s, o)|s.is_strict_equals(o, interpreter, pos)) &&
                self.combinator_function_provided_arguments.len() == other.combinator_function_provided_arguments.len() &&
                self.combinator_function_provided_arguments.iter().zip(other.combinator_function_provided_arguments.iter()).
                        all(|(s, o)|operators::is_strict_equals(interpreter, s, o, pos))
    }

    pub fn to_function_signature_string(&self) -> String {
        let mut builder = String::new();

        builder += "(";

        for parameter in self.parameter_list.iter() {
            let variable_name = &parameter.parameter_name;

            if matches!(parameter.parameter_type, ParameterType::CallByPointer) {
                if variable_name.is_empty() {
                    builder += "$[]";
                }else {
                    let _ = write!(builder, "$[{}]", &variable_name[1..variable_name.len()]);
                }
            }else {
                builder += variable_name;
            }

            match parameter.parameter_type {
                ParameterType::VarArgs => {
                    if *parameter.type_constraint != *data::CONSTRAINT_NORMAL {
                        builder += &parameter.type_constraint.to_type_constraint_syntax();
                    }

                    builder += "...";
                },

                ParameterType::RawVarArgs => {
                    if *parameter.type_constraint != *data::CONSTRAINT_NORMAL {
                        builder += &parameter.type_constraint.to_type_constraint_syntax();
                    }

                    builder += "...{raw}";
                },

                ParameterType::Number => {
                    builder += "{number}";
                },

                ParameterType::Callable => {
                    builder += "{callable}";
                },

                ParameterType::Boolean => {
                    builder += "{boolean}";
                },

                _ => {
                    if *parameter.type_constraint != *DataObject::get_type_constraint_for(Some(variable_name)) {
                        builder += &parameter.type_constraint.to_type_constraint_syntax();
                    }
                },
            }

            builder += ", ";
        }

        if !builder.is_empty() {
            builder = builder[..builder.len() - 2].to_string();
        }

        builder += ")";

        builder
    }

    pub fn function(&self) -> &FunctionData {
        &self.function
    }

    pub fn parameter_list(&self) -> &[Parameter] {
        &self.parameter_list
    }

    pub fn var_args_parameter(&self) -> Option<(usize, VarArgsType)> {
        self.var_args_parameter
    }

    pub fn return_value_type_constraint(&self) -> Option<&DataTypeConstraint> {
        self.return_value_type_constraint.as_deref()
    }

    pub fn combinator_function_call_count(&self) -> Option<usize> {
        self.combinator_function_call_count
    }

    pub fn combinator_function_provided_arguments(&self) -> &[DataObjectRef] {
        &self.combinator_function_provided_arguments
    }

    pub fn function_name(&self) -> Option<&str> {
        self.function_name.as_deref()
    }
}

#[derive(Debug, Clone, Trace, Finalize)]
pub struct InternalFunction {
    super_level: Option<usize>,

    function: Gc<Function>,

    member_of_class: OptionLangObjectRef,
    //SAFETY: There are no GC reference inside DeprecationInfo
    #[unsafe_ignore_trace]
    member_visibility: Option<Visibility>,
}

impl InternalFunction {
    pub fn new(function: Gc<Function>) -> Self {
        Self {
            super_level: None,

            function,

            member_of_class: None,
            member_visibility: None,
        }
    }

    #[must_use]
    pub(crate) fn copy_with_class_member_attributes(
        &self,
        member_of_class: LangObjectRef,
        member_visibility: Visibility
    ) -> Self {
        Self {
            super_level: self.super_level,

            function: self.function.clone(),

            member_of_class: Some(member_of_class),
            member_visibility: Some(member_visibility),
        }
    }

    #[must_use]
    pub(crate) fn copy_with_super_level(
        &self,
        super_level: usize,
    ) -> Self {
        Self {
            super_level: Some(super_level),

            function: self.function.clone(),

            member_of_class: self.member_of_class.clone(),
            member_visibility: self.member_visibility,
        }
    }

    pub fn is_accessible(&self, accessing_class: Option<&LangObjectRef>) -> bool {
        let Some(member_of_class) = &self.member_of_class else {
            return true;
        };

        let Some(member_visibility) = self.member_visibility else {
            return true;
        };

        if member_visibility == Visibility::Public {
            return true;
        }

        let Some(accessing_class) = accessing_class else {
            return false;
        };

        ptr::eq(accessing_class.borrow().deref(), member_of_class.borrow().deref()) || (
            member_visibility == Visibility::Protected &&
                    accessing_class.borrow().is_instance_of(member_of_class.borrow().deref())
        )
    }

    pub fn to_function_signature_string(&self) -> String {
        self.function.to_function_signature_string()
    }

    pub fn lang_path(&self) -> Option<&str> {
        match self.function.function.as_ref() {
            FunctionData::Normal(function) => function.lang_path(),

            _ => None,
        }
    }

    pub fn lang_file(&self) -> Option<&str> {
        match self.function.function.as_ref() {
            FunctionData::Normal(function) => function.lang_file(),

            _ => None,
        }
    }

    pub fn is_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        self.super_level == other.super_level &&
                self.function.is_equals(other.function.as_ref(), interpreter, pos)
    }

    pub fn is_strict_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        self.super_level == other.super_level &&
                self.function.is_strict_equals(other.function.as_ref(), interpreter, pos)
    }

    pub fn super_level(&self) -> Option<usize> {
        self.super_level
    }

    pub fn function(&self) -> &Function {
        self.function.as_ref()
    }

    pub fn member_of_class(&self) -> Option<&LangObjectRef> {
        self.member_of_class.as_ref()
    }

    pub fn member_visibility(&self) -> Option<Visibility> {
        self.member_visibility
    }
}

#[derive(Debug, Clone, Trace, Finalize)]
pub struct FunctionPointerObject {
    this_object: OptionLangObjectRef,

    function_name: Option<Box<str>>,
    function_info: Option<Box<str>>,

    linker_function: bool,
    //SAFETY: There are no GC reference inside DeprecationInfo
    #[unsafe_ignore_trace]
    deprecated: Option<DeprecationInfo>,

    functions: Vec<InternalFunction>,
}

impl FunctionPointerObject {
    pub fn create_function_pointer_objects_from_native_functions(
        mut functions: Vec<(FunctionMetadata, Function)>,
    ) -> HashMap<Box<str>, Self> {
        let mut functions_by_name = HashMap::new();

        //Functions with the "has_info" contain the metadata for the function pointer object.
        //They must be inserted before the functions without "has_info".
        functions.sort_unstable_by_key(|(metadata, _)| !metadata.has_info);

        for (metadata, function) in functions.into_iter() {
            let function_name = function.function_name.as_ref().expect("Function name may not be None");

            let entry = functions_by_name.entry(function_name.clone());
            let vec = entry.or_insert(Vec::new());
            vec.push((metadata, function));
        }

        let mut function_map = HashMap::new();

        for (function_name, functions) in functions_by_name {
            let (metadata, functions): (Vec<FunctionMetadata>, Vec<InternalFunction>) = functions.
                    into_iter().
                    map(|(metadata, function)|
                            (metadata, InternalFunction::new(Gc::new(function)))).
                    unzip();

            #[cfg(debug_assertions)]
            {
                if functions.len() == 1 && metadata.iter().
                        map(|metadata| metadata.has_info as u32).
                        sum::<u32>() > 0 {
                    panic!("has_info can only be used if there are at least two functions (Invalid for function: {function_name})");
                }

                if functions.len() > 1 && metadata.iter().
                        map(|metadata| metadata.has_info as u32).
                        sum::<u32>() != 1 {
                    panic!("has_info can only be once (Invalid for function: {function_name})");
                }
            }

            function_map.insert(function_name, FunctionPointerObject::new_with_functions(
                None,
                &functions,
                &metadata[0],
            ).unwrap());
        }

        function_map
    }

    pub(crate) fn new_dummy_definition() -> Self {
        Self {
            this_object: None,

            function_name: None,
            function_info: None,
            linker_function: false,
            deprecated: None,
            functions: Vec::new(),
        }
    }

    pub fn new(metadata: &FunctionMetadata, func: Function) -> Self {
        Self {
            this_object: None,

            function_name: metadata.function_name.clone(),
            function_info: metadata.function_info.clone(),

            linker_function: metadata.linker_function,
            deprecated: metadata.deprecated.clone(),

            functions: vec![
                InternalFunction::new(Gc::new(func)),
            ],
        }
    }

    pub fn new_with_function_name(function_name: &str, func: Function) -> Self {
        Self {
            this_object: None,

            function_name: Some(Box::from(function_name)),
            function_info: None,

            linker_function: false,
            deprecated: None,

            functions: vec![
                InternalFunction::new(Gc::new(func)),
            ],
        }
    }

    pub fn new_with_functions(
        this_object: OptionLangObjectRef,
        functions: &[InternalFunction],
        metadata: &FunctionMetadata,
    ) -> Result<Self, DataTypeConstraintError> {
        if let Some(this_object) = &this_object {
            if this_object.borrow().is_class() {
                return Err(DataTypeConstraintError::with_message(
                    "The this-object must be an object",
                ));
            }
        }

        Ok(Self {
            this_object,

            function_name: metadata.function_name.clone(),
            function_info: metadata.function_info.clone(),

            linker_function: metadata.linker_function,
            deprecated: metadata.deprecated.clone(),

            functions: Vec::from(functions),
        })
    }

    pub(crate) fn copy_with_this_object(
        func: &Self,
        this_object: LangObjectRef,
    ) -> Result<Self, DataTypeConstraintError> {
        if this_object.borrow().is_class() {
            return Err(DataTypeConstraintError::with_message(
                "The this-object must be an object",
            ));
        }

        Ok(Self {
            this_object: Some(this_object),

            function_name: func.function_name.clone(),
            function_info: func.function_info.clone(),

            linker_function: func.linker_function,
            deprecated: func.deprecated.clone(),

            functions: func.functions.clone(),
        })
    }

    #[must_use]
    pub fn copy_with_function_name(&self, function_name: &str) -> Self {
        Self {
            this_object: self.this_object.clone(),

            function_name: Some(Box::from(function_name)),
            function_info: self.function_info.clone(),

            linker_function: self.linker_function,
            deprecated: self.deprecated.clone(),

            functions: self.functions.clone(),
        }
    }

    #[must_use]
    pub fn copy_with_function_info(&self, function_info: &str) -> Self {
        Self {
            this_object: self.this_object.clone(),

            function_name: self.function_name.clone(),
            function_info: Some(Box::from(function_info)),

            linker_function: self.linker_function,
            deprecated: self.deprecated.clone(),

            functions: self.functions.clone(),
        }
    }

    #[must_use]
    pub fn copy_with_linker(&self, linker_function: bool) -> Self {
        Self {
            this_object: self.this_object.clone(),

            function_name: self.function_name.clone(),
            function_info: self.function_info.clone(),

            linker_function,
            deprecated: self.deprecated.clone(),

            functions: self.functions.clone(),
        }
    }

    #[must_use]
    pub fn copy_with_deprecation_info(&self, deprecated: DeprecationInfo) -> Self {
        Self {
            this_object: self.this_object.clone(),

            function_name: self.function_name.clone(),
            function_info: self.function_info.clone(),

            linker_function: self.linker_function,
            deprecated: Some(deprecated),

            functions: self.functions.clone(),
        }
    }

    #[must_use]
    pub fn copy_with_functions(&self, functions: &[InternalFunction]) -> Self {
        Self {
            this_object: self.this_object.clone(),

            function_name: self.function_name.clone(),
            function_info: self.function_info.clone(),

            linker_function: self.linker_function,
            deprecated: self.deprecated.clone(),

            functions: Vec::from(functions),
        }
    }

    #[must_use]
    pub fn copy_with_added_function(&self, function: InternalFunction) -> Self {
        let mut functions = self.functions.clone();
        functions.push(function);

        Self {
            this_object: self.this_object.clone(),

            function_name: self.function_name.clone(),
            function_info: self.function_info.clone(),

            linker_function: self.linker_function,
            deprecated: self.deprecated.clone(),

            functions,
        }
    }

    #[must_use]
    pub fn copy_with_added_functions(&self, function_pointer_object: &Self) -> Self {
        let mut functions = self.functions.clone();
        functions.append(&mut function_pointer_object.functions.clone());

        Self {
            this_object: self.this_object.clone(),

            function_name: self.function_name.clone(),
            function_info: self.function_info.clone(),

            linker_function: self.linker_function,
            deprecated: self.deprecated.clone(),

            functions,
        }
    }

    #[must_use]
    pub fn copy_with_mapped_functions(&self, mapper: impl FnMut(&InternalFunction) -> InternalFunction) -> Self {
        let functions = self.functions.iter().
                map(mapper).
                collect();

        Self {
            this_object: self.this_object.clone(),

            function_name: self.function_name.clone(),
            function_info: self.function_info.clone(),

            linker_function: self.linker_function,
            deprecated: self.deprecated.clone(),

            functions,
        }
    }

    pub fn is_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        match (&self.this_object, &other.this_object) {
            (Some(s), Some(o)) => {
                //Check for same reference of thisObjects
                if !ptr::eq(s.borrow().deref(), o.borrow().deref()) {
                    return false;
                }
            },

            (None, None) => {},

            _ => {
                return false;
            },
        }

        self.functions.len() == other.functions.len() &&
                self.functions.iter().zip(other.functions.iter()).
                        all(|(s, o)|s.is_equals(o, interpreter, pos))
    }

    pub fn is_strict_equals(&self, other: &Self, interpreter: &mut Interpreter, pos: CodePosition) -> bool {
        match (&self.this_object, &other.this_object) {
            (Some(s), Some(o)) => {
                //Check for same reference of thisObjects
                if !ptr::eq(s.borrow().deref(), o.borrow().deref()) {
                    return false;
                }
            },

            (None, None) => {},

            _ => {
                return false;
            },
        }

        self.functions.len() == other.functions.len() &&
                self.functions.iter().zip(other.functions.iter()).
                        all(|(s, o)|s.is_strict_equals(o, interpreter, pos))
    }

    pub fn this_object(&self) -> Option<&LangObjectRef> {
        self.this_object.as_ref()
    }

    pub fn function_name(&self) -> Option<&str> {
        self.function_name.as_deref()
    }

    pub fn function_info(&self) -> Option<&str> {
        self.function_info.as_deref()
    }

    pub fn linker_function(&self) -> bool {
        self.linker_function
    }

    pub fn deprecated(&self) -> Option<&DeprecationInfo> {
        self.deprecated.as_ref()
    }

    pub fn functions(&self) -> &[InternalFunction] {
        &self.functions
    }

    pub(crate) fn functions_mut(&mut self) -> &mut [InternalFunction] {
        &mut self.functions
    }

    pub fn get_function(&self, index: usize) -> Option<&InternalFunction> {
        self.functions.get(index)
    }

    pub fn get_overloaded_function_count(&self) -> usize {
        self.functions.len()
    }
}

impl Display for FunctionPointerObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(function_name) = &self.function_name {
            return f.write_str(function_name);
        }

        f.write_str(if self.this_object.is_some() {
            "<Method>"
        }else {
            "<Function>"
        })
    }
}

impl From<(FunctionMetadata, Function)> for FunctionPointerObject {
    fn from((metadata, func): (FunctionMetadata, Function)) -> Self {
        Self::new(&metadata, func)
    }
}

impl From<(&FunctionMetadata, Function)> for FunctionPointerObject {
    fn from((metadata, func): (&FunctionMetadata, Function)) -> Self {
        Self::new(metadata, func)
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum ParameterType {
    Normal,
    Number,
    Callable,
    Boolean,
    CallByPointer,
    VarArgs,
    RawVarArgs,
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum VarArgsType {
    Normal,
    Text,
    Raw,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    parameter_name: Box<str>,
    parameter_info: Option<Box<str>>,
    type_constraint: Box<DataTypeConstraint>,
    parameter_type: ParameterType,
}

impl Parameter {
    pub fn new(
        parameter_name: &str,
        parameter_info: Option<&str>,
        type_constraint: Box<DataTypeConstraint>,
        parameter_type: ParameterType,
    ) -> Self {
        Self {
            parameter_name: Box::from(parameter_name),
            parameter_info: parameter_info.map(Box::from),
            type_constraint,
            parameter_type,
        }
    }

    pub fn is_equals(&self, other: &Self, _interpreter: &mut Interpreter, _pos: CodePosition) -> bool {
        self.parameter_name == other.parameter_name && self.type_constraint == other.type_constraint &&
                self.parameter_type == other.parameter_type
    }

    pub fn is_strict_equals(&self, other: &Self, _interpreter: &mut Interpreter, _pos: CodePosition) -> bool {
        self.parameter_name == other.parameter_name && self.type_constraint == other.type_constraint &&
                self.parameter_type == other.parameter_type
    }

    pub fn parameter_name(&self) -> &str {
        &self.parameter_name
    }

    pub fn parameter_info(&self) -> Option<&str> {
        self.parameter_info.as_deref()
    }

    pub fn type_constraint(&self) -> &DataTypeConstraint {
        &self.type_constraint
    }

    pub fn parameter_type(&self) -> ParameterType {
        self.parameter_type
    }
}

impl From<ParameterMetadata> for Parameter {
    fn from(value: ParameterMetadata) -> Self {
        let type_constraint = value.type_constraint.map(Box::new).
                unwrap_or_else(|| if matches!(value.parameter_type, ParameterType::VarArgs | ParameterType::RawVarArgs) {
                    Box::new(data::CONSTRAINT_NORMAL.clone())
                }else {
                    Box::new(DataObject::get_type_constraint_for(Some(&value.parameter_name)).clone())
                });

        Self {
            parameter_name: value.parameter_name,
            parameter_info: value.parameter_info,
            type_constraint,
            parameter_type: value.parameter_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DeprecationInfo {
    remove_version: Option<Box<str>>,
    replacement_function: Option<Box<str>>,
}

impl DeprecationInfo {
    pub fn new(remove_version: Option<&str>, replacement_function: Option<&str>) -> Self {
        Self {
            remove_version: remove_version.map(Box::from),
            replacement_function: replacement_function.map(Box::from),
        }
    }

    pub fn remove_version(&self) -> Option<&str> {
        self.remove_version.as_deref()
    }

    pub fn replacement_function(&self) -> Option<&str> {
        self.replacement_function.as_deref()
    }
}

#[derive(Debug, Clone)]
pub struct ParameterMetadata {
    parameter_name: Box<str>,
    parameter_info: Option<Box<str>>,
    type_constraint: Option<DataTypeConstraint>,
    parameter_type: ParameterType,
}

impl ParameterMetadata {
    pub fn new(
        parameter_name: &str,
        parameter_info: Option<&str>,
        type_constraint: Option<DataTypeConstraint>,
        parameter_type: ParameterType,
    ) -> Self {
        Self {
            parameter_name: Box::from(parameter_name),
            parameter_info: parameter_info.map(Box::from),
            type_constraint,
            parameter_type,
        }
    }

    pub fn parameter_name(&self) -> &str {
        &self.parameter_name
    }

    pub fn parameter_info(&self) -> Option<&str> {
        self.parameter_info.as_deref()
    }

    pub fn type_constraint(&self) -> Option<&DataTypeConstraint> {
        self.type_constraint.as_ref()
    }

    pub fn parameter_type(&self) -> ParameterType {
        self.parameter_type
    }
}

#[derive(Debug, Clone)]
pub struct FunctionMetadata {
    function_name: Option<Box<str>>,
    function_info: Option<Box<str>>,

    has_info: bool,

    combinator_function: bool,

    linker_function: bool,
    deprecated: Option<DeprecationInfo>,

    parameter_list: Vec<ParameterMetadata>,

    return_value_type_constraint: Option<DataTypeConstraint>,
}

impl FunctionMetadata {
    #[expect(clippy::too_many_arguments)]
    pub fn new(
        function_name: Option<&str>,
        function_info: Option<&str>,

        has_info: bool,

        combinator_function: bool,

        linker_function: bool,
        deprecated: Option<DeprecationInfo>,

        parameter_list: Vec<ParameterMetadata>,

        return_value_type_constraint: Option<DataTypeConstraint>,
    ) -> Self {
        Self {
            function_name: function_name.map(Box::from),
            function_info: function_info.map(Box::from),

            has_info,
            combinator_function,

            linker_function,
            deprecated,

            parameter_list,

            return_value_type_constraint,
        }
    }

    pub fn function_name(&self) -> Option<&str> {
        self.function_name.as_deref()
    }

    pub fn function_info(&self) -> Option<&str> {
        self.function_info.as_deref()
    }

    pub fn has_info(&self) -> bool {
        self.has_info
    }

    pub fn combinator_function(&self) -> bool {
        self.combinator_function
    }

    pub fn linker_function(&self) -> bool {
        self.linker_function
    }

    pub fn deprecated(&self) -> Option<&DeprecationInfo> {
        self.deprecated.as_ref()
    }

    pub fn parameter_list(&self) -> &[ParameterMetadata] {
        &self.parameter_list
    }

    pub fn return_value_type_constraint(&self) -> Option<&DataTypeConstraint> {
        self.return_value_type_constraint.as_ref()
    }
}
