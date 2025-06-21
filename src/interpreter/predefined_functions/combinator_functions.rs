use crate::interpreter::data::function::{Function, FunctionMetadata};
use crate::interpreter::{operators, Interpreter, InterpretingError};
use crate::interpreter::data::{DataObjectRef, OptionDataObjectRef};
use crate::lexer::CodePosition;
use crate::utils;

pub fn add_functions(functions: &mut Vec<(FunctionMetadata, Function)>) {
    functions.push(crate::lang_func!(
            comb_bn_function,
            crate::lang_func_metadata!(
                name="combBN",
                combinator_function=true,
                info="Combinator execution: a(b(args[0]), b(args[1]), ...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn comb_bn_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        b: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let mut args_a = Vec::with_capacity(args.len());
        for arg in args {
            let ret_b = operators::op_call(
                interpreter,
                &b,
                &[arg],
                CodePosition::EMPTY,
            );
            args_a.push(utils::none_to_lang_void(ret_b));
        }
        args_a = utils::separate_arguments_with_argument_separators(&args_a);

        operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
    }

    functions.push(crate::lang_func!(
            comb_bv_function,
            crate::lang_func_metadata!(
                name="combBV",
                combinator_function=true,
                info="Combinator execution: a(b(args[0]), b(args[1]), ...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
            ),
        ));
    fn comb_bv_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        b: DataObjectRef,
        args: DataObjectRef,
    ) -> OptionDataObjectRef {
        let args = args.array_value().unwrap().borrow().clone();

        let mut args_a = Vec::with_capacity(args.len());
        for arg in args {
            let ret_b = operators::op_call(
                interpreter,
                &b,
                &[arg],
                CodePosition::EMPTY,
            );
            args_a.push(utils::none_to_lang_void(ret_b));
        }
        args_a = utils::separate_arguments_with_argument_separators(&args_a);

        operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
    }

    functions.push(crate::lang_func!(
            comb_bz_function,
            crate::lang_func_metadata!(
                name="combBZ",
                combinator_function=true,
                info="Combinator execution: a(..., b(args[1]), b(args[0]))",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn comb_bz_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        b: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let mut args_a = Vec::with_capacity(args.len());
        for arg in args.into_iter().rev() {
            let ret_b = operators::op_call(
                interpreter,
                &b,
                &[arg],
                CodePosition::EMPTY,
            );
            args_a.push(utils::none_to_lang_void(ret_b));
        }
        args_a = utils::separate_arguments_with_argument_separators(&args_a);

        operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
    }

    functions.push(crate::lang_func!(
            comb_nn_function,
            crate::lang_func_metadata!(
                name="combNN",
                combinator_function=true,
                info="Combinator execution: a(args[0])(args[1])(...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn comb_nn_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut ret = a;
        for (i, n) in args.into_iter().
                enumerate() {
            if !utils::is_callable(&ret) {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The return value after iteration {} must be callable",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            }

            ret = utils::none_to_lang_void(
                operators::op_call(interpreter, &ret, &[n], CodePosition::EMPTY),
            );
        }

        ret
    }

    functions.push(crate::lang_func!(
            comb_nv_function,
            crate::lang_func_metadata!(
                name="combNV",
                combinator_function=true,
                info="Combinator execution: a(args[0])(args[1])(...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
            ),
        ));
    fn comb_nv_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        args: DataObjectRef,
    ) -> DataObjectRef {
        let args = args.array_value().unwrap().borrow().clone();

        let mut ret = a;
        for (i, n) in <Box<[_]> as IntoIterator>::into_iter(args).
                enumerate() {
            if !utils::is_callable(&ret) {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The return value after iteration {} must be callable",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            }

            ret = utils::none_to_lang_void(
                operators::op_call(interpreter, &ret, &[n], CodePosition::EMPTY),
            );
        }

        ret
    }

    functions.push(crate::lang_func!(
            comb_nz_function,
            crate::lang_func_metadata!(
                name="combNZ",
                combinator_function=true,
                info="Combinator execution: a(...)(args[1])(args[0])",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn comb_nz_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut ret = a;
        for (i, n) in args.into_iter().
                rev().
                enumerate() {
            if !utils::is_callable(&ret) {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The return value after iteration {} must be callable",
                        i + 1,
                    )),
                    CodePosition::EMPTY,
                );
            }

            ret = utils::none_to_lang_void(
                operators::op_call(interpreter, &ret, &[n], CodePosition::EMPTY),
            );
        }

        ret
    }

    functions.push(crate::lang_func!(
            comb_pn_function,
            crate::lang_func_metadata!(
                name="combPN",
                combinator_function=true,
                info="Combinator execution: a(args[0](b), args[1](b), ...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn comb_pn_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        b: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let mut args_a = Vec::with_capacity(args.len());
        for (i, n) in args.iter().
                enumerate() {
            if !utils::is_callable(n) {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The value at index {} must be callable",
                        i + 3,
                    )),
                    CodePosition::EMPTY,
                ));
            }

            let ret_n = operators::op_call(
                interpreter,
                n,
                &[b.clone()],
                CodePosition::EMPTY,
            );
            args_a.push(utils::none_to_lang_void(ret_n));
        }
        args_a = utils::separate_arguments_with_argument_separators(&args_a);

        operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
    }

    functions.push(crate::lang_func!(
            comb_pv_function,
            crate::lang_func_metadata!(
                name="combPV",
                combinator_function=true,
                info="Combinator execution: a(args[0](b), args[1](b), ...)",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                ),
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
            ),
        ));
    fn comb_pv_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        b: DataObjectRef,
        args: DataObjectRef,
    ) -> OptionDataObjectRef {
        let args = args.array_value().unwrap().borrow().clone();

        let mut args_a = Vec::with_capacity(args.len());
        for (i, n) in args.iter().
                enumerate() {
            if !utils::is_callable(n) {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "Value at index {} of Argument 3 (\"&args\") must be callable",
                        i,
                    )),
                    CodePosition::EMPTY,
                ));
            }

            let ret_n = operators::op_call(
                interpreter,
                n,
                &[b.clone()],
                CodePosition::EMPTY,
            );
            args_a.push(utils::none_to_lang_void(ret_n));
        }
        args_a = utils::separate_arguments_with_argument_separators(&args_a);

        operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
    }

    functions.push(crate::lang_func!(
            comb_pz_function,
            crate::lang_func_metadata!(
                name="combPZ",
                combinator_function=true,
                info="Combinator execution: a(..., args[1](b), args[0](b))",
                parameter(
                    name="$a",
                    parameter_type(callable),
                ),
                parameter(
                    name="$b",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn comb_pz_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        b: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> OptionDataObjectRef {
        let mut args_a = Vec::with_capacity(args.len());
        for (i, n) in args.iter().
                enumerate().
                rev() {
            if !utils::is_callable(n) {
                return Some(interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The value at index {} must be callable",
                        i + 3,
                    )),
                    CodePosition::EMPTY,
                ));
            }

            let ret_n = operators::op_call(
                interpreter,
                n,
                &[b.clone()],
                CodePosition::EMPTY,
            );
            args_a.push(utils::none_to_lang_void(ret_n));
        }
        args_a = utils::separate_arguments_with_argument_separators(&args_a);

        operators::op_call(interpreter, &a, &args_a, CodePosition::EMPTY)
    }

    functions.push(crate::lang_func!(
            comb_qn_function,
            crate::lang_func_metadata!(
                name="combQN",
                combinator_function=true,
                info="Combinator execution: ...(args[1](args[0](a)))",
                parameter(
                    name="$a",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn comb_qn_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut ret = a;
        for (i, n) in args.into_iter().
                enumerate() {
            if !utils::is_callable(&n) {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The value at index {} must be callable",
                        i + 2,
                    )),
                    CodePosition::EMPTY,
                );
            }

            ret = utils::none_to_lang_void(
                operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
            );
        }

        ret
    }

    functions.push(crate::lang_func!(
            comb_qv_function,
            crate::lang_func_metadata!(
                name="combQV",
                combinator_function=true,
                info="Combinator execution: ...(args[1](args[0](a)))",
                parameter(
                    name="$a",
                ),
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
            ),
        ));
    fn comb_qv_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        args: DataObjectRef,
    ) -> DataObjectRef {
        let args = args.array_value().unwrap().borrow().clone();

        let mut ret = a;
        for (i, n) in <Box<[_]> as IntoIterator>::into_iter(args).
                enumerate() {
            if !utils::is_callable(&n) {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "Value at index {} of Argument 2 (\"&args\") must be callable",
                        i,
                    )),
                    CodePosition::EMPTY,
                );
            }

            ret = utils::none_to_lang_void(
                operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
            );
        }

        ret
    }

    functions.push(crate::lang_func!(
            comb_qz_function,
            crate::lang_func_metadata!(
                name="combQZ",
                combinator_function=true,
                info="Combinator execution: args[0](args[1](...(a)))",
                parameter(
                    name="$a",
                ),
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
            ),
        ));
    fn comb_qz_function(
        interpreter: &mut Interpreter,
        a: DataObjectRef,
        args: Vec<DataObjectRef>,
    ) -> DataObjectRef {
        let mut ret = a;
        for (i, n) in args.into_iter().
                enumerate().
                rev() {
            if !utils::is_callable(&n) {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The value at index {} must be callable",
                        i + 2,
                    )),
                    CodePosition::EMPTY,
                );
            }

            ret = utils::none_to_lang_void(
                operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
            );
        }

        ret
    }

    functions.push(crate::lang_func!(
            comb_tn_function,
            crate::lang_func_metadata!(
                name="combTN",
                combinator_function=true,
                info="Combinator execution: ...(args[1](args[0](z)))",
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
                parameter(
                    name="$z",
                ),
            ),
        ));
    fn comb_tn_function(
        interpreter: &mut Interpreter,
        args: Vec<DataObjectRef>,
        z: DataObjectRef,
    ) -> DataObjectRef {
        let mut ret = z;
        for (i, n) in args.into_iter().
                enumerate() {
            if !utils::is_callable(&n) {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The value at index {} must be callable",
                        i + 2,
                    )),
                    CodePosition::EMPTY,
                );
            }

            ret = utils::none_to_lang_void(
                operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
            );
        }

        ret
    }

    functions.push(crate::lang_func!(
            comb_tv_function,
            crate::lang_func_metadata!(
                name="combTV",
                combinator_function=true,
                info="Combinator execution: ...(args[1](args[0](z)))",
                parameter(
                    name="&args",
                    type_constraint(
                        allowed=["ARRAY"],
                    ),
                ),
                parameter(
                    name="$z",
                ),
            ),
        ));
    fn comb_tv_function(
        interpreter: &mut Interpreter,
        args: DataObjectRef,
        z: DataObjectRef,
    ) -> DataObjectRef {
        let args = args.array_value().unwrap().borrow().clone();

        let mut ret = z;
        for (i, n) in <Box<[_]> as IntoIterator>::into_iter(args).
                enumerate() {
            if !utils::is_callable(&n) {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "Value at index {} of Argument 2 (\"&args\") must be callable",
                        i,
                    )),
                    CodePosition::EMPTY,
                );
            }

            ret = utils::none_to_lang_void(
                operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
            );
        }

        ret
    }

    functions.push(crate::lang_func!(
            comb_tz_function,
            crate::lang_func_metadata!(
                name="combTZ",
                combinator_function=true,
                info="Combinator execution: args[0](args[1](...(z)))",
                parameter(
                    name="&args",
                    parameter_type(var_args),
                ),
                parameter(
                    name="$z",
                ),
            ),
        ));
    fn comb_tz_function(
        interpreter: &mut Interpreter,
        args: Vec<DataObjectRef>,
        z: DataObjectRef,
    ) -> DataObjectRef {
        let mut ret = z;
        for (i, n) in args.into_iter().
                enumerate().
                rev() {
            if !utils::is_callable(&n) {
                return interpreter.set_errno_error_object(
                    InterpretingError::InvalidArguments,
                    Some(&format!(
                        "The value at index {} must be callable",
                        i + 2,
                    )),
                    CodePosition::EMPTY,
                );
            }

            ret = utils::none_to_lang_void(
                operators::op_call(interpreter, &n, &[ret.clone()], CodePosition::EMPTY),
            );
        }

        ret
    }
}
