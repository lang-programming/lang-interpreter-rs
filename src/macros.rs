/// This macro generates a [Function](crate::interpreter::data::function::Function)
/// instance for the given function identifier and metadata combination
///
/// The return type of this macro is `(FunctionMetadata, Box<dyn NativeFunctionAdapter>)`
///
/// The function provided to this macro must start with a parameter of type `&mut Interpreter` and
/// the must be an implementation of [FromLangArgs](lang_interpreter::interpreter::data::function::native::FromLangArgs)
/// for the tuple of all parameter types after the `&mut Interpreter`.
///
/// The return type of the function must one of the following:
/// - `Result<OptionDataObjectRef, NativeError>`
/// - `Result<DataObjectRet, NativeError>`
/// - `Result<(), NativeError>`
/// - `OptionDataObjectRef`
/// - `DataObjectRef`
/// - `()` (= No return value)
///
/// If move is used to capture local values in a closure,
/// a copy for basic values (like i32) or a clone for Arc/Rc/Gc values must be created and must be
/// provided to the second argument (`$value_dependencies`) (After function name and before function metadata) of this macro
///
/// # Arguments
///
/// ## 2 Argument variant
///
/// * `$func` - An identifier of a rust function or lambda
/// * `$metadata` - The Lang function metadata ([FunctionMetadata](crate::interpreter::data::function::FunctionMetadata))
///
/// ## 3 Argument variant
///
/// * `$func` - An identifier of a rust function or lambda
/// * `$value_dependencies` - A vector (`vec![val1, val2, ...]`) which contains all dependencies which are captured inside closure function
///                           (This is important for the equals and strict equals operators when comparing lang functions)
/// * `$metadata` - The Lang function metadata ([FunctionMetadata](crate::interpreter::data::function::FunctionMetadata))
///
/// # Examples
///
/// ```
/// use lang_interpreter::interpreter::data::{DataObject, DataObjectRef};
/// use lang_interpreter::interpreter::data::function::{Function, FunctionMetadata, FunctionPointerObject, Parameter, ParameterType};
/// use lang_interpreter::interpreter::{conversions, Interpreter};
/// use lang_interpreter::{lang_func, lang_func_metadata};
/// use lang_interpreter::lexer::CodePosition;
///
/// //The rust implementation of the lang native function "func.trim"
/// fn trim_function(
///     interpreter: &mut Interpreter,
///     text_object: DataObjectRef,
/// ) -> DataObjectRef {
///     DataObjectRef::new(DataObject::new_text(
///         conversions::to_text(interpreter, &text_object, CodePosition::EMPTY).trim(),
///     ))
/// }
///
/// let (metadata, function): (FunctionMetadata, Function) = lang_func!(
///     trim_function,
///     lang_func_metadata!(
///         name="trim",
///         return_type_constraint(
///             allowed=["TEXT"],
///         ),
///         parameter(
///             name="$text",
///         ),
///     ),
/// );
/// ```
///
/// `FunctionPointerObject::new` can be used to create a [FunctionPointerObject](crate::interpreter::data::function::FunctionPointerObject) with a single non-overloaded function:
/// ```
/// # use lang_interpreter::interpreter::data::{DataObject, DataObjectRef};
/// # use lang_interpreter::interpreter::data::function::{Function, FunctionMetadata, Parameter, ParameterType};
/// # use lang_interpreter::interpreter::{conversions, Interpreter};
/// # use lang_interpreter::{lang_func, lang_func_metadata};
/// # use lang_interpreter::lexer::CodePosition;
/// #
/// # fn trim_function(
/// #     interpreter: &mut Interpreter,
/// #     text_object: DataObjectRef,
/// # ) -> DataObjectRef {
/// #     DataObjectRef::new(DataObject::new_text(
/// #         conversions::to_text(interpreter, &text_object, CodePosition::EMPTY).trim(),
/// #     ))
/// # }
/// #
/// # let (metadata, function): (FunctionMetadata, Function) = lang_func!(
/// #     trim_function,
/// #     lang_func_metadata!(
/// #         name="trim",
/// #         return_type_constraint(
/// #             allowed=["TEXT"],
/// #         ),
/// #         parameter(
/// #             name="$text",
/// #         ),
/// #     ),
/// # );
/// #
/// use lang_interpreter::interpreter::data::function::FunctionPointerObject;
///
/// //`(metadata, function)` is used from the example above
///
/// let fp: FunctionPointerObject = FunctionPointerObject::new(&metadata, function);
///
/// assert_eq!(fp.function_name(), Some("trim"));
/// assert_eq!(fp.function_info(), None);
/// assert_eq!(fp.functions().len(), 1);
/// ```
///
/// Overloaded functions can be created with `FunctionPointerObject::create_function_pointer_objects_from_native_functions`:
/// ```
/// # use lang_interpreter::interpreter::data::{DataObject, DataObjectRef};
/// # use lang_interpreter::interpreter::data::function::{Function, FunctionMetadata, Parameter, ParameterType};
/// # use lang_interpreter::interpreter::{conversions, Interpreter};
/// # use lang_interpreter::{lang_func, lang_func_metadata};
/// # use lang_interpreter::lexer::CodePosition;
/// #
/// # fn trim_function(
/// #     interpreter: &mut Interpreter,
/// #     text_object: DataObjectRef,
/// # ) -> DataObjectRef {
/// #     DataObjectRef::new(DataObject::new_text(
/// #         conversions::to_text(interpreter, &text_object, CodePosition::EMPTY).trim(),
/// #     ))
/// # }
/// #
/// # let (metadata, function): (FunctionMetadata, Function) = lang_func!(
/// #     trim_function,
/// #     lang_func_metadata!(
/// #         name="trim",
/// #         return_type_constraint(
/// #             allowed=["TEXT"],
/// #         ),
/// #         parameter(
/// #             name="$text",
/// #         ),
/// #     ),
/// # );
/// #
/// use std::collections::HashMap;
/// use lang_interpreter::interpreter::data::function::FunctionPointerObject;
///
/// //`(metadata, function)` is used from the example above
///
/// let overloaded_functions: HashMap<Box<str>, FunctionPointerObject> = FunctionPointerObject::create_function_pointer_objects_from_native_functions(vec![
///     (metadata, function),
/// ]);
///
/// assert_eq!(overloaded_functions.len(), 1);
///
/// let fp = &overloaded_functions["trim"];
///
/// assert_eq!(fp.function_name(), Some("trim"));
/// assert_eq!(fp.function_info(), None);
/// assert_eq!(fp.functions().len(), 1);
/// ```
#[macro_export]
macro_rules! lang_func {
    ( $func:ident, $metadata:expr $(,)? ) => {{
        let (metadata, func, id) = $crate::lang_func_adapter!($func, $metadata);

        let native_function = $crate::interpreter::data::function::native::create_native_function(func, id, ::std::vec![]);
        let function = $crate::interpreter::data::function::Function::new_native(native_function, &metadata);

        (metadata, function)
    }};

    ( $func:ident, $value_dependencies:expr, $metadata:expr $(,)? ) => {{
        let (metadata, func, id) = $crate::lang_func_adapter!($func, $metadata);

        let native_function = $crate::interpreter::data::function::native::create_native_function(func, id, $value_dependencies);
        let function = $crate::interpreter::data::function::Function::new_native(native_function, &metadata);

        (metadata, function)
    }};
}

#[macro_use]
pub(crate) mod lang_io {
    macro_rules! lang_print {
        ($($arg:tt)*) => {{
            #[cfg(not(feature = "wasm"))]
            {
                print!($($arg)*)
            }
            #[cfg(feature = "wasm")]
            {
                let text = format!($($arg)*);

                if text.is_empty() {
                    web_sys::console::log_0();
                }else {
                    web_sys::console::log_1(&text.into());
                }
            }
        }};
    }

    macro_rules! lang_println {
        () => {
            #[cfg(not(feature = "wasm"))]
            {
                println!();
            }
            #[cfg(feature = "wasm")]
            {
                web_sys::console::log_0();
            }
        };
        ($($arg:tt)*) => {{
            #[cfg(not(feature = "wasm"))]
            {
                println!($($arg)*)
            }
            #[cfg(feature = "wasm")]
            {
                let text = format!($($arg)*);

                if text.is_empty() {
                    web_sys::console::log_0();
                }else {
                    web_sys::console::log_1(&text.into());
                }
            }
        }};
    }

    macro_rules! lang_eprint {
        ($($arg:tt)*) => {{
            #[cfg(not(feature = "wasm"))]
            {
                eprint!($($arg)*)
            }
            #[cfg(feature = "wasm")]
            {
                let text = format!($($arg)*);

                if text.is_empty() {
                    web_sys::console::error_0();
                }else {
                    web_sys::console::error_1(&text.into());
                }
            }
        }};
    }

    macro_rules! lang_eprintln {
        () => {
            #[cfg(not(feature = "wasm"))]
            {
                eprintln!();
            }
            #[cfg(feature = "wasm")]
            {
                web_sys::console::error_0();
            }
        };
        ($($arg:tt)*) => {{
            #[cfg(not(feature = "wasm"))]
            {
                eprintln!($($arg)*)
            }
            #[cfg(feature = "wasm")]
            {
                let text = format!($($arg)*);

                if text.is_empty() {
                    web_sys::console::error_0();
                }else {
                    web_sys::console::error_1(&text.into());
                }
            }
        }};
    }
}
