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

