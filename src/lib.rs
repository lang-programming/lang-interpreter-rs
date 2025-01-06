pub(crate) mod regex_patterns;

#[macro_use]
#[doc(hidden)]
pub mod macros;

pub mod terminal_io;
pub mod lexer;
pub mod parser;
pub mod utils;
pub mod interpreter;

extern crate self as lang_interpreter;

extern crate lang_interpreter_macros;

/// This macro generates a unique ID for Lang Native Function
///
/// This macro is used within the [crate::lang_func_adapter!] macro
pub use lang_interpreter_macros::lang_func_id;

/// This macro generates an instance of [FunctionMetadata](crate::interpreter::data::function::FunctionMetadata)
///
/// It should be used in combination with [crate::lang_func!]
pub use lang_interpreter_macros::lang_func_metadata;

/// This macro generates a boxed [NativeFunctionAdapter](crate::interpreter::data::function::native::NativeFunctionAdapter)
/// instance for the given function identifier and metadata combination
///
/// The return type of this macro is `(FunctionMetadata, Box<dyn NativeFunctionAdapter>, NativeFuncId)`
///
/// This macro is used within the [crate::lang_func!] macro
pub use lang_interpreter_macros::lang_func_adapter;

use lang_interpreter_macros::internal_tuple_from_lang_args_impl;
use lang_interpreter_macros::internal_native_function_adapter_impl;
