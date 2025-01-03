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
pub use lang_interpreter_macros::lang_func_id;
pub use lang_interpreter_macros::lang_func_metadata;
pub use lang_interpreter_macros::lang_func_adapter;
use lang_interpreter_macros::internal_tuple_from_lang_args_impl;
use lang_interpreter_macros::internal_native_function_adapter_impl;
