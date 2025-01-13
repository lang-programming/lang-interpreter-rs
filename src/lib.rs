#![warn(clippy::allow_attributes)]
#![warn(clippy::assigning_clones)]
#![warn(clippy::cloned_instead_of_copied)]
#![warn(clippy::collection_is_never_read)]
#![warn(clippy::debug_assert_with_mut_call)]
#![warn(clippy::filetype_is_file)]
#![warn(clippy::filter_map_next)]
#![warn(clippy::flat_map_option)]
#![warn(clippy::fn_to_numeric_cast_any)]
#![warn(clippy::format_push_string)]
#![warn(clippy::implicit_clone)]
#![warn(clippy::imprecise_flops)]
#![warn(clippy::inconsistent_struct_constructor)]
#![warn(clippy::inefficient_to_string)]
#![warn(clippy::iter_filter_is_ok)]
#![warn(clippy::iter_filter_is_some)]
#![warn(clippy::large_types_passed_by_value)]
#![warn(clippy::manual_string_new)]
#![warn(clippy::option_as_ref_cloned)]
#![warn(clippy::renamed_function_params)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::suboptimal_flops)]
#![warn(clippy::too_long_first_doc_paragraph)]

//Triggered by for Trace and Finalize derive
#![allow(non_local_definitions)]

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
