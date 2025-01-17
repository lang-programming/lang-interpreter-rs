mod util;

use proc_macro::TokenStream;
use darling::ast::NestedMeta;
use darling::{Error, FromMeta};
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::LitStr;
use syn::parse::Parser;

#[derive(Debug, FromMeta)]
enum TypeConstraintMacroArgs {
    #[darling(rename = "allowed")]
    Allowed(Vec<LitStr>),

    #[darling(rename = "not_allowed")]
    NotAllowed(Vec<LitStr>),
}

#[derive(Debug, Default, FromMeta)]
enum ParameterTypeMacroArgs {
    #[default]
    Normal,

    #[darling(rename = "number")]
    Number,

    #[darling(rename = "callable")]
    Callable,

    #[darling(rename = "boolean")]
    Boolean,

    #[darling(rename = "call_by_pointer")]
    CallByPointer,

    #[darling(rename = "var_args")]
    VarArgs,

    #[darling(rename = "raw_var_args")]
    RawVarArgs,
}

impl ParameterTypeMacroArgs {
    fn type_name(&self) -> &'static str {
        match self {
            ParameterTypeMacroArgs::Normal => "Normal",
            ParameterTypeMacroArgs::Number => "Number",
            ParameterTypeMacroArgs::Callable => "Callable",
            ParameterTypeMacroArgs::Boolean => "Boolean",
            ParameterTypeMacroArgs::CallByPointer => "CallByPointer",
            ParameterTypeMacroArgs::VarArgs => "VarArgs",
            ParameterTypeMacroArgs::RawVarArgs => "RawVarArgs",
        }
    }
}

#[derive(Debug, FromMeta)]
struct ParameterMacroArgs {
    name: String,

    #[darling(default)]
    info: Option<String>,

    #[darling(default)]
    parameter_type: ParameterTypeMacroArgs,

    #[darling(default)]
    type_constraint: Option<TypeConstraintMacroArgs>,
}

#[derive(Debug, FromMeta)]
struct DeprecationInfo {
    #[darling(default)]
    remove_version: Option<String>,

    #[darling(default)]
    replacement_function: Option<String>,
}

#[derive(Debug, FromMeta)]
struct LangFuncMetadata {
    name: String,

    #[darling(default)]
    info: Option<String>,

    #[darling(multiple, rename="parameter")]
    parameters: Vec<ParameterMacroArgs>,

    #[darling(default)]
    has_info: bool,

    #[darling(default)]
    combinator_function: bool,

    #[darling(default)]
    linker_function: bool,

    #[darling(default)]
    deprecated: Option<DeprecationInfo>,

    #[darling(default)]
    return_type_constraint: Option<TypeConstraintMacroArgs>,
}

#[proc_macro]
pub fn lang_func_id(
    args: TokenStream
) -> TokenStream {
    let func_id_ident = syn::parse_macro_input!(args as syn::Ident);

    let native_func_id_path: proc_macro2::TokenStream = "::lang_interpreter::interpreter::data::function::native::NativeFuncId".parse().unwrap();
    let gen_next_native_func_id_path: proc_macro2::TokenStream = "::lang_interpreter::interpreter::data::function::native::gen_next_native_func_id".parse().unwrap();
    let lazy_lock_path: proc_macro2::TokenStream = "::std::sync::LazyLock".parse().unwrap();

    TokenStream::from(quote! {
        static #func_id_ident: #lazy_lock_path<#native_func_id_path, fn() -> #native_func_id_path> = #lazy_lock_path::new(#gen_next_native_func_id_path);

        //Generate new ID instantly
        let _ = *#func_id_ident;
    })
}

#[proc_macro]
pub fn lang_func_metadata(
    args: TokenStream,
) -> TokenStream {
    let attr_args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(Error::from(e).write_errors());
        }
    };

    let args = match LangFuncMetadata::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    let func_name = util::option_string_to_token_stream(&Some(args.name));
    let func_info = util::option_string_to_token_stream(&args.info);

    let has_info: proc_macro2::TokenStream = args.has_info.to_string().parse().unwrap();

    let combinator_function: proc_macro2::TokenStream = args.combinator_function.to_string().parse().unwrap();

    let linker_function: proc_macro2::TokenStream = args.linker_function.to_string().parse().unwrap();

    let deprecation_info = util::option_deprecation_info_to_token_stream(&args.deprecated);

    let vec_macro_path: proc_macro2::TokenStream = "::std::vec!".parse().unwrap();
    let parameter_metadata_path: proc_macro2::TokenStream = "::lang_interpreter::interpreter::data::function::ParameterMetadata".parse().unwrap();
    let parameter_type_path: proc_macro2::TokenStream = "::lang_interpreter::interpreter::data::function::ParameterType".parse().unwrap();

    let mut parameters = Vec::new();
    for parameter in &args.parameters {
        let parameter_name = proc_macro2::Literal::string(&parameter.name);

        let parameter_info = util::option_string_to_token_stream(&parameter.info);

        let parameter_data_type_constraint = util::option_type_constraint_macro_args_to_token_stream(&parameter.type_constraint);

        let parameter_type = proc_macro2::Ident::new(
            parameter.parameter_type.type_name(),
            Span::mixed_site(),
        );

        parameters.push(quote! {
            #parameter_metadata_path::new(
                #parameter_name,
                #parameter_info,
                #parameter_data_type_constraint,
                #parameter_type_path::#parameter_type,
            )
        });
    }

    let return_value_type_constraint = util::option_type_constraint_macro_args_to_token_stream(&args.return_type_constraint);

    let function_metadata_path: proc_macro2::TokenStream = "::lang_interpreter::interpreter::data::function::FunctionMetadata".parse().unwrap();

    TokenStream::from(quote!{
        #function_metadata_path::new(
            #func_name,
            #func_info,

            #has_info,

            #combinator_function,

            #linker_function,
            #deprecation_info,

            #vec_macro_path[
                #(#parameters,)*
            ],

            #return_value_type_constraint,
        )
    })
}

#[proc_macro]
pub fn lang_func_adapter(
    args: TokenStream,
) -> TokenStream {
    let data = syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated.parse(args);
    let data = match data {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(Error::from(e).write_errors());
        }
    };

    if data.len() != 2 {
        return TokenStream::from(quote!{
            compile_error!("Expected two arguments: function identifier, metadata")
        });
    }

    let func_ident = &data[0];
    let metadata = &data[1];

    let box_path: proc_macro2::TokenStream = "::std::boxed::Box".parse().unwrap();

    let lang_func_id_macro_path: proc_macro2::TokenStream = "::lang_interpreter::lang_func_id!".parse().unwrap();
    let func_trait_path: proc_macro2::TokenStream =
            "::lang_interpreter::interpreter::data::function::native::ConvertToFuncTrait::func_trait".parse().unwrap();
    let native_function_adapter_path: proc_macro2::TokenStream =
            "::lang_interpreter::interpreter::data::function::native::NativeFunctionAdapter".parse().unwrap();

    let span = Span::mixed_site();

    quote_spanned!(span => {
        #lang_func_id_macro_path(FUNC_ID);

        (
            #metadata,
            #box_path::new(#func_trait_path(#func_ident)) as #box_path<dyn #native_function_adapter_path + 'static>,
            *FUNC_ID,
        )
    }).into()
}

#[doc(hidden)]
#[proc_macro]
pub fn internal_tuple_from_lang_args_impl(
    args: TokenStream,
) -> TokenStream {
    let count = syn::parse_macro_input!(args as syn::LitInt).base10_parse::<usize>();
    let count = match count {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.into_compile_error());
        }
    };

    let data_object_ref_ident = proc_macro2::Ident::new(
        "DataObjectRef",
        Span::mixed_site(),
    );

    let args_tokens = vec![data_object_ref_ident; count];

    let count_lit = proc_macro2::Literal::usize_unsuffixed(count);
    let count_plus_1_lit = proc_macro2::Literal::usize_unsuffixed(count + 1);

    let next_ident = proc_macro2::Ident::new(
        "next",
        Span::mixed_site(),
    );
    let next_tokens = vec![next_ident; count];

    //TODO implement var args (normal and raw) in any position (text is of type DataObject anyway)

    TokenStream::from(quote! {
        //Args only
        impl FromLangArgs for (#(#args_tokens,)*) {
            fn from_lang_args(
                this_object: OptionLangObjectRef,
                args: Vec<DataObjectRef>,
            ) -> Result<Self> {
                if args.len() != #count_lit {
                    return Err(NativeError::new("Invalid argument count for native function", None));
                }

                if this_object.is_some() {
                    return Err(NativeError::new("This object may not be set for native function without a this parameter", None));
                }
                
                let mut args = args;
                let mut args = args.drain(..);

                Ok((#(args.#next_tokens().unwrap(),)*))
            }

            fn lang_parameter_count() -> usize {
                #count_lit
            }

            fn is_method() -> bool {
                false
            }
        }

        //VarArgs + args
        impl FromLangArgs for (Vec<DataObjectRef>, #(#args_tokens,)*) {
            fn from_lang_args(
                this_object: OptionLangObjectRef,
                args: Vec<DataObjectRef>,
            ) -> Result<Self> {
                if args.len() < #count_lit {
                    return Err(NativeError::new("Not enough arguments for native function", None));
                }

                if this_object.is_some() {
                    return Err(NativeError::new("This object may not be set for native function without a this parameter", None));
                }

                let var_args_count = args.len() - #count_lit;

                let mut args = args;
                let mut var_args = args.split_off(var_args_count);
                mem::swap(&mut args, &mut var_args);
                
                let mut args = args.drain(..);

                Ok((var_args, #(args.#next_tokens().unwrap(),)*))
            }

            fn lang_parameter_count() -> usize {
                #count_plus_1_lit
            }

            fn is_method() -> bool {
                false
            }
        }

        //Args + VarArgs
        impl FromLangArgs for (#(#args_tokens,)* Vec<DataObjectRef>,) {
            fn from_lang_args(
                this_object: OptionLangObjectRef,
                args: Vec<DataObjectRef>,
            ) -> Result<Self> {
                if args.len() < #count_lit {
                    return Err(NativeError::new("Not enough arguments for native function", None));
                }

                if this_object.is_some() {
                    return Err(NativeError::new("This object may not be set for native function without a this parameter", None));
                }

                let mut args = args;
                let var_args = args.split_off(#count_lit);
                
                let mut args = args.drain(..);

                Ok((#(args.#next_tokens().unwrap(),)* var_args,))
            }

            fn lang_parameter_count() -> usize {
                #count_plus_1_lit
            }

            fn is_method() -> bool {
                false
            }
        }

        //This arg + args
        impl FromLangArgs for (LangObjectRef, #(#args_tokens,)*) {
            fn from_lang_args(
                this_object: OptionLangObjectRef,
                args: Vec<DataObjectRef>,
            ) -> Result<Self> {
                if args.len() != #count_lit {
                    return Err(NativeError::new("Invalid argument count for native function", None));
                }

                let Some(this_object) = this_object.as_ref() else {
                    return Err(NativeError::new("This object must be set for native function with a this parameter", None));
                };

                let mut args = args;
                let mut args = args.drain(..);

                Ok((this_object.clone(), #(args.#next_tokens().unwrap(),)*))
            }

            fn lang_parameter_count() -> usize {
                #count_lit
            }

            fn is_method() -> bool {
                true
            }
        }
        
        //This arg + VarArgs + args
        impl FromLangArgs for (LangObjectRef, Vec<DataObjectRef>, #(#args_tokens,)*) {
            fn from_lang_args(
                this_object: OptionLangObjectRef,
                args: Vec<DataObjectRef>,
            ) -> Result<Self> {
                if args.len() < #count_lit {
                    return Err(NativeError::new("Not enough arguments for native function", None));
                }

                let Some(this_object) = this_object.as_ref() else {
                    return Err(NativeError::new("This object must be set for native function with a this parameter", None));
                };

                let var_args_count = args.len() - #count_lit;

                let mut args = args;
                let mut var_args = args.split_off(var_args_count);
                mem::swap(&mut args, &mut var_args);

                let mut args = args.drain(..);

                Ok((this_object.clone(), var_args, #(args.#next_tokens().unwrap(),)*))
            }

            fn lang_parameter_count() -> usize {
                #count_plus_1_lit
            }

            fn is_method() -> bool {
                true
            }
        }

        //This arg + args + VarArgs
        impl FromLangArgs for (LangObjectRef, #(#args_tokens,)* Vec<DataObjectRef>,) {
            fn from_lang_args(
                this_object: OptionLangObjectRef,
                args: Vec<DataObjectRef>,
            ) -> Result<Self> {
                if args.len() < #count_lit {
                    return Err(NativeError::new("Not enough arguments for native function", None));
                }

                let Some(this_object) = this_object.as_ref() else {
                    return Err(NativeError::new("This object must be set for native function with a this parameter", None));
                };

                let mut args = args;
                let var_args = args.split_off(#count_lit);

                let mut args = args.drain(..);

                Ok((this_object.clone(), #(args.#next_tokens().unwrap(),)* var_args,))
            }

            fn lang_parameter_count() -> usize {
                #count_plus_1_lit
            }

            fn is_method() -> bool {
                true
            }
        }
    })
}

#[doc(hidden)]
#[proc_macro]
pub fn internal_native_function_adapter_impl(
    args: TokenStream,
) -> TokenStream {
    let count = syn::parse_macro_input!(args as syn::LitInt).base10_parse::<usize>();
    let count = match count {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.into_compile_error());
        }
    };

    let index_literal_tokens = (0..count).
            map(proc_macro2::Literal::usize_unsuffixed).
            collect::<Vec<_>>();

    let generic_type_tokens = (0..count).
            map(|i| proc_macro2::Ident::new(
                &format!("T{}", i + 1),
                Span::mixed_site(),
            )).
            collect::<Vec<_>>();

    TokenStream::from(quote! {
        impl<
            #(#generic_type_tokens,)*
            Ret: ReturnType,
            F: Fn(&mut Interpreter, #(#generic_type_tokens,)*) -> Ret + 'static,
        > ConvertToFuncTrait<Box<dyn Fn(&mut Interpreter, #(#generic_type_tokens,)*) -> Ret + 'static>> for F {
            #[inline(always)]
            fn func_trait(self) -> Box<dyn Fn(&mut Interpreter, #(#generic_type_tokens,)*) -> Ret + 'static> {
                Box::new(self)
            }
        }

        impl<
            #(#generic_type_tokens,)*
            Ret: ReturnType,
        > NativeFunctionAdapter for Box<dyn Fn(&mut Interpreter, #(#generic_type_tokens,)*) -> Ret> where
                (#(#generic_type_tokens,)*): FromLangArgs,
        {
            fn lang_call(
                &self,
                interpreter: &mut Interpreter,
                this_object: OptionLangObjectRef,
                args: Vec<DataObjectRef>,
            ) -> Result<OptionDataObjectRef> {
                let args = <(#(#generic_type_tokens,)*)>::from_lang_args(this_object, args)?;

                self(interpreter, #(args.#index_literal_tokens,)*).into()
            }

            fn lang_parameter_count(&self) -> usize {
                <(#(#generic_type_tokens,)*)>::lang_parameter_count()
            }

            fn is_method(&self) -> bool {
                <(#(#generic_type_tokens,)*)>::is_method()
            }
        }

        impl<
            #(#generic_type_tokens,)*
            Ret: ReturnType,
        > private::Sealed for Box<dyn Fn(&mut Interpreter, #(#generic_type_tokens,)*) -> Ret> where
                (#(#generic_type_tokens,)*): FromLangArgs,
        {}
    })
}
