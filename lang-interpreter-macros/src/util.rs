use proc_macro2::Span;
use quote::quote;
use crate::{DeprecationInfo, TypeConstraintMacroArgs};

pub fn option_string_to_token_stream(str_option: &Option<String>) -> proc_macro2::TokenStream {
    let option_path: proc_macro2::TokenStream = "::std::option::Option".parse().unwrap();

    if let Some(val) = str_option {
        let lit = proc_macro2::Literal::string(val);

        quote! {
            #option_path::Some(#lit)
        }
    }else {
        quote! {
            #option_path::None
        }
    }
}

pub fn option_type_constraint_macro_args_to_token_stream(type_constraint_option: &Option<TypeConstraintMacroArgs>) -> proc_macro2::TokenStream {
    let option_path: proc_macro2::TokenStream = "::std::option::Option".parse().unwrap();
    let data_type_constraint_path: proc_macro2::TokenStream = "::lang_interpreter::interpreter::data::DataTypeConstraint".parse().unwrap();
    let data_type_path: proc_macro2::TokenStream = "::lang_interpreter::interpreter::data::DataType".parse().unwrap();

    if let Some(val) = &type_constraint_option {
        let type_constraint_method = if matches!(val, TypeConstraintMacroArgs::Allowed(_)) {
            "from_allowed_types"
        }else {
            "from_not_allowed_types"
        };

        let type_constraint_method_ident = proc_macro2::Ident::new(
            type_constraint_method, Span::mixed_site(),
        );

        let types = match val {
            TypeConstraintMacroArgs::Allowed(types) |
            TypeConstraintMacroArgs::NotAllowed(types) => types,
        };

        let types = types.iter().
                map(|type_str| {
                    let type_ident = proc_macro2::Ident::new(
                        &type_str.value(), Span::mixed_site(),
                    );

                    quote! {
                        #data_type_path::#type_ident
                    }
                }).
                collect::<Vec<_>>();

        quote! {
            #option_path::Some(#data_type_constraint_path::#type_constraint_method_ident(
                &[#(#types,)*]
            ))
        }
    }else {
        quote! {
            #option_path::None
        }
    }
}



pub fn option_deprecation_info_to_token_stream(deprecated: &Option<DeprecationInfo>) -> proc_macro2::TokenStream {
    let option_path: proc_macro2::TokenStream = "::std::option::Option".parse().unwrap();
    let deprecation_info_path: proc_macro2::TokenStream = "::lang_interpreter::interpreter::data::function::DeprecationInfo".parse().unwrap();

    if let Some(deprecated) = deprecated {
        let remove_version = option_string_to_token_stream(&deprecated.remove_version);
        let replacement_function = option_string_to_token_stream(&deprecated.replacement_function);

        quote! {
            #option_path::Some(#deprecation_info_path::new(
                #remove_version,
                #replacement_function,
            ))
        }
    }else {
        quote! {
            #option_path::None
        }
    }
}
