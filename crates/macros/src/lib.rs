use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, punctuated::Punctuated, Attribute, DeriveInput, Expr, Field, Fields, Ident,
    Lifetime, Token, Type, TypeNever, TypeReference,
};

#[proc_macro_derive(Struct)]
pub fn derive_struct(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = input.ident;
    let struct_definition_name = struct_name.to_string();
    let struct_definition_name2 = struct_definition_name.clone();

    let fields = match input.data {
        syn::Data::Struct(data) => match data.fields {
            Fields::Named(fields) => fields.named,
            _ => panic!("Struct must have named fields."),
        },
        _ => {
            panic!("Only structs can be made available to scripts.");
        }
    };
    let fields = fields.into_iter().map(|field| {
        let name = field.ident.unwrap().to_string();

        quote! {
            MemberVariable {
                name: S::from_str(#name),
                ty: MemberVariableType {
                    ty: VariableType::Vector(2, S::from_str("Length")),
                    constraints: None,
                    default_value: None,
                }
            }
        }
    });

    quote! {
	impl<S: crate::script::parsing::Span> From<crate::script::execution::types::Value<S>> for #struct_name {
	    fn from(_: crate::script::execution::types::Value<S>) -> Self { todo!() }
	}
	impl #struct_name {
	    fn define_struct<S: crate::script::parsing::Span>(context: &mut crate::script::execution::ExecutionContext<'_, S>) {
		context.stack.new_variable_str(
		    #struct_definition_name,
		    StructDefinition {
			definition: Rc::new(parsing::StructDefinition {
			    name: S::from_str(#struct_definition_name2),
			    members: vec![
				#(#fields),*
			    ],
			}),
		    }
		    .into(),
		);
	    }
	}
    }.into()
}
