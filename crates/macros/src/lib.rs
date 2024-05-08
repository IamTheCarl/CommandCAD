use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Fields};

#[proc_macro_derive(Struct)]
pub fn derive_struct(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = input.ident;

    let contains_span = input.generics.type_params().any(|param| param.ident == "S");

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let struct_name_string = struct_name.to_string();
    let struct_definition_name = struct_name.to_string();
    let struct_definition_name2 = struct_definition_name.clone();

    let function_generics = if !contains_span {
        quote! { <S: crate::script::parsing::Span> }
    } else {
        quote! {}
    };

    let fields = match input.data {
        syn::Data::Struct(data) => match data.fields {
            Fields::Named(fields) => fields.named,
            _ => panic!("Struct must have named fields."),
        },
        _ => {
            panic!("Only structs can be made available to scripts.");
        }
    };
    let define_fields = fields.clone().into_iter().map(|field| {
        let name = field.ident.unwrap().to_string();
        let ty = field.ty;

        quote! {
            MemberVariable {
                name: S::from_str(#name),
                ty: MemberVariableType {
                    ty: <#ty as crate::script::execution::types::TypedObject>::get_type(),
                    constraints: None,
                    default_value: None,
                }
            }
        }
    });

    let extract_fields = fields.clone().into_iter().map(|field| {
        let name = field.ident.unwrap();
        let name_string = name.to_string();
        let ty = field.ty;

        quote! {
            let #name = members.remove(#name_string).unwrap();
            let #name = #name.downcast::<#ty>(span)?;
        }
    });

    let import_fields = fields.clone().into_iter().map(|field| {
        let name = field.ident.unwrap();
        quote! { #name }
    });

    quote! {
	impl #impl_generics #struct_name #ty_generics #where_clause {
	    pub fn unpack_struct #function_generics (span: &S, structure: crate::script::execution::types::Structure<S>) -> std::result::Result<Self, crate::script::execution::Failure<S>> {
		// Check that it's the correct type.
		if structure.name() == #struct_name_string {
		    let mut members = Rc::unwrap_or_clone(structure.members);

		    // Extract fields.
		    #(#extract_fields)*

		    // Import fields into the struct.
		    Ok(#struct_name {
			#(#import_fields),*
		    })
		} else {
		    Err(crate::script::execution::Failure::ExpectedGot(span.clone(), #struct_name_string.into(), structure.name().to_string().into()))
		}
	    }
	}
	impl #impl_generics #struct_name #ty_generics #where_clause {
	    fn define_struct #function_generics (context: &mut crate::script::execution::ExecutionContext<'_, S>) {
		context.stack.new_variable_str(
		    #struct_definition_name,
		    StructDefinition {
			definition: Rc::new(parsing::StructDefinition {
			    name: S::from_str(#struct_definition_name2),
			    members: vec![
				#(#define_fields),*
			    ],
			}),
		    }
		    .into(),
		);
	    }
	}
    }.into()
}
