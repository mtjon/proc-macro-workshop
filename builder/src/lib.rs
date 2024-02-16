use proc_macro::TokenStream;

use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Type};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    eprintln!("{:#?}", ast);

    let name = &ast.ident;
    let bident = format_ident!("{}Builder", name);
    let fields = if let syn::Data::Struct(syn::DataStruct { fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }), .. }) = ast.data {
        named
    } else {
        unimplemented!()
    };

    fn is_opt_ty(ty: &Type) -> Option<&Type> {
        if let syn::Type::Path(syn::TypePath{ path: syn::Path { segments: ref ps, .. } ,.. }) = ty {
            if ps.len() != 1 || ps[0].ident != "Option" {
                return None 
            } 

            if let syn::PathArguments::AngleBracketed(ref inner_ty) = ps[0].arguments {
                if inner_ty.args.len() != 1 {
                    return None
                } 

                let inner_ty = inner_ty.args.first().unwrap();
                if let syn::GenericArgument::Type(ref t) = inner_ty {
                    return Some(t)
                }
            } 
        } 
        None
    }

    let optionized_fields = fields.iter().map(|f| {
        let ident = &f.ident;
        let ty = &f.ty;
        // No need to wrap option types in another option
        if is_opt_ty(ty).is_some() {
            quote!{ #ident: #ty }
        } else {
            quote!{ #ident: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|f| {
        let ident = &f.ident;
        // check attributs of field
        // if attribute 'each' has value == ident, skip the regular setter
        
        let attrs = &f.attrs.iter().map(|a| {
            if let syn::Meta::List { 0: ref meta  } = a.meta {
                if let syn::MetaList {path: syn::Path { segments: ref ps, .. }, .. } = meta {
                    if ps.len() == 0 { return None }

                    if ps[0].ident == "builder" {
                        if let syn::MetaList { tokens: ref ts, .. } = meta {
                            let ts: Vec<syn::token::TokenStream> = ts.into_iter().collect();
                            // The builder attribute must have 3 tokens; Ident, Punct, and Literal
                            if &ts.into_iter().count() != 3.into() {
                                panic!("Invalid Arguments");
                            }
                            if {
                            }
                        }
                    }
                }
            }
            None
        });

        let ty = match is_opt_ty(&f.ty) {
            Some(ty) => ty,
            None => &f.ty,
        };
        quote!{
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        }
    });

    let build_fields = fields.iter().filter_map(|f| {
        let ident = &f.ident;
        let _ = &f.ty;
        if is_opt_ty(&f.ty).is_some() { 
            Some(quote!{ #ident: self.#ident.clone() }) 
        } else { 
            Some(quote!{ #ident: self.#ident.clone().expect(concat!("`", stringify!(#ident), "`", " field should be set")) }) 
        }
    });

    let build = quote!{
        pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
            Ok(#name {
                #(#build_fields,)*
            })
        }
    };

    quote!(
    pub struct #bident {
        #(#optionized_fields,)*
    }

    impl #bident {
        #(#methods)*
        #build
    }

    impl #name {
        pub fn builder() -> #bident {
            #bident {
                executable: None,
                args: None,
                env: None,
                current_dir: None,
            }
        }
    }
    )
    .into()
}
