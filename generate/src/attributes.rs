// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.
//
// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

// Parts of this code are taken from the pest project https://github.com/pest-parser/pest/blob/5d30733b0a094da36c7376f90c3bf9c4fc0119e7/generator/src/lib.rs

use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse_quote, punctuated::Punctuated, Attribute, Data, DeriveInput, Error, Expr, ExprLit,
    Generics, Lit, Meta, Result, Token, Visibility,
};

#[derive(Debug, PartialEq)]
pub enum GrammarSource {
    File(String),
    Inline(String),
}

#[derive(Debug)]
pub struct AstOptions {
    pub structname_prefix: String,
    pub structname_postfix: String,
    pub astnodes_name: String,
    pub derives: TokenStream,
    pub entry_rule: String,
}

impl Default for AstOptions {
    fn default() -> Self {
        Self {
            structname_prefix: "".to_string(),
            structname_postfix: "".to_string(),
            astnodes_name: "".to_string(),
            entry_rule: "entry".to_string(),
            derives: TokenStream::new(),
        }
    }
}

#[derive(Debug)]
pub struct GenerateOptions {
    pub derive_for: Ident,
    pub vis: Visibility,
    pub generics: Generics,
    pub ast: AstOptions,
}

impl Default for GenerateOptions {
    fn default() -> Self {
        Self {
            ast: AstOptions::default(),
            derive_for: format_ident!("Def"),
            generics: Generics {
                ..Default::default()
            },
            vis: Visibility::Public(syn::token::Pub {
                span: Span::call_site(),
            }),
        }
    }
}

/// Parses the attributes of the derive input. Returns the referenced grammars and the generation options for the language workbench
/// including the ast options.
pub fn parse_attributes(
    parsed_input: &DeriveInput,
) -> Result<(Vec<GrammarSource>, GenerateOptions)> {
    // Get the basic information of the referenced struct
    let name = parsed_input.ident.clone();
    let generics = parsed_input.generics.clone();
    let vis = parsed_input.vis.clone();

    //Check if input is a struct
    match parsed_input.data {
        Data::Enum(..) | Data::Union(..) => {
            return Err(Error::new_spanned(
                parsed_input,
                "The input must be a struct",
            ));
        }
        _ => (),
    }

    //Collect grammar attributes
    let grammar: Vec<&Attribute> = parsed_input
        .attrs
        .iter()
        .filter(|attr| {
            let path = attr.meta.path();
            path.is_ident("grammar") || path.is_ident("grammar_inline")
        })
        .collect();

    if grammar.is_empty() {
        return Err(Error::new_spanned(
            parsed_input,
            "A grammar file needs to be provided with the #[grammar = \"PATH\"] or #[grammar_inline = \"GRAMMAR CONTENTS\"] attribute",
        ));
    }

    let mut grammar_sources = Vec::with_capacity(grammar.len());
    for attr in grammar {
        grammar_sources.push(get_grammar_type(attr)?)
    }
    //Collect ast attributes
    let opts: Vec<&Attribute> = parsed_input
        .attrs
        .iter()
        .filter(|attr| {
            let path = attr.meta.path();
            path.is_ident("ast")
        })
        .collect();

    if opts.is_empty() {
        return Err(Error::new_spanned(
            parsed_input,
            "Configuration needs to be provided with the #[ast()] attribute",
        ));
    }
    let mut gen_opts = GenerateOptions {
        derive_for: name,
        vis,
        generics,
        ..Default::default()
    };
    get_opts(opts, &mut gen_opts)?;
    // validate_opts(&gen_opts)?;
    Ok((grammar_sources, gen_opts))
}

fn get_opts(attrs: Vec<&Attribute>, opts: &mut GenerateOptions) -> Result<()> {
    for attr in attrs.iter() {
        let nested = attr
            .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
            .map_err(|e| {
                Error::new_spanned(
                    attr,
                    format!(
                        "Configuration needs to be provided with the #[ast()] attribute: {}",
                        e
                    ),
                )
            })?;
        for meta in nested {
            if meta.path().is_ident("astnodes_name") {
                let Ok(nv) = meta.require_name_value() else {
                    return Err(Error::new_spanned(
                        attr,
                        "astnodes_name must be in the form of `astnodes_name=...`",
                    ));
                };
                let val = &nv.value;
                opts.ast.astnodes_name = quote!(#val).to_string();
            } else if meta.path().is_ident("struct_pre") {
                let Ok(nv) = meta.require_name_value() else {
                    return Err(Error::new_spanned(
                        attr,
                        "struct_pre must be in the form of `struct_pre=...`",
                    ));
                };
                let val = &nv.value;
                opts.ast.structname_prefix = quote!(#val).to_string();
            } else if meta.path().is_ident("struct_post") {
                let Ok(nv) = meta.require_name_value() else {
                    return Err(Error::new_spanned(
                        attr,
                        "struct_post must be in the form of `struct_post=...`",
                    ));
                };
                let val = &nv.value;
                opts.ast.structname_postfix = quote!(#val).to_string();
            } else if meta.path().is_ident("derives") {
                let Ok(nv) = meta.require_name_value() else {
                    return Err(Error::new_spanned(
                        attr,
                        "derives must be in the form of `derive=(..., ...)`",
                    ));
                };
                let val = &nv.value;
                opts.ast.derives = parse_quote!(#val);
            } else if meta.path().is_ident("entry") {
                let Ok(nv) = meta.require_name_value() else {
                    return Err(Error::new_spanned(
                        attr,
                        "struct_post must be in the form of `struct_post=...`",
                    ));
                };
                let val = &nv.value;
                opts.ast.entry_rule = quote!(#val).to_string();
            } else {
                return Err(Error::new_spanned(
                    attr,
                    format!(
                        "Unkown parameter `{:?}` for attribute `ast`",
                        meta.path()
                            .get_ident()
                            .unwrap_or(&Ident::new("??", proc_macro2::Span::call_site()))
                            .to_string()
                    ),
                ));
            }
        }
    }
    if opts.ast.astnodes_name.is_empty() {
        return Err(Error::new_spanned(
            attrs[0],
            "astnodes_name is not allowed to be empty. Please define `#[ast(astnodes_name=...,...)]`",
        ));
    }
    if opts.ast.structname_postfix.is_empty() && opts.ast.structname_prefix.is_empty() {
        return Err(Error::new_spanned(
            attrs[0],
            "struct_post and struct_pre must not both be empty. Please define `#[ast(struct_post=...,...)]` or define `#[ast(struct_pre=...,...)]`",
        ));
    }
    Ok(())
}

fn get_grammar_type(attr: &Attribute) -> Result<GrammarSource> {
    match &attr.meta {
        Meta::NameValue(name_value) => match &name_value.value {
            Expr::Lit(ExprLit {
                lit: Lit::Str(string),
                ..
            }) => {
                if name_value.path.is_ident("grammar") {
                    Ok(GrammarSource::File(string.value()))
                } else {
                    Ok(GrammarSource::Inline(string.value()))
                }
            }
            _ => Err(Error::new_spanned(
                attr,
                "grammar attribute must be a string",
            )),
        },
        _ => Err(Error::new_spanned(
            attr,
            "grammar attribute must be of the form `grammar = \"...\"`",
        )),
    }
}

#[cfg(test)]
mod tests {

    use proc_macro2::Span;
    use syn::parse_quote;
    use syn::token::Pub;
    use syn::Attribute;
    use syn::DeriveInput;
    use syn::Visibility;

    use super::parse_attributes;
    use super::GrammarSource;

    #[test]
    fn test_non_struct() {
        let definition = "
            #[other_attr]
            #[grammar_inline = \"GRAMMAR\"]
            #[ast(astnodes_name=hjkl, struct_post=llkl, derives=(Debug,Clone))]
            pub enum MyParser{}
        ";
        let ast: DeriveInput = syn::parse_str(definition).unwrap();
        assert!(parse_attributes(&ast).is_err());
    }

    #[test]
    fn get_attributes() {
        let definition = "
            #[other_attr]
            #[grammar_inline = \"GRAMMAR\"]
            #[ast(astnodes_name=hjkl, struct_post=llkl, derives=(Debug,Clone))]
            pub struct MyParser<'a, T>;
        ";
        let ast: DeriveInput = syn::parse_str(definition).unwrap();
        let (_, opts) = parse_attributes(&ast).unwrap();
        assert_eq!(opts.ast.astnodes_name, "hjkl");
        assert_eq!(opts.ast.structname_postfix, "llkl");
        println!("{:?}", opts);
    }

    #[test]
    fn test_aatribtues() {
        let x: Attribute = parse_quote!(
            #[ast(astnodes_name=hjkl, struct_post=llkl, derives=(Debug, Clone))]
        );
        println!("{:?}", x);
    }

    #[test]
    fn derive_inline_file() {
        let definition = "
            #[other_attr]
            #[grammar_inline = \"GRAMMAR\"]
            #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone))]
            pub struct MyParser<'a, T>;
        ";
        let ast: DeriveInput = syn::parse_str(definition).unwrap();
        let (filenames, opts) = parse_attributes(&ast).unwrap();
        println!("{:#?}", filenames);
        assert_eq!(filenames, [GrammarSource::Inline("GRAMMAR".to_string())]);
        assert_eq!(
            opts.vis,
            Visibility::Public(Pub {
                span: Span::call_site()
            })
        )
    }

    #[test]
    fn derive_ok() {
        let definition = "
            #[other_attr]
            #[grammar = \"myfile.pest\"]
            #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone))]
            pub struct MyParser<'a, T>;
        ";
        let ast: DeriveInput = syn::parse_str(definition).unwrap();
        let (filenames, _) = parse_attributes(&ast).unwrap();
        assert_eq!(filenames, [GrammarSource::File("myfile.pest".to_string())]);
    }

    #[test]
    fn derive_multiple_grammars() {
        let definition = "
            #[other_attr]
            #[grammar = \"myfile1.pest\"]
            #[grammar = \"myfile2.pest\"]
            #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone))]
            pub struct MyParser<'a, T>;
        ";
        let ast: DeriveInput = syn::parse_str(definition).unwrap();
        let (filenames, _) = parse_attributes(&ast).unwrap();
        assert_eq!(
            filenames,
            [
                GrammarSource::File("myfile1.pest".to_string()),
                GrammarSource::File("myfile2.pest".to_string())
            ]
        );
    }

    #[test]
    fn derive_wrong_arg() {
        let definition = "
            #[other_attr]
            #[grammar = 1]
            #[astnode = \"Tests\"]
            pub struct MyParser<'a, T>;
        ";
        let ast: DeriveInput = syn::parse_str(definition).unwrap();
        assert!(parse_attributes(&ast).is_err());
    }

    #[test]
    fn derive_no_grammar() {
        let definition = "
            #[other_attr]
            pub struct MyParser<'a, T>;
        ";
        let ast: DeriveInput = syn::parse_str(definition).unwrap();
        assert!(parse_attributes(&ast).is_err());
    }

    #[test]
    fn derive_no_opts() {
        let definition = "
            #[other_attr]
            #[grammar_inline = \"GRAMMAR\"]
            pub struct MyParser<'a, T>;
        ";
        let ast: DeriveInput = syn::parse_str(definition).unwrap();
        assert!(parse_attributes(&ast).is_err());
    }
}
