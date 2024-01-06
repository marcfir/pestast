// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use pest_meta::optimizer::OptimizedRule;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::{

    abstracter::extract_idents, generate::ast::{rule2enumname_id, rule2structname_id, rule2_id},
};

use crate::attributes::GenerateOptions;

pub fn generate_iterators(
    optrules: &[OptimizedRule],
    defaults: &[&str],
    opts: &GenerateOptions,
) -> TokenStream {
    let enumname = format_ident!("{}", opts.ast.astnodes_name);
    let iter_match: Vec<_> = extract_idents(optrules).iter().map(|fir| { 
        
        let nodename = rule2enumname_id(&fir.rule);
        let mut fields: Vec<_> = vec![];
        let _: Vec<_> = fir.referenced_idents
            .iter()
            .map(|fi| {
                //Skip builtin ASCII and UTF8
                if defaults.iter().any(|d| *d == fi.rule) {
                    return;
                }
                let name = match &fi.is_tag {
                    None => rule2_id(&fi.rule),
                    Some(tag) => rule2_id( tag),
                };
                let id_nodename = rule2enumname_id(&fi.rule);
                match fi.ty {
                    crate::abstracter::FlatIdentType::Repetition => {
                        fields.push(quote!(for el in inner.#name.into_iter() {
                            flat_vec.append(&mut recursive_flattening(#enumname::#id_nodename(el)));
                        }));
                    }
                    crate::abstracter::FlatIdentType::Choice =>{
                        fields.push(quote!(
                            if let Some(#name) = inner.#name {
                                flat_vec.append(&mut recursive_flattening(#enumname::#id_nodename(#name)));
                            }
                        ));
                    },
                    crate::abstracter::FlatIdentType::Single =>{
                        fields.push(quote!(
                            flat_vec.append(&mut recursive_flattening(#enumname::#id_nodename(*inner.#name)));
                        ));
                    }
                }
            })
            .collect();
        if fields.is_empty() {
            quote!(
                #enumname::#nodename(..)=>flat_vec,
            )
        } else {
            quote!(
                #enumname::#nodename(inner)=>{
                    #(#fields)*
                    flat_vec
                }
            )
        }}).collect();
    let entry_id = rule2structname_id(&opts.ast.entry_rule, opts);
    let entry_enum = rule2enumname_id(&opts.ast.entry_rule);

    quote!(
        impl<'a> IntoIterator for &'a #entry_id<'a> {
            type Item = #enumname<'a>;
            type IntoIter = std::vec::IntoIter<Self::Item>;
            fn into_iter(self) -> Self::IntoIter {
                fn recursive_flattening(node: #enumname) -> Vec<#enumname> {
                    let mut flat_vec = vec![node.clone()];
                    match node {
                        #(#iter_match)*
                        #enumname::PestAstUnexpected => flat_vec,
                    }
                }

                let flat_vec = recursive_flattening(#enumname::#entry_enum(self.clone()));
                flat_vec.into_iter()
            }
        }
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{generate::parse_grammar, attributes::AstOptions,attributes::GenerateOptions};

    static GRAMMAR: &str = r#"
        program = _{ SOI ~ implicit ~ EOI  }
        implicit= ${ #head = or ~ (WHITESPACE+ ~  #tail = or)* }

        or  = !{ #more_and = and ~ (or_op ~ and)+ | #one_and = and }
        and = { #more_comp = comp ~ (and_op ~ comp)+ | #one_comp = comp }
        comp = { #more_array = array ~ eq_op ~ array | #one_array = array }

        array = ${ term }

        term = _{ ASCII_ALPHANUMERIC+ }
        or_op = { "||" }
        and_op = { "&&" }
        eq_op = { "=" }
        WHITESPACE = _{ " " | "\t" | NEWLINE }
        "#;

    #[test]
    fn gen_iter() {
        let options = GenerateOptions {
            ast: AstOptions{ 
                structname_prefix: "PestAst".to_string(),
                astnodes_name: "PestAstNode".to_string(),  
                ..Default::default()
            },
            derive_for: format_ident!("MyTest"),
            ..Default::default()
        };
        let (opt,_,_) = parse_grammar(GRAMMAR).unwrap();
        let t = generate_iterators(&opt, &[], &options);
        println!("{:#}", t);
    }
}
