// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use heck::{ToSnakeCase, ToUpperCamelCase};
use pest_meta::{ast::RuleType, optimizer::OptimizedRule};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

use crate::abstracter::{extract_idents, RuleRefType};
use crate::attributes::GenerateOptions;
use pest_generator::docs::DocComment;

pub fn rule2structname_id(name: &str, opts: &GenerateOptions) -> Ident {
    format_ident!(
        "{}{}{}",
        opts.ast.structname_prefix,
        name.to_upper_camel_case(),
        opts.ast.structname_postfix
    )
}
pub fn rule2enumname_id(rulename: &str) -> Ident {
    format_ident!("{}", rulename.to_upper_camel_case())
}

pub fn rule2_id(rulename: &str) -> Ident {
    format_ident!("{}", rulename.to_snake_case())
}
fn rule2_id_inner(rulename: &str) -> Ident {
    format_ident!("{}_innerworkbench", rulename.to_snake_case())
}

fn rule2_id_pest(rulename: &str) -> Ident {
    format_ident!("{}", rulename)
}

fn rule2_fn_rule2ast(rulename: &str) -> Ident {
    format_ident!("rule2ast_{}", rulename)
}

pub fn generate_structs(
    optrules: &[OptimizedRule],
    defaults: &[&str],
    docs: &DocComment,
    opts: &GenerateOptions,
) -> TokenStream {
    let rule_idents = extract_idents(optrules);
    let ast_structs: Vec<_> = rule_idents
        .iter()
        .map(|fir| {
            let structname = rule2structname_id(&fir.rule, opts);
            let vis = &opts.vis;

            let mut fields: Vec<_> = vec![];
            let _: Vec<_> = fir
                .referred_rules
                .iter()
                .map(|fi| {
                    //Skip builtin ASCII and UTF8
                    if defaults.iter().any(|d| *d == fi.referred_rule) {
                        return;
                    }
                    let name = match &fi.is_tag {
                        None => rule2_id(&fi.referred_rule),
                        Some(tag) => rule2_id(tag),
                    };
                    let astname = rule2structname_id(&fi.referred_rule, opts);
                    match fi.ty {
                        RuleRefType::Repetition => {
                            fields.push(quote!(#vis #name: Vec<#astname<'i>>));
                        }
                        RuleRefType::Choice => {
                            fields.push(quote!(#vis #name: Option<#astname<'i>>));
                        }
                        RuleRefType::Single => {
                            fields.push(quote!(#vis #name: Box<#astname<'i>>));
                        }
                    }
                })
                .collect();
            if fields.is_empty() {
                fields.push(quote!(#vis content: String))
            }

            fields.push(quote!(#vis astspan: pestast::AstSpan<'i>));

            let derives = opts.ast.get_derive_as_quote();
            let doc: TokenStream = match docs.line_docs.get(&fir.rule) {
                Some(doc) => {
                    quote!(#[doc=#doc])
                }
                None => {
                    quote!()
                }
            };
            quote!(
            #doc     
            #derives
            #[allow(unused_variables)]
            #vis struct #structname<'i>{#(#fields,)*})
        })
        .collect();
    quote!(
        #(#ast_structs)*
    )
}

pub fn generate_enumwrapper(optrules: &[OptimizedRule], opts: &GenerateOptions) -> TokenStream {
    let mut ast_nodes = vec![];
    let vis = &opts.vis;
    let _: Vec<_> = optrules
        .iter()
        .map(|rule| match rule.ty {
            RuleType::Silent => (),
            _ => {
                let name = rule2structname_id(&rule.name, opts);
                let enumname = rule2enumname_id(&rule.name);
                ast_nodes.push(quote!(#enumname(#name<'i>)));
            }
        })
        .collect();

    let enumname = format_ident!("{}", opts.ast.astnodes_name);
    let derives = &opts.ast.derives;
    quote!(
        #[derive #derives]
        #vis enum #enumname<'i>{
            #(#ast_nodes,)*
            PestAstUnexpected,
        }
    )
}

pub fn generate_as_ast(
    optrules: &[OptimizedRule],
    defaults: &[&str],
    opts: &GenerateOptions,
) -> TokenStream {
    let mut mapping: Vec<_> = vec![];
    let rule_idents = extract_idents(optrules);
    let _: Vec<_> = rule_idents
        .iter()
        .map(|fir| {
            let structname = rule2structname_id(&fir.rule, opts);
            let mut convert_inloop_rule: Vec<_> = vec![];
            let mut convert_inloop_tag: Vec<_> = vec![];
            let mut convert_defs: Vec<_> = vec![];
            let mut convert_after: Vec<_> = vec![];
            let mut fields: Vec<_> = vec![];
            let _: Vec<_> = fir
                .referred_rules
                .iter()
                .map(|fi| {
                    //Skip builtin ASCII and UTF8
                    if defaults.iter().any(|d| *d == fi.referred_rule) {
                        return;
                    }
                    let name_inner = rule2_id_inner(fi.get_effective_name());
                    let name = rule2_id(fi.get_effective_name());
                    let name_fnname = rule2_fn_rule2ast(&fi.referred_rule);
                    let name_rule_pest = rule2_id_pest(fi.get_effective_name());
                    let name_child_str = fi.get_effective_name();
                    match fi.ty {
                        RuleRefType::Repetition => {
                            convert_defs.push(quote!(let mut #name_inner = vec![];));
                            if fi.is_tag.is_some() {
                                convert_inloop_tag.push(
                                    quote!(#name_child_str => {
                                        #name_inner.push(#name_fnname(pair____.clone()));
                                    })
                                );
                            }else{
                                convert_inloop_rule.push(
                                    quote!( Rule::#name_rule_pest => {
                                        #name_inner.push(#name_fnname(pair____.clone()));
                                    })
                                );
                            }
                            fields.push(quote!(#name:#name_inner));
                        }
                        RuleRefType::Choice => {
                            convert_defs.push(quote!(let mut #name_inner = None;));
                            if fi.is_tag.is_some() {
                                convert_inloop_tag.push(
                                    quote!( #name_child_str => {
                                        #name_inner = Some(#name_fnname(pair____.clone()));
                                    })
                                );
                            }else{
                                convert_inloop_rule.push(
                                    quote!( Rule::#name_rule_pest => {
                                        #name_inner = Some(#name_fnname(pair____.clone()));
                                    })
                                );
                            }
                            fields.push(quote!(#name:#name_inner));
                        }
                        RuleRefType::Single => {
                            convert_defs.push(quote!(let mut #name_inner = None;));                            
                            if fi.is_tag.is_some() {
                                convert_inloop_tag.push(
                                    quote!( #name_child_str => {
                                            #name_inner = Some(Box::new(#name_fnname(pair____.clone())));
                                    })
                                );
                            }else{
                                convert_inloop_rule.push(
                                    quote!( Rule::#name_rule_pest => {
                                        #name_inner = Some(Box::new(#name_fnname(pair____.clone())));
                                    })
                                );
                            }
                            convert_after.push(quote!(let Some(#name_inner) = #name_inner else{
                                unreachable!();
                            };));
                            fields.push(quote!(#name:#name_inner));
                        }
                    }
                })
                .collect();
            if fields.is_empty() {
                fields.push(quote!(content:pair____.as_str().to_string()))
            }

            let rule2ast_fn = rule2_fn_rule2ast(&fir.rule);
            let convert  = if convert_inloop_rule.is_empty() && convert_inloop_tag.is_empty(){
                quote!()
            }else{
                quote!(
                    #(#convert_defs)*
                    for pair____ in pair____.clone().into_inner(){
                        if let Some(nodename) = pair____.as_node_tag(){
                            match nodename {
                                #(#convert_inloop_tag)*
                                _ => (),
                            }
                        }else{
                            match pair____.as_rule() {
                                #(#convert_inloop_rule)*
                                _ => (),
                            }
                        }
                    }
                    #(#convert_after)*
                )
            };
            mapping.push(quote!(
                fn #rule2ast_fn(pair____:pest::iterators::Pair<Rule>) -> #structname {
                    #convert
                    #structname{
                        #(#fields,)*
                        astspan: pestast::AstSpan::new(pair____.line_col(),pair____.as_span().clone()),
                    }
                }
            ));
        })
        .collect();
    let derivefor = format_ident!("{}", opts.derive_for);
    let entry_id = rule2structname_id(&opts.ast.entry_rule, opts);
    let entry_id_pest = rule2_id_pest(&opts.ast.entry_rule);
    let entry_fn = rule2_fn_rule2ast(&opts.ast.entry_rule);

    quote!(
        #[automatically_derived]
        #[allow(clippy::all)]
        impl <'a> pestast::Ast<'a,Rule,#entry_id<'a>> for #derivefor{
            fn as_ast(pairs____: pest::iterators::Pairs<'a, Rule>) -> Result<#entry_id<'a>, pestast::PestastError> {
                #( #mapping )*
                //We force that the grammer has one single entry point with the rule "entry"
                if pairs____.len() != 1 {
                    return Err(pestast::PestastError::GetAst("No single entry point".to_string()));
                }
                match pairs____.into_iter().next() {
                    Some(pair____) => {
                        //We feed the entry token to the foreward recurisve iterator
                        match pair____.as_rule() {
                            Rule::#entry_id_pest => Ok(#entry_fn(pair____)),
                            _ => Err(pestast::PestastError::GetAst("as".to_string())),
                        }
                    }
                    None => unreachable!(),
                }
            }
        }
    )
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::attributes::AstOptions;
    use crate::generate::parse_grammar;

    use super::*;
    use pest_meta::optimizer::OptimizedExpr;
    use pest_meta::optimizer::OptimizedRule;

    static GRAMMAR: &str = r#"
        program = _{ SOI ~ implicit ~ EOI  }
        implicit= ${ #head = or ~   (WHITESPACE+ ~ #tail = or)* }

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

    fn pretty(tokens: TokenStream) -> String {
        prettyplease::unparse(&syn::parse2(tokens).unwrap())
    }

    #[test]
    fn naming() {
        let id = rule2structname_id(
            "name",
            &GenerateOptions {
                ..Default::default()
            },
        );
        assert_eq!(id.to_string(), "Name");
        let id2 = rule2structname_id(
            "name",
            &GenerateOptions {
                ast: AstOptions {
                    structname_prefix: "mypre".to_string(),
                    structname_postfix: "Post".to_string(),
                    ..Default::default()
                },
                ..Default::default()
            },
        );
        assert_eq!(id2.to_string(), "mypreNamePost");
    }

    #[test]
    fn gen_structs() {
        let options = GenerateOptions {
            ast: AstOptions {
                astnodes_name: "EName".to_string(),
                ..Default::default()
            },
            ..Default::default()
        };
        let rules = vec![OptimizedRule {
            name: "test".to_string(),
            ty: pest_meta::ast::RuleType::CompoundAtomic,
            expr: OptimizedExpr::Ident("XY".to_string()),
        }];
        let t = generate_structs(
            &rules,
            &[],
            &DocComment {
                grammar_doc: "".to_string(),
                line_docs: HashMap::new(),
            },
            &options,
        );
        assert_eq!(
            t.to_string(),
            quote!(
                #[allow(unused_variables)]
                pub struct Test<'i> {
                    pub xy: Box<Xy<'i>>,
                    pub astspan: pestast::AstSpan<'i>,
                }
            )
            .to_string()
        );
    }

    #[test]
    fn gen_structs_2() {
        let gram = r#"
        entry = _{ SOI ~ main ~ EOI }
        main= ${ "test" ~ #mytag = id }
        id = @{ (ASCII_ALPHA | "_") ~ (ASCII_DIGIT | ASCII_ALPHA | "_")* }
        /// tt
        /// PestAst(parse)
        integer = @{ "-"? ~ ASCII_DIGIT+ }
        string  = @{ "\"" ~ (!("\"") ~ (ANY))* ~ "\"" }
        boolean = @{ "True" | "False" }
        value = { boolean | integer | string }

        "#;

        let options = GenerateOptions {
            ast: AstOptions {
                structname_prefix: "PestAst".to_string(),
                astnodes_name: "PestAstNode".to_string(),
                ..Default::default()
            },
            ..Default::default()
        };
        let (opt, defaults, docs) = parse_grammar(gram).unwrap();
        let t = generate_structs(&opt, &defaults, &docs, &options);
        println!("{}", pretty(t.clone()));
        assert_eq!(
            t.to_string(),
            quote!(
                #[allow(unused_variables)]
                pub struct PestAstMain<'i> {
                    pub mytag: Box<PestAstId<'i>>,
                    pub astspan: pestast::AstSpan<'i>,
                }
                #[allow(unused_variables)]
                pub struct PestAstId<'i> {
                    pub content: String,
                    pub astspan: pestast::AstSpan<'i>,
                }
                #[doc = "tt\nPestAst(parse)"]
                #[allow(unused_variables)]
                pub struct PestAstInteger<'i> {
                    pub content: String,
                    pub astspan: pestast::AstSpan<'i>,
                }
                #[allow(unused_variables)]
                pub struct PestAstString<'i> {
                    pub content: String,
                    pub astspan: pestast::AstSpan<'i>,
                }
                #[allow(unused_variables)]
                pub struct PestAstBoolean<'i> {
                    pub content: String,
                    pub astspan: pestast::AstSpan<'i>,
                }
                #[allow(unused_variables)]
                pub struct PestAstValue<'i> {
                    pub boolean: Option<PestAstBoolean<'i>>,
                    pub integer: Option<PestAstInteger<'i>>,
                    pub string: Option<PestAstString<'i>>,
                    pub astspan: pestast::AstSpan<'i>,
                }
            )
            .to_string()
        );
    }

    #[test]
    fn gen_node() {
        let options = GenerateOptions {
            ast: AstOptions {
                structname_prefix: "PestAst".to_string(),
                astnodes_name: "PestAstNode".to_string(),
                ..Default::default()
            },
            derive_for: format_ident!("Test"),
            ..Default::default()
        };
        let (opt, _, _) = parse_grammar(GRAMMAR).unwrap();
        let _t = generate_enumwrapper(&opt, &options);
        // TODO: Add assert!
    }

    #[test]
    fn gen_getast() {
        let options = GenerateOptions {
            ast: AstOptions {
                structname_prefix: "PestAst".to_string(),
                astnodes_name: "PestAstNode".to_string(),
                ..Default::default()
            },
            derive_for: format_ident!("test"),
            ..Default::default()
        };
        let (opt, _, _) = parse_grammar(GRAMMAR).unwrap();
        let _t = generate_as_ast(&opt, &[], &options);
        // TODO: Add assert!
    }

    #[test]
    fn gen_all() {
        let gram = r#"
        entry = { SOI ~ main ~ EOI }
        main= ${ "test" ~ id }
        id = @{ (ASCII_ALPHA | "_") ~ (ASCII_DIGIT | ASCII_ALPHA | "_")* }
        /// tt
        /// PestAst(parse)
        integer = @{ "-"? ~ ASCII_DIGIT+ }
        string  = @{ "\"" ~ (!("\"") ~ (ANY))* ~ "\"" }
        boolean = @{ "True" | "False" }
        value = { boolean | integer | string }

        "#;
        let options = GenerateOptions {
            ast: AstOptions {
                structname_prefix: "PestAst".to_string(),
                astnodes_name: "PestAstNode".to_string(),
                ..Default::default()
            },
            derive_for: format_ident!("MyAstGen"),
            ..Default::default()
        };
        let (opt, defaults, docs) = parse_grammar(gram).unwrap();
        let _enumw = pretty(generate_enumwrapper(&opt, &options));
        let _structs = pretty(generate_structs(&opt, &defaults, &docs, &options));
        let _make = pretty(generate_as_ast(&opt, &defaults, &options));
        // TODO: Add assert!
    }
}
