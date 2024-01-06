// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

mod ast;
mod iterators;

use crate::{
    attributes::GenerateOptions,
    docs::{self, DocComment},
    generate::ast::{generate_enumwrapper, generate_makeast, generate_structs},
};

pub use iterators::generate_iterators;
use pest_meta::{
    ast::RuleType,
    optimizer::{self, OptimizedRule},
    parser::{self, rename_meta_rule, Rule},
    unwrap_or_report, validator,
};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{spanned::Spanned, DeriveInput, Error, Result};

/// Generates the code for the language workbench.
pub fn generate_workbench(
    data: &str,
    opts: &GenerateOptions,
    inp: &DeriveInput,
) -> Result<TokenStream> {
    let (rules, defaults, docs) = parse_grammar(data)?;
    validate_grammar(&rules, opts, inp)?;
    let structs = generate_structs(&rules, &defaults, &docs, opts);
    let enumwrapper = generate_enumwrapper(&rules, opts);
    let makeast = generate_makeast(&rules, &defaults, opts);
    let iterators = generate_iterators(&rules, &defaults, opts);

    Ok(quote!(
        #structs
        #enumwrapper
        #makeast
        #iterators
    ))
}
/// Parses the grammar and returns a list with the defined grammar rules `Vec<OptimizedRule>`
/// and a list of used builtin rules `Vec<&str>`. This is the input for our generators.
pub fn parse_grammar(data: &str) -> Result<(Vec<OptimizedRule>, Vec<&str>, DocComment)> {
    let grammar_pairs = match parser::parse(Rule::grammar_rules, data) {
        Ok(pairs) => pairs,
        Err(error) => {
            return Err(Error::new(
                Span::call_site(),
                format!("Error parsing: \n{}", error.renamed_rules(rename_meta_rule)),
            ))
        }
    };

    // List of used builtin rules
    let builtin_defaults = unwrap_or_report(validator::validate_pairs(grammar_pairs.clone()));

    let doc_comment = docs::consume(grammar_pairs.clone());
    let grammar_ast = unwrap_or_report(parser::consume_rules(grammar_pairs));
    let optimized_ast = optimizer::optimize(grammar_ast);
    Ok((optimized_ast, builtin_defaults, doc_comment))
}

/// Validates the pest grammar with additional rules required for our workbench.
fn validate_grammar(
    optrules: &Vec<OptimizedRule>,
    opts: &GenerateOptions,
    inp: &DeriveInput,
) -> Result<()> {
    for rule in optrules {
        if rule.name.to_lowercase() == opts.ast.entry_rule && rule.ty != RuleType::Silent {
            return Ok(());
        }
    }
    Err(Error::new(
        inp.span(),
        "Entry rule cannot be find defined. Define the entry point of your grammar with a non silent `entry = {{...}}` or set the `entry` option with #[ast(entry=..., ...)] ",
    ))
}

#[cfg(test)]
mod tests {
    use crate::generate::parse_grammar;

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

    #[test]
    fn gen() {
        let (ast, defaults, _) = parse_grammar(GRAMMAR).unwrap();
        assert_ne!(ast.len(), 0);
        assert_ne!(defaults.len(), 0);
    }
}
