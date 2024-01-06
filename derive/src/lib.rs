// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use pestast_gen::derive_pestast_gen;
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

/// The main method that's called by the proc macro
#[proc_macro_derive(Ast, attributes(grammar, grammar_inline, ast))]
pub fn derive_ast(input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);
    derive_pestast_gen(&mut input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
