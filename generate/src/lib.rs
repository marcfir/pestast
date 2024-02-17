// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use proc_macro2::TokenStream;
use syn::{DeriveInput, Result};

mod attributes;
use attributes::parse_attributes;

mod abstracter;
mod generate;
mod grammar;
use grammar::load_grammar;

use crate::generate::generate_workbench;

/// Processes the derive/proc macro input and generates the corresponding ast code
pub fn derive_pestast_gen(input: &mut DeriveInput) -> Result<TokenStream> {
    let (contents, opts) = parse_attributes(input)?;
    let (data, _) = load_grammar(contents)?;
    generate_workbench(&data, &opts, input)
}
