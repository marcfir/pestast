// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use pest::Parser;
use pestast::Ast;

pub mod csv {
    use pest_derive::Parser;
    use pestast::pestast_derive::Ast;
    #[derive(Parser, Ast)]
    #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone),entry=file)]
    #[grammar = "tests/csv.pest"]
    pub struct Workbench;
}

#[test]
fn it_adds_two() {
    //Parse your content with the PEST parser
    let json_file = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/data/data.csv"));
    let pairs = csv::Workbench::parse(csv::Rule::file, json_file).unwrap();

    //Make the ast
    let ast = csv::Workbench::as_ast(pairs).unwrap();
    println!("{:#?}", ast);
}
