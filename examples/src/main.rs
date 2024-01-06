// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

mod test_langs;
use test_langs::{http, json, smalljava, toml};

use pest::Parser;
use pestast::Ast;

fn main() {
    ////////
    // JSON
    ////////
    {
        //Parse your content with the PEST parser
        let json_file = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/data/examples.json"));
        let pairs = json::Workbench::parse(json::Rule::json, json_file).unwrap();

        //Make the ast
        let ast = json::Workbench::as_ast(pairs).unwrap();
        println!("{:#?}", ast);

        // Iterate over the ast
        for i in ast.into_iter() {
            println!("{:#?}", i);
        }
    }

    ////////
    // HTTP
    ////////
    {
        //Parse your content with the PEST parser
        let http_file = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/data/examples.http"));
        let pairs = http::Workbench::parse(http::Rule::http, http_file).unwrap();

        //Make the ast
        let ast = http::Workbench::as_ast(pairs).unwrap();
        println!("{:#?}", ast);

        //Iterate over the ast
        for i in ast.into_iter() {
            println!("{:#?}", i);
        }
    }

    ////////
    // TOML
    ////////
    {
        //Parse your content with the PEST parser
        let toml_file = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/data/examples.toml"));
        let pairs = toml::Workbench::parse(toml::Rule::toml, toml_file).unwrap();

        //Make the ast
        let ast = toml::Workbench::as_ast(pairs).unwrap();
        println!("{:#?}", ast);

        //Iterate over the ast
        for i in ast.into_iter() {
            println!("{:#?}", i);
        }
    }

    ////////
    // SMALLJAVA
    ////////
    {
        //Parse your content with the PEST parser
        let sj_file = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/data/examples.sj"));
        let pairs = smalljava::Workbench::parse(smalljava::Rule::smalljava, sj_file).unwrap();

        //Make the ast
        let ast = smalljava::Workbench::as_ast(pairs).unwrap();
        println!("{:#?}", ast);

        //Iterate over the ast
        for i in ast.into_iter() {
            println!("{:#?}", i);
        }
    }
}
