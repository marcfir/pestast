// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#[test]
fn astnode_plain() {
    use pest::Parser;
    use pestast::Ast;
    mod test2 {
        use pest_derive::Parser;
        use pestast_derive::Ast;

        #[derive(Parser, Ast)]
        #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone))]
        #[grammar_inline = r#"
        WHITESPACE = _{ " " | "\t" | NEWLINE }
        entry = { SOI ~ (value)* ~ EOI }
        value = { boolean | integer | string }

        integer = @{ "-"? ~ ASCII_DIGIT+ }
        string  = @{ "\"" ~ (!("\"") ~ (ANY))* ~ "\"" }
        boolean = @{ "True" | "False" }
        "#]
        pub struct PestParser;
    }
    let pairs = test2::PestParser::parse(
        test2::Rule::entry,
        r#"-45 "testmy" 888 99            True        "#,
    )
    .unwrap();
    let ast = test2::PestParser::as_ast(pairs).unwrap();
    println!("{:#?}", ast);
    for i in ast.into_iter() {
        println!("{:#?}", i);
    }
}

#[test]
fn astnode_calc() {
    use pest::Parser;
    use pestast::Ast;
    mod calc {
        use pest_derive::Parser;
        use pestast_derive::Ast;

        #[derive(Parser, Ast)]
        // #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone))]
        #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone))]
        #[grammar_inline = r#"
        entry = { SOI ~ expr ~ EOI }
        num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }
            int = { ("+" | "-")? ~ ASCII_DIGIT+ }
        
        operation = _{ add | subtract | multiply | divide | power }
            add      = { "+" }
            subtract = { "-" }
            multiply = { "*" }
            divide   = { "/" }
            power    = { "^" }
        
        expr = { term ~ (operation ~ term)* }
        term = _{ num | "(" ~ expr ~ ")" }
        
        WHITESPACE = _{ " " | "\t" }
        "#]
        pub struct PestParser;
    }
    let pairs = calc::PestParser::parse(calc::Rule::entry, r#"45 + 3 *6 -99"#).unwrap();
    println!("{:#?}", pairs);
    let ast = calc::PestParser::as_ast(pairs).unwrap();
    println!("{:#?}", ast);
}
