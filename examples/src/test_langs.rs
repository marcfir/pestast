// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

pub mod json {
    use pest_derive::Parser;
    use pestast::pestast_derive::Ast;

    #[derive(Parser, Ast)]
    #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone),entry=json)]
    #[grammar = "json.pest"]
    pub struct Workbench;
}

pub mod http {
    use pest_derive::Parser;
    use pestast::pestast_derive::Ast;

    #[derive(Parser, Ast)]
    #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone),entry=http)]
    #[grammar = "http.pest"]
    pub struct Workbench;
}

pub mod toml {
    use pest_derive::Parser;
    use pestast::pestast_derive::Ast;

    #[derive(Parser, Ast)]
    #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone),entry=toml)]
    #[grammar = "toml.pest"]
    pub struct Workbench;
}

pub mod smalljava {
    use pest_derive::Parser;
    use pestast::pestast_derive::Ast;
    #[derive(Parser, Ast)]
    #[ast(astnodes_name=PestAst,struct_pre=PestAst,derives=(Debug,Clone),entry=smalljava)]
    #[grammar = "smalljava.pest"]
    pub struct Workbench;
}
