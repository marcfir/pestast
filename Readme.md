# pestast

Pestast generates a static typed abstract syntax tree (AST) for your pest grammar. The generation is done in a separate `Ast` derive macro, which makes it very modular.

> [!WARNING]
> This code comes from the development of a language workbench in Rust based on pest and was intended as a prototype. There is no intensive testing. If there is more interest, this AST generation can be stabilized and published on crates.io.

## Structure of the repository

* **pestast** Contains the `pestast' crate, which contains the general types and traits for pestast.
* **derive** Contains the `pestast_derive` crate with the derive macro. There is no logic because we cannot export any functions in a `proc-macro` crate.
* **generate** Contains the `pestast_gen` crate with the logic to generate the pestast stuff.

## Build and test

```bash
cargo build --workspace
cargo test --workspace
cargo clippy --workspace
```

## Performance

The benchmark in `example/benches` shows a doubeling of the runtime time and can be executed with `cargo bench`.


| Benchmark        | Time          | Overhead |
| ---------------- |:-------------:| :------: |
| JSON parse       | 108ms         | 1        |
| JSON parse + AST | 213ms         | 1,91     |



## How it works

### About ASTs
To map the token tree into an AST after parsing, we need some rules. As the name Abstract Syntax Tree suggests, this is an abstraction process, which means we lose some information. There are many possible rules, so some approaches require user configuration of the AST.

We provide a fixed set of rules to reduce the implementation effort for the user, because by changing the grammar, the AST can also be customized.

### Rules

We use the same rules as in MontiCore ([MontiCore Language Workbench and Library Handbook](https://monticore.github.io/monticore/) Chapter 5). The Rules define the mapping of nonterminals to Rust structures.

Each Rule is mapped to a rust struct. Silent rules are ignored. Structure naming can be configured with a common prefix and suffix and is associated with a capitalized rule name (See *AST node naming*).

 - A **Sequence** like `entry = { SOI ~ expr ~ number ~ EOI }` is mapped to single struct fields `struct Entry {expr: Expr, number: Number}`. 
 - A **Ordered Choice** like `operation = { add | subtract | multiply | divide | power }` is mapped to multiple optional fields `struct Operation {add: Option<Add>, subtract: Option<Subtract>, multiply: Option<Multiply>, divide: Option<Divide>, power: Option<Power> }`. 
 - A **Repetition** like `expr = { ( term)* }` is mapped to a vector `struct Expr{term:Vec<Term>}`
 - **Tags** are used for renaming the refered rules. So you can do the following `expr = { #tout = term ~ (term)* }`, which is mapped to `struct Expr{tout: Term, term: Vec<Term>}`
 - **Built-in rules** like `SOI`.`EOI`,... are ignored
 - All nonterminals in **Atomic rules** are silent. Therfore we map the token of an atomic rule to a string. `num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }` is represented as `struct Num{content: String}`
 - **Builtin ASCII Rules** are ignored for AST generation. If a rule has only terminals or builtin rules the token is mapped to a string like for atomic rules.

## How to use

Add the following dependencies:
* pest = "2.7.6"
* pest_derive = { version = "2.7.6", features = ["grammar-extras"] }
* pestast = { git = "https://github.com/marcfir/pestast.git" }
* pestast_derive = { git = "https://github.com/marcfir/pestast.git" }

```rust
use pest::Parser;
use pestast::Ast;
mod TestLang {
    use pest_derive::Parser;
    use pestast_derive::Ast;
    #[derive(Parser, Ast)]
    #[ast(astnodes_name=PestAst, struct_pre=PestAst, derives=(Debug,Clone))]
    #[grammar_inline = r###"
    WHITESPACE = _{ " " | "\t" | NEWLINE }
    entry = { SOI ~ (value)* ~ EOI }
    value = { boolean | integer | string }

    integer = @{ "-"? ~ ASCII_DIGIT+ }
    string  = @{ "\"" ~ (!("\"") ~ (ANY))* ~ "\"" }
    boolean = @{ "True" | "False" }

    "###]
    pub struct Workbench;
}
fn main(){
    //Parse your content with the PEST parser  
    let pairs = TestLang::Workbench::parse(
        TestLang::Rule::entry,
        r#"-45 "testmy" 888 99            True        "#,
    )
    .unwrap();

    //Make the ast
    let ast = TestLang::Workbench::as_ast(pairs).unwrap();
    println!("{:#?}", ast);

    //Iterate over the ast
    for i in ast.into_iter() {
        println!("{:#?}", i);
    }
}
```

### Configuration of the AST generation with the `ast` attribute

You can configure the following properties with the `#[ast(...)]` attribute

#### AST node naming
For each rule a struct is generated which represents an AST node. The naming follows `{struct_pre}{rule_name}{struct_post}` which is transformed to CamelCase.
E.G. with struct_pre=MyAst and the rule `main` the AST node struct `MyAstMain` is generated.
* **struct_pre** 
* **struct_post**

#### AST node enum wrapper
One enum is generated which wraps all AST node. This is used for iterating over the AST nodes. The name of the enum must be set with **astnodes_name**
* **astnodes_name**

#### Derive
The content of the `Derive` attribute for the AST node structs and the AST node enum wrapper can be set with **derives**. E.g. `derives=(Debug,Clone)`.
* **derives**

#### Grammar entry
The entry point of the grammar can be set with **entry**. E.g. `entry=myentryrule`.
* **entry**

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.