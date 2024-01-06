// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use criterion::{criterion_group, criterion_main, Criterion};
use pest::Parser;
use pestast::Ast;
use simple_example_lib::test_langs::json;

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("json");
    group.sample_size(20);
    group.bench_function("json_parse", |b| {
        b.iter(|| {
            let json_file = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/data/Canada.json"));
            let _ = json::Workbench::parse(json::Rule::json, json_file).unwrap();
        })
    });
    group.bench_function("json_all", |b| {
        b.iter(|| {
            let json_file = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/data/Canada.json"));
            let pairs = json::Workbench::parse(json::Rule::json, json_file).unwrap();
            //Make the ast
            let _ = json::Workbench::as_ast(pairs).unwrap();
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
