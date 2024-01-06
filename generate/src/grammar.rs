// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.
//
// pest. The Elegant Parser
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

// This code is taken from the pest project https://github.com/pest-parser/pest/blob/5d30733b0a094da36c7376f90c3bf9c4fc0119e7/generator/src/lib.rs

use std::{
    env,
    fs::File,
    io::{self, Read},
    path::{Path, PathBuf},
};

use proc_macro2::Span;
use syn::{Error, Result};

use crate::attributes::GrammarSource;

/// Processes the grammar input and returns the content of all reference grammars.
pub fn load_grammar(contents: Vec<GrammarSource>) -> Result<(String, Vec<PathBuf>)> {
    let mut grammars_content = String::new();
    let mut paths = vec![];
    for content in contents {
        let (_data, _path) = match content {
            GrammarSource::File(ref path) => {
                let root = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());

                // Check whether we can find a file at the path relative to the CARGO_MANIFEST_DIR
                // first.
                //
                // If we cannot find the expected file over there, fallback to the
                // `CARGO_MANIFEST_DIR/src`, which is the old default and kept for convenience
                // reasons.
                // TODO: This could be refactored once `std::path::absolute()` get's stabilized.
                // https://doc.rust-lang.org/std/path/fn.absolute.html
                let path = if Path::new(&root).join(path).exists() {
                    Path::new(&root).join(path)
                } else {
                    Path::new(&root).join("src/").join(path)
                };
                let file_name = match path.file_name() {
                    Some(file_name) => file_name,
                    None => {
                        return Err(Error::new(
                            Span::call_site(),
                            "grammar attribute should point to a file",
                        ));
                    }
                };

                let data = match read_file(&path) {
                    Ok(data) => data,
                    Err(error) => {
                        return Err(Error::new(
                            Span::call_site(),
                            format!("error opening {:?}: {}", file_name, error),
                        ))
                    }
                };
                (data, Some(path.clone()))
            }
            GrammarSource::Inline(content) => (content, None),
        };
        grammars_content.push_str(&_data);
        if let Some(path) = _path {
            paths.push(path);
        }
    }
    Ok((grammars_content, paths))
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path.as_ref())?;
    let mut string = String::new();
    file.read_to_string(&mut string)?;
    Ok(string)
}

#[cfg(test)]
mod tests {
    use super::load_grammar;
    use super::GrammarSource;

    #[test]
    fn derive_inline_file() {
        let gramsrc = vec![GrammarSource::Inline("GRAMMAR".to_string())];
        let (cont, paths) = load_grammar(gramsrc).unwrap();
        assert_eq!(cont, "GRAMMAR");
        assert_eq!(paths.len(), 0);
    }

    #[test]
    fn derive_ok() {
        let gramsrc = vec![GrammarSource::File("../tests/easytest.pest".to_string())];
        let (cont, paths) = load_grammar(gramsrc).unwrap();
        assert_eq!(cont, "this is my file");
        assert_eq!(paths.len(), 1);
    }
}
