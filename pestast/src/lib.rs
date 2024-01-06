// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

pub use pest::{RuleType, Span};
pub use pestast_derive;
use thiserror::Error;

/// Errors for pestast
#[derive(Error, Debug)]
pub enum PestastError {
    #[error("{0}")]
    GetAst(String),
}

/// A trait which provides a function to convert the pest parse tree into an AST with the pairs API.
pub trait Ast<'a, R: RuleType, E> {
    fn as_ast(pairs: pest::iterators::Pairs<'a, R>) -> Result<E, PestastError>
    where
        Self: Sized;
}

/// A span for an AST with a `Span` defined in Pest and `linecol`
#[derive(Debug, Clone)]
pub struct AstSpan<'i> {
    linecol: (usize, usize),
    span: Span<'i>,
}

impl<'i> AstSpan<'i> {
    /// Create a new `AstSpan`
    pub fn new(linecol: (usize, usize), span: Span<'i>) -> Self {
        Self { linecol, span }
    }
    /// Returns the `Span` defined by the `AstNode`, **without** consuming it.
    pub fn as_span(&self) -> Span<'i> {
        self.span
    }
    /// Returns the `line`, `col` of this ast node span start.
    pub fn line_col(&self) -> (usize, usize) {
        self.linecol
    }
}
