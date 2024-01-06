// pestast
// Copyright (c) 2023 Marc Fischer
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

//! This module has helpers and types to abstract and flatten the rules of a pest grammar

use std::collections::HashMap;

use pest_meta::{
    ast::RuleType,
    optimizer::{OptimizedExpr, OptimizedRule},
};

#[derive(Debug, Clone, PartialEq)]
enum IterState {
    Repetition,
    Choice,
    Optional,
    Tag,
}

#[derive(Debug, Clone)]
pub struct FlatIdent {
    pub rule: String,
    pub ty: FlatIdentType,
    pub is_tag: Option<String>,
}

impl FlatIdent {
    pub fn get_effective_name(&self) -> &str {
        match &self.is_tag {
            None => &self.rule,
            Some(tag) => tag,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FlatIdentType {
    Repetition,
    Choice,
    Single,
}
#[derive(Debug, Clone)]
pub struct FlatIdentRule {
    pub rule: String,
    pub is_silent: bool,
    pub referenced_idents: Vec<FlatIdent>,
}

/// Looks into the stack and identifies the ident type (Single|Choice|Repetition)
fn resolve_id(
    id: &str,
    is_tag: Option<String>,
    state: &mut [IterState],
    idents: &mut Vec<FlatIdent>,
) {
    //Go through the existing idents
    match idents.iter_mut().find(|fi| fi.rule == id) {
        //If there is a ident with the same name, we change the IdentType to repetition.
        Some(fi) => {
            fi.ty = FlatIdentType::Repetition;
        }
        //If there is no other ident with the same name, we identify the IdentType based on the state.
        None => {
            //Look downwards the state stack and check for tag, repetition,choice and optional ids
            let has_tag = state.iter().rev().any(|s| *s == IterState::Tag);
            let has_rep = state.iter().rev().any(|s| *s == IterState::Repetition);
            let has_choice = state.iter().rev().any(|s| *s == IterState::Choice);
            let has_opt = state.iter().rev().any(|s| *s == IterState::Optional);
            //Order is important
            if has_tag {
                return;
            }
            if has_rep {
                idents.push(FlatIdent {
                    rule: id.to_string(),
                    ty: FlatIdentType::Repetition,
                    is_tag,
                });
                return;
            }
            if has_choice || has_opt {
                idents.push(FlatIdent {
                    rule: id.to_string(),
                    ty: FlatIdentType::Choice,
                    is_tag,
                });
                return;
            }
            idents.push(FlatIdent {
                rule: id.to_string(),
                ty: FlatIdentType::Single,
                is_tag,
            });
        }
    }
}

fn resolve_repetition(
    expr: &OptimizedExpr,
    state: &mut Vec<IterState>,
    idents: &mut Vec<FlatIdent>,
) {
    for new_ident in resolve_childidents_recursive(expr, state) {
        match idents.iter_mut().find(|fi| {
            (fi.rule == new_ident.rule) && (fi.is_tag.is_none() && new_ident.is_tag.is_none())
        }) {
            None => {
                // println!("push {:?}", &new_ident);
                idents.push(new_ident);
            }
            Some(e) => {
                e.ty = FlatIdentType::Repetition;
                // println!("change to rep {:?}", e);
            }
        }
    }
}
/// Resolves the child ident types recursively.
/// We keep track of the state of the expressions to identify the FlatIdentType
fn resolve_childidents_recursive(
    expr: &OptimizedExpr,
    state: &mut Vec<IterState>,
) -> Vec<FlatIdent> {
    let mut idents: Vec<FlatIdent> = vec![];
    match expr {
        OptimizedExpr::Seq(lhs, rhs) => {
            resolve_repetition(lhs, state, &mut idents);
            resolve_repetition(rhs, state, &mut idents);
        }
        OptimizedExpr::Choice(lhs, rhs) => {
            state.push(IterState::Choice);
            resolve_repetition(lhs, state, &mut idents);
            resolve_repetition(rhs, state, &mut idents);
            state.pop();
        }
        OptimizedExpr::Rep(expr) => {
            state.push(IterState::Repetition);
            resolve_repetition(expr, state, &mut idents);
            state.pop();
        }
        OptimizedExpr::Opt(expr) => {
            state.push(IterState::Optional);
            resolve_repetition(expr, state, &mut idents);
            state.pop();
        }
        OptimizedExpr::NodeTag(expr, tagname) => {
            let id = match (**expr).clone() {
                OptimizedExpr::Ident(id) => id,
                _ => panic!("Tags can only be used for rule references. Expect `#mytag = myrule`"),
            };
            resolve_id(&id, Some(tagname.clone()), state, &mut idents);
        }
        OptimizedExpr::PosPred(expr) | OptimizedExpr::NegPred(expr) | OptimizedExpr::Push(expr) => {
            resolve_repetition(expr, state, &mut idents);
        }
        OptimizedExpr::Ident(id) => resolve_id(id, None, state, &mut idents),
        _ => (),
    }
    idents
}

fn find_silent_replaces(
    issilence_map: &HashMap<String, FlatIdentRule>,
) -> HashMap<String, Vec<FlatIdent>> {
    fn find_nonsilent_replaces(
        start: FlatIdent,
        issilence_map: &HashMap<String, FlatIdentRule>,
    ) -> Option<Vec<FlatIdent>> {
        let Some(fir) = issilence_map.get(&start.rule) else {
            return None;
        };

        if fir.is_silent {
            let mut unsilent_childrules = vec![];
            for child in fir.referenced_idents.iter() {
                if let Some(mut inner) = find_nonsilent_replaces(child.clone(), issilence_map) {
                    unsilent_childrules.append(&mut inner);
                }
            }
            match unsilent_childrules.len() {
                0 => return None,
                _ => return Some(unsilent_childrules),
            }
        }
        Some(vec![start])
    }
    let mut res = HashMap::new();
    for (rule, fir) in issilence_map {
        if fir.is_silent {
            match find_nonsilent_replaces(
                FlatIdent {
                    rule: rule.to_string(),
                    ty: FlatIdentType::Single,
                    is_tag: None,
                },
                issilence_map,
            ) {
                Some(replacesable) => res.insert(rule.clone(), replacesable),
                None => res.insert(rule.clone(), vec![]),
            };
        }
    }
    res
}

/// Reads an optimized expression of the PEST grammar and returns a flat map of rule references.
/// The flattening process is the core logic of the AST generation.
pub fn extract_idents(rules: &[OptimizedRule]) -> Vec<FlatIdentRule> {
    let mut state: Vec<IterState> = vec![];
    let mut flat_ident_rules: Vec<FlatIdentRule> = vec![];
    let mut issilence_map: HashMap<String, FlatIdentRule> = HashMap::new();

    for rule in rules {
        let mut child_idents = vec![];
        //In an Atomic rule, interior matching rules are silent.
        if rule.ty != RuleType::Atomic {
            child_idents = resolve_childidents_recursive(&rule.expr, &mut state);
        }
        let flat_ident_rule = FlatIdentRule {
            referenced_idents: child_idents,
            rule: rule.name.clone(),
            is_silent: rule.ty == RuleType::Silent,
        };
        if !flat_ident_rule.is_silent {
            flat_ident_rules.push(flat_ident_rule.clone());
        }
        issilence_map.insert(rule.name.clone(), flat_ident_rule);
    }
    let mut replaces = find_silent_replaces(&issilence_map);
    for fir in flat_ident_rules.iter_mut() {
        for (replace_rule, new_childs) in &mut replaces {
            if !new_childs.is_empty() {
                let mut old_type = FlatIdentType::Single;
                if fir.referenced_idents.iter().any(|fi| {
                    old_type = fi.ty.clone();
                    fi.rule == *replace_rule
                }) {
                    //Use the FlatIdenType and adapt new_childs
                    for c in new_childs.iter_mut() {
                        if c.ty == FlatIdentType::Repetition {
                            continue;
                        }
                        if old_type == FlatIdentType::Repetition {
                            c.ty = FlatIdentType::Repetition;
                            continue;
                        }
                        if old_type == FlatIdentType::Choice {
                            c.ty = FlatIdentType::Choice;
                            continue;
                        }
                    }
                    fir.referenced_idents.retain(|fi| fi.rule != *replace_rule);
                    fir.referenced_idents.append(&mut new_childs.clone());
                }
            } else {
                fir.referenced_idents.retain(|fi| fi.rule != *replace_rule);
            }
        }
    }
    flat_ident_rules
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::generate::parse_grammar;

    #[test]
    #[should_panic(
        expected = "Tags can only be used for rule references. Expect `#mytag = myrule`"
    )]
    fn test_tag() {
        let gram = r#"
        num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }
        int = { ("+" | "-")? ~ ASCII_DIGIT+ }
        hans = {#left = (num)* ~ "|" ~ #right = num }
        "#;
        let (opt, _, _) = parse_grammar(gram).unwrap();
        let _ = extract_idents(&opt);
    }

    #[test]
    fn test_tag_repetition() {
        let gram = r#"
        num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }
        int = { ("+" | "-")? ~ ASCII_DIGIT+ }
        hans = {(#left = num)* ~ "|" ~ #right = num }
        peter = {(#left = num)* ~ "|" ~ #right = num  ~  num }
        olga = {(#left = num)* ~ "|" ~ #right = num  ~  num ~ num }
        "#;
        let (opt, _, _) = parse_grammar(gram).unwrap();
        let res = extract_idents(&opt);
        for r in &res {
            println!("{:?}", r)
        }
        assert!(
            res.iter()
                .find(|r| r.rule == "hans")
                .unwrap()
                .referenced_idents
                .iter()
                .find(|r| r.is_tag == Some("left".to_string()))
                .unwrap()
                .ty
                == FlatIdentType::Repetition
        );
        assert!(
            res.iter()
                .find(|r| r.rule == "hans")
                .unwrap()
                .referenced_idents
                .iter()
                .find(|r| r.is_tag == Some("right".to_string()))
                .unwrap()
                .ty
                == FlatIdentType::Single
        );
        assert!(
            res.iter()
                .find(|r| r.rule == "peter")
                .unwrap()
                .referenced_idents
                .iter()
                .find(|r| r.is_tag == Some("left".to_string()))
                .unwrap()
                .ty
                == FlatIdentType::Repetition
        );
        assert!(
            res.iter()
                .find(|r| r.rule == "peter")
                .unwrap()
                .referenced_idents
                .iter()
                .find(|r| r.is_tag == Some("right".to_string()))
                .unwrap()
                .ty
                == FlatIdentType::Single
        );
        assert!(
            res.iter()
                .find(|r| r.rule == "peter")
                .unwrap()
                .referenced_idents
                .iter()
                .find(|r| r.is_tag.is_none())
                .unwrap()
                .ty
                == FlatIdentType::Single
        );
        assert!(
            res.iter()
                .find(|r| r.rule == "olga")
                .unwrap()
                .referenced_idents
                .iter()
                .find(|r| r.is_tag.is_none())
                .unwrap()
                .ty
                == FlatIdentType::Repetition
        );
    }

    #[test]
    fn new_idents() {
        let gram = r#"
        entry = { SOI ~ #hans = expr ~ EOI }
        num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }
        int = { ("+" | "-")? ~ ASCII_DIGIT+ }
        hans = {#left = num ~ "|" ~ #right = num }
        
        operation = _{ add | subtract | multiply | divide | power }
            add      = { "+" }
            subtract = { "-" }
            multiply = { "*" }
            divide   = { "/" }
            power    = { "^" }
        
        expr = { term ~ (operation ~ term)* }
        term = _{ num | "(" ~ expr ~ ")" }
        
        WHITESPACE = _{ " " | "\t" }

        "#;
        let (opt, _, _) = parse_grammar(gram).unwrap();
        let res = extract_idents(&opt);
        for r in &res {
            println!("{:?}", r)
        }
        assert!(
            res.iter()
                .find(|r| r.rule == "expr")
                .unwrap()
                .referenced_idents
                .iter()
                .find(|r| r.rule == "add")
                .unwrap()
                .ty
                == FlatIdentType::Repetition
        );
        assert!(
            res.iter()
                .find(|r| r.rule == "expr")
                .unwrap()
                .referenced_idents
                .iter()
                .find(|r| r.rule == "expr")
                .unwrap()
                .ty
                == FlatIdentType::Repetition
        );
    }

    #[test]
    fn test_tags() {
        let gram = r#"
        entry = { SOI ~ #hans = expr ~ EOI }
        num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }
        int = { ("+" | "-")? ~ ASCII_DIGIT+ }
        hans = {#left = num ~ "|" ~ #right = num }
        row = { (hans ~ num)* ~ hans }
        row2 = { (#ihans = hans ~ num)* ~ #ohans = hans }
        
        operation = _{ add | subtract | multiply | divide | power }
            add      = { "+" }
            subtract = { "-" }
            multiply = { "*" }
            divide   = { "/" }
            power    = { "^" }
        
        expr = { term ~ (operation ~ term)* }
        term = _{ num | "(" ~ expr ~ ")" }
        
        WHITESPACE = _{ " " | "\t" }

        "#;
        let (opt, _, _) = parse_grammar(gram).unwrap();
        let res = extract_idents(&opt);
        for r in &res {
            println!("{:?}", r)
        }
        assert!(
            res.iter()
                .find(|r| r.rule == "row2")
                .unwrap()
                .referenced_idents
                .iter()
                .find(|r| r.is_tag == Some("ohans".to_string()))
                .unwrap()
                .ty
                == FlatIdentType::Single
        );
    }
}
