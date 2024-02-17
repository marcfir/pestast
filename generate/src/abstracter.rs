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
    Tag(String, bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FlatRuleRef {
    pub referred_rule: String,
    pub ty: RuleRefType,
    pub is_tag: Option<String>,
}

impl FlatRuleRef {
    /// Returns the name of the referred rule or the tag if a tag is used.
    pub fn get_effective_name(&self) -> &str {
        match &self.is_tag {
            None => &self.referred_rule,
            Some(tag) => tag,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuleRefType {
    Repetition,
    Choice,
    Single,
}
#[derive(Debug, Clone, PartialEq)]
pub struct FlatRule {
    pub rule: String,
    pub is_silent: bool,
    pub referred_rules: Vec<FlatRuleRef>,
}

/// Looks into the stack and identifies the ident type (Single|Choice|Repetition)
fn resolve_id(id: &str, state: &mut [IterState], idents: &mut Vec<FlatRuleRef>) {
    //Go through the existing idents
    match idents.iter_mut().find(|fi| fi.referred_rule == id) {
        //If there is a ident with the same name, we change the IdentType to repetition.
        Some(fi) => {
            fi.ty = RuleRefType::Repetition;
        }
        //If there is no other ident with the same name, we identify the IdentType based on the state.
        None => {
            //Look downwards the state stack and check for tag, repetition,choice and optional ids
            let is_tag = state.iter_mut().rev().find_map(|s| match s {
                IterState::Tag(name, is_used) => {
                    if *is_used {
                        panic!(
                            "Multiple rule references are not supported for tags. Tag `{name}` already referenced."
                        );
                    } else {
                        *is_used = true;
                        Some(name.clone())
                    }
                }
                _ => None,
            });
            let has_rep = state.iter().rev().any(|s| *s == IterState::Repetition);
            let has_choice = state.iter().rev().any(|s| *s == IterState::Choice);
            let has_opt = state.iter().rev().any(|s| *s == IterState::Optional);
            //Order is important
            if has_rep {
                idents.push(FlatRuleRef {
                    referred_rule: id.to_string(),
                    ty: RuleRefType::Repetition,
                    is_tag,
                });
                return;
            }
            if has_choice || has_opt {
                idents.push(FlatRuleRef {
                    referred_rule: id.to_string(),
                    ty: RuleRefType::Choice,
                    is_tag,
                });
                return;
            }
            idents.push(FlatRuleRef {
                referred_rule: id.to_string(),
                ty: RuleRefType::Single,
                is_tag,
            });
        }
    }
}

fn resolve_repetition(
    expr: &OptimizedExpr,
    state: &mut Vec<IterState>,
    idents: &mut Vec<FlatRuleRef>,
) {
    for new_ident in resolve_childidents_recursive(expr, state) {
        match idents.iter_mut().find(|fi| {
            (fi.referred_rule == new_ident.referred_rule)
                && (fi.is_tag.is_none() && new_ident.is_tag.is_none())
        }) {
            None => idents.push(new_ident),
            Some(e) => e.ty = RuleRefType::Repetition,
        }
    }
}
/// Resolves the child ident types recursively.
/// We keep track of the state of the expressions to idenitfy the FlatIdentType
fn resolve_childidents_recursive(
    expr: &OptimizedExpr,
    state: &mut Vec<IterState>,
) -> Vec<FlatRuleRef> {
    let mut idents: Vec<FlatRuleRef> = vec![];
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
            if state.iter().rev().any(|s| matches!(*s, IterState::Tag(..))) {
                panic!("Nested tags are not supported. Tag `{tagname}` is inside another tag.");
            }
            state.push(IterState::Tag(tagname.clone(), false));
            resolve_repetition(expr, state, &mut idents);
            state.pop();
        }
        OptimizedExpr::PosPred(expr) | OptimizedExpr::NegPred(expr) | OptimizedExpr::Push(expr) => {
            resolve_repetition(expr, state, &mut idents);
        }
        OptimizedExpr::Ident(id) => resolve_id(id, state, &mut idents),
        _ => (),
    }
    idents
}

fn find_silent_replaces(
    issilence_map: &HashMap<String, FlatRule>,
) -> HashMap<String, Vec<FlatRuleRef>> {
    fn find_nonsilent_replaces(
        start: FlatRuleRef,
        issilence_map: &HashMap<String, FlatRule>,
    ) -> Option<Vec<FlatRuleRef>> {
        let Some(fir) = issilence_map.get(&start.referred_rule) else {
            return None;
        };

        if fir.is_silent {
            let mut unsilent_childrules = vec![];
            for child in fir.referred_rules.iter() {
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
                FlatRuleRef {
                    referred_rule: rule.to_string(),
                    ty: RuleRefType::Single,
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
pub fn extract_idents(rules: &[OptimizedRule]) -> Vec<FlatRule> {
    let mut state: Vec<IterState> = vec![];
    let mut flat_ident_rules: Vec<FlatRule> = vec![];
    let mut issilence_map: HashMap<String, FlatRule> = HashMap::new();

    for rule in rules {
        let mut child_idents = vec![];
        //In an Atomic rule, interior matching rules are silent.
        if rule.ty != RuleType::Atomic {
            child_idents = resolve_childidents_recursive(&rule.expr, &mut state);
        }

        let flat_ident_rule = FlatRule {
            referred_rules: child_idents,
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
                let mut old_type = RuleRefType::Single;
                if fir.referred_rules.iter().any(|fi| {
                    old_type = fi.ty.clone();
                    fi.referred_rule == *replace_rule
                }) {
                    //Use the FlatIdenType and adapt new_childs
                    for c in new_childs.iter_mut() {
                        if c.ty == RuleRefType::Repetition {
                            continue;
                        }
                        if old_type == RuleRefType::Repetition {
                            c.ty = RuleRefType::Repetition;
                            continue;
                        }
                        if old_type == RuleRefType::Choice {
                            c.ty = RuleRefType::Choice;
                            continue;
                        }
                    }
                    fir.referred_rules
                        .retain(|fi| fi.referred_rule != *replace_rule);
                    fir.referred_rules.append(&mut new_childs.clone());
                }
            } else {
                fir.referred_rules
                    .retain(|fi| fi.referred_rule != *replace_rule);
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
    #[should_panic(expected = "Nested tags are not supported. Tag `inner` is inside another tag.")]
    fn test_tag() {
        let gram = r#"
        num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }
        int = { ("+" | "-")? ~ ASCII_DIGIT+ }
        peter = {#left = (#inner = num)* ~ "|" ~ #right = num }
        "#;
        let (opt, _, _) = parse_grammar(gram).unwrap();
        let _ = extract_idents(&opt);
    }

    #[test]
    #[should_panic(
        expected = "Multiple rule references are not supported for tags. Tag `left` already referenced."
    )]
    fn test_tag_mul_ref() {
        let gram = r#"
        num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }
        int = { ("+" | "-")? ~ ASCII_DIGIT+ }
        olga = {#left = (num | int) ~ "|" ~ #right = num  ~  num ~ num }
        "#;
        let (opt, _, _) = parse_grammar(gram).unwrap();
        let _ = extract_idents(&opt);
    }

    #[test]
    fn test_tag_expression() {
        let gram = r#"
        num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }
        int = { ("+" | "-")? ~ ASCII_DIGIT+ }
        peter = {#left = (num)* ~ "|" ~ #right = num }
        "#;
        let (opt, _, _) = parse_grammar(gram).unwrap();
        let f = extract_idents(&opt);
        let mut peter = f
            .iter()
            .find(|r| r.rule == "peter")
            .unwrap()
            .referred_rules
            .iter();
        assert!(
            *peter.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Repetition,
                    is_tag: Some("left".to_string())
                }
        );
        assert!(
            *peter.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Single,
                    is_tag: Some("right".to_string())
                }
        );
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
        let f = extract_idents(&opt);

        let mut hans = f
            .iter()
            .find(|r| r.rule == "hans")
            .unwrap()
            .referred_rules
            .iter();
        assert!(
            *hans.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Repetition,
                    is_tag: Some("left".to_string())
                }
        );
        assert!(
            *hans.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Single,
                    is_tag: Some("right".to_string())
                }
        );
        let mut peter = f
            .iter()
            .find(|r| r.rule == "peter")
            .unwrap()
            .referred_rules
            .iter();
        assert!(
            *peter.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Repetition,
                    is_tag: Some("left".to_string())
                }
        );
        assert!(
            *peter.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Single,
                    is_tag: Some("right".to_string())
                }
        );
        assert!(
            *peter.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Single,
                    is_tag: None,
                }
        );
        let mut olga = f
            .iter()
            .find(|r| r.rule == "olga")
            .unwrap()
            .referred_rules
            .iter();
        assert!(
            *olga.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Repetition,
                    is_tag: Some("left".to_string())
                }
        );
        assert!(
            *olga.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Single,
                    is_tag: Some("right".to_string())
                }
        );
        assert!(
            *olga.next().unwrap()
                == FlatRuleRef {
                    referred_rule: "num".to_string(),
                    ty: RuleRefType::Repetition,
                    is_tag: None,
                }
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
                .referred_rules
                .iter()
                .find(|r| r.referred_rule == "add")
                .unwrap()
                .ty
                == RuleRefType::Repetition
        );
        assert!(
            res.iter()
                .find(|r| r.rule == "expr")
                .unwrap()
                .referred_rules
                .iter()
                .find(|r| r.referred_rule == "expr")
                .unwrap()
                .ty
                == RuleRefType::Repetition
        );
    }
}
