use std::collections::HashMap;
use std::fmt;

use crate::core::{CaseArm, CoreModule, CorePattern, CoreTerm};
use crate::errors::TypeError;

#[derive(Debug, Clone)]
pub struct CtorInfo {
    pub parent: String,
}

#[derive(Debug, Clone)]
pub struct CoverageEnv {
    pub ctors: HashMap<String, CtorInfo>,
    pub siblings: HashMap<String, Vec<(String, usize)>>,
}

impl CoverageEnv {
    pub fn from_module(module: &CoreModule) -> Self {
        let mut ctors = HashMap::new();
        let mut siblings = HashMap::new();
        for tdef in &module.type_defs {
            let mut entries = Vec::new();
            for dctor in &tdef.constructors {
                let arity = arrow_arity(&dctor.ty);
                ctors.insert(
                    dctor.name.clone(),
                    CtorInfo {
                        parent: tdef.name.clone(),
                    },
                );
                entries.push((dctor.name.clone(), arity));
            }
            siblings.insert(tdef.name.clone(), entries);
        }
        CoverageEnv { ctors, siblings }
    }
}

fn arrow_arity(ty: &crate::core::CoreType) -> usize {
    let mut current = ty;
    while let crate::core::CoreType::Forall(_, body) = current {
        current = body;
    }
    let mut n = 0;
    while let crate::core::CoreType::Arrow(_, rest) = current {
        n += 1;
        current = rest;
    }
    n
}

#[derive(Debug, Clone)]
pub enum Witness {
    Wild,
    Ctor(String, Vec<Witness>),
}

impl fmt::Display for Witness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Witness::Wild => write!(f, "_"),
            Witness::Ctor(name, args) if args.is_empty() => write!(f, "{}", name),
            Witness::Ctor(name, args) => {
                write!(f, "{}", name)?;
                for a in args {
                    match a {
                        Witness::Ctor(_, sub) if !sub.is_empty() => write!(f, " ({})", a)?,
                        _ => write!(f, " {}", a)?,
                    }
                }
                Ok(())
            }
        }
    }
}

type Row = Vec<CorePattern>;
type Matrix = Vec<Row>;

fn head_ctor(p: &CorePattern) -> Option<(&str, &[CorePattern])> {
    match p {
        CorePattern::Constructor { name, args } => Some((name.as_str(), args.as_slice())),
        _ => None,
    }
}

fn specialize(matrix: &Matrix, ctor: &str, arity: usize) -> Matrix {
    let mut out = Matrix::new();
    for row in matrix {
        let Some((head, rest)) = row.split_first() else {
            continue;
        };
        match head {
            CorePattern::Constructor { name, args } if name == ctor => {
                let mut new_row: Row = args.clone();
                new_row.extend_from_slice(rest);
                out.push(new_row);
            }
            CorePattern::Wildcard | CorePattern::Var(_) => {
                let mut new_row: Row = vec![CorePattern::Wildcard; arity];
                new_row.extend_from_slice(rest);
                out.push(new_row);
            }
            CorePattern::Constructor { .. } => {}
        }
    }
    out
}

fn default_matrix(matrix: &Matrix) -> Matrix {
    let mut out = Matrix::new();
    for row in matrix {
        let Some((head, rest)) = row.split_first() else {
            continue;
        };
        match head {
            CorePattern::Wildcard | CorePattern::Var(_) => out.push(rest.to_vec()),
            CorePattern::Constructor { .. } => {}
        }
    }
    out
}

fn column_ctors(matrix: &Matrix) -> Vec<(String, usize)> {
    let mut seen = Vec::new();
    for row in matrix {
        if let Some(first) = row.first() {
            if let Some((name, args)) = head_ctor(first) {
                if !seen.iter().any(|(n, _): &(String, usize)| n == name) {
                    seen.push((name.to_string(), args.len()));
                }
            }
        }
    }
    seen
}

fn find_witness(matrix: &Matrix, width: usize, env: &CoverageEnv) -> Option<Vec<Witness>> {
    if width == 0 {
        return if matrix.is_empty() {
            Some(Vec::new())
        } else {
            None
        };
    }
    let present = column_ctors(matrix);
    if present.is_empty() {
        let sub = default_matrix(matrix);
        let tail = find_witness(&sub, width - 1, env)?;
        let mut out = vec![Witness::Wild];
        out.extend(tail);
        return Some(out);
    }
    let parent = env.ctors.get(&present[0].0).map(|c| c.parent.clone());
    let all_siblings = parent.as_ref().and_then(|p| env.siblings.get(p)).cloned();
    if let Some(siblings) = all_siblings {
        for (sname, sarity) in &siblings {
            if !present.iter().any(|(n, _)| n == sname) {
                let sub = default_matrix(matrix);
                let tail = find_witness(&sub, width - 1, env)?;
                let args = vec![Witness::Wild; *sarity];
                let mut out = vec![Witness::Ctor(sname.clone(), args)];
                out.extend(tail);
                return Some(out);
            }
        }
        for (cname, carity) in &present {
            let sub = specialize(matrix, cname, *carity);
            if let Some(witness) = find_witness(&sub, carity + width - 1, env) {
                let (head_args, tail) = witness.split_at(*carity);
                let mut out = vec![Witness::Ctor(cname.clone(), head_args.to_vec())];
                out.extend(tail.iter().cloned());
                return Some(out);
            }
        }
        None
    } else {
        let sub = default_matrix(matrix);
        let tail = find_witness(&sub, width - 1, env)?;
        let mut out = vec![Witness::Wild];
        out.extend(tail);
        Some(out)
    }
}

fn is_useful(matrix: &Matrix, row: &Row, env: &CoverageEnv) -> bool {
    if row.is_empty() {
        return matrix.is_empty();
    }
    let (head, rest) = row.split_first().unwrap();
    match head {
        CorePattern::Constructor { name, args } => {
            let sub = specialize(matrix, name, args.len());
            let mut new_row: Row = args.clone();
            new_row.extend_from_slice(rest);
            is_useful(&sub, &new_row, env)
        }
        CorePattern::Wildcard | CorePattern::Var(_) => {
            let present = column_ctors(matrix);
            let parent = env
                .ctors
                .get(&present.first().map(|c| c.0.clone()).unwrap_or_default());
            let siblings = parent.and_then(|c| env.siblings.get(&c.parent)).cloned();
            let exhaustive = siblings
                .as_ref()
                .map(|s| s.iter().all(|(n, _)| present.iter().any(|(p, _)| p == n)))
                .unwrap_or(false);
            if exhaustive {
                present.iter().any(|(name, arity)| {
                    let sub = specialize(matrix, name, *arity);
                    let mut new_row: Row = vec![CorePattern::Wildcard; *arity];
                    new_row.extend_from_slice(rest);
                    is_useful(&sub, &new_row, env)
                })
            } else {
                let sub = default_matrix(matrix);
                is_useful(&sub, &rest.to_vec(), env)
            }
        }
    }
}

pub fn check_match(arms: &[CaseArm], env: &CoverageEnv) -> Result<(), TypeError> {
    let mut matrix: Matrix = Vec::new();
    for (i, arm) in arms.iter().enumerate() {
        let row = vec![arm.pattern.clone()];
        if !is_useful(&matrix, &row, env) {
            return Err(TypeError::UnreachableMatchArm {
                index: i,
                span: None,
            });
        }
        matrix.push(row);
    }
    if let Some(witness) = find_witness(&matrix, 1, env) {
        let pretty = witness
            .into_iter()
            .map(|w| w.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        return Err(TypeError::NonExhaustiveMatch {
            missing: pretty,
            span: None,
        });
    }
    Ok(())
}

pub fn check_module(module: &CoreModule) -> Result<(), TypeError> {
    let env = CoverageEnv::from_module(module);
    for tdef in &module.term_defs {
        walk_term(&tdef.body, &env)?;
    }
    Ok(())
}

fn walk_term(term: &CoreTerm, env: &CoverageEnv) -> Result<(), TypeError> {
    match term {
        CoreTerm::Var(_) | CoreTerm::LitInt(_) => Ok(()),
        CoreTerm::Lambda { body, .. } | CoreTerm::TypeLambda { body, .. } => walk_term(body, env),
        CoreTerm::App { func, arg } => {
            walk_term(func, env)?;
            walk_term(arg, env)
        }
        CoreTerm::Constructor { args, .. } | CoreTerm::IntrinsicCall { args, .. } => {
            for a in args {
                walk_term(a, env)?;
            }
            Ok(())
        }
        CoreTerm::BinOp { left, right, .. } => {
            walk_term(left, env)?;
            walk_term(right, env)
        }
        CoreTerm::If {
            cond,
            then_branch,
            else_branch,
        } => {
            walk_term(cond, env)?;
            walk_term(then_branch, env)?;
            walk_term(else_branch, env)
        }
        CoreTerm::Case { scrutinee, arms } => {
            walk_term(scrutinee, env)?;
            for arm in arms {
                walk_term(&arm.body, env)?;
            }
            check_match(arms, env)
        }
    }
}
