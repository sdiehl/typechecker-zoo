use std::collections::HashMap;

use crate::ast::{MatchArm, Pattern, Term, Universe};

/// Substitute universe variables in a term
pub fn substitute_universes(term: &Term, subst: &HashMap<String, Universe>) -> Term {
    match term {
        Term::Sort(u) => Term::Sort(substitute_universe(u, subst)),
        Term::Var(x) => Term::Var(x.clone()),
        Term::Const(c) => Term::Const(c.clone()),
        Term::App(f, arg) => Term::App(
            Box::new(substitute_universes(f, subst)),
            Box::new(substitute_universes(arg, subst)),
        ),
        Term::Abs(x, ty, body) => Term::Abs(
            x.clone(),
            Box::new(substitute_universes(ty, subst)),
            Box::new(substitute_universes(body, subst)),
        ),
        Term::Pi(x, ty, body, implicit) => Term::Pi(
            x.clone(),
            Box::new(substitute_universes(ty, subst)),
            Box::new(substitute_universes(body, subst)),
            *implicit,
        ),
        Term::Let(x, ty, val, body) => Term::Let(
            x.clone(),
            Box::new(substitute_universes(ty, subst)),
            Box::new(substitute_universes(val, subst)),
            Box::new(substitute_universes(body, subst)),
        ),
        _ => term.clone(), // For other terms, no substitution needed
    }
}

/// Substitute universe variables in a universe expression
pub fn substitute_universe(u: &Universe, subst: &HashMap<String, Universe>) -> Universe {
    let result = match u {
        Universe::ScopedVar(_scope, var) => {
            // For scoped variables, try to substitute using just the variable name
            // This allows substitution within the correct scope
            subst.get(var).cloned().unwrap_or_else(|| u.clone())
        }
        Universe::Const(n) => Universe::Const(*n),
        Universe::Add(u, n) => {
            let base = substitute_universe(u, subst);
            Universe::Add(Box::new(base), *n)
        }
        Universe::Max(u1, u2) => Universe::Max(
            Box::new(substitute_universe(u1, subst)),
            Box::new(substitute_universe(u2, subst)),
        ),
        Universe::IMax(u1, u2) => Universe::IMax(
            Box::new(substitute_universe(u1, subst)),
            Box::new(substitute_universe(u2, subst)),
        ),
        _ => u.clone(),
    };
    normalize_universe(&result)
}

/// Normalize a universe expression by simplifying it
pub fn normalize_universe(u: &Universe) -> Universe {
    match u {
        Universe::Add(base, n) => match **base {
            Universe::Const(m) => Universe::Const(m + n),
            _ => u.clone(),
        },
        Universe::Max(u1, u2) => {
            let u1_norm = normalize_universe(u1);
            let u2_norm = normalize_universe(u2);
            match (&u1_norm, &u2_norm) {
                (Universe::Const(n1), Universe::Const(n2)) => Universe::Const((*n1).max(*n2)),
                _ => Universe::Max(Box::new(u1_norm), Box::new(u2_norm)),
            }
        }
        _ => u.clone(),
    }
}

/// Rename variable in term
pub fn rename_var(old: &str, new: &str, term: &Term) -> Term {
    match term {
        Term::Var(x) if x == old => Term::Var(new.to_string()),
        Term::Var(_) => term.clone(),
        Term::App(f, arg) => Term::App(
            Box::new(rename_var(old, new, f)),
            Box::new(rename_var(old, new, arg)),
        ),
        Term::Abs(x, ty, body) if x == old => {
            // Don't rename bound occurrences
            Term::Abs(
                new.to_string(),
                Box::new(rename_var(old, new, ty)),
                Box::new(body.as_ref().clone()),
            )
        }
        Term::Abs(x, ty, body) => Term::Abs(
            x.clone(),
            Box::new(rename_var(old, new, ty)),
            Box::new(rename_var(old, new, body)),
        ),
        Term::Pi(x, ty, body, implicit) if x == old => Term::Pi(
            new.to_string(),
            Box::new(rename_var(old, new, ty)),
            Box::new(body.as_ref().clone()),
            *implicit,
        ),
        Term::Pi(x, ty, body, implicit) => Term::Pi(
            x.clone(),
            Box::new(rename_var(old, new, ty)),
            Box::new(rename_var(old, new, body)),
            *implicit,
        ),
        Term::Let(x, ty, val, body) if x == old => Term::Let(
            new.to_string(),
            Box::new(rename_var(old, new, ty)),
            Box::new(rename_var(old, new, val)),
            Box::new(body.as_ref().clone()),
        ),
        Term::Let(x, ty, val, body) => Term::Let(
            x.clone(),
            Box::new(rename_var(old, new, ty)),
            Box::new(rename_var(old, new, val)),
            Box::new(rename_var(old, new, body)),
        ),
        Term::Sigma(x, domain, codomain) if x == old => Term::Sigma(
            new.to_string(),
            Box::new(rename_var(old, new, domain)),
            Box::new(codomain.as_ref().clone()),
        ),
        Term::Sigma(x, domain, codomain) => Term::Sigma(
            x.clone(),
            Box::new(rename_var(old, new, domain)),
            Box::new(rename_var(old, new, codomain)),
        ),
        Term::Pair(fst, snd) => Term::Pair(
            Box::new(rename_var(old, new, fst)),
            Box::new(rename_var(old, new, snd)),
        ),
        Term::Fst(pair) => Term::Fst(Box::new(rename_var(old, new, pair))),
        Term::Snd(pair) => Term::Snd(Box::new(rename_var(old, new, pair))),
        Term::Proj(term, field) => Term::Proj(Box::new(rename_var(old, new, term)), field.clone()),
        Term::Match(scrutinee, arms) => {
            let scrutinee_renamed = rename_var(old, new, scrutinee);
            let arms_renamed = arms
                .iter()
                .map(|arm| {
                    // For match arms, we need to be careful about pattern variable bindings
                    let body_renamed = if pattern_binds_var(&arm.pattern, old) {
                        // Variable is bound by pattern, don't rename in body
                        arm.body.clone()
                    } else {
                        rename_var(old, new, &arm.body)
                    };

                    MatchArm {
                        pattern: arm.pattern.clone(), // Patterns don't contain renameable terms
                        body: body_renamed,
                    }
                })
                .collect();

            Term::Match(Box::new(scrutinee_renamed), arms_renamed)
        }
        _ => term.clone(),
    }
}

/// Check if a pattern binds a specific variable
pub fn pattern_binds_var(pattern: &Pattern, var: &str) -> bool {
    match pattern {
        Pattern::Var(x) => x == var,
        Pattern::Wildcard => false,
        Pattern::Constructor(_, args) => args.iter().any(|arg| pattern_binds_var(arg, var)),
    }
}

/// Generate fresh variable name avoiding conflicts
pub fn fresh_var(base: &str, avoid: &[String]) -> String {
    let mut counter = 0;
    loop {
        let candidate = if counter == 0 {
            base.to_string()
        } else {
            format!("{}{}", base, counter)
        };

        if !avoid.contains(&candidate) {
            return candidate;
        }
        counter += 1;
    }
}

/// Check if a type is "simple" (like Nat, Bool, etc.)
pub fn is_simple_type(ty: &Term) -> bool {
    matches!(ty, Term::Const(_))
}

/// Check if a term has implicit parameters
pub fn has_implicit_params(ty: &Term) -> bool {
    match ty {
        Term::Pi(_, _, _, implicit) => *implicit,
        _ => false,
    }
}

/// Extract the return type from a constructor type (the final codomain after
/// stripping Pi types)
pub fn extract_constructor_return_type(ctor_type: &Term) -> Term {
    match ctor_type {
        Term::Pi(_, _, codomain, _) => extract_constructor_return_type(codomain),
        other => other.clone(),
    }
}

/// Extract argument types from a constructor type, skipping type parameters
pub fn extract_constructor_arg_types(ctor_type: &Term) -> Vec<Term> {
    let mut arg_types = Vec::new();
    let mut current = ctor_type;

    while let Term::Pi(_, domain, codomain, _) = current {
        // Skip type parameters (those whose type is a Sort)
        if !matches!(domain.as_ref(), Term::Sort(_)) {
            arg_types.push(domain.as_ref().clone());
        }
        current = codomain;
    }

    arg_types
}

/// Check if a term contains universe metavariables
pub fn contains_universe_metavariables(term: &Term) -> bool {
    match term {
        Term::Sort(u) => universe_contains_metavariables(u),
        Term::App(f, arg) => {
            contains_universe_metavariables(f) || contains_universe_metavariables(arg)
        }
        Term::Abs(_, ty, body) | Term::Pi(_, ty, body, _) => {
            contains_universe_metavariables(ty) || contains_universe_metavariables(body)
        }
        Term::Let(_, ty, val, body) => {
            contains_universe_metavariables(ty)
                || contains_universe_metavariables(val)
                || contains_universe_metavariables(body)
        }
        _ => false,
    }
}

/// Check if a universe expression contains metavariables
pub fn universe_contains_metavariables(universe: &Universe) -> bool {
    match universe {
        Universe::Meta(_) => true,
        Universe::Add(base, _) => universe_contains_metavariables(base),
        Universe::Max(u, v) | Universe::IMax(u, v) => {
            universe_contains_metavariables(u) || universe_contains_metavariables(v)
        }
        _ => false,
    }
}
