use std::collections::HashMap;
use std::fmt;

use crate::ast::{MatchArm, Term, Universe};

/// Substitution mapping variables to terms
#[derive(Debug, Clone, PartialEq)]
pub struct Substitution {
    term_subst: HashMap<String, Term>,
    universe_subst: HashMap<String, Universe>,
}

impl Substitution {
    pub fn new() -> Self {
        Substitution {
            term_subst: HashMap::new(),
            universe_subst: HashMap::new(),
        }
    }

    pub fn extend_term(&mut self, var: String, term: Term) {
        self.term_subst.insert(var, term);
    }

    pub fn extend_universe(&mut self, var: String, universe: Universe) {
        self.universe_subst.insert(var, universe);
    }

    pub fn add_substitution(&mut self, var: String, term: Term) {
        self.term_subst.insert(var, term);
    }

    pub fn apply_term(&self, term: &Term) -> Term {
        self.apply_term_impl(term, &mut Vec::new())
    }

    pub fn apply_universe(&self, universe: &Universe) -> Universe {
        match universe {
            Universe::ScopedVar(_scope, v) => self
                .universe_subst
                .get(v)
                .cloned()
                .unwrap_or_else(|| universe.clone()),
            Universe::Const(n) => Universe::Const(*n),
            Universe::Add(u, n) => Universe::Add(Box::new(self.apply_universe(u)), *n),
            Universe::Max(u, v) => Universe::Max(
                Box::new(self.apply_universe(u)),
                Box::new(self.apply_universe(v)),
            ),
            Universe::IMax(u, v) => Universe::IMax(
                Box::new(self.apply_universe(u)),
                Box::new(self.apply_universe(v)),
            ),
            _ => universe.clone(),
        }
    }

    /// Apply substitution with bound variable tracking to avoid capture
    fn apply_term_impl(&self, term: &Term, bound_vars: &mut Vec<String>) -> Term {
        match term {
            Term::Var(x) => {
                if bound_vars.contains(x) {
                    term.clone()
                } else {
                    self.term_subst
                        .get(x)
                        .cloned()
                        .unwrap_or_else(|| term.clone())
                }
            }
            Term::App(f, arg) => Term::App(
                Box::new(self.apply_term_impl(f, bound_vars)),
                Box::new(self.apply_term_impl(arg, bound_vars)),
            ),
            Term::Abs(x, ty, body) => {
                bound_vars.push(x.clone());
                let result = Term::Abs(
                    x.clone(),
                    Box::new(self.apply_term_impl(ty, bound_vars)),
                    Box::new(self.apply_term_impl(body, bound_vars)),
                );
                bound_vars.pop();
                result
            }
            Term::Pi(x, ty, body, implicit) => {
                bound_vars.push(x.clone());
                let result = Term::Pi(
                    x.clone(),
                    Box::new(self.apply_term_impl(ty, bound_vars)),
                    Box::new(self.apply_term_impl(body, bound_vars)),
                    *implicit,
                );
                bound_vars.pop();
                result
            }
            Term::Sort(u) => Term::Sort(self.apply_universe(u)),
            Term::Let(x, ty, val, body) => {
                let ty_result = self.apply_term_impl(ty, bound_vars);
                let val_result = self.apply_term_impl(val, bound_vars);
                bound_vars.push(x.clone());
                let body_result = self.apply_term_impl(body, bound_vars);
                bound_vars.pop();
                Term::Let(
                    x.clone(),
                    Box::new(ty_result),
                    Box::new(val_result),
                    Box::new(body_result),
                )
            }
            Term::Match(scrutinee, arms) => {
                Term::Match(
                    Box::new(self.apply_term_impl(scrutinee, bound_vars)),
                    arms.iter()
                        .map(|arm| {
                            MatchArm {
                                pattern: arm.pattern.clone(), /* Patterns don't contain
                                                               * substitutable terms */
                                body: self.apply_term_impl(&arm.body, bound_vars),
                            }
                        })
                        .collect(),
                )
            }
            Term::Constructor(name, args) => Term::Constructor(
                name.clone(),
                args.iter()
                    .map(|arg| self.apply_term_impl(arg, bound_vars))
                    .collect(),
            ),
            Term::Meta(name) => {
                // Look up meta-variable in substitution
                self.term_subst
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| term.clone())
            }
            Term::Const(_) => term.clone(),
            Term::Proj(term, field) => Term::Proj(
                Box::new(self.apply_term_impl(term, bound_vars)),
                field.clone(),
            ),
            Term::Sigma(x, domain, codomain) => {
                bound_vars.push(x.clone());
                let domain_result = self.apply_term_impl(domain, bound_vars);
                let codomain_result = self.apply_term_impl(codomain, bound_vars);
                bound_vars.pop();
                Term::Sigma(
                    x.clone(),
                    Box::new(domain_result),
                    Box::new(codomain_result),
                )
            }
            Term::Pair(fst, snd) => Term::Pair(
                Box::new(self.apply_term_impl(fst, bound_vars)),
                Box::new(self.apply_term_impl(snd, bound_vars)),
            ),
            Term::Fst(pair) => Term::Fst(Box::new(self.apply_term_impl(pair, bound_vars))),
            Term::Snd(pair) => Term::Snd(Box::new(self.apply_term_impl(pair, bound_vars))),
        }
    }

    /// Compose two substitutions
    pub fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = other.clone();

        // Apply self to all terms in other's substitution
        for (var, term) in &other.term_subst {
            result.term_subst.insert(var.clone(), self.apply_term(term));
        }

        // Add terms from self that are not in other
        for (var, term) in &self.term_subst {
            if !result.term_subst.contains_key(var) {
                result.term_subst.insert(var.clone(), term.clone());
            }
        }

        // Similar for universes
        for (var, universe) in &other.universe_subst {
            result
                .universe_subst
                .insert(var.clone(), self.apply_universe(universe));
        }

        for (var, universe) in &self.universe_subst {
            if !result.universe_subst.contains_key(var) {
                result.universe_subst.insert(var.clone(), universe.clone());
            }
        }

        result
    }
}

impl Default for Substitution {
    fn default() -> Self {
        Self::new()
    }
}

/// Unification errors
#[derive(Debug, Clone, PartialEq)]
pub enum UnificationError {
    OccursCheck {
        var: String,
        term: Term,
    },
    TypeMismatch {
        expected: Term,
        actual: Term,
    },
    UniverseMismatch {
        expected: Universe,
        actual: Universe,
    },
    CannotUnify {
        left: Term,
        right: Term,
    },
}

impl fmt::Display for UnificationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnificationError::OccursCheck { var, term } => {
                write!(f, "Occurs check failed: {} occurs in {}", var, term)
            }
            UnificationError::TypeMismatch { expected, actual } => {
                write!(f, "Type mismatch: expected {}, got {}", expected, actual)
            }
            UnificationError::UniverseMismatch { expected, actual } => {
                write!(
                    f,
                    "Universe mismatch: expected {}, got {}",
                    expected, actual
                )
            }
            UnificationError::CannotUnify { left, right } => {
                write!(f, "Cannot unify {} with {}", left, right)
            }
        }
    }
}

impl std::error::Error for UnificationError {}

pub type UnificationResult<T> = Result<T, UnificationError>;

/// Unification algorithm for the Calculus of Constructions
pub struct Unifier {
    meta_counter: usize,
}

impl Unifier {
    pub fn new() -> Self {
        Unifier { meta_counter: 0 }
    }

    /// Generate fresh meta-variable
    pub fn fresh_meta(&mut self) -> Term {
        let meta = Term::Meta(format!("?m{}", self.meta_counter));
        self.meta_counter += 1;
        meta
    }

    /// Check if variable occurs in term (occurs check)
    pub fn occurs_check(&self, var: &str, term: &Term) -> bool {
        match term {
            Term::Var(x) => x == var,
            Term::Meta(x) => x == var,
            Term::App(f, arg) => self.occurs_check(var, f) || self.occurs_check(var, arg),
            Term::Abs(_, ty, body) => self.occurs_check(var, ty) || self.occurs_check(var, body),
            Term::Pi(_, ty, body, _) => self.occurs_check(var, ty) || self.occurs_check(var, body),
            Term::Let(_, ty, val, body) => {
                self.occurs_check(var, ty)
                    || self.occurs_check(var, val)
                    || self.occurs_check(var, body)
            }
            Term::Match(scrutinee, arms) => {
                self.occurs_check(var, scrutinee)
                    || arms.iter().any(|arm| self.occurs_check(var, &arm.body))
            }
            Term::Constructor(_, args) => args.iter().any(|arg| self.occurs_check(var, arg)),
            Term::Proj(term, _) => self.occurs_check(var, term),
            Term::Sigma(_, domain, codomain) => {
                self.occurs_check(var, domain) || self.occurs_check(var, codomain)
            }
            Term::Pair(fst, snd) => self.occurs_check(var, fst) || self.occurs_check(var, snd),
            Term::Fst(pair) | Term::Snd(pair) => self.occurs_check(var, pair),
            Term::Sort(_) | Term::Const(_) => false,
        }
    }

    /// Unify two terms
    pub fn unify(&mut self, t1: &Term, t2: &Term) -> UnificationResult<Substitution> {
        self.unify_impl(t1, t2, &mut Substitution::new())
    }

    fn unify_impl(
        &mut self,
        t1: &Term,
        t2: &Term,
        subst: &mut Substitution,
    ) -> UnificationResult<Substitution> {
        let t1 = subst.apply_term(t1);
        let t2 = subst.apply_term(t2);

        match (&t1, &t2) {
            // Same variable
            (Term::Var(x), Term::Var(y)) if x == y => Ok(subst.clone()),

            // Meta-variable unification
            (Term::Meta(x), term) | (term, Term::Meta(x)) => {
                if let Term::Meta(y) = term {
                    if x == y {
                        return Ok(subst.clone());
                    }
                }

                if self.occurs_check(x, term) {
                    Err(UnificationError::OccursCheck {
                        var: x.clone(),
                        term: term.clone(),
                    })
                } else {
                    subst.extend_term(x.clone(), term.clone());
                    Ok(subst.clone())
                }
            }

            // Application
            (Term::App(f1, a1), Term::App(f2, a2)) => {
                let subst1 = self.unify_impl(f1, f2, subst)?;
                self.unify_impl(a1, a2, &mut subst1.clone())
            }

            // Lambda abstraction
            (Term::Abs(x1, ty1, body1), Term::Abs(x2, ty2, body2)) => {
                let subst1 = self.unify_impl(ty1, ty2, subst)?;
                // Rename bound variables to be the same
                let fresh_var = format!("_unify_{}", self.meta_counter);
                self.meta_counter += 1;

                let body1_renamed = self.rename_bound_var(body1, x1, &fresh_var);
                let body2_renamed = self.rename_bound_var(body2, x2, &fresh_var);

                self.unify_impl(&body1_renamed, &body2_renamed, &mut subst1.clone())
            }

            // Pi types
            (Term::Pi(x1, ty1, body1, impl1), Term::Pi(x2, ty2, body2, impl2)) => {
                // Check that implicit flags match
                if impl1 != impl2 {
                    return Err(UnificationError::CannotUnify {
                        left: t1.clone(),
                        right: t2.clone(),
                    });
                }

                let subst1 = self.unify_impl(ty1, ty2, subst)?;

                let fresh_var = format!("_unify_{}", self.meta_counter);
                self.meta_counter += 1;

                let body1_renamed = self.rename_bound_var(body1, x1, &fresh_var);
                let body2_renamed = self.rename_bound_var(body2, x2, &fresh_var);

                self.unify_impl(&body1_renamed, &body2_renamed, &mut subst1.clone())
            }

            // Sorts
            (Term::Sort(u1), Term::Sort(u2)) => self.unify_universe(u1, u2, subst),

            // Constants
            (Term::Const(c1), Term::Const(c2)) if c1 == c2 => Ok(subst.clone()),

            // Constructor applications
            (Term::Constructor(name1, args1), Term::Constructor(name2, args2))
                if name1 == name2 =>
            {
                if args1.len() != args2.len() {
                    return Err(UnificationError::CannotUnify {
                        left: t1.clone(),
                        right: t2.clone(),
                    });
                }

                let mut current_subst = subst.clone();
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    current_subst = self.unify_impl(arg1, arg2, &mut current_subst)?;
                }
                Ok(current_subst)
            }

            // Field projections
            (Term::Proj(term1, field1), Term::Proj(term2, field2)) if field1 == field2 => {
                self.unify_impl(term1, term2, subst)
            }

            // Sigma types
            (Term::Sigma(x1, domain1, codomain1), Term::Sigma(x2, domain2, codomain2)) => {
                let subst1 = self.unify_impl(domain1, domain2, subst)?;
                let fresh_var = format!("_unify_{}", self.meta_counter);
                self.meta_counter += 1;

                let codomain1_renamed = self.rename_bound_var(codomain1, x1, &fresh_var);
                let codomain2_renamed = self.rename_bound_var(codomain2, x2, &fresh_var);

                self.unify_impl(&codomain1_renamed, &codomain2_renamed, &mut subst1.clone())
            }

            // Pairs
            (Term::Pair(fst1, snd1), Term::Pair(fst2, snd2)) => {
                let subst1 = self.unify_impl(fst1, fst2, subst)?;
                self.unify_impl(snd1, snd2, &mut subst1.clone())
            }

            // Projections
            (Term::Fst(pair1), Term::Fst(pair2)) | (Term::Snd(pair1), Term::Snd(pair2)) => {
                self.unify_impl(pair1, pair2, subst)
            }

            // Cannot unify
            _ => Err(UnificationError::CannotUnify {
                left: t1,
                right: t2,
            }),
        }
    }

    fn unify_universe(
        &mut self,
        u1: &Universe,
        u2: &Universe,
        subst: &mut Substitution,
    ) -> UnificationResult<Substitution> {
        let u1 = subst.apply_universe(u1);
        let u2 = subst.apply_universe(u2);

        match (&u1, &u2) {
            (Universe::Const(n1), Universe::Const(n2)) if n1 == n2 => Ok(subst.clone()),
            (Universe::Const(_), Universe::Const(_)) => Err(UnificationError::UniverseMismatch {
                expected: u1.clone(),
                actual: u2.clone(),
            }),
            (Universe::ScopedVar(_, x), universe) | (universe, Universe::ScopedVar(_, x)) => {
                subst.extend_universe(x.clone(), universe.clone());
                Ok(subst.clone())
            }
            (Universe::Add(u1, n1), Universe::Add(u2, n2)) if n1 == n2 => {
                self.unify_universe(u1, u2, subst)
            }
            (Universe::Add(_, _), Universe::Add(_, _)) => Err(UnificationError::UniverseMismatch {
                expected: u1.clone(),
                actual: u2.clone(),
            }),
            (Universe::Max(u1, v1), Universe::Max(u2, v2)) => {
                let subst1 = self.unify_universe(u1, u2, subst)?;
                self.unify_universe(v1, v2, &mut subst1.clone())
            }
            (Universe::IMax(u1, v1), Universe::IMax(u2, v2)) => {
                let subst1 = self.unify_universe(u1, u2, subst)?;
                self.unify_universe(v1, v2, &mut subst1.clone())
            }
            _ => Err(UnificationError::UniverseMismatch {
                expected: u1,
                actual: u2,
            }),
        }
    }

    /// Rename bound variable in term
    fn rename_bound_var(&self, term: &Term, old_var: &str, new_var: &str) -> Term {
        match term {
            Term::Var(x) if x == old_var => Term::Var(new_var.to_string()),
            Term::Var(_) => term.clone(),
            Term::App(f, arg) => Term::App(
                Box::new(self.rename_bound_var(f, old_var, new_var)),
                Box::new(self.rename_bound_var(arg, old_var, new_var)),
            ),
            Term::Abs(x, ty, body) => {
                if x == old_var {
                    Term::Abs(
                        new_var.to_string(),
                        Box::new(self.rename_bound_var(ty, old_var, new_var)),
                        Box::new(self.rename_bound_var(body, old_var, new_var)),
                    )
                } else {
                    Term::Abs(
                        x.clone(),
                        Box::new(self.rename_bound_var(ty, old_var, new_var)),
                        Box::new(self.rename_bound_var(body, old_var, new_var)),
                    )
                }
            }
            Term::Pi(x, ty, body, implicit) => {
                if x == old_var {
                    Term::Pi(
                        new_var.to_string(),
                        Box::new(self.rename_bound_var(ty, old_var, new_var)),
                        Box::new(self.rename_bound_var(body, old_var, new_var)),
                        *implicit,
                    )
                } else {
                    Term::Pi(
                        x.clone(),
                        Box::new(self.rename_bound_var(ty, old_var, new_var)),
                        Box::new(self.rename_bound_var(body, old_var, new_var)),
                        *implicit,
                    )
                }
            }
            _ => term.clone(), // Other cases don't bind variables
        }
    }
}

impl Default for Unifier {
    fn default() -> Self {
        Self::new()
    }
}
