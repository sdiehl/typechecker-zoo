use std::collections::HashMap;

use crate::ast::{Pattern, Term, Universe};

/// Alpha equivalence checker for the Calculus of Constructions
pub struct AlphaEquivalence {
    counter: usize,
}

impl AlphaEquivalence {
    pub fn new() -> Self {
        AlphaEquivalence { counter: 0 }
    }

    /// Check if two terms are alpha equivalent
    pub fn equivalent(&mut self, t1: &Term, t2: &Term) -> bool {
        self.alpha_eq_terms(t1, t2, &mut HashMap::new(), &mut HashMap::new())
    }

    /// Check if two universes are alpha equivalent
    pub fn equivalent_universe(&mut self, u1: &Universe, u2: &Universe) -> bool {
        self.alpha_eq_universe(u1, u2, &mut HashMap::new())
    }

    fn alpha_eq_terms(
        &mut self,
        t1: &Term,
        t2: &Term,
        env1: &mut HashMap<String, String>,
        env2: &mut HashMap<String, String>,
    ) -> bool {
        match (t1, t2) {
            // Variables - check if they map to the same renamed variable
            (Term::Var(x1), Term::Var(x2)) => {
                match (env1.get(x1), env2.get(x2)) {
                    (Some(renamed1), Some(renamed2)) => renamed1 == renamed2,
                    (None, None) => x1 == x2, // Both free variables
                    _ => false,               // One bound, one free
                }
            }

            // Application
            (Term::App(f1, a1), Term::App(f2, a2)) => {
                self.alpha_eq_terms(f1, f2, env1, env2) && self.alpha_eq_terms(a1, a2, env1, env2)
            }

            // Lambda abstraction - bind variables to fresh names
            (Term::Abs(x1, ty1, body1), Term::Abs(x2, ty2, body2)) => {
                let fresh = self.fresh_var();

                let old1 = env1.insert(x1.clone(), fresh.clone());
                let old2 = env2.insert(x2.clone(), fresh);

                let ty_eq = self.alpha_eq_terms(ty1, ty2, env1, env2);
                let body_eq = self.alpha_eq_terms(body1, body2, env1, env2);

                // Restore old bindings
                match old1 {
                    Some(old) => env1.insert(x1.clone(), old),
                    None => env1.remove(x1),
                };
                match old2 {
                    Some(old) => env2.insert(x2.clone(), old),
                    None => env2.remove(x2),
                };

                ty_eq && body_eq
            }

            // Pi types - similar to lambda
            (Term::Pi(x1, ty1, body1, impl1), Term::Pi(x2, ty2, body2, impl2)) => {
                let fresh = self.fresh_var();

                let old1 = env1.insert(x1.clone(), fresh.clone());
                let old2 = env2.insert(x2.clone(), fresh);

                let ty_eq = self.alpha_eq_terms(ty1, ty2, env1, env2);
                let body_eq = self.alpha_eq_terms(body1, body2, env1, env2);

                // Restore old bindings
                match old1 {
                    Some(old) => env1.insert(x1.clone(), old),
                    None => env1.remove(x1),
                };
                match old2 {
                    Some(old) => env2.insert(x2.clone(), old),
                    None => env2.remove(x2),
                };

                ty_eq && body_eq && (impl1 == impl2)
            }

            // Sorts
            (Term::Sort(u1), Term::Sort(u2)) => self.alpha_eq_universe(u1, u2, &mut HashMap::new()),

            // Let bindings
            (Term::Let(x1, ty1, val1, body1), Term::Let(x2, ty2, val2, body2)) => {
                let ty_eq = self.alpha_eq_terms(ty1, ty2, env1, env2);
                let val_eq = self.alpha_eq_terms(val1, val2, env1, env2);

                if !ty_eq || !val_eq {
                    return false;
                }

                let fresh = self.fresh_var();

                let old1 = env1.insert(x1.clone(), fresh.clone());
                let old2 = env2.insert(x2.clone(), fresh);

                let body_eq = self.alpha_eq_terms(body1, body2, env1, env2);

                // Restore old bindings
                match old1 {
                    Some(old) => env1.insert(x1.clone(), old),
                    None => env1.remove(x1),
                };
                match old2 {
                    Some(old) => env2.insert(x2.clone(), old),
                    None => env2.remove(x2),
                };

                ty_eq && body_eq
            }

            // Match expressions
            (Term::Match(scrut1, arms1), Term::Match(scrut2, arms2)) => {
                self.alpha_eq_terms(scrut1, scrut2, env1, env2)
                    && arms1.len() == arms2.len()
                    && arms1.iter().zip(arms2.iter()).all(|(arm1, arm2)| {
                        self.alpha_eq_pattern(&arm1.pattern, &arm2.pattern)
                            && self.alpha_eq_terms(&arm1.body, &arm2.body, env1, env2)
                    })
            }

            // Constructor applications
            (Term::Constructor(name1, args1), Term::Constructor(name2, args2)) => {
                name1 == name2
                    && args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(a1, a2)| self.alpha_eq_terms(a1, a2, env1, env2))
            }

            // Constants and meta-variables
            (Term::Const(c1), Term::Const(c2)) => c1 == c2,
            (Term::Meta(m1), Term::Meta(m2)) => m1 == m2,

            // Field projections
            (Term::Proj(term1, field1), Term::Proj(term2, field2)) => {
                field1 == field2 && self.alpha_eq_terms(term1, term2, env1, env2)
            }

            // Sigma types
            (Term::Sigma(x1, domain1, codomain1), Term::Sigma(x2, domain2, codomain2)) => {
                if !self.alpha_eq_terms(domain1, domain2, env1, env2) {
                    return false;
                }
                let fresh_var = format!("_alpha_{}", env1.len());
                let mut env1_extended = env1.clone();
                let mut env2_extended = env2.clone();
                env1_extended.insert(x1.clone(), fresh_var.clone());
                env2_extended.insert(x2.clone(), fresh_var);
                self.alpha_eq_terms(codomain1, codomain2, &mut env1_extended, &mut env2_extended)
            }

            // Pairs
            (Term::Pair(fst1, snd1), Term::Pair(fst2, snd2)) => {
                self.alpha_eq_terms(fst1, fst2, env1, env2)
                    && self.alpha_eq_terms(snd1, snd2, env1, env2)
            }

            // Projections
            (Term::Fst(pair1), Term::Fst(pair2)) | (Term::Snd(pair1), Term::Snd(pair2)) => {
                self.alpha_eq_terms(pair1, pair2, env1, env2)
            }

            // Different constructors
            _ => false,
        }
    }

    fn alpha_eq_universe(
        &mut self,
        u1: &Universe,
        u2: &Universe,
        env: &mut HashMap<String, String>,
    ) -> bool {
        match (u1, u2) {
            (Universe::Const(n1), Universe::Const(n2)) => n1 == n2,
            (Universe::ScopedVar(s1, v1), Universe::ScopedVar(s2, v2)) => {
                // For scoped variables, they must have same scope and variable name, or be
                // mapped equivalently
                if s1 == s2 && v1 == v2 {
                    true
                } else {
                    // Check if they map to equivalent names in the environment
                    match (env.get(v1), env.get(v2)) {
                        (Some(renamed1), Some(renamed2)) => renamed1 == renamed2,
                        (None, None) => v1 == v2,
                        _ => false,
                    }
                }
            }
            (Universe::Meta(id1), Universe::Meta(id2)) => id1 == id2,
            (Universe::Add(u1, n1), Universe::Add(u2, n2)) => {
                n1 == n2 && self.alpha_eq_universe(u1, u2, env)
            }
            (Universe::Max(u1, v1), Universe::Max(u2, v2)) => {
                self.alpha_eq_universe(u1, u2, env) && self.alpha_eq_universe(v1, v2, env)
            }
            (Universe::IMax(u1, v1), Universe::IMax(u2, v2)) => {
                self.alpha_eq_universe(u1, u2, env) && self.alpha_eq_universe(v1, v2, env)
            }
            _ => false,
        }
    }

    fn alpha_eq_pattern(&self, p1: &Pattern, p2: &Pattern) -> bool {
        match (p1, p2) {
            (Pattern::Var(_), Pattern::Var(_)) => true, // Pattern variables always match
            (Pattern::Wildcard, Pattern::Wildcard) => true,
            (Pattern::Constructor(name1, args1), Pattern::Constructor(name2, args2)) => {
                name1 == name2
                    && args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(a1, a2)| self.alpha_eq_pattern(a1, a2))
            }
            _ => false,
        }
    }

    fn fresh_var(&mut self) -> String {
        let var = format!("_alpha{}", self.counter);
        self.counter += 1;
        var
    }
}

impl Default for AlphaEquivalence {
    fn default() -> Self {
        Self::new()
    }
}

/// Alpha normalization - convert term to canonical form with De Bruijn indices
pub struct DeBruijnConverter {
    _level: usize,
}

impl DeBruijnConverter {
    pub fn new() -> Self {
        DeBruijnConverter { _level: 0 }
    }

    /// Convert term to De Bruijn representation
    pub fn to_de_bruijn(&mut self, term: &Term) -> DeBruijnTerm {
        self.convert_term(term, &mut Vec::new())
    }

    fn convert_term(&mut self, term: &Term, env: &mut Vec<String>) -> DeBruijnTerm {
        match term {
            Term::Var(x) => match env.iter().rev().position(|v| v == x) {
                Some(index) => DeBruijnTerm::Bound(index),
                None => DeBruijnTerm::Free(x.clone()),
            },
            Term::App(f, arg) => DeBruijnTerm::App(
                Box::new(self.convert_term(f, env)),
                Box::new(self.convert_term(arg, env)),
            ),
            Term::Abs(x, ty, body) => {
                let ty_db = self.convert_term(ty, env);
                env.push(x.clone());
                let body_db = self.convert_term(body, env);
                env.pop();
                DeBruijnTerm::Abs(Box::new(ty_db), Box::new(body_db))
            }
            Term::Pi(x, ty, body, _implicit) => {
                let ty_db = self.convert_term(ty, env);
                env.push(x.clone());
                let body_db = self.convert_term(body, env);
                env.pop();
                DeBruijnTerm::Pi(Box::new(ty_db), Box::new(body_db))
            }
            Term::Sort(u) => DeBruijnTerm::Sort(u.clone()),
            Term::Let(x, ty, val, body) => {
                let ty_db = self.convert_term(ty, env);
                let val_db = self.convert_term(val, env);
                env.push(x.clone());
                let body_db = self.convert_term(body, env);
                env.pop();
                DeBruijnTerm::Let(Box::new(ty_db), Box::new(val_db), Box::new(body_db))
            }
            Term::Const(c) => DeBruijnTerm::Const(c.clone()),
            Term::Meta(m) => DeBruijnTerm::Meta(m.clone()),
            _ => DeBruijnTerm::Other, // For constructors, match, etc.
        }
    }
}

impl Default for DeBruijnConverter {
    fn default() -> Self {
        Self::new()
    }
}

/// De Bruijn indexed terms for alpha equivalence testing
#[derive(Debug, Clone, PartialEq)]
pub enum DeBruijnTerm {
    Bound(usize), // De Bruijn index
    Free(String), // Free variable
    App(Box<DeBruijnTerm>, Box<DeBruijnTerm>),
    Abs(Box<DeBruijnTerm>, Box<DeBruijnTerm>),
    Pi(Box<DeBruijnTerm>, Box<DeBruijnTerm>),
    Sort(Universe),
    Let(Box<DeBruijnTerm>, Box<DeBruijnTerm>, Box<DeBruijnTerm>),
    Const(String),
    Meta(String),
    Other, // For complex constructs not handled yet
}

/// Quick alpha equivalence check using De Bruijn indices
pub fn alpha_equivalent(t1: &Term, t2: &Term) -> bool {
    let mut converter1 = DeBruijnConverter::new();
    let mut converter2 = DeBruijnConverter::new();

    let db1 = converter1.to_de_bruijn(t1);
    let db2 = converter2.to_de_bruijn(t2);

    db1 == db2
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Universe;

    #[test]
    fn test_alpha_equivalent_variables() {
        let t1 = Term::Var("x".to_string());
        let t2 = Term::Var("x".to_string());
        let t3 = Term::Var("y".to_string());

        let mut alpha = AlphaEquivalence::new();
        assert!(alpha.equivalent(&t1, &t2));
        assert!(!alpha.equivalent(&t1, &t3));
    }

    #[test]
    fn test_alpha_equivalent_lambda() {
        // λx:T.x ≡ λy:T.y
        let t1 = Term::Abs(
            "x".to_string(),
            Box::new(Term::Sort(Universe::Const(0))),
            Box::new(Term::Var("x".to_string())),
        );
        let t2 = Term::Abs(
            "y".to_string(),
            Box::new(Term::Sort(Universe::Const(0))),
            Box::new(Term::Var("y".to_string())),
        );

        let mut alpha = AlphaEquivalence::new();
        assert!(alpha.equivalent(&t1, &t2));
    }

    #[test]
    fn test_not_alpha_equivalent() {
        // λx:T.x ≢ λy:T.x (free x in second term)
        let t1 = Term::Abs(
            "x".to_string(),
            Box::new(Term::Sort(Universe::Const(0))),
            Box::new(Term::Var("x".to_string())),
        );
        let t2 = Term::Abs(
            "y".to_string(),
            Box::new(Term::Sort(Universe::Const(0))),
            Box::new(Term::Var("x".to_string())),
        );

        let mut alpha = AlphaEquivalence::new();
        assert!(!alpha.equivalent(&t1, &t2));
    }

    #[test]
    fn test_de_bruijn_conversion() {
        let term = Term::Abs(
            "x".to_string(),
            Box::new(Term::Sort(Universe::Const(0))),
            Box::new(Term::Var("x".to_string())),
        );

        let expected = DeBruijnTerm::Abs(
            Box::new(DeBruijnTerm::Sort(Universe::Const(0))),
            Box::new(DeBruijnTerm::Bound(0)),
        );

        let mut converter = DeBruijnConverter::new();
        assert_eq!(converter.to_de_bruijn(&term), expected);
    }

    #[test]
    fn test_alpha_equivalent_function() {
        let t1 = Term::Abs(
            "x".to_string(),
            Box::new(Term::Sort(Universe::Const(0))),
            Box::new(Term::Var("x".to_string())),
        );
        let t2 = Term::Abs(
            "y".to_string(),
            Box::new(Term::Sort(Universe::Const(0))),
            Box::new(Term::Var("y".to_string())),
        );

        assert!(alpha_equivalent(&t1, &t2));
    }
}
