use std::collections::{HashMap, HashSet};

/// Universe level constraint solver for polymorphic universe levels
use crate::ast::{Universe, UniverseConstraint};

/// Universe level constraint solver
#[derive(Debug, Clone)]
pub struct UniverseSolver {
    /// Substitutions for universe variables
    substitutions: HashMap<String, Universe>,
}

impl UniverseSolver {
    pub fn new() -> Self {
        UniverseSolver {
            substitutions: HashMap::new(),
        }
    }

    /// Solve universe constraints and return substitutions
    pub fn solve(&mut self, constraints: &[UniverseConstraint]) -> Result<(), String> {
        // Simple constraint solver - in a full system this would be much more
        // sophisticated
        for constraint in constraints {
            self.solve_constraint(constraint)?;
        }
        Ok(())
    }

    /// Solve a single constraint
    fn solve_constraint(&mut self, constraint: &UniverseConstraint) -> Result<(), String> {
        match constraint {
            UniverseConstraint::Equal(u1, u2) => self.unify_universes(u1, u2),
            UniverseConstraint::LessEq(u1, u2) => {
                // For now, treat less-equal as equality (simplified)
                self.unify_universes(u1, u2)
            }
        }
    }

    /// Unify two universe levels
    fn unify_universes(&mut self, u1: &Universe, u2: &Universe) -> Result<(), String> {
        let u1_subst = self.apply_substitution(u1);
        let u2_subst = self.apply_substitution(u2);

        match (&u1_subst, &u2_subst) {
            (Universe::ScopedVar(_, x), u) | (u, Universe::ScopedVar(_, x)) => {
                if let Universe::ScopedVar(_, y) = u {
                    if x == y {
                        return Ok(());
                    }
                }

                // Occurs check
                if self.occurs_check(x, u) {
                    return Err(format!(
                        "Universe variable {} occurs in {}",
                        x,
                        self.universe_to_string(u)
                    ));
                }

                self.substitutions.insert(x.clone(), u.clone());
                Ok(())
            }

            (Universe::Const(n1), Universe::Const(n2)) if n1 == n2 => Ok(()),
            (Universe::Const(_), Universe::Const(_)) => {
                Err("Cannot unify different constants".to_string())
            }

            (Universe::Add(u1, n1), Universe::Add(u2, n2)) if n1 == n2 => {
                self.unify_universes(u1, u2)
            }
            (Universe::Add(_, _), Universe::Add(_, _)) => {
                Err("Cannot unify different additions".to_string())
            }

            (Universe::Max(u1, v1), Universe::Max(u2, v2)) => {
                self.unify_universes(u1, u2)?;
                self.unify_universes(v1, v2)
            }

            (Universe::IMax(u1, v1), Universe::IMax(u2, v2)) => {
                self.unify_universes(u1, u2)?;
                self.unify_universes(v1, v2)
            }

            _ => Err(format!(
                "Cannot unify universe levels {} and {}",
                self.universe_to_string(&u1_subst),
                self.universe_to_string(&u2_subst)
            )),
        }
    }

    /// Apply substitutions to a universe level
    fn apply_substitution(&self, u: &Universe) -> Universe {
        match u {
            Universe::ScopedVar(_scope, x) => {
                if let Some(subst) = self.substitutions.get(x) {
                    self.apply_substitution(subst)
                } else {
                    u.clone()
                }
            }
            Universe::Const(n) => Universe::Const(*n),
            Universe::Add(u, n) => Universe::Add(Box::new(self.apply_substitution(u)), *n),
            Universe::Max(u, v) => Universe::Max(
                Box::new(self.apply_substitution(u)),
                Box::new(self.apply_substitution(v)),
            ),
            Universe::IMax(u, v) => Universe::IMax(
                Box::new(self.apply_substitution(u)),
                Box::new(self.apply_substitution(v)),
            ),
            _ => u.clone(),
        }
    }

    /// Check if a universe variable occurs in a universe level
    #[allow(clippy::only_used_in_recursion)]
    fn occurs_check(&self, var: &str, u: &Universe) -> bool {
        match u {
            Universe::ScopedVar(_, x) => x == var,
            Universe::Add(u, _) => self.occurs_check(var, u),
            Universe::Max(u, v) | Universe::IMax(u, v) => {
                self.occurs_check(var, u) || self.occurs_check(var, v)
            }
            Universe::Const(_) => false,
            Universe::Meta(_) => false, // Meta variables don't contain named variables
        }
    }

    /// Convert universe to string representation
    #[allow(clippy::only_used_in_recursion)]
    fn universe_to_string(&self, u: &Universe) -> String {
        match u {
            Universe::Const(n) => n.to_string(),
            Universe::ScopedVar(scope, var) => format!("{}::{}", scope, var),
            Universe::Meta(id) => format!("?u{}", id),
            Universe::Add(u, n) => format!("({} + {})", self.universe_to_string(u), n),
            Universe::Max(u, v) => format!(
                "max({}, {})",
                self.universe_to_string(u),
                self.universe_to_string(v)
            ),
            Universe::IMax(u, v) => format!(
                "imax({}, {})",
                self.universe_to_string(u),
                self.universe_to_string(v)
            ),
        }
    }

    /// Get the final substitution for a universe variable
    pub fn get_substitution(&self, var: &str) -> Option<Universe> {
        self.substitutions
            .get(var)
            .map(|u| self.apply_substitution(u))
    }

    /// Apply all substitutions to a universe level
    pub fn substitute_universe(&self, u: &Universe) -> Universe {
        self.apply_substitution(u)
    }

    /// Get all substitutions
    pub fn get_all_substitutions(&self) -> &HashMap<String, Universe> {
        &self.substitutions
    }

    /// Check if universe level constraints are satisfiable
    pub fn is_satisfiable(&self, constraints: &[UniverseConstraint]) -> bool {
        let mut solver = self.clone();
        solver.solve(constraints).is_ok()
    }

    /// Generate fresh universe variable
    pub fn fresh_universe_var(&self, base: &str, avoid: &HashSet<String>) -> String {
        let mut counter = 0;
        loop {
            let name = if counter == 0 {
                base.to_string()
            } else {
                format!("{}{}", base, counter)
            };
            if !avoid.contains(&name) && !self.substitutions.contains_key(&name) {
                return name;
            }
            counter += 1;
        }
    }
}

impl Default for UniverseSolver {
    fn default() -> Self {
        Self::new()
    }
}
