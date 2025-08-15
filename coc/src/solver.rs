use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;

use crate::ast::{Term, Universe};
use crate::context::Context;
use crate::errors::TypeError;

/// Meta-variable identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MetaId(pub u64);

impl fmt::Display for MetaId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?m{}", self.0)
    }
}

/// Universe meta-variable identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UniverseMetaId(pub u64);

impl fmt::Display for UniverseMetaId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?u{}", self.0)
    }
}

/// Information about a meta-variable
#[derive(Debug, Clone)]
pub struct MetaInfo {
    pub id: MetaId,
    pub context: Vec<String>, // Variables in scope when meta was created
    pub expected_type: Option<Term>, // Expected type of the meta-variable
    pub solution: Option<Term>, // Solution if solved
    pub dependencies: HashSet<MetaId>, // Meta-variables this depends on
    pub occurrences: Vec<ConstraintId>, // Constraints this meta appears in
}

/// Constraint identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstraintId(pub u64);

/// Constraint types
#[derive(Debug, Clone)]
pub enum Constraint {
    /// Unification: t1 ≡ t2
    Unify {
        id: ConstraintId,
        left: Term,
        right: Term,
        strength: ConstraintStrength,
    },
    /// Type constraint: term : type
    HasType {
        id: ConstraintId,
        term: Term,
        expected_type: Term,
    },
    /// Universe unification: u1 ≡ u2
    UnifyUniverse {
        id: ConstraintId,
        left: Universe,
        right: Universe,
        strength: ConstraintStrength,
    },
    /// Universe level constraint: u1 ≤ u2
    UniverseLevel {
        id: ConstraintId,
        left: Universe,
        right: Universe,
    },
    /// Delayed constraint (for complex patterns)
    Delayed {
        id: ConstraintId,
        constraint: Box<Constraint>,
        waiting_on: HashSet<MetaId>,
    },
}

/// Constraint strength for prioritization
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstraintStrength {
    Required,  // Must be solved
    Preferred, // Should be solved if possible
    Weak,      // Can be postponed
}

/// Substitution mapping meta-variables to terms
#[derive(Debug, Clone, Default)]
pub struct Substitution {
    mapping: HashMap<MetaId, Term>,
    universe_mapping: HashMap<u32, Universe>,
}

impl Substitution {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, meta: MetaId, term: Term) {
        self.mapping.insert(meta, term);
    }

    pub fn get(&self, meta: &MetaId) -> Option<&Term> {
        self.mapping.get(meta)
    }

    pub fn insert_universe(&mut self, meta_id: u32, universe: Universe) {
        self.universe_mapping.insert(meta_id, universe);
    }

    pub fn get_universe(&self, meta_id: &u32) -> Option<&Universe> {
        self.universe_mapping.get(meta_id)
    }

    /// Apply substitution to a term
    pub fn apply(&self, term: &Term) -> Term {
        match term {
            Term::Meta(name) => {
                // Try to parse meta ID and look up substitution
                if let Some(meta_id) = self.parse_meta_name(name) {
                    if let Some(subst) = self.mapping.get(&meta_id) {
                        // Recursively apply substitution
                        return self.apply(subst);
                    }
                }
                term.clone()
            }
            Term::App(f, arg) => Term::App(Box::new(self.apply(f)), Box::new(self.apply(arg))),
            Term::Abs(x, ty, body) => Term::Abs(
                x.clone(),
                Box::new(self.apply(ty)),
                Box::new(self.apply(body)),
            ),
            Term::Pi(x, ty, body, implicit) => Term::Pi(
                x.clone(),
                Box::new(self.apply(ty)),
                Box::new(self.apply(body)),
                *implicit,
            ),
            Term::Let(x, ty, val, body) => Term::Let(
                x.clone(),
                Box::new(self.apply(ty)),
                Box::new(self.apply(val)),
                Box::new(self.apply(body)),
            ),
            Term::Sort(u) => Term::Sort(self.apply_universe(u)),
            // Other cases remain unchanged
            _ => term.clone(),
        }
    }

    /// Apply substitution to a universe
    pub fn apply_universe(&self, universe: &Universe) -> Universe {
        let result = match universe {
            Universe::Meta(id) => {
                if let Some(subst) = self.universe_mapping.get(id) {
                    // Recursively apply substitution
                    self.apply_universe(subst)
                } else {
                    universe.clone()
                }
            }
            Universe::Add(base, n) => Universe::Add(Box::new(self.apply_universe(base)), *n),
            Universe::Max(u, v) => Universe::Max(
                Box::new(self.apply_universe(u)),
                Box::new(self.apply_universe(v)),
            ),
            Universe::IMax(u, v) => Universe::IMax(
                Box::new(self.apply_universe(u)),
                Box::new(self.apply_universe(v)),
            ),
            // Constants, variables, and scoped variables remain unchanged during substitution
            Universe::Const(_) | Universe::ScopedVar(_, _) => universe.clone(),
        };
        // Normalize the result to simplify expressions like Const(0) + 1 -> Const(1)
        self.normalize_universe_static(&result)
    }

    /// Static normalization (doesn't require &self)
    #[allow(clippy::only_used_in_recursion)]
    fn normalize_universe_static(&self, u: &Universe) -> Universe {
        match u {
            Universe::Add(base, n) => {
                let base_norm = self.normalize_universe_static(base);
                match base_norm {
                    Universe::Const(m) => Universe::Const(m + n),
                    _ => Universe::Add(Box::new(base_norm), *n),
                }
            }
            Universe::Max(u1, u2) => {
                let u1_norm = self.normalize_universe_static(u1);
                let u2_norm = self.normalize_universe_static(u2);
                match (&u1_norm, &u2_norm) {
                    (Universe::Const(n1), Universe::Const(n2)) => Universe::Const((*n1).max(*n2)),
                    _ => Universe::Max(Box::new(u1_norm), Box::new(u2_norm)),
                }
            }
            _ => u.clone(),
        }
    }

    fn parse_meta_name(&self, name: &str) -> Option<MetaId> {
        if let Some(stripped) = name.strip_prefix("?m") {
            stripped.parse::<u64>().ok().map(MetaId)
        } else {
            None
        }
    }
}

/// Advanced constraint solver with dependency tracking
pub struct Solver {
    /// Meta-variables
    metas: HashMap<MetaId, MetaInfo>,
    /// Active constraints
    constraints: HashMap<ConstraintId, Constraint>,
    /// Constraint queue (ordered by priority)
    queue: VecDeque<ConstraintId>,
    /// Solved substitutions
    substitution: Substitution,
    /// Next meta ID
    next_meta_id: u64,
    /// Next universe meta ID
    next_universe_meta_id: u32,
    /// Next constraint ID
    next_constraint_id: u64,
    /// Type checking context
    context: Context,
    /// Dependency graph: meta -> constraints that depend on it
    dependencies: HashMap<MetaId, HashSet<ConstraintId>>,
    /// Delayed constraints that couldn't be solved immediately
    delayed_constraints: HashSet<ConstraintId>,
    /// Recursion depth counter to prevent stack overflow
    recursion_depth: u32,
    /// Flag to enable Miller pattern solving (advanced)
    enable_miller_patterns: bool,
    /// Flag to enable HasType constraint solving (advanced)
    enable_has_type_solving: bool,
}

impl Solver {
    pub fn new(context: Context) -> Self {
        Self {
            metas: HashMap::new(),
            constraints: HashMap::new(),
            queue: VecDeque::new(),
            substitution: Substitution::new(),
            next_meta_id: 0,
            next_universe_meta_id: 0,
            next_constraint_id: 0,
            context,
            dependencies: HashMap::new(),
            delayed_constraints: HashSet::new(),
            recursion_depth: 0,
            enable_miller_patterns: false,  // Advanced feature
            enable_has_type_solving: false, // Advanced feature
        }
    }

    /// Enable Miller pattern solving (advanced)
    pub fn enable_miller_patterns(&mut self) {
        self.enable_miller_patterns = true;
    }

    /// Check if Miller pattern solving is enabled
    pub fn miller_patterns_enabled(&self) -> bool {
        self.enable_miller_patterns
    }

    /// Enable HasType constraint solving (advanced)
    pub fn enable_has_type_solving(&mut self) {
        self.enable_has_type_solving = true;
    }

    /// Check if HasType constraint solving is enabled
    pub fn has_type_solving_enabled(&self) -> bool {
        self.enable_has_type_solving
    }

    /// Create a fresh meta-variable
    pub fn fresh_meta(&mut self, ctx_vars: Vec<String>, expected_type: Option<Term>) -> MetaId {
        let id = MetaId(self.next_meta_id);
        self.next_meta_id += 1;

        let info = MetaInfo {
            id,
            context: ctx_vars,
            expected_type,
            solution: None,
            dependencies: HashSet::new(),
            occurrences: Vec::new(),
        };

        self.metas.insert(id, info);
        id
    }

    /// Create a fresh universe meta-variable
    pub fn fresh_universe_meta(&mut self) -> Universe {
        let id = self.next_universe_meta_id;
        self.next_universe_meta_id += 1;
        Universe::Meta(id)
    }

    /// Add a constraint to the solver
    pub fn add_constraint(&mut self, constraint: Constraint) -> ConstraintId {
        let id = match &constraint {
            Constraint::Unify { id, .. }
            | Constraint::HasType { id, .. }
            | Constraint::UnifyUniverse { id, .. }
            | Constraint::UniverseLevel { id, .. }
            | Constraint::Delayed { id, .. } => *id,
        };

        // Track which metas appear in this constraint
        self.track_meta_occurrences(&constraint);

        self.constraints.insert(id, constraint);
        self.queue.push_back(id);
        id
    }

    /// Create a new constraint ID
    pub fn new_constraint_id(&mut self) -> ConstraintId {
        let id = ConstraintId(self.next_constraint_id);
        self.next_constraint_id += 1;
        id
    }

    /// Track meta-variable occurrences in constraints
    fn track_meta_occurrences(&mut self, constraint: &Constraint) {
        let metas = self.collect_metas_in_constraint(constraint);
        let constraint_id = match constraint {
            Constraint::Unify { id, .. }
            | Constraint::HasType { id, .. }
            | Constraint::UnifyUniverse { id, .. }
            | Constraint::UniverseLevel { id, .. }
            | Constraint::Delayed { id, .. } => *id,
        };

        for meta_id in metas {
            if let Some(info) = self.metas.get_mut(&meta_id) {
                info.occurrences.push(constraint_id);
            }
            self.dependencies
                .entry(meta_id)
                .or_default()
                .insert(constraint_id);
        }
    }

    /// Collect all meta-variables in a constraint
    fn collect_metas_in_constraint(&self, constraint: &Constraint) -> HashSet<MetaId> {
        match constraint {
            Constraint::Unify { left, right, .. } => {
                let mut metas = self.collect_metas_in_term(left);
                metas.extend(self.collect_metas_in_term(right));
                metas
            }
            Constraint::HasType {
                term,
                expected_type,
                ..
            } => {
                let mut metas = self.collect_metas_in_term(term);
                metas.extend(self.collect_metas_in_term(expected_type));
                metas
            }
            Constraint::UnifyUniverse { .. } => {
                // Universe constraints don't directly contain term meta-variables
                // but they might contain universe meta-variables (which we track separately)
                HashSet::new()
            }
            Constraint::UniverseLevel { .. } => {
                // Same as above
                HashSet::new()
            }
            Constraint::Delayed {
                constraint,
                waiting_on,
                ..
            } => {
                let mut metas = waiting_on.clone();
                metas.extend(self.collect_metas_in_constraint(constraint));
                metas
            }
        }
    }

    /// Collect all meta-variables in a term
    fn collect_metas_in_term(&self, term: &Term) -> HashSet<MetaId> {
        let mut metas = HashSet::new();
        self.collect_metas_recursive(term, &mut metas);
        metas
    }

    fn collect_metas_recursive(&self, term: &Term, metas: &mut HashSet<MetaId>) {
        match term {
            Term::Meta(name) => {
                if let Some(id) = self.parse_meta_name(name) {
                    metas.insert(id);
                }
            }
            Term::App(f, arg) => {
                self.collect_metas_recursive(f, metas);
                self.collect_metas_recursive(arg, metas);
            }
            Term::Abs(_, ty, body) | Term::Pi(_, ty, body, _) => {
                self.collect_metas_recursive(ty, metas);
                self.collect_metas_recursive(body, metas);
            }
            Term::Let(_, ty, val, body) => {
                self.collect_metas_recursive(ty, metas);
                self.collect_metas_recursive(val, metas);
                self.collect_metas_recursive(body, metas);
            }
            _ => {}
        }
    }

    fn parse_meta_name(&self, name: &str) -> Option<MetaId> {
        if let Some(stripped) = name.strip_prefix("?m") {
            stripped.parse::<u64>().ok().map(MetaId)
        } else {
            None
        }
    }

    /// Main solving loop
    pub fn solve(&mut self) -> Result<Substitution, TypeError> {
        let max_iterations = 1000;
        let mut iterations = 0;

        while !self.queue.is_empty() && iterations < max_iterations {
            iterations += 1;

            // Pick the best constraint to solve
            if let Some(constraint_id) = self.pick_constraint() {
                let constraint = self.constraints.remove(&constraint_id).unwrap();

                match self.solve_constraint(constraint.clone()) {
                    Ok(progress) => {
                        if progress {
                            // Propagate the solution
                            self.propagate_solution()?;

                            // Wake up delayed constraints that might now be solvable
                            let woken_constraints = self.wake_delayed_constraints()?;

                            // Add woken constraints back to queue with high priority
                            for woken_id in woken_constraints {
                                self.queue.push_front(woken_id);
                            }
                        }
                    }
                    Err(e) => {
                        // Try to delay the constraint if possible
                        if self.can_delay(&constraint_id) {
                            // Determine which meta-variables to wait for
                            let waiting_on = match &constraint {
                                Constraint::Unify { left, right, .. } => {
                                    let mut metas = self.collect_metas_in_term(left);
                                    metas.extend(self.collect_metas_in_term(right));
                                    metas
                                        .into_iter()
                                        .filter(|meta_id| self.is_unsolved_meta(meta_id))
                                        .collect()
                                }
                                Constraint::HasType {
                                    term,
                                    expected_type,
                                    ..
                                } => {
                                    let mut metas = self.collect_metas_in_term(term);
                                    metas.extend(self.collect_metas_in_term(expected_type));
                                    metas
                                        .into_iter()
                                        .filter(|meta_id| self.is_unsolved_meta(meta_id))
                                        .collect()
                                }
                                _ => HashSet::new(),
                            };

                            if !waiting_on.is_empty() {
                                // Put constraint back and delay it
                                self.constraints.insert(constraint_id, constraint);
                                self.delay_constraint(constraint_id, waiting_on)?;
                            } else {
                                // Can't delay, return error
                                return Err(e);
                            }
                        } else {
                            return Err(e);
                        }
                    }
                }
            }
        }

        if iterations >= max_iterations {
            return Err(TypeError::Internal {
                message: "Constraint solving did not converge".to_string(),
            });
        }

        // Check for unsolved required constraints
        for constraint in self.constraints.values() {
            if let Constraint::Unify {
                strength: ConstraintStrength::Required,
                ..
            } = constraint
            {
                return Err(TypeError::Internal {
                    message: "Unsolved required constraints remain".to_string(),
                });
            }
        }

        Ok(self.substitution.clone())
    }

    /// Pick the next constraint to solve based on heuristics
    fn pick_constraint(&mut self) -> Option<ConstraintId> {
        // Priority order:
        // 1. Constraints with no meta-variables
        // 2. Constraints with only solved meta-variables
        // 3. Simple pattern constraints (Miller patterns)
        // 4. Other constraints

        let mut best_constraint = None;
        let mut best_score = i32::MAX;

        for &id in &self.queue {
            if let Some(constraint) = self.constraints.get(&id) {
                let score = self.score_constraint(constraint);
                if score < best_score {
                    best_score = score;
                    best_constraint = Some(id);
                }
            }
        }

        if let Some(id) = best_constraint {
            self.queue.retain(|&x| x != id);
            Some(id)
        } else {
            self.queue.pop_front()
        }
    }

    /// Score a constraint for solving priority (lower is better)
    fn score_constraint(&self, constraint: &Constraint) -> i32 {
        let metas = self.collect_metas_in_constraint(constraint);

        if metas.is_empty() {
            return 0; // No metas, can solve immediately
        }

        let unsolved_count = metas
            .iter()
            .filter(|m| {
                self.metas
                    .get(m)
                    .and_then(|info| info.solution.as_ref())
                    .is_none()
            })
            .count();

        if unsolved_count == 0 {
            return 1; // All metas solved
        }

        // Check for Miller patterns
        if let Constraint::Unify { left, right, .. } = constraint {
            if self.is_miller_pattern_unification(left, right)
                || self.is_miller_pattern_unification(right, left)
            {
                return 10 + unsolved_count as i32;
            }
        }

        100 + unsolved_count as i32
    }

    /// Check if a unification is a Miller pattern (?M x1 ... xn = t)
    fn is_miller_pattern_unification(&self, left: &Term, right: &Term) -> bool {
        self.is_simple_miller_pattern(left, right)
    }

    /// Check if left is a simple Miller pattern and right is safe to solve with
    pub fn is_simple_miller_pattern(&self, left: &Term, right: &Term) -> bool {
        let (head, args) = self.decompose_application(left);

        if let Term::Meta(meta_name) = head {
            // Must have at least one argument to be interesting
            if args.is_empty() {
                return false;
            }

            // Check if all arguments are distinct variables
            let mut seen = HashSet::new();
            for arg in &args {
                if let Term::Var(x) = arg {
                    if !seen.insert(x.clone()) {
                        return false; // Not distinct
                    }
                } else {
                    return false; // Not a variable
                }
            }

            // Check occurs check - right must not contain the meta-variable
            if let Some(meta_id) = self.parse_meta_name(meta_name) {
                if self.collect_metas_in_term(right).contains(&meta_id) {
                    return false; // Occurs check failure
                }
            }

            // For now, only handle simple cases where right is a variable or constant
            match right {
                Term::Var(_) | Term::Const(_) | Term::Sort(_) => true,
                Term::Meta(_) => {
                    // Allow meta-to-meta if they're different
                    if let Term::Meta(right_name) = right {
                        meta_name != right_name
                    } else {
                        false
                    }
                }
                _ => false, // More complex cases need careful handling
            }
        } else {
            false
        }
    }

    /// Decompose an application into head and arguments
    fn decompose_application<'a>(&self, term: &'a Term) -> (&'a Term, Vec<&'a Term>) {
        let mut current = term;
        let mut args = Vec::new();

        while let Term::App(f, arg) = current {
            args.push(arg.as_ref());
            current = f;
        }

        args.reverse();
        (current, args)
    }

    /// Solve a single constraint
    fn solve_constraint(&mut self, constraint: Constraint) -> Result<bool, TypeError> {
        const MAX_RECURSION_DEPTH: u32 = 100;

        if self.recursion_depth >= MAX_RECURSION_DEPTH {
            return Err(TypeError::Internal {
                message: format!(
                    "Maximum recursion depth {} exceeded in solve_constraint",
                    MAX_RECURSION_DEPTH
                ),
            });
        }

        self.recursion_depth += 1;
        let result = self.solve_constraint_impl(constraint);
        self.recursion_depth -= 1;
        result
    }

    fn solve_constraint_impl(&mut self, constraint: Constraint) -> Result<bool, TypeError> {
        match constraint {
            Constraint::Unify {
                left,
                right,
                strength,
                ..
            } => self.unify(&left, &right, strength),
            Constraint::HasType {
                term,
                expected_type,
                ..
            } => {
                if self.enable_has_type_solving {
                    self.solve_has_type_constraint(&term, &expected_type)
                } else {
                    // Default behavior: don't solve HasType constraints
                    Ok(false)
                }
            }
            Constraint::UnifyUniverse {
                left,
                right,
                strength,
                ..
            } => self.unify_universe(&left, &right, strength),
            Constraint::UniverseLevel { left, right, .. } => {
                // For now, treat level constraints as unification constraints
                self.unify_universe(&left, &right, ConstraintStrength::Preferred)
            }
            Constraint::Delayed { constraint, .. } => {
                // Re-queue the delayed constraint
                let inner = *constraint;
                self.add_constraint(inner);
                Ok(false)
            }
        }
    }

    /// Unify two terms
    fn unify(
        &mut self,
        left: &Term,
        right: &Term,
        strength: ConstraintStrength,
    ) -> Result<bool, TypeError> {
        const MAX_RECURSION_DEPTH: u32 = 50;

        if self.recursion_depth >= MAX_RECURSION_DEPTH {
            return Err(TypeError::Internal {
                message: format!(
                    "Maximum recursion depth {} exceeded in unify",
                    MAX_RECURSION_DEPTH
                ),
            });
        }

        self.recursion_depth += 1;
        let result = self.unify_impl(left, right, strength);
        self.recursion_depth -= 1;
        result
    }

    fn unify_impl(
        &mut self,
        left: &Term,
        right: &Term,
        strength: ConstraintStrength,
    ) -> Result<bool, TypeError> {
        // Apply current substitution
        let left = self.substitution.apply(left);
        let right = self.substitution.apply(right);

        // Check if already equal
        if self.alpha_equal(&left, &right) {
            return Ok(false);
        }

        // Miller pattern detection and solving (if enabled)
        if self.enable_miller_patterns {
            if self.is_simple_miller_pattern(&left, &right) {
                if let Some((meta_name, spine)) = self.extract_miller_spine(&left) {
                    // Try to solve the Miller pattern directly
                    return self.solve_miller_pattern(&meta_name, &spine, &right);
                }
            } else if self.is_simple_miller_pattern(&right, &left) {
                if let Some((meta_name, spine)) = self.extract_miller_spine(&right) {
                    // Try to solve the Miller pattern directly
                    return self.solve_miller_pattern(&meta_name, &spine, &left);
                }
            }
        }

        match (&left, &right) {
            // Meta-variable cases
            (Term::Meta(m), t) | (t, Term::Meta(m)) => {
                if let Some(meta_id) = self.parse_meta_name(m) {
                    self.solve_meta(meta_id, t)
                } else {
                    Err(TypeError::Internal {
                        message: format!("Invalid meta-variable: {}", m),
                    })
                }
            }

            // Structural cases
            (Term::App(f1, a1), Term::App(f2, a2)) => {
                let id1 = self.new_constraint_id();
                let id2 = self.new_constraint_id();

                self.add_constraint(Constraint::Unify {
                    id: id1,
                    left: *f1.clone(),
                    right: *f2.clone(),
                    strength,
                });

                self.add_constraint(Constraint::Unify {
                    id: id2,
                    left: *a1.clone(),
                    right: *a2.clone(),
                    strength,
                });

                Ok(true)
            }

            (Term::Pi(x1, t1, b1, i1), Term::Pi(x2, t2, b2, i2)) if i1 == i2 => {
                let id1 = self.new_constraint_id();
                let id2 = self.new_constraint_id();

                self.add_constraint(Constraint::Unify {
                    id: id1,
                    left: *t1.clone(),
                    right: *t2.clone(),
                    strength,
                });

                // Rename bound variables to be consistent
                let fresh_var = format!("x{}", self.next_meta_id);
                let b1_renamed = self.rename_var(x1, &fresh_var, b1);
                let b2_renamed = self.rename_var(x2, &fresh_var, b2);

                self.add_constraint(Constraint::Unify {
                    id: id2,
                    left: b1_renamed,
                    right: b2_renamed,
                    strength,
                });

                Ok(true)
            }

            (Term::Abs(x1, t1, b1), Term::Abs(x2, t2, b2)) => {
                let id1 = self.new_constraint_id();
                let id2 = self.new_constraint_id();

                self.add_constraint(Constraint::Unify {
                    id: id1,
                    left: *t1.clone(),
                    right: *t2.clone(),
                    strength,
                });

                let fresh_var = format!("x{}", self.next_meta_id);
                let b1_renamed = self.rename_var(x1, &fresh_var, b1);
                let b2_renamed = self.rename_var(x2, &fresh_var, b2);

                self.add_constraint(Constraint::Unify {
                    id: id2,
                    left: b1_renamed,
                    right: b2_renamed,
                    strength,
                });

                Ok(true)
            }

            // Universe (Sort) cases
            (Term::Sort(u1), Term::Sort(u2)) => self.unify_universe(u1, u2, strength),

            _ => {
                if strength == ConstraintStrength::Required {
                    Err(TypeError::TypeMismatch {
                        expected: left,
                        actual: right,
                    })
                } else {
                    Ok(false) // Can't solve, but not an error for weak
                              // constraints
                }
            }
        }
    }

    /// Unify two universes
    fn unify_universe(
        &mut self,
        left: &Universe,
        right: &Universe,
        strength: ConstraintStrength,
    ) -> Result<bool, TypeError> {
        // Apply current substitution
        let left = self.substitution.apply_universe(left);
        let right = self.substitution.apply_universe(right);

        // Normalize universes (simplify expressions like 0+1 -> 1)
        let left = self.normalize_universe(&left);
        let right = self.normalize_universe(&right);

        // Check if already equal
        if left == right {
            return Ok(false);
        }

        match (&left, &right) {
            // Same constants
            (Universe::Const(n1), Universe::Const(n2)) if n1 == n2 => Ok(false),

            // Universe variable cases
            (Universe::ScopedVar(s1, v1), Universe::ScopedVar(s2, v2)) if s1 == s2 && v1 == v2 => {
                Ok(false)
            }
            (Universe::ScopedVar(_, _), Universe::ScopedVar(_, _)) => {
                // Different universe variables cannot be unified
                if strength == ConstraintStrength::Required {
                    Err(TypeError::Internal {
                        message: format!(
                            "Cannot unify distinct universe variables {} and {}",
                            left, right
                        ),
                    })
                } else {
                    Ok(false)
                }
            }

            // Meta-variable cases
            (Universe::Meta(id), u) | (u, Universe::Meta(id)) => self.solve_universe_meta(*id, u),

            // Structural cases
            (Universe::Add(base1, n1), Universe::Add(base2, n2)) if n1 == n2 => {
                let constraint_id = self.new_constraint_id();
                self.add_constraint(Constraint::UnifyUniverse {
                    id: constraint_id,
                    left: *base1.clone(),
                    right: *base2.clone(),
                    strength,
                });
                Ok(true)
            }

            // Arithmetic constraints: ?u+n = m means ?u = m-n
            (Universe::Add(base, n), Universe::Const(m))
            | (Universe::Const(m), Universe::Add(base, n)) => {
                if *m >= *n {
                    if let Universe::Meta(id) = base.as_ref() {
                        self.solve_universe_meta(*id, &Universe::Const(m - n))
                    } else {
                        // More complex case - generate constraint for base
                        let constraint_id = self.new_constraint_id();
                        self.add_constraint(Constraint::UnifyUniverse {
                            id: constraint_id,
                            left: *base.clone(),
                            right: Universe::Const(m - n),
                            strength,
                        });
                        Ok(true)
                    }
                } else {
                    Err(TypeError::Internal {
                        message: format!("Cannot solve constraint: {} cannot equal {} (would require negative universe)",
                                       if matches!(&left, Universe::Add(_, _)) { &left } else { &right },
                                       if matches!(&left, Universe::Const(_)) { &left } else { &right }),
                    })
                }
            }

            (Universe::Max(u1, v1), Universe::Max(u2, v2)) => {
                let id1 = self.new_constraint_id();
                let id2 = self.new_constraint_id();

                self.add_constraint(Constraint::UnifyUniverse {
                    id: id1,
                    left: *u1.clone(),
                    right: *u2.clone(),
                    strength,
                });

                self.add_constraint(Constraint::UnifyUniverse {
                    id: id2,
                    left: *v1.clone(),
                    right: *v2.clone(),
                    strength,
                });

                Ok(true)
            }

            (Universe::IMax(u1, v1), Universe::IMax(u2, v2)) => {
                let id1 = self.new_constraint_id();
                let id2 = self.new_constraint_id();

                self.add_constraint(Constraint::UnifyUniverse {
                    id: id1,
                    left: *u1.clone(),
                    right: *u2.clone(),
                    strength,
                });

                self.add_constraint(Constraint::UnifyUniverse {
                    id: id2,
                    left: *v1.clone(),
                    right: *v2.clone(),
                    strength,
                });

                Ok(true)
            }

            _ => {
                if strength == ConstraintStrength::Required {
                    Err(TypeError::Internal {
                        message: format!("Cannot unify universes {} and {}", left, right),
                    })
                } else {
                    Ok(false)
                }
            }
        }
    }

    /// Normalize a universe expression
    #[allow(clippy::only_used_in_recursion)]
    fn normalize_universe(&self, u: &Universe) -> Universe {
        match u {
            Universe::Add(base, n) => {
                let base_norm = self.normalize_universe(base);
                match base_norm {
                    Universe::Const(m) => Universe::Const(m + n),
                    _ => Universe::Add(Box::new(base_norm), *n),
                }
            }
            Universe::Max(u1, u2) => {
                let u1_norm = self.normalize_universe(u1);
                let u2_norm = self.normalize_universe(u2);
                match (&u1_norm, &u2_norm) {
                    (Universe::Const(n1), Universe::Const(n2)) => Universe::Const((*n1).max(*n2)),
                    _ => Universe::Max(Box::new(u1_norm), Box::new(u2_norm)),
                }
            }
            _ => u.clone(),
        }
    }

    /// Solve a universe meta-variable
    fn solve_universe_meta(
        &mut self,
        meta_id: u32,
        solution: &Universe,
    ) -> Result<bool, TypeError> {
        // Occurs check for universe meta-variables
        if self.occurs_in_universe(meta_id, solution) {
            return Err(TypeError::Internal {
                message: format!("Universe occurs check failed for ?u{}", meta_id),
            });
        }

        // Record solution
        self.substitution.insert_universe(meta_id, solution.clone());
        Ok(true)
    }

    /// Check if a universe meta-variable occurs in a universe expression
    #[allow(clippy::only_used_in_recursion)]
    fn occurs_in_universe(&self, meta_id: u32, universe: &Universe) -> bool {
        match universe {
            Universe::Meta(id) => *id == meta_id,
            Universe::Add(base, _) => self.occurs_in_universe(meta_id, base),
            Universe::Max(u, v) | Universe::IMax(u, v) => {
                self.occurs_in_universe(meta_id, u) || self.occurs_in_universe(meta_id, v)
            }
            _ => false,
        }
    }

    /// Solve a meta-variable
    fn solve_meta(&mut self, meta_id: MetaId, solution: &Term) -> Result<bool, TypeError> {
        // Occurs check
        if self.collect_metas_in_term(solution).contains(&meta_id) {
            return Err(TypeError::Internal {
                message: format!("Occurs check failed for ?m{}", meta_id.0),
            });
        }

        // Check solution is well-scoped
        if let Some(meta_info) = self.metas.get(&meta_id) {
            if !self.is_well_scoped(solution, &meta_info.context) {
                return Err(TypeError::Internal {
                    message: format!("Solution for ?m{} is not well-scoped", meta_id.0),
                });
            }
        }

        // Record solution
        self.substitution.insert(meta_id, solution.clone());

        if let Some(info) = self.metas.get_mut(&meta_id) {
            info.solution = Some(solution.clone());
        }

        Ok(true)
    }

    /// Check if a term is well-scoped in a context
    fn is_well_scoped(&self, term: &Term, context: &[String]) -> bool {
        match term {
            Term::Var(x) => {
                // Check if it's a bound variable or a global constant
                context.contains(x)
                    || self.context.lookup(x).is_some()
                    || self.context.lookup_axiom(x).is_some()
                    || self.context.lookup_constructor(x).is_some()
            }
            Term::App(f, arg) => {
                self.is_well_scoped(f, context) && self.is_well_scoped(arg, context)
            }
            Term::Abs(x, ty, body) => {
                let mut extended_ctx = context.to_vec();
                extended_ctx.push(x.clone());
                self.is_well_scoped(ty, context) && self.is_well_scoped(body, &extended_ctx)
            }
            Term::Pi(x, ty, body, _) => {
                let mut extended_ctx = context.to_vec();
                extended_ctx.push(x.clone());
                self.is_well_scoped(ty, context) && self.is_well_scoped(body, &extended_ctx)
            }
            Term::Const(name) => {
                // Constants should be in the global context
                self.context.lookup_axiom(name).is_some()
                    || self.context.lookup_constructor(name).is_some()
            }
            _ => true, // Sorts, meta-variables, etc.
        }
    }

    /// Alpha equality check
    fn alpha_equal(&self, t1: &Term, t2: &Term) -> bool {
        self.alpha_equal_aux(t1, t2, &mut vec![])
    }

    #[allow(clippy::only_used_in_recursion)]
    fn alpha_equal_aux(&self, t1: &Term, t2: &Term, renamings: &mut Vec<(String, String)>) -> bool {
        match (t1, t2) {
            (Term::Var(x), Term::Var(y)) => {
                // Check if this is a bound variable that was renamed
                for (a, b) in renamings.iter() {
                    if a == x && b == y {
                        return true;
                    }
                }
                x == y
            }
            (Term::App(f1, a1), Term::App(f2, a2)) => {
                self.alpha_equal_aux(f1, f2, renamings) && self.alpha_equal_aux(a1, a2, renamings)
            }
            (Term::Abs(x1, t1, b1), Term::Abs(x2, t2, b2)) => {
                if !self.alpha_equal_aux(t1, t2, renamings) {
                    return false;
                }
                renamings.push((x1.clone(), x2.clone()));
                let result = self.alpha_equal_aux(b1, b2, renamings);
                renamings.pop();
                result
            }
            (Term::Pi(x1, t1, b1, i1), Term::Pi(x2, t2, b2, i2)) if i1 == i2 => {
                if !self.alpha_equal_aux(t1, t2, renamings) {
                    return false;
                }
                renamings.push((x1.clone(), x2.clone()));
                let result = self.alpha_equal_aux(b1, b2, renamings);
                renamings.pop();
                result
            }
            (Term::Sort(u1), Term::Sort(u2)) => u1 == u2,
            (Term::Const(c1), Term::Const(c2)) => c1 == c2,
            (Term::Meta(m1), Term::Meta(m2)) => m1 == m2,
            _ => false,
        }
    }

    /// Rename a variable in a term
    #[allow(clippy::only_used_in_recursion)]
    fn rename_var(&self, old: &str, new: &str, term: &Term) -> Term {
        match term {
            Term::Var(x) if x == old => Term::Var(new.to_string()),
            Term::Var(x) => Term::Var(x.clone()),
            Term::App(f, arg) => Term::App(
                Box::new(self.rename_var(old, new, f)),
                Box::new(self.rename_var(old, new, arg)),
            ),
            Term::Abs(x, ty, body) if x != old => Term::Abs(
                x.clone(),
                Box::new(self.rename_var(old, new, ty)),
                Box::new(self.rename_var(old, new, body)),
            ),
            Term::Pi(x, ty, body, implicit) if x != old => Term::Pi(
                x.clone(),
                Box::new(self.rename_var(old, new, ty)),
                Box::new(self.rename_var(old, new, body)),
                *implicit,
            ),
            _ => term.clone(),
        }
    }

    /// Propagate solutions to dependent constraints
    fn propagate_solution(&mut self) -> Result<(), TypeError> {
        // Apply substitution to all remaining constraints
        let constraints: Vec<_> = self.constraints.drain().collect();

        for (id, constraint) in constraints {
            let updated = self.apply_subst_to_constraint(constraint);
            self.constraints.insert(id, updated);
        }

        Ok(())
    }

    /// Apply substitution to a constraint
    fn apply_subst_to_constraint(&self, constraint: Constraint) -> Constraint {
        match constraint {
            Constraint::Unify {
                id,
                left,
                right,
                strength,
            } => Constraint::Unify {
                id,
                left: self.substitution.apply(&left),
                right: self.substitution.apply(&right),
                strength,
            },
            Constraint::HasType {
                id,
                term,
                expected_type,
            } => Constraint::HasType {
                id,
                term: self.substitution.apply(&term),
                expected_type: self.substitution.apply(&expected_type),
            },
            Constraint::UnifyUniverse {
                id,
                left,
                right,
                strength,
            } => Constraint::UnifyUniverse {
                id,
                left: self.substitution.apply_universe(&left),
                right: self.substitution.apply_universe(&right),
                strength,
            },
            Constraint::UniverseLevel { id, left, right } => Constraint::UniverseLevel {
                id,
                left: self.substitution.apply_universe(&left),
                right: self.substitution.apply_universe(&right),
            },
            Constraint::Delayed {
                id,
                constraint,
                waiting_on,
            } => Constraint::Delayed {
                id,
                constraint: Box::new(self.apply_subst_to_constraint(*constraint)),
                waiting_on,
            },
        }
    }

    /// Solve a HasType constraint by type inference and unification
    fn solve_has_type_constraint(
        &mut self,
        term: &Term,
        expected_type: &Term,
    ) -> Result<bool, TypeError> {
        match term {
            Term::Meta(meta_name) => {
                // If the term is a meta-variable, we can potentially solve it
                if let Some(meta_id) = self.parse_meta_name(meta_name) {
                    if let Some(meta_info) = self.metas.get(&meta_id) {
                        if meta_info.solution.is_none() {
                            // Try to construct a term that has the expected type
                            if let Some(solution) = self.synthesize_term_of_type(expected_type)? {
                                return self.solve_meta(meta_id, &solution);
                            }
                        }
                    }
                }
                // If we can't solve the meta, try unifying with expected type
                self.unify(term, expected_type, ConstraintStrength::Preferred)
            }

            Term::App(f, arg) => {
                // For applications, we need to ensure the function type is compatible
                // Create a fresh meta for the function type: ?F : ?A -> ?B
                let arg_type_meta_id = self.fresh_meta(vec![], None);
                let result_type_meta_id = self.fresh_meta(vec![], None);
                let arg_type_meta = Term::Meta(format!("?m{}", arg_type_meta_id.0));
                let result_type_meta = Term::Meta(format!("?m{}", result_type_meta_id.0));
                let func_type = Term::Pi(
                    "_".to_string(),
                    Box::new(arg_type_meta.clone()),
                    Box::new(result_type_meta.clone()),
                    false,
                );

                // Add constraints:
                // 1. f : ?A -> ?B
                // 2. arg : ?A
                // 3. ?B ≡ expected_type
                let f_constraint = Constraint::HasType {
                    id: self.new_constraint_id(),
                    term: f.as_ref().clone(),
                    expected_type: func_type,
                };
                let arg_constraint = Constraint::HasType {
                    id: self.new_constraint_id(),
                    term: arg.as_ref().clone(),
                    expected_type: arg_type_meta,
                };
                let result_constraint = Constraint::Unify {
                    id: self.new_constraint_id(),
                    left: result_type_meta,
                    right: expected_type.clone(),
                    strength: ConstraintStrength::Required,
                };

                self.add_constraint(f_constraint);
                self.add_constraint(arg_constraint);
                self.add_constraint(result_constraint);

                Ok(true)
            }

            Term::Abs(param, param_type, body) => {
                // For lambda abstractions, expected type should be a Pi type
                match expected_type {
                    Term::Pi(_, expected_param_type, expected_body_type, _) => {
                        // Unify parameter types and check body type
                        let param_unify = Constraint::Unify {
                            id: self.new_constraint_id(),
                            left: param_type.as_ref().clone(),
                            right: expected_param_type.as_ref().clone(),
                            strength: ConstraintStrength::Required,
                        };

                        let body_check = Constraint::HasType {
                            id: self.new_constraint_id(),
                            term: body.as_ref().clone(),
                            expected_type: expected_body_type.as_ref().clone(),
                        };

                        self.add_constraint(param_unify);
                        self.add_constraint(body_check);

                        Ok(true)
                    }
                    Term::Meta(_) => {
                        // Expected type is a meta-variable, create a Pi type for it
                        let body_type_meta_id = self.fresh_meta(vec![param.clone()], None);
                        let body_type_meta = Term::Meta(format!("?m{}", body_type_meta_id.0));
                        let pi_type = Term::Pi(
                            param.clone(),
                            param_type.clone(),
                            Box::new(body_type_meta.clone()),
                            false,
                        );

                        let type_unify = Constraint::Unify {
                            id: self.new_constraint_id(),
                            left: expected_type.clone(),
                            right: pi_type,
                            strength: ConstraintStrength::Required,
                        };

                        let body_check = Constraint::HasType {
                            id: self.new_constraint_id(),
                            term: body.as_ref().clone(),
                            expected_type: body_type_meta,
                        };

                        self.add_constraint(type_unify);
                        self.add_constraint(body_check);

                        Ok(true)
                    }
                    _ => {
                        // Type mismatch - lambda can't have non-function type
                        Err(TypeError::TypeMismatch {
                            expected: expected_type.clone(),
                            actual: Term::Pi(
                                param.clone(),
                                param_type.clone(),
                                Box::new(Term::Meta("?unknown".to_string())),
                                false,
                            ),
                        })
                    }
                }
            }

            _ => {
                // For other terms, just unify with the expected type
                self.unify(term, expected_type, ConstraintStrength::Preferred)
            }
        }
    }

    /// Attempt to synthesize a term of a given type (basic heuristics)
    fn synthesize_term_of_type(&self, expected_type: &Term) -> Result<Option<Term>, TypeError> {
        match expected_type {
            Term::Sort(_) => {
                // For Sort types, we could return a fresh meta or a simple type
                Ok(Some(Term::Meta(format!("?synth{}", self.next_meta_id))))
            }
            Term::Pi(param, param_type, _body_type, _implicit) => {
                // For Pi types, synthesize a lambda that matches the Pi structure
                // Use a fresh meta-variable for the body to avoid infinite recursion
                let body_term = Term::Meta(format!("?body{}", self.next_meta_id));

                Ok(Some(Term::Abs(
                    param.clone(),
                    param_type.clone(),
                    Box::new(body_term),
                )))
            }
            Term::Meta(_) => {
                // Can't synthesize for unknown types
                Ok(None)
            }
            _ => {
                // For concrete types, return a meta-variable
                Ok(Some(Term::Meta(format!("?synth{}", self.next_meta_id))))
            }
        }
    }

    /// Check if a term is a Miller pattern
    /// Miller patterns are terms of the form: ?M x1 x2 ... xn where xi are
    /// distinct bound variables
    fn is_miller_pattern(&self, term: &Term) -> bool {
        let (head, args) = self.decompose_application(term);

        match head {
            Term::Meta(_) => {
                // Check that all arguments are distinct variables
                let mut seen = HashSet::new();
                for arg in args {
                    if let Term::Var(x) = arg {
                        if !seen.insert(x.clone()) {
                            return false; // Not distinct
                        }
                    } else {
                        return false; // Not a variable
                    }
                }
                true
            }
            _ => false,
        }
    }

    /// Extract the spine of a Miller pattern (the meta-variable and its
    /// arguments)
    fn extract_miller_spine(&self, term: &Term) -> Option<(String, Vec<String>)> {
        // First validate it's actually a Miller pattern
        if !self.is_miller_pattern(term) {
            return None;
        }

        let (head, args) = self.decompose_application(term);

        if let Term::Meta(meta_name) = head {
            // Collect variable names (we already know they're all variables from
            // is_miller_pattern)
            let var_names: Vec<String> = args
                .iter()
                .map(|arg| {
                    if let Term::Var(name) = arg {
                        name.clone()
                    } else {
                        unreachable!()
                    }
                })
                .collect();

            Some((meta_name.clone(), var_names))
        } else {
            None
        }
    }

    /// Solve simple Miller pattern unification
    /// Only handles simple cases
    fn solve_miller_pattern(
        &mut self,
        meta_name: &str,
        spine: &[String],
        other: &Term,
    ) -> Result<bool, TypeError> {
        // Check if we can abstract over the spine variables
        if !Self::can_abstract_over_variables(other, spine) {
            // Can't abstract, fall back to regular unification
            return self.unify(
                &Term::Meta(meta_name.to_string()),
                other,
                ConstraintStrength::Required,
            );
        }

        if let Some(meta_id) = self.parse_meta_name(meta_name) {
            // Only handle simple cases to avoid complexity
            match other {
                // Case 1: ?M x ≡ y (different variable) -> ?M := λx. y
                Term::Var(y) if spine.len() == 1 && spine[0] != *y => {
                    let solution = Term::Abs(
                        spine[0].clone(),
                        Box::new(Term::Meta(format!("?T{}", self.next_meta_id))), /* Type inferred later */
                        Box::new(other.clone()),
                    );
                    return self.solve_meta(meta_id, &solution);
                }

                // Case 2: ?M x ≡ c (constant) -> ?M := λx. c
                Term::Const(_) | Term::Sort(_) if spine.len() == 1 => {
                    let solution = Term::Abs(
                        spine[0].clone(),
                        Box::new(Term::Meta(format!("?T{}", self.next_meta_id))),
                        Box::new(other.clone()),
                    );
                    return self.solve_meta(meta_id, &solution);
                }

                _ => {
                    // For more complex cases, we could build the full lambda
                    // abstraction but for now, fall back to
                    // regular unification
                }
            }
        }

        // Fall back to regular unification
        self.unify(
            &Term::Meta(meta_name.to_string()),
            other,
            ConstraintStrength::Required,
        )
    }

    /// Check if a term can be abstracted over the given variables
    fn can_abstract_over_variables(term: &Term, vars: &[String]) -> bool {
        // For now, implement a simple check
        // In a full implementation, this would check that the term only uses variables
        // that are either in the spine or are bound locally
        match term {
            Term::Var(v) => vars.contains(v), // Variable must be in spine
            Term::App(f, arg) => {
                Self::can_abstract_over_variables(f, vars)
                    && Self::can_abstract_over_variables(arg, vars)
            }
            Term::Abs(param, ty, body) => {
                // Extend the scope with the bound parameter
                let mut extended_vars = vars.to_vec();
                extended_vars.push(param.clone());
                Self::can_abstract_over_variables(ty, vars)
                    && Self::can_abstract_over_variables(body, &extended_vars)
            }
            Term::Meta(_) => true,  // Meta-variables are always abstractable
            Term::Const(_) => true, // Constants are always abstractable
            Term::Sort(_) => true,  // Sorts are always abstractable
            _ => false,             // Conservative for complex constructs
        }
    }

    /// Delay a constraint for later processing
    fn delay_constraint(
        &mut self,
        constraint_id: ConstraintId,
        waiting_on: HashSet<MetaId>,
    ) -> Result<(), TypeError> {
        if let Some(constraint) = self.constraints.remove(&constraint_id) {
            let delayed = Constraint::Delayed {
                id: constraint_id,
                constraint: Box::new(constraint),
                waiting_on,
            };
            self.constraints.insert(constraint_id, delayed);
            self.delayed_constraints.insert(constraint_id);
        }
        Ok(())
    }

    /// Wake up delayed constraints that are no longer blocked
    fn wake_delayed_constraints(&mut self) -> Result<Vec<ConstraintId>, TypeError> {
        let mut woken = Vec::new();
        let mut to_wake = Vec::new();

        // Check which delayed constraints can now be woken up
        for constraint_id in &self.delayed_constraints {
            if let Some(Constraint::Delayed {
                waiting_on,
                constraint,
                ..
            }) = self.constraints.get(constraint_id)
            {
                // Check if all blocking metas are now solved
                let all_solved = waiting_on.iter().all(|meta_id| {
                    self.metas
                        .get(meta_id)
                        .is_some_and(|info| info.solution.is_some())
                });

                if all_solved {
                    to_wake.push((*constraint_id, constraint.as_ref().clone()));
                }
            }
        }

        // Wake up the constraints
        for (constraint_id, original_constraint) in to_wake {
            self.constraints.insert(constraint_id, original_constraint);
            self.delayed_constraints.remove(&constraint_id);
            woken.push(constraint_id);
        }

        Ok(woken)
    }

    /// Check if we can delay a constraint
    fn can_delay(&self, constraint_id: &ConstraintId) -> bool {
        if let Some(constraint) = self.constraints.get(constraint_id) {
            match constraint {
                Constraint::Unify {
                    left,
                    right,
                    strength,
                    ..
                } => {
                    // We can delay weak constraints that involve unsolved metas
                    if *strength == ConstraintStrength::Weak {
                        let left_metas = self.collect_metas_in_term(left);
                        let right_metas = self.collect_metas_in_term(right);

                        // Check if any involved metas are unsolved
                        for meta_id in left_metas.iter().chain(right_metas.iter()) {
                            if self.is_unsolved_meta(meta_id) {
                                return true;
                            }
                        }
                    }
                    false
                }
                Constraint::HasType {
                    term,
                    expected_type,
                    ..
                } => {
                    // Can delay HasType constraints involving unsolved metas
                    let term_metas = self.collect_metas_in_term(term);
                    let type_metas = self.collect_metas_in_term(expected_type);

                    for meta_id in term_metas.iter().chain(type_metas.iter()) {
                        if let Some(meta_info) = self.metas.get(meta_id) {
                            if meta_info.solution.is_none() {
                                return true;
                            }
                        }
                    }
                    false
                }
                Constraint::Delayed { .. } => true, // Already delayed
                _ => false,
            }
        } else {
            false
        }
    }

    /// Check if a meta-variable is unsolved
    fn is_unsolved_meta(&self, meta_id: &MetaId) -> bool {
        self.metas
            .get(meta_id)
            .is_some_and(|info| info.solution.is_none())
    }
}
