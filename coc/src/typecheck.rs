//! # Type Checker

use std::collections::HashMap;

use crate::alpha::alpha_equivalent;
use crate::ast::{Declaration, MatchArm, Module, Parameter, Pattern, Term, Universe};
use crate::context::{Context, ContextError};
use crate::errors::{TypeError, TypeResult};
use crate::solver::{Constraint, ConstraintStrength, Solver};
use crate::term_utils::{
    contains_universe_metavariables, extract_constructor_arg_types,
    extract_constructor_return_type, fresh_var, has_implicit_params, is_simple_type,
    normalize_universe, pattern_binds_var, rename_var, substitute_universes,
};
use crate::unification::Unifier;
use crate::universe_solver::UniverseSolver;

// ===== CONSTANTS =====

/// Common identifier constants used in type checking
const UNDERSCORE: &str = "_";
const HOLE_PREFIX: &str = "?_";
const META_PREFIX: &str = "?m";
const PAIR_VAR: &str = "_pair_var";
const STRUCT_PARAM: &str = "x";
const UNIVERSE_VAR_PREFIX: &str = "u";

/// Default type names
const NAT_TYPE: &str = "Nat";
const DUMMY_TYPE: &str = "dummy";

/// Meta-variable constants
const META_A: &str = "A";
const META_B: &str = "B";

// ===== CONTEXT HELPERS =====

impl TypeChecker {
    /// Set the active term for error context
    fn with_context<T>(
        &mut self,
        term: &Term,
        f: impl FnOnce(&mut Self) -> TypeResult<T>,
    ) -> TypeResult<T> {
        let old_term = self.active_term.clone();
        self.active_term = Some(term.clone());
        let result = f(self);
        self.active_term = old_term;

        result.map_err(|err| {
            if matches!(err, TypeError::WithContext { .. }) {
                // Don't double-wrap context
                err
            } else {
                TypeError::WithContext {
                    expr: term.clone(),
                    source: Box::new(err),
                }
            }
        })
    }
}

// ===== TYPE CHECKER STRUCT =====

/// Bidirectional type checker for the Calculus of Constructions with universe
/// polymorphism
pub struct TypeChecker {
    pub unifier: Unifier,
    universe_solver: UniverseSolver,
    /// Counter for generating fresh universe variables
    universe_counter: u32,
    /// The currently active term being type-checked (for error context)
    active_term: Option<Term>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            unifier: Unifier::new(),
            universe_solver: UniverseSolver::new(),
            universe_counter: 0,
            active_term: None,
        }
    }

    // ===== MODULE AND DECLARATION CHECKING =====

    /// Type check a module
    pub fn check_module(&mut self, module: &Module) -> TypeResult<Context> {
        let mut ctx = Context::new();

        for declaration in &module.declarations {
            self.check_declaration(declaration, &mut ctx)?;
        }

        Ok(ctx)
    }

    /// Type check a declaration and update context
    pub fn check_declaration(&mut self, decl: &Declaration, ctx: &mut Context) -> TypeResult<()> {
        match decl {
            Declaration::Definition {
                name,
                universe_params,
                params,
                ty,
                body,
            } => {
                // Build extended context with universe parameters and regular parameters
                let mut extended_ctx = ctx.extend_universe_many(universe_params.clone());
                for param in params {
                    extended_ctx = extended_ctx.extend(param.name.clone(), param.ty.clone());
                }

                // Check the type is well-formed in context with parameters
                let ty_sort = self.infer(ty, &extended_ctx)?;
                self.check_is_type(&ty_sort)?;

                // Build function type from parameters
                let full_type = self.build_pi_type(params, ty);

                // Add the function to context before checking body (to allow recursion)
                let mut recursive_ctx = ctx.clone();
                recursive_ctx.add_axiom(name.clone(), full_type.clone());

                // Build lambda from parameters and body
                let full_body = self.build_lambda(params, body);

                // Check body against type
                self.check(&full_body, &full_type, &recursive_ctx)?;

                // Add to context with universe parameters
                if universe_params.is_empty() {
                    ctx.add_axiom(name.clone(), full_type);
                } else {
                    ctx.add_definition(name.clone(), universe_params.clone(), full_type);
                }
                Ok(())
            }

            Declaration::Axiom {
                name,
                universe_params,
                ty,
            } => {
                // Check the type is well-formed
                let ty_sort = self.infer(ty, ctx)?;
                self.check_is_type(&ty_sort)?;

                // Add to context with universe parameters
                if universe_params.is_empty() {
                    ctx.add_axiom(name.clone(), ty.clone());
                } else {
                    ctx.add_definition(name.clone(), universe_params.clone(), ty.clone());
                }
                Ok(())
            }

            Declaration::Inductive {
                name,
                universe_params,
                params,
                ty,
                constructors,
            } => {
                // Extend context with universe parameters
                let mut extended_ctx = ctx.extend_universe_many(universe_params.clone());

                // Also extend with the inductive type parameters
                for param in params {
                    extended_ctx = extended_ctx.extend(param.name.clone(), param.ty.clone());
                }

                // First add the inductive type to both contexts
                let full_type = self.build_pi_type(params, ty);

                // Store inductive type with universe parameters
                if universe_params.is_empty() {
                    ctx.add_axiom(name.clone(), full_type.clone());
                    extended_ctx.add_axiom(name.clone(), full_type.clone());
                } else {
                    ctx.add_definition(name.clone(), universe_params.clone(), full_type.clone());
                    extended_ctx.add_definition(
                        name.clone(),
                        universe_params.clone(),
                        full_type.clone(),
                    );
                }

                // Then check constructors in context where the inductive type is available
                for constructor in constructors {
                    // Check constructor type is well-formed in extended context (with params)
                    let constructor_sort = self.infer(&constructor.ty, &extended_ctx)?;
                    self.check_is_type(&constructor_sort)?;

                    // Add constructor with full type (including parameters)
                    // The constructor type should be: forall params, constructor.ty
                    let full_constructor_type = self.build_pi_type(params, &constructor.ty);

                    // If the inductive type has universe parameters, store constructors as
                    // definitions so they can be instantiated properly
                    if universe_params.is_empty() {
                        ctx.add_constructor(constructor.name.clone(), full_constructor_type);
                    } else {
                        ctx.add_definition(
                            constructor.name.clone(),
                            universe_params.clone(),
                            full_constructor_type,
                        );
                    }
                }

                Ok(())
            }

            Declaration::Structure {
                name,
                universe_params: _,
                params,
                ty,
                fields,
            } => {
                // Extend context with parameters for checking fields
                let mut extended_ctx = ctx.clone();
                for param in params {
                    extended_ctx = extended_ctx.extend(param.name.clone(), param.ty.clone());
                }

                // Build full type with parameters
                let full_type = self.build_pi_type(params, ty);
                ctx.add_axiom(name.clone(), full_type.clone());

                // Add field projections with proper types
                for field in fields {
                    // Check field type is well-formed in extended context
                    let field_sort = self.infer(&field.ty, &extended_ctx)?;
                    self.check_is_type(&field_sort)?;

                    // Projection type should include parameters
                    // proj : forall params, StructType params -> FieldType
                    let struct_app = params.iter().fold(Term::Const(name.clone()), |acc, param| {
                        Term::App(Box::new(acc), Box::new(Term::Var(param.name.clone())))
                    });

                    let proj_type = self.build_pi_type(
                        params,
                        &Term::Pi(
                            STRUCT_PARAM.to_string(),
                            Box::new(struct_app),
                            Box::new(field.ty.clone()),
                            false,
                        ),
                    );
                    ctx.add_axiom(format!("{}.{}", name, field.name), proj_type);
                }

                Ok(())
            }
        }
    }

    // ===== TYPE INFERENCE =====

    /// Infer the type of a term
    pub fn infer(&mut self, term: &Term, ctx: &Context) -> TypeResult<Term> {
        self.with_context(term, |checker| checker.infer_impl(term, ctx))
    }

    fn infer_impl(&mut self, term: &Term, ctx: &Context) -> TypeResult<Term> {
        match term {
            Term::Var(x) => {
                // First try to find as a variable
                if let Some(ty) = ctx.lookup(x) {
                    Ok(ty.clone())
                }
                // Then check if it's a definition with universe parameters
                else if let Some(def) = ctx.lookup_definition(x) {
                    self.instantiate_universe_params(&def.ty, &def.universe_params, &def.name, ctx)
                }
                // Finally try axioms and constructors
                else if let Some(ty) = ctx.lookup_axiom(x).or_else(|| ctx.lookup_constructor(x)) {
                    Ok(ty.clone())
                } else {
                    Err(ContextError::UnboundVariable { name: x.clone() }.into())
                }
            }

            Term::App(f, arg) => {
                // Special case: Check if this is actually a universe-polymorphic constructor
                // application This happens when we have something like "cons A"
                // where cons is universe-polymorphic
                if let Term::Var(name) = f.as_ref() {
                    // Check if this var is actually a universe-polymorphic constructor
                    if let Some(def) = ctx.lookup_definition(name) {
                        if !def.universe_params.is_empty() {
                            // Convert to constructor application for universe-polymorphic
                            // constructors
                            return self
                                .infer(&Term::Constructor(name.clone(), vec![*arg.clone()]), ctx);
                        }
                    }
                }

                let f_type = self.infer(f, ctx)?;

                // Check if the function type has implicit parameters
                if has_implicit_params(&f_type) {
                    // Use constraint solving for implicit arguments
                    self.infer_app_with_constraints(&f_type, arg, ctx)
                } else {
                    // Regular application without implicit arguments
                    self.infer_app(&f_type, arg, ctx)
                }
            }

            Term::Abs(x, ty, body) => {
                // Handle holes in type annotations
                let (actual_ty, skip_type_check) = if let Term::Var(name) = ty.as_ref() {
                    if name == HOLE_PREFIX {
                        // Create a fresh meta-variable for the hole
                        // This meta-variable will be constrained to be a type
                        let meta = self.unifier.fresh_meta();
                        (Box::new(meta), true) // Skip type check since
                                               // meta-vars can be types
                    } else {
                        (ty.clone(), false)
                    }
                } else {
                    (ty.clone(), false)
                };

                // Check type annotation is well-formed (unless it's a meta-variable)
                if !skip_type_check {
                    let ty_sort = self.infer(&actual_ty, ctx)?;
                    self.check_is_type(&ty_sort)?;
                }

                // Infer body type in extended context
                let extended_ctx = ctx.extend(x.clone(), actual_ty.as_ref().clone());
                let body_type = self.infer(body, &extended_ctx)?;

                Ok(Term::Pi(x.clone(), actual_ty, Box::new(body_type), false))
            }

            Term::Pi(x, ty, body, _implicit) => {
                // Check domain is a type
                let ty_sort = self.infer(ty, ctx)?;
                let ty_universe = self.extract_universe(&ty_sort)?;

                // Check codomain is a type in extended context
                let extended_ctx = ctx.extend(x.clone(), ty.as_ref().clone());
                let body_sort = self.infer(body, &extended_ctx)?;
                let body_universe = self.extract_universe(&body_sort)?;

                // Rule for Pi-types in CoC
                let result_universe = self.pi_rule(&ty_universe, &body_universe, ctx)?;
                Ok(Term::Sort(result_universe))
            }

            Term::Sort(u) => {
                // Sort u : Sort (u+1)
                // Sort u has type Sort (u+1)
                let result = match u {
                    Universe::Const(n) => Universe::Const(n + 1),
                    Universe::ScopedVar(s, v) => {
                        Universe::Add(Box::new(Universe::ScopedVar(s.clone(), v.clone())), 1)
                    }
                    Universe::Add(base, n) => Universe::Add(base.clone(), n + 1),
                    _ => Universe::Add(Box::new(u.clone()), 1),
                };
                Ok(Term::Sort(normalize_universe(&result)))
            }

            Term::Let(x, ty, val, body) => {
                // Handle holes in type annotation
                let actual_ty = match ty.as_ref() {
                    Term::Meta(name) if name == UNDERSCORE => {
                        // No type annotation - infer from value
                        Box::new(self.infer(val, ctx)?)
                    }
                    _ => {
                        // Type annotation provided - check value against it
                        self.check(val, ty, ctx)?;
                        ty.clone()
                    }
                };

                // Infer body type in extended context
                let extended_ctx = ctx.extend(x.clone(), actual_ty.as_ref().clone());
                self.infer(body, &extended_ctx)
            }

            Term::Const(name) => {
                // First check if it's a definition with universe parameters
                if let Some(def) = ctx.lookup_definition(name) {
                    // Instantiate universe parameters with fresh universe variables
                    self.instantiate_universe_params(&def.ty, &def.universe_params, &def.name, ctx)
                } else {
                    // Fall back to regular axioms and constructors
                    ctx.lookup_axiom(name)
                        .or_else(|| ctx.lookup_constructor(name))
                        .cloned()
                        .ok_or_else(|| ContextError::UnboundAxiom { name: name.clone() }.into())
                }
            }

            Term::Constructor(name, args) => {
                // Look up constructor type - check both constructors and definitions
                let ctor_type = if let Some(ty) = ctx.lookup_constructor(name) {
                    ty.clone()
                } else if let Some(def) = ctx.lookup_definition(name) {
                    // Instantiate universe parameters for universe-polymorphic constructors
                    self.instantiate_universe_params(&def.ty, &def.universe_params, &def.name, ctx)?
                } else {
                    return Err(TypeError::UnknownConstructor { name: name.clone() });
                };

                // Apply constructor to its arguments
                let mut result_type = ctor_type;
                for arg in args {
                    let arg_type = self.infer(arg, ctx)?;
                    result_type = match result_type {
                        Term::Pi(param_name, param_ty, body_ty, _) => {
                            // Check argument matches parameter type
                            if !self.definitionally_equal(&arg_type, &param_ty, ctx)? {
                                return Err(TypeError::TypeMismatch {
                                    expected: *param_ty,
                                    actual: arg_type,
                                });
                            }
                            // Substitute argument for parameter in body type
                            self.substitute(&param_name, arg, &body_ty)
                        }
                        _ => {
                            return Err(TypeError::NotAFunction {
                                term: Term::Constructor(name.clone(), args.clone()),
                                ty: result_type,
                            })
                        }
                    };
                }

                Ok(result_type)
            }

            Term::Meta(_) => {
                // Meta-variables should be solved by unification
                Ok(self.unifier.fresh_meta())
            }

            Term::Proj(term, field) => {
                let term_type = self.infer(term, ctx)?;

                // Look for field projection function in context
                let proj_name = match &term_type {
                    Term::Const(type_name) | Term::Var(type_name) => {
                        format!("{}.{}", type_name, field)
                    }
                    _ => {
                        return Err(TypeError::InvalidProjection {
                            field: field.clone(),
                            ty: term_type,
                        })
                    }
                };

                if let Some(proj_type) = ctx.lookup_axiom(&proj_name) {
                    // Apply the projection function to the term
                    // proj_type is (x : StructType) -> FieldType
                    // We need to return FieldType after substituting term for x
                    match proj_type {
                        Term::Pi(_, domain, codomain, _) => {
                            // Verify domain matches term type
                            if self.definitionally_equal(domain, &term_type, ctx)? {
                                Ok(codomain.as_ref().clone())
                            } else {
                                Err(TypeError::TypeMismatch {
                                    expected: domain.as_ref().clone(),
                                    actual: term_type,
                                })
                            }
                        }
                        _ => Ok(proj_type.clone()),
                    }
                } else {
                    Err(TypeError::FieldNotFound {
                        field: field.clone(),
                        ty: term_type,
                    })
                }
            }

            Term::Sigma(x, domain, codomain) => {
                // Check domain is a type
                let domain_sort = self.infer(domain, ctx)?;
                let domain_universe = self.extract_universe(&domain_sort)?;

                // Check codomain is a type in extended context
                let extended_ctx = ctx.extend(x.clone(), domain.as_ref().clone());
                let codomain_sort = self.infer(codomain, &extended_ctx)?;
                let codomain_universe = self.extract_universe(&codomain_sort)?;

                // Sigma types follow similar universe rules as Pi types
                let result_universe = self.pi_rule(&domain_universe, &codomain_universe, ctx)?;
                Ok(Term::Sort(result_universe))
            }

            Term::Pair(fst, snd) => {
                let fst_type = self.infer(fst, ctx)?;
                let snd_type = self.infer(snd, ctx)?;

                // Create a fresh variable name for the dependent pair type
                let fresh_var = PAIR_VAR.to_string();
                // For now, create a non-dependent product (could be enhanced for dependency)
                Ok(Term::Sigma(
                    fresh_var,
                    Box::new(fst_type),
                    Box::new(snd_type),
                ))
            }

            Term::Fst(pair) => {
                let pair_type = self.infer(pair, ctx)?;
                match &pair_type {
                    Term::Sigma(_, fst_type, _) => Ok(fst_type.as_ref().clone()),
                    _ => Err(TypeError::TypeMismatch {
                        expected: Term::Sigma(
                            UNDERSCORE.to_string(),
                            Box::new(Term::Meta(META_A.to_string())),
                            Box::new(Term::Meta(META_B.to_string())),
                        ),
                        actual: pair_type,
                    }),
                }
            }

            Term::Snd(pair) => {
                let pair_type = self.infer(pair, ctx)?;
                match &pair_type {
                    Term::Sigma(x, _fst_type, snd_type) => {
                        // For dependent pairs Σ (x : A), B x, when projecting π₂(p),
                        // we need to substitute π₁(p) for x in B
                        let fst_projection = Term::Fst(Box::new(pair.as_ref().clone()));
                        let substituted_snd_type = self.substitute(x, &fst_projection, snd_type);
                        Ok(substituted_snd_type)
                    }
                    _ => Err(TypeError::TypeMismatch {
                        expected: Term::Sigma(
                            UNDERSCORE.to_string(),
                            Box::new(Term::Meta(META_A.to_string())),
                            Box::new(Term::Meta(META_B.to_string())),
                        ),
                        actual: pair_type,
                    }),
                }
            }

            Term::Match(scrutinee, arms) => {
                if arms.is_empty() {
                    return Err(TypeError::EmptyMatch);
                }

                let scrutinee_type = self.infer(scrutinee, ctx)?;

                // Check all arms and ensure they have the same type
                let mut result_type = None;

                for arm in arms {
                    // Create a separate TypeChecker instance for pattern checking to avoid state
                    // corruption
                    let mut pattern_checker = TypeChecker::new();
                    pattern_checker.universe_counter = self.universe_counter;

                    // Use the separate instance for pattern checking
                    let pattern_ctx =
                        pattern_checker.check_pattern(&arm.pattern, &scrutinee_type, ctx)?;

                    // Create a fresh context that combines original context with pattern bindings
                    // but doesn't inherit any corrupted constructor state
                    let fresh_ctx = self.create_fresh_branch_context(ctx, &pattern_ctx)?;

                    // Type check the arm body with fresh context
                    let arm_type = self.infer(&arm.body, &fresh_ctx)?;

                    match &result_type {
                        None => result_type = Some(arm_type),
                        Some(expected_type) => {
                            if !self.definitionally_equal(&arm_type, expected_type, ctx)? {
                                return Err(TypeError::TypeMismatch {
                                    expected: expected_type.clone(),
                                    actual: arm_type,
                                });
                            }
                        }
                    }
                }

                result_type.ok_or_else(|| TypeError::Internal {
                    message: "No arms processed in match".to_string(),
                })
            }
        }
    }

    // ===== TYPE CHECKING =====

    /// Check that a term has a given type
    pub fn check(&mut self, term: &Term, ty: &Term, ctx: &Context) -> TypeResult<()> {
        // Special handling for checking against meta-variable types
        if let Term::Meta(_) = ty {
            // When checking against a meta-variable, infer the term's type and unify
            let _inferred = self.infer(term, ctx)?;
            // For now, just check they're compatible - in a full system we'd unify
            // and update the meta-variable
            return Ok(());
        }

        match term {
            Term::Abs(x, param_ty, body) => {
                // Check if ty is a Pi-type
                if let Term::Pi(y, expected_param_ty, expected_body_ty, _impl) = ty {
                    // Handle holes in parameter type
                    let actual_param_ty = match param_ty.as_ref() {
                        Term::Meta(name) if name == UNDERSCORE => {
                            // Hole in parameter type - use the expected type
                            expected_param_ty.clone()
                        }
                        _ => param_ty.clone(),
                    };

                    // Check parameter types are definitionally equal (with conversion)
                    if !self.definitionally_equal(&actual_param_ty, expected_param_ty, ctx)? {
                        return Err(TypeError::TypeMismatch {
                            expected: expected_param_ty.as_ref().clone(),
                            actual: actual_param_ty.as_ref().clone(),
                        });
                    }

                    // Check body in extended context
                    let extended_ctx = ctx.extend(x.clone(), actual_param_ty.as_ref().clone());

                    // Substitute parameter in expected body type - this is the key for dependent
                    // types
                    let expected_body_ty = if x == y {
                        expected_body_ty.as_ref().clone()
                    } else {
                        // Rename bound variable y to x in the body type
                        self.substitute(y, &Term::Var(x.clone()), expected_body_ty)
                    };

                    self.check(body, &expected_body_ty, &extended_ctx)
                } else {
                    Err(TypeError::NotAFunction {
                        term: term.clone(),
                        ty: ty.clone(),
                    })
                }
            }

            _ => {
                // General case: infer type and check definitional equivalence
                let inferred_ty = self.infer(term, ctx)?;

                // Handle implicit parameters: if inferred type has implicit params,
                // try to instantiate them to match the expected type
                let elaborated_ty = self.elaborate_implicit_parameters(&inferred_ty, ty, ctx)?;

                // Check definitional equivalence
                if self.definitionally_equal(&elaborated_ty, ty, ctx)? {
                    Ok(())
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: ty.clone(),
                        actual: elaborated_ty,
                    })
                }
            }
        }
    }

    /// Infer type of application with implicit argument handling
    fn infer_app(&mut self, f_type: &Term, arg: &Term, ctx: &Context) -> TypeResult<Term> {
        // Use advanced solver for complex implicit argument scenarios
        let mut adv_solver = Solver::new(ctx.clone());

        // Process implicit arguments and collect meta-variables
        let mut current_type = f_type.clone();
        let mut implicit_metas = Vec::new();

        // Handle implicit parameters
        while let Term::Pi(x, param_ty, body_ty, true) = &current_type {
            // Create meta-variable for implicit argument
            let meta_id = adv_solver.fresh_meta(
                ctx.variables().iter().map(|v| v.to_string()).collect(),
                Some(param_ty.as_ref().clone()),
            );
            let meta_term = Term::Meta(format!("{}{}", META_PREFIX, meta_id.0));
            implicit_metas.push((meta_id, param_ty.as_ref().clone()));

            // Substitute meta-variable in body
            current_type = self.substitute(x, &meta_term, body_ty);
        }

        // Now handle the explicit argument
        match &current_type {
            Term::Pi(x, param_ty, body_ty, false) => {
                // Infer the type of the argument
                let arg_type = self.infer(arg, ctx)?;

                // Add unification constraint for the explicit argument
                let constraint_id = adv_solver.new_constraint_id();
                adv_solver.add_constraint(Constraint::Unify {
                    id: constraint_id,
                    left: arg_type,
                    right: param_ty.as_ref().clone(),
                    strength: ConstraintStrength::Required,
                });

                // Solve all constraints
                let substitution = adv_solver.solve()?;

                // Apply substitution to get final result
                let body = substitution.apply(body_ty);
                let result = self.substitute(x, arg, &body);
                Ok(substitution.apply(&result))
            }
            Term::Pi(x, _, _, true) => {
                // Still have implicit parameters - shouldn't happen
                Err(TypeError::UnexpectedImplicitParameter { param: x.clone() })
            }

            Term::Meta(_) => {
                // Create fresh meta-variables for function type
                let param_meta = self.unifier.fresh_meta();
                let body_meta = self.unifier.fresh_meta();
                let pi_type = Term::Pi(
                    UNDERSCORE.to_string(),
                    Box::new(param_meta.clone()),
                    Box::new(body_meta.clone()),
                    false, // explicit
                );

                // Unify function type with Pi-type
                let subst = self.unifier.unify(f_type, &pi_type)?;

                // Check argument against parameter type
                let param_ty = subst.apply_term(&param_meta);
                self.check(arg, &param_ty, ctx)?;

                // Return body type
                Ok(subst.apply_term(&body_meta))
            }

            _ => Err(TypeError::NotAFunction {
                term: Term::App(
                    Box::new(Term::Const(UNDERSCORE.to_string())),
                    Box::new(arg.clone()),
                ),
                ty: f_type.clone(),
            }),
        }
    }

    /// Check if a term represents a type (has a Sort type)
    fn check_is_type(&mut self, sort_term: &Term) -> TypeResult<Universe> {
        match sort_term {
            Term::Sort(u) => Ok(u.clone()),
            Term::Meta(_) => {
                // Meta-variables can represent types
                // We assume they'll be solved to be Type at some universe level
                Ok(Universe::Const(0))
            }
            _ => Err(TypeError::NotAType {
                term: Term::Const(UNDERSCORE.to_string()),
                ty: sort_term.clone(),
            }),
        }
    }

    /// Extract universe from Sort term
    fn extract_universe(&self, term: &Term) -> TypeResult<Universe> {
        match term {
            Term::Sort(u) => Ok(u.clone()),
            _ => Err(TypeError::UniverseError {
                universe: Universe::Const(0), // Placeholder
            }),
        }
    }

    /// Universe rules for Pi-types with polymorphism support
    fn pi_rule(
        &mut self,
        domain_universe: &Universe,
        codomain_universe: &Universe,
        ctx: &Context,
    ) -> TypeResult<Universe> {
        // Apply universe substitutions before computing the rule
        let domain_norm = self.universe_solver.substitute_universe(domain_universe);
        let codomain_norm = self.universe_solver.substitute_universe(codomain_universe);

        match (&domain_norm, &codomain_norm) {
            (Universe::Const(0), u) => Ok(u.clone()), // Prop → u = u
            (u, Universe::Const(0)) => Ok(u.clone()), // u → Prop = u
            (Universe::ScopedVar(_, _), Universe::ScopedVar(_, _)) => {
                // For polymorphic universes, create a fresh variable and add constraints
                let fresh_var = ctx.fresh_universe_var(UNIVERSE_VAR_PREFIX);
                let _result_universe = Universe::ScopedVar("global".to_string(), fresh_var);

                // Add constraints: result_universe >= domain_universe and result_universe >=
                // codomain_universe For simplicity, we use max(domain,
                // codomain)
                Ok(Universe::Max(
                    Box::new(domain_norm),
                    Box::new(codomain_norm),
                ))
            }
            (u, v) => Ok(Universe::Max(Box::new(u.clone()), Box::new(v.clone()))),
        }
    }

    /// Build Pi-type from parameters
    fn build_pi_type(&self, params: &[Parameter], result_ty: &Term) -> Term {
        params.iter().rev().fold(result_ty.clone(), |acc, param| {
            Term::Pi(
                param.name.clone(),
                Box::new(param.ty.clone()),
                Box::new(acc),
                param.implicit,
            )
        })
    }

    /// Build lambda from parameters
    fn build_lambda(&self, params: &[Parameter], body: &Term) -> Term {
        params.iter().rev().fold(body.clone(), |acc, param| {
            Term::Abs(
                param.name.clone(),
                Box::new(param.ty.clone()),
                Box::new(acc),
            )
        })
    }

    /// Definitional equality - crucial for dependent types
    /// This includes beta reduction and eta conversion
    pub fn definitionally_equal(
        &mut self,
        t1: &Term,
        t2: &Term,
        ctx: &Context,
    ) -> TypeResult<bool> {
        let t1_norm = self.normalize(t1, ctx);
        let t2_norm = self.normalize(t2, ctx);

        // If either term contains universe metavariables, we need to solve constraints
        if contains_universe_metavariables(&t1_norm) || contains_universe_metavariables(&t2_norm) {
            return self.unify_with_universe_constraints(&t1_norm, &t2_norm, ctx);
        }

        Ok(alpha_equivalent(&t1_norm, &t2_norm))
    }

    /// Unify two terms that contain universe metavariables
    fn unify_with_universe_constraints(
        &mut self,
        t1: &Term,
        t2: &Term,
        ctx: &Context,
    ) -> TypeResult<bool> {
        // Create a constraint solver
        let mut solver = Solver::new(ctx.clone());

        // Generate constraints for universe unification
        self.generate_universe_constraints(t1, t2, &mut solver)?;

        // Solve the constraints
        match solver.solve() {
            Ok(substitution) => {
                // Apply substitution to both terms
                let t1_subst = substitution.apply(t1);
                let t2_subst = substitution.apply(t2);

                // Normalize both terms after substitution
                let t1_norm = self.normalize(&t1_subst, ctx);
                let t2_norm = self.normalize(&t2_subst, ctx);

                // Check alpha equivalence of the normalized terms
                Ok(alpha_equivalent(&t1_norm, &t2_norm))
            }
            Err(_) => Ok(false), // Unification failed
        }
    }

    /// Generate universe constraints between two terms
    fn generate_universe_constraints(
        &mut self,
        t1: &Term,
        t2: &Term,
        solver: &mut Solver,
    ) -> TypeResult<()> {
        match (t1, t2) {
            (Term::Sort(u1), Term::Sort(u2)) => {
                let constraint_id = solver.new_constraint_id();
                solver.add_constraint(Constraint::UnifyUniverse {
                    id: constraint_id,
                    left: u1.clone(),
                    right: u2.clone(),
                    strength: ConstraintStrength::Required,
                });
                Ok(())
            }
            (Term::App(f1, a1), Term::App(f2, a2)) => {
                self.generate_universe_constraints(f1, f2, solver)?;
                self.generate_universe_constraints(a1, a2, solver)
            }
            (Term::Pi(_, t1, b1, _), Term::Pi(_, t2, b2, _)) => {
                self.generate_universe_constraints(t1, t2, solver)?;
                self.generate_universe_constraints(b1, b2, solver)
            }
            (Term::Abs(_, t1, b1), Term::Abs(_, t2, b2)) => {
                self.generate_universe_constraints(t1, t2, solver)?;
                self.generate_universe_constraints(b1, b2, solver)
            }
            _ => Ok(()), // Other cases don't generate universe constraints
        }
    }

    /// Normalize a term by reducing redexes (beta reduction, eta-conversion,
    /// let expansion)
    pub fn normalize(&self, term: &Term, ctx: &Context) -> Term {
        match term {
            Term::App(f, arg) => {
                let f_norm = self.normalize(f, ctx);
                let arg_norm = self.normalize(arg, ctx);

                // Beta reduction: (λx.t) s ~> t[s/x]
                if let Term::Abs(x, _ty, body) = &f_norm {
                    self.substitute(x, &arg_norm, body)
                } else {
                    Term::App(Box::new(f_norm), Box::new(arg_norm))
                }
            }

            Term::Abs(x, ty, body) => {
                let ty_norm = self.normalize(ty, ctx);
                let extended_ctx = ctx.extend(x.clone(), ty_norm.clone());
                let body_norm = self.normalize(body, &extended_ctx);

                // Eta-conversion: λx. f x ≡ f when x is not free in f
                if let Term::App(f, arg) = &body_norm {
                    if let Term::Var(arg_var) = arg.as_ref() {
                        if arg_var == x && !Term::occurs_free(x, f) {
                            // Apply eta-conversion: λx. f x ~> f
                            return f.as_ref().clone();
                        }
                    }
                }

                Term::Abs(x.clone(), Box::new(ty_norm), Box::new(body_norm))
            }

            Term::Pi(x, ty, body, implicit) => {
                let ty_norm = self.normalize(ty, ctx);
                let extended_ctx = ctx.extend(x.clone(), ty_norm.clone());
                let body_norm = self.normalize(body, &extended_ctx);
                Term::Pi(x.clone(), Box::new(ty_norm), Box::new(body_norm), *implicit)
            }

            Term::Let(x, _ty, val, body) => {
                // Let reduction: let x : T := v in b ~> b[v/x]
                let val_norm = self.normalize(val, ctx);
                self.substitute(x, &val_norm, body)
            }

            Term::Var(x) => {
                // Look up definitions and unfold them
                if let Some(def) = ctx.lookup_axiom(x) {
                    def.clone() // Could be further normalized if it's a
                                // definition
                } else {
                    term.clone()
                }
            }

            _ => term.clone(), // Other terms are already in normal form
        }
    }

    /// Enhanced substitution that handles capture avoidance properly
    fn substitute(&self, var: &str, replacement: &Term, target: &Term) -> Term {
        self.substitute_with_context(var, replacement, target, &mut Vec::new())
    }

    fn substitute_with_context(
        &self,
        var: &str,
        replacement: &Term,
        target: &Term,
        bound: &mut Vec<String>,
    ) -> Term {
        match target {
            Term::Var(x) if x == var && !bound.contains(x) => replacement.clone(),
            Term::Var(_) => target.clone(),

            Term::App(f, arg) => Term::App(
                Box::new(self.substitute_with_context(var, replacement, f, bound)),
                Box::new(self.substitute_with_context(var, replacement, arg, bound)),
            ),

            Term::Abs(x, ty, body) => {
                if x == var {
                    // Variable is shadowed
                    Term::Abs(
                        x.clone(),
                        Box::new(self.substitute_with_context(var, replacement, ty, bound)),
                        Box::new(body.as_ref().clone()),
                    )
                } else if self.occurs_free(x, replacement) {
                    // Avoid capture by alpha-renaming
                    let fresh = fresh_var(x, bound);
                    let renamed_body = rename_var(x, &fresh, body);
                    bound.push(fresh.clone());
                    let result = Term::Abs(
                        fresh.clone(),
                        Box::new(self.substitute_with_context(var, replacement, ty, bound)),
                        Box::new(self.substitute_with_context(
                            var,
                            replacement,
                            &renamed_body,
                            bound,
                        )),
                    );
                    bound.pop();
                    result
                } else {
                    bound.push(x.clone());
                    let result = Term::Abs(
                        x.clone(),
                        Box::new(self.substitute_with_context(var, replacement, ty, bound)),
                        Box::new(self.substitute_with_context(var, replacement, body, bound)),
                    );
                    bound.pop();
                    result
                }
            }

            Term::Pi(x, ty, body, implicit) => {
                // Similar logic to Abs
                if x == var {
                    Term::Pi(
                        x.clone(),
                        Box::new(self.substitute_with_context(var, replacement, ty, bound)),
                        Box::new(body.as_ref().clone()),
                        *implicit,
                    )
                } else if self.occurs_free(x, replacement) {
                    let fresh = fresh_var(x, bound);
                    let renamed_body = rename_var(x, &fresh, body);
                    bound.push(fresh.clone());
                    let result = Term::Pi(
                        fresh.clone(),
                        Box::new(self.substitute_with_context(var, replacement, ty, bound)),
                        Box::new(self.substitute_with_context(
                            var,
                            replacement,
                            &renamed_body,
                            bound,
                        )),
                        *implicit,
                    );
                    bound.pop();
                    result
                } else {
                    bound.push(x.clone());
                    let result = Term::Pi(
                        x.clone(),
                        Box::new(self.substitute_with_context(var, replacement, ty, bound)),
                        Box::new(self.substitute_with_context(var, replacement, body, bound)),
                        *implicit,
                    );
                    bound.pop();
                    result
                }
            }

            Term::Let(x, ty, val, body) => {
                let ty_subst = self.substitute_with_context(var, replacement, ty, bound);
                let val_subst = self.substitute_with_context(var, replacement, val, bound);

                if x == var {
                    Term::Let(
                        x.clone(),
                        Box::new(ty_subst),
                        Box::new(val_subst),
                        Box::new(body.as_ref().clone()),
                    )
                } else {
                    bound.push(x.clone());
                    let body_subst = self.substitute_with_context(var, replacement, body, bound);
                    bound.pop();
                    Term::Let(
                        x.clone(),
                        Box::new(ty_subst),
                        Box::new(val_subst),
                        Box::new(body_subst),
                    )
                }
            }

            Term::Sigma(x, domain, codomain) => {
                let domain_subst = self.substitute_with_context(var, replacement, domain, bound);

                if x == var {
                    // Variable is shadowed, don't substitute in codomain
                    Term::Sigma(
                        x.clone(),
                        Box::new(domain_subst),
                        Box::new(codomain.as_ref().clone()),
                    )
                } else {
                    bound.push(x.clone());
                    let codomain_subst =
                        self.substitute_with_context(var, replacement, codomain, bound);
                    bound.pop();
                    Term::Sigma(x.clone(), Box::new(domain_subst), Box::new(codomain_subst))
                }
            }

            Term::Pair(fst, snd) => Term::Pair(
                Box::new(self.substitute_with_context(var, replacement, fst, bound)),
                Box::new(self.substitute_with_context(var, replacement, snd, bound)),
            ),

            Term::Fst(pair) => Term::Fst(Box::new(self.substitute_with_context(
                var,
                replacement,
                pair,
                bound,
            ))),

            Term::Snd(pair) => Term::Snd(Box::new(self.substitute_with_context(
                var,
                replacement,
                pair,
                bound,
            ))),

            Term::Proj(term, field) => Term::Proj(
                Box::new(self.substitute_with_context(var, replacement, term, bound)),
                field.clone(),
            ),

            Term::Match(scrutinee, arms) => {
                let scrutinee_subst =
                    self.substitute_with_context(var, replacement, scrutinee, bound);
                let arms_subst = arms
                    .iter()
                    .map(|arm| {
                        // For each arm, we need to be careful about pattern variable bindings
                        let body_subst = if pattern_binds_var(&arm.pattern, var) {
                            // Variable is bound by pattern, don't substitute in body
                            arm.body.clone()
                        } else {
                            self.substitute_with_context(var, replacement, &arm.body, bound)
                        };

                        MatchArm {
                            pattern: arm.pattern.clone(), /* Patterns don't contain substitutable
                                                           * terms */
                            body: body_subst,
                        }
                    })
                    .collect();

                Term::Match(Box::new(scrutinee_subst), arms_subst)
            }

            _ => target.clone(), // Other cases unchanged
        }
    }

    // ===== PATTERN MATCHING =====

    /// Check a pattern against a type and return extended context with pattern
    /// variables
    fn check_pattern(
        &mut self,
        pattern: &Pattern,
        expected_type: &Term,
        ctx: &Context,
    ) -> TypeResult<Context> {
        match pattern {
            Pattern::Var(x) => {
                // Check if this is actually a constructor with no arguments
                if ctx.lookup_constructor(x).is_some() {
                    // It's actually a constructor pattern with no arguments
                    return self.check_pattern(
                        &Pattern::Constructor(x.clone(), vec![]),
                        expected_type,
                        ctx,
                    );
                }
                // Variable pattern binds the scrutinee to the variable
                Ok(ctx.extend(x.clone(), expected_type.clone()))
            }

            Pattern::Wildcard => {
                // Wildcard matches anything, adds no bindings
                Ok(ctx.clone())
            }

            Pattern::Constructor(ctor_name, ctor_args) => {
                // Look up constructor type - check both constructors and definitions
                let (ctor_type, is_instantiated) =
                    if let Some(ty) = ctx.lookup_constructor(ctor_name) {
                        (ty.clone(), false)
                    } else if let Some(def) = ctx.lookup_definition(ctor_name) {
                        // Instantiate universe parameters for universe-polymorphic constructors
                        let instantiated = self.instantiate_universe_params(
                            &def.ty,
                            &def.universe_params,
                            &def.name,
                            ctx,
                        )?;
                        (instantiated, true)
                    } else {
                        return Err(TypeError::UnknownConstructor {
                            name: ctor_name.clone(),
                        });
                    };

                if is_instantiated {
                    // For instantiated constructors, use constraint-based approach
                    // The constructor type already has metavariables that can unify correctly
                    let return_type = extract_constructor_return_type(&ctor_type);

                    // Use constraint solving to check the return type matches expected type
                    let mut solver = Solver::new(ctx.clone());
                    let constraint_id = solver.new_constraint_id();
                    let constraint = crate::solver::Constraint::Unify {
                        id: constraint_id,
                        left: return_type.clone(),
                        right: expected_type.clone(),
                        strength: ConstraintStrength::Required,
                    };
                    solver.add_constraint(constraint);
                    let _subst = solver.solve()?; // This will error if unification fails

                    // Extract argument types directly from instantiated constructor type
                    let arg_types = extract_constructor_arg_types(&ctor_type);

                    // Check sub-patterns against argument types
                    if arg_types.len() != ctor_args.len() {
                        return Err(TypeError::ConstructorArityMismatch {
                            name: ctor_name.clone(),
                            expected: arg_types.len(),
                            actual: ctor_args.len(),
                        });
                    }

                    let mut extended_ctx = ctx.clone();
                    for (arg_pattern, arg_type) in ctor_args.iter().zip(arg_types.iter()) {
                        extended_ctx = self.check_pattern(arg_pattern, arg_type, &extended_ctx)?;
                    }
                    Ok(extended_ctx)
                } else {
                    // For regular constructors, use the existing specialization approach
                    let specialized_ctor_type =
                        self.specialize_constructor_type(&ctor_type, expected_type, ctx)?;
                    let return_type = extract_constructor_return_type(&specialized_ctor_type);

                    if !self.definitionally_equal(&return_type, expected_type, ctx)? {
                        return Err(TypeError::TypeMismatch {
                            expected: expected_type.clone(),
                            actual: return_type,
                        });
                    }

                    // Extract argument types from specialized constructor type and check
                    // sub-patterns
                    let arg_types = extract_constructor_arg_types(&specialized_ctor_type);

                    if ctor_args.len() != arg_types.len() {
                        return Err(TypeError::ConstructorArityMismatch {
                            name: ctor_name.clone(),
                            expected: arg_types.len(),
                            actual: ctor_args.len(),
                        });
                    }

                    let mut extended_ctx = ctx.clone();
                    for (arg_pattern, arg_type) in ctor_args.iter().zip(arg_types.iter()) {
                        extended_ctx = self.check_pattern(arg_pattern, arg_type, &extended_ctx)?;
                    }

                    Ok(extended_ctx)
                }
            }
        }
    }

    /// Specialize a constructor type by instantiating type parameters to match
    /// expected type
    fn specialize_constructor_type(
        &mut self,
        ctor_type: &Term,
        expected_type: &Term,
        _ctx: &Context,
    ) -> TypeResult<Term> {
        // For a constructor like nil : (A : Type) → List A
        // and expected type List B, we need to instantiate A with B

        let mut current_type = ctor_type.clone();
        let mut substitutions = Vec::new();

        // Collect all type parameters (Pi types where domain is Type)
        while let Term::Pi(param_name, param_ty, body, _implicit) = &current_type {
            if matches!(param_ty.as_ref(), Term::Sort(_)) {
                // This is a type parameter
                // We'll need to figure out what to substitute for it
                substitutions.push(param_name.clone());
                current_type = body.as_ref().clone();
            } else {
                break;
            }
        }

        // Now current_type should be the return type with free variables
        if substitutions.is_empty() {
            // No type parameters, just return the original type
            return Ok(ctor_type.clone());
        }

        // Handle type parameter substitution
        if !substitutions.is_empty() {
            // Extract type arguments from expected_type
            let mut type_args = Vec::new();
            let mut current_expected = expected_type;

            // For types like Pair A B, we need to extract both A and B
            while let Term::App(f, arg) = current_expected {
                type_args.push(arg.as_ref().clone());
                current_expected = f;
            }
            type_args.reverse();

            // Apply substitutions
            let mut specialized = ctor_type.clone();
            for (param, arg) in substitutions.iter().zip(type_args.iter()) {
                // Skip the Pi binding for this parameter and substitute in the body
                if let Term::Pi(p, _, body, _) = &specialized {
                    if p == param {
                        specialized = self.substitute(param, arg, body);
                    }
                }
            }

            return Ok(specialized);
        }

        // For more complex cases, would need proper unification
        Ok(ctor_type.clone())
    }

    /// Check if variable occurs free in term
    fn occurs_free(&self, var: &str, term: &Term) -> bool {
        Term::occurs_free(var, term)
    }

    /// Infer type of application with constraint solving for implicit arguments
    fn infer_app_with_constraints(
        &mut self,
        f_type: &Term,
        arg: &Term,
        ctx: &Context,
    ) -> TypeResult<Term> {
        // Just use the unified infer_app which now handles constraints properly
        self.infer_app(f_type, arg, ctx)
    }

    /// Expand references to an inductive type to include its parameters
    /// E.g., List -> List A when List has parameter A
    // ===== UNIVERSE POLYMORPHISM =====
    /// Instantiate universe parameters with fresh metavariables
    /// This is the core of the universe polymorphism system - every
    /// instantiation gets completely fresh metavariables to prevent any
    /// name collisions
    fn instantiate_universe_params(
        &mut self,
        ty: &Term,
        universe_params: &[String],
        _definition_name: &str,
        _ctx: &Context,
    ) -> TypeResult<Term> {
        if universe_params.is_empty() {
            return Ok(ty.clone());
        }

        // Create completely fresh universe metavariables for each parameter
        // This ensures that each instantiation is completely independent
        let mut substitution = HashMap::new();
        for param in universe_params {
            let fresh_meta_id = self.universe_counter;
            self.universe_counter += 1;
            substitution.insert(param.clone(), Universe::Meta(fresh_meta_id));
        }

        // Apply the substitution completely to the entire type
        Ok(substitute_universes(ty, &substitution))
    }

    // ===== IMPLICIT ARGUMENT ELABORATION =====

    /// Elaborate implicit parameters by trying to instantiate them to match
    /// expected type This is a simple version that handles common cases
    /// without full constraint solving
    fn elaborate_implicit_parameters(
        &mut self,
        inferred_ty: &Term,
        expected_ty: &Term,
        ctx: &Context,
    ) -> TypeResult<Term> {
        // If inferred type has implicit parameters, try to instantiate them
        match inferred_ty {
            Term::Pi(param_name, param_ty, body_ty, true) => {
                // This is an implicit parameter - try to infer what it should be
                let implicit_arg = self.infer_implicit_argument(param_ty, expected_ty, ctx)?;

                // Substitute the implicit argument and continue elaboration
                let instantiated_ty = self.substitute(param_name, &implicit_arg, body_ty);
                self.elaborate_implicit_parameters(&instantiated_ty, expected_ty, ctx)
            }
            _ => {
                // No more implicit parameters, return the type as-is
                Ok(inferred_ty.clone())
            }
        }
    }

    /// Infer what an implicit argument should be based on context and expected
    /// type
    fn infer_implicit_argument(
        &mut self,
        param_ty: &Term,
        expected_ty: &Term,
        _ctx: &Context,
    ) -> TypeResult<Term> {
        // Simple heuristic: if param_ty is Type, try to infer from expected_ty
        match param_ty {
            Term::Sort(_) => {
                // Parameter is a type, try to extract it from expected type pattern
                match expected_ty {
                    Term::Pi(_, arg_ty, _, _) => {
                        // Expected type is a function, use its argument type
                        Ok(*arg_ty.clone())
                    }
                    _ => {
                        // For now, default to the expected type itself if it's simple
                        if is_simple_type(expected_ty) {
                            Ok(expected_ty.clone())
                        } else {
                            // Default to Nat as a fallback (this is a simple heuristic)
                            Ok(Term::Const(NAT_TYPE.to_string()))
                        }
                    }
                }
            }
            _ => {
                // For non-type parameters, this is more complex
                // For now, return a dummy value
                Ok(Term::Const(DUMMY_TYPE.to_string()))
            }
        }
    }

    // ===== UTILITY FUNCTIONS =====

    /// Create a fresh context for branch body that combines original context
    /// with pattern bindings but doesn't inherit any corrupted constructor
    /// state from pattern matching
    fn create_fresh_branch_context(
        &self,
        original_ctx: &Context,
        pattern_ctx: &Context,
    ) -> TypeResult<Context> {
        // Start with original context (which has clean constructor definitions)
        let mut fresh_ctx = original_ctx.clone();

        // Add only the variable bindings from pattern context, not constructor state
        for (var, ty) in pattern_ctx.get_all_bindings() {
            if !original_ctx.has_binding(var) {
                fresh_ctx = fresh_ctx.extend(var.clone(), ty.clone());
            }
        }

        Ok(fresh_ctx)
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
