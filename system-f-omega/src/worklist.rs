use std::collections::HashMap;

use crate::core::{CoreBinOp, CorePattern, CoreTerm, CoreType};
use crate::errors::{TypeError, TypeResult};

/// DK Worklist Algorithm for System-F-ω
/// Based on "A Mechanical Formalization of Higher-Ranked Polymorphic Type
/// Inference"
pub type TyVar = String;
pub type TmVar = String;

#[derive(Debug, Clone, PartialEq)]
pub enum WorklistEntry {
    /// Type variable binding: α
    TVar(TyVar, TyVarKind),
    /// Term variable binding: x : T
    Var(TmVar, CoreType),
    /// Judgment: Sub A B | Inf e ⊢ A | Chk e ⇐ A
    Judgment(Judgment),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyVarKind {
    /// Universal type variable: α
    Universal,
    /// Existential type variable: ^α
    Existential,
    /// Solved existential: ^α = τ
    Solved(CoreType),
    /// Marker: ►α (for scoping)
    Marker,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Judgment {
    /// Subtyping: A <: B
    Sub { left: CoreType, right: CoreType },
    /// Type inference: e ⊢ A
    Inf { term: CoreTerm, ty: CoreType },
    /// Type checking: e ⇐ A
    Chk { term: CoreTerm, ty: CoreType },
    /// Application inference helper: A • e ⊢ C
    InfApp {
        func_ty: CoreType,
        arg: CoreTerm,
        result_ty: CoreType,
    },
}

#[derive(Debug, Clone)]
pub struct Worklist {
    entries: Vec<WorklistEntry>,
    next_var: usize,
}

impl Default for Worklist {
    fn default() -> Self {
        Self::new()
    }
}

impl Worklist {
    pub fn new() -> Self {
        Worklist {
            entries: Vec::new(),
            next_var: 0,
        }
    }

    pub fn fresh_var(&mut self) -> TyVar {
        let var = format!("α{}", self.next_var);
        self.next_var += 1;
        var
    }

    pub fn fresh_evar(&mut self) -> TyVar {
        let var = format!("^α{}", self.next_var);
        self.next_var += 1;
        var
    }

    pub fn push(&mut self, entry: WorklistEntry) {
        self.entries.push(entry);
    }

    pub fn pop(&mut self) -> Option<WorklistEntry> {
        self.entries.pop()
    }

    pub fn find_var(&self, name: &str) -> Option<&CoreType> {
        for entry in self.entries.iter().rev() {
            if let WorklistEntry::Var(var_name, ty) = entry {
                if var_name == name {
                    return Some(ty);
                }
            }
        }
        None
    }

    pub fn solve_evar(&mut self, name: &str, ty: CoreType) -> TypeResult<()> {
        for entry in self.entries.iter_mut() {
            if let WorklistEntry::TVar(var_name, kind) = entry {
                if var_name == name {
                    match kind {
                        TyVarKind::Existential => {
                            *kind = TyVarKind::Solved(ty);
                            return Ok(());
                        }
                        TyVarKind::Solved(_) => {
                            // Variable already solved, that's OK
                            return Ok(());
                        }
                        _ => {
                            // Skip universal variables, markers, etc.
                            continue;
                        }
                    }
                }
            }
        }
        Err(TypeError::UnboundVariable {
            name: name.to_string(),
            span: None,
        })
    }

    pub fn before(&self, a: &str, b: &str) -> bool {
        let mut pos_a = None;
        let mut pos_b = None;

        for (i, entry) in self.entries.iter().enumerate() {
            if let WorklistEntry::TVar(name, _) = entry {
                if name == a {
                    pos_a = Some(i);
                }
                if name == b {
                    pos_b = Some(i);
                }
            }
        }

        match (pos_a, pos_b) {
            (Some(pa), Some(pb)) => pa < pb,
            _ => false,
        }
    }
}

pub struct DKInference {
    worklist: Worklist,
    trace: Vec<String>,
    data_constructors: HashMap<String, CoreType>,
    /// Variable typing context for pattern-bound variables
    var_context: HashMap<String, CoreType>,
}

impl DKInference {
    pub fn with_context(
        data_constructors: HashMap<String, CoreType>,
        var_context: HashMap<String, CoreType>,
    ) -> Self {
        DKInference {
            worklist: Worklist::new(),
            trace: Vec::new(),
            data_constructors,
            var_context,
        }
    }

    pub fn check_type(&mut self, term: &CoreTerm, expected_ty: &CoreType) -> TypeResult<()> {
        self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
            term: term.clone(),
            ty: expected_ty.clone(),
        }));

        self.solve()
    }

    fn solve(&mut self) -> TypeResult<()> {
        while let Some(entry) = self.worklist.pop() {
            match entry {
                WorklistEntry::TVar(_, _) => {
                    // Skip variable bindings during processing
                    continue;
                }
                WorklistEntry::Var(_, _) => {
                    // Skip term variable bindings during processing
                    continue;
                }
                WorklistEntry::Judgment(judgment) => {
                    self.solve_judgment(judgment)?;
                }
            }
        }
        Ok(())
    }

    fn solve_judgment(&mut self, judgment: Judgment) -> TypeResult<()> {
        match judgment {
            Judgment::Sub { left, right } => self.solve_subtype(left, right),
            Judgment::Inf { term, ty } => self.solve_inference(term, ty),
            Judgment::Chk { term, ty } => self.solve_checking(term, ty),
            Judgment::InfApp {
                func_ty,
                arg,
                result_ty,
            } => self.solve_inf_app(func_ty, arg, result_ty),
        }
    }

    fn solve_subtype(&mut self, left: CoreType, right: CoreType) -> TypeResult<()> {
        self.trace.push(format!("Sub {} <: {}", left, right));

        if left == right {
            return Ok(());
        }

        match (&left, &right) {
            // Reflexivity
            (CoreType::Con(a), CoreType::Con(b)) if a == b => Ok(()),
            (CoreType::Var(a), CoreType::Var(b)) if a == b => Ok(()),
            (CoreType::ETVar(a), CoreType::ETVar(b)) if a == b => Ok(()),

            // Function subtyping (contravariant in argument, covariant in result)
            (CoreType::Arrow(a1, a2), CoreType::Arrow(b1, b2)) => {
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *b1.clone(),
                    right: *a1.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *a2.clone(),
                    right: *b2.clone(),
                }));
                Ok(())
            }

            // Application subtyping (covariant in both components)
            (CoreType::App(a1, a2), CoreType::App(b1, b2)) => {
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *a1.clone(),
                    right: *b1.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *a2.clone(),
                    right: *b2.clone(),
                }));
                Ok(())
            }

            // Forall right
            (_, CoreType::Forall(var, ty)) => {
                let fresh_var = self.worklist.fresh_var();
                self.worklist
                    .push(WorklistEntry::TVar(fresh_var.clone(), TyVarKind::Universal));
                let substituted_ty = self.substitute_type(var, &CoreType::Var(fresh_var), ty);
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left,
                    right: substituted_ty,
                }));
                Ok(())
            }

            // Forall left
            (CoreType::Forall(var, ty), _) => {
                let fresh_evar = self.worklist.fresh_evar();
                self.worklist
                    .push(WorklistEntry::TVar(fresh_evar.clone(), TyVarKind::Marker));
                self.worklist.push(WorklistEntry::TVar(
                    fresh_evar.clone(),
                    TyVarKind::Existential,
                ));
                let substituted_ty = self.substitute_type(var, &CoreType::ETVar(fresh_evar), ty);
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: substituted_ty,
                    right,
                }));
                Ok(())
            }

            // Existential variable instantiation
            (CoreType::ETVar(a), _) if !self.occurs_check(a, &right) => {
                self.instantiate_left(a, &right)
            }
            (_, CoreType::ETVar(a)) if !self.occurs_check(a, &left) => {
                self.instantiate_right(&left, a)
            }

            _ => Err(TypeError::SubtypingError {
                left,
                right,
                span: None,
            }),
        }
    }

    fn solve_inference(&mut self, term: CoreTerm, ty: CoreType) -> TypeResult<()> {
        self.trace
            .push(format!("Inf {} ⊢ {}", self.term_to_string(&term), ty));

        match term {
            CoreTerm::Var(name) => {
                // Check pattern variable context first
                if let Some(var_ty) = self.var_context.get(&name).cloned() {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: var_ty,
                        right: ty,
                    }));
                    Ok(())
                } else if let Some(var_ty) = self.worklist.find_var(&name).cloned() {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: var_ty,
                        right: ty,
                    }));
                    Ok(())
                } else if let Some(var_ty) = self.data_constructors.get(&name).cloned() {
                    // Check data constructors for constructor variables
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: var_ty,
                        right: ty,
                    }));
                    Ok(())
                } else {
                    Err(TypeError::UnboundVariable {
                        name: name.to_string(),
                        span: None,
                    })
                }
            }

            CoreTerm::LitInt(_) => {
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: CoreType::Con("Int".to_string()),
                    right: ty,
                }));
                Ok(())
            }

            CoreTerm::Lambda {
                param,
                param_ty,
                body,
            } => {
                let result_ty = CoreType::ETVar(self.worklist.fresh_evar());
                if let CoreType::ETVar(var_name) = &result_ty {
                    self.worklist.push(WorklistEntry::TVar(
                        var_name.clone(),
                        TyVarKind::Existential,
                    ));
                }

                let arrow_ty =
                    CoreType::Arrow(Box::new(param_ty.clone()), Box::new(result_ty.clone()));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: arrow_ty,
                    right: ty,
                }));

                self.worklist.push(WorklistEntry::Var(param, param_ty));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Inf {
                    term: *body,
                    ty: result_ty,
                }));

                Ok(())
            }

            CoreTerm::App { func, arg } => {
                let func_ty = CoreType::ETVar(self.worklist.fresh_evar());
                if let CoreType::ETVar(var_name) = &func_ty {
                    self.worklist.push(WorklistEntry::TVar(
                        var_name.clone(),
                        TyVarKind::Existential,
                    ));
                }

                self.worklist
                    .push(WorklistEntry::Judgment(Judgment::InfApp {
                        func_ty: func_ty.clone(),
                        arg: *arg,
                        result_ty: ty,
                    }));

                self.worklist.push(WorklistEntry::Judgment(Judgment::Inf {
                    term: *func,
                    ty: func_ty,
                }));

                Ok(())
            }

            CoreTerm::TypeLambda { param, body } => {
                let body_ty = CoreType::ETVar(self.worklist.fresh_evar());
                if let CoreType::ETVar(var_name) = &body_ty {
                    self.worklist.push(WorklistEntry::TVar(
                        var_name.clone(),
                        TyVarKind::Existential,
                    ));
                }

                let forall_ty = CoreType::Forall(param.clone(), Box::new(body_ty.clone()));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: forall_ty,
                    right: ty,
                }));

                self.worklist
                    .push(WorklistEntry::TVar(param, TyVarKind::Universal));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Inf {
                    term: *body,
                    ty: body_ty,
                }));

                Ok(())
            }

            CoreTerm::Constructor { name, args: _ } => {
                if let Some(constructor_ty) = self.data_constructors.get(&name) {
                    // Instantiate the constructor type with fresh existential variables
                    let instantiated_ty = self.instantiate_constructor_type(constructor_ty.clone());

                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: instantiated_ty,
                        right: ty,
                    }));
                    Ok(())
                } else {
                    Err(TypeError::UnboundDataConstructor {
                        name: name.clone(),
                        span: None,
                    })
                }
            }

            CoreTerm::BinOp { op, left, right } => {
                let (left_ty, right_ty, result_ty) = self.infer_binop_types(&op);

                // Add any existential variables to the worklist
                self.add_etvars_to_worklist(&left_ty);
                self.add_etvars_to_worklist(&right_ty);
                self.add_etvars_to_worklist(&result_ty);

                // Check that the operands have the expected types
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term: *left,
                    ty: left_ty,
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term: *right,
                    ty: right_ty,
                }));

                // Check that the result type matches the expected type
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: result_ty,
                    right: ty,
                }));

                Ok(())
            }

            CoreTerm::If {
                cond,
                then_branch,
                else_branch,
            } => {
                // The condition must be Bool
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term: *cond,
                    ty: CoreType::Con("Bool".to_string()),
                }));

                // Both branches must have the same type as the expected result
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term: *then_branch,
                    ty: ty.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term: *else_branch,
                    ty,
                }));

                Ok(())
            }

            CoreTerm::Case { scrutinee, arms } => {
                // Create a fresh type variable for the scrutinee
                let scrutinee_ty = CoreType::ETVar(self.worklist.fresh_evar());
                self.add_etvars_to_worklist(&scrutinee_ty);

                // Check that the scrutinee has the inferred type
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term: *scrutinee,
                    ty: scrutinee_ty.clone(),
                }));

                // Process each pattern arm
                for arm in arms {
                    // Check pattern constraints and get pattern variable bindings
                    let pattern_bindings = self.check_pattern(&arm.pattern, &scrutinee_ty)?;

                    // Add pattern variable bindings to worklist as regular variables
                    for (var_name, var_type) in pattern_bindings {
                        self.worklist.push(WorklistEntry::Var(var_name, var_type));
                    }

                    // Check the body with pattern variables in the worklist
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                        term: arm.body.clone(),
                        ty: ty.clone(),
                    }));
                }

                Ok(())
            }
        }
    }

    fn solve_checking(&mut self, term: CoreTerm, ty: CoreType) -> TypeResult<()> {
        self.trace
            .push(format!("Chk {} ⇐ {}", self.term_to_string(&term), ty));

        match (&term, &ty) {
            (
                CoreTerm::Lambda {
                    param,
                    param_ty,
                    body,
                },
                CoreType::Arrow(expected_param, result_ty),
            ) => {
                // Check that parameter types match
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: param_ty.clone(),
                    right: *expected_param.clone(),
                }));

                self.worklist
                    .push(WorklistEntry::Var(param.clone(), param_ty.clone()));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term: *body.clone(),
                    ty: *result_ty.clone(),
                }));

                Ok(())
            }

            (_, CoreType::Forall(var, body_ty)) => {
                let fresh_var = self.worklist.fresh_var();
                self.worklist
                    .push(WorklistEntry::TVar(fresh_var.clone(), TyVarKind::Universal));
                let substituted_ty = self.substitute_type(var, &CoreType::Var(fresh_var), body_ty);
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term,
                    ty: substituted_ty,
                }));
                Ok(())
            }

            _ => {
                // Fallback: infer type and check subtyping
                let inferred_ty = CoreType::ETVar(self.worklist.fresh_evar());
                if let CoreType::ETVar(var_name) = &inferred_ty {
                    self.worklist.push(WorklistEntry::TVar(
                        var_name.clone(),
                        TyVarKind::Existential,
                    ));
                }

                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: inferred_ty.clone(),
                    right: ty,
                }));

                self.worklist.push(WorklistEntry::Judgment(Judgment::Inf {
                    term,
                    ty: inferred_ty,
                }));

                Ok(())
            }
        }
    }

    fn solve_inf_app(
        &mut self,
        func_ty: CoreType,
        arg: CoreTerm,
        result_ty: CoreType,
    ) -> TypeResult<()> {
        match func_ty {
            CoreType::Arrow(param_ty, ret_ty) => {
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *ret_ty,
                    right: result_ty,
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term: arg,
                    ty: *param_ty,
                }));
                Ok(())
            }

            CoreType::Forall(var, body) => {
                let fresh_evar = self.worklist.fresh_evar();
                self.worklist.push(WorklistEntry::TVar(
                    fresh_evar.clone(),
                    TyVarKind::Existential,
                ));
                let substituted = self.substitute_type(&var, &CoreType::ETVar(fresh_evar), &body);
                self.worklist
                    .push(WorklistEntry::Judgment(Judgment::InfApp {
                        func_ty: substituted,
                        arg,
                        result_ty,
                    }));
                Ok(())
            }

            CoreType::ETVar(a) => {
                let param_ty_name = self.worklist.fresh_evar();
                let ret_ty_name = self.worklist.fresh_evar();
                let param_ty = CoreType::ETVar(param_ty_name.clone());
                let ret_ty = CoreType::ETVar(ret_ty_name.clone());

                // Add the fresh existential variables to the worklist
                self.worklist
                    .push(WorklistEntry::TVar(param_ty_name, TyVarKind::Existential));
                self.worklist
                    .push(WorklistEntry::TVar(ret_ty_name, TyVarKind::Existential));

                let arrow_ty =
                    CoreType::Arrow(Box::new(param_ty.clone()), Box::new(ret_ty.clone()));
                self.worklist.solve_evar(&a, arrow_ty)?;

                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ret_ty,
                    right: result_ty,
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    term: arg,
                    ty: param_ty,
                }));

                Ok(())
            }

            _ => Err(TypeError::NotAFunction {
                ty: func_ty,
                span: None,
            }),
        }
    }

    fn instantiate_left(&mut self, var: &str, ty: &CoreType) -> TypeResult<()> {
        match ty {
            CoreType::ETVar(b) if self.worklist.before(var, b) => {
                self.worklist
                    .solve_evar(b, CoreType::ETVar(var.to_string()))?;
                Ok(())
            }
            CoreType::Arrow(t1, t2) => {
                let a1 = self.worklist.fresh_evar();
                let a2 = self.worklist.fresh_evar();
                let arrow_ty = CoreType::Arrow(
                    Box::new(CoreType::ETVar(a1.clone())),
                    Box::new(CoreType::ETVar(a2.clone())),
                );
                self.worklist.solve_evar(var, arrow_ty)?;

                self.worklist
                    .push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist
                    .push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));

                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *t1.clone(),
                    right: CoreType::ETVar(a1),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: CoreType::ETVar(a2),
                    right: *t2.clone(),
                }));

                Ok(())
            }
            CoreType::App(t1, t2) => {
                let a1 = self.worklist.fresh_evar();
                let a2 = self.worklist.fresh_evar();
                let app_ty = CoreType::App(
                    Box::new(CoreType::ETVar(a1.clone())),
                    Box::new(CoreType::ETVar(a2.clone())),
                );
                self.worklist.solve_evar(var, app_ty)?;

                self.worklist
                    .push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist
                    .push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));

                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: CoreType::ETVar(a1),
                    right: *t1.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: CoreType::ETVar(a2),
                    right: *t2.clone(),
                }));

                Ok(())
            }
            CoreType::Forall(b, t) => {
                let fresh_var = self.worklist.fresh_var();
                self.worklist
                    .push(WorklistEntry::TVar(fresh_var.clone(), TyVarKind::Universal));
                let substituted = self.substitute_type(b, &CoreType::Var(fresh_var), t);
                self.instantiate_left(var, &substituted)
            }
            _ if self.is_monotype(ty) => {
                self.worklist.solve_evar(var, ty.clone())?;
                Ok(())
            }
            _ => Err(TypeError::InstantiationError {
                var: var.to_string(),
                ty: ty.clone(),
                span: None,
            }),
        }
    }

    fn instantiate_right(&mut self, ty: &CoreType, var: &str) -> TypeResult<()> {
        match ty {
            CoreType::ETVar(a) if self.worklist.before(var, a) => {
                self.worklist
                    .solve_evar(a, CoreType::ETVar(var.to_string()))?;
                Ok(())
            }
            CoreType::Arrow(t1, t2) => {
                let a1 = self.worklist.fresh_evar();
                let a2 = self.worklist.fresh_evar();
                let arrow_ty = CoreType::Arrow(
                    Box::new(CoreType::ETVar(a1.clone())),
                    Box::new(CoreType::ETVar(a2.clone())),
                );
                self.worklist.solve_evar(var, arrow_ty)?;

                self.worklist
                    .push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist
                    .push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));

                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: CoreType::ETVar(a1),
                    right: *t1.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *t2.clone(),
                    right: CoreType::ETVar(a2),
                }));

                Ok(())
            }
            CoreType::App(t1, t2) => {
                let a1 = self.worklist.fresh_evar();
                let a2 = self.worklist.fresh_evar();
                let app_ty = CoreType::App(
                    Box::new(CoreType::ETVar(a1.clone())),
                    Box::new(CoreType::ETVar(a2.clone())),
                );
                self.worklist.solve_evar(var, app_ty)?;

                self.worklist
                    .push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist
                    .push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));

                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *t1.clone(),
                    right: CoreType::ETVar(a1),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *t2.clone(),
                    right: CoreType::ETVar(a2),
                }));

                Ok(())
            }
            CoreType::Forall(a, t) => {
                let fresh_evar = self.worklist.fresh_evar();
                self.worklist
                    .push(WorklistEntry::TVar(fresh_evar.clone(), TyVarKind::Marker));
                self.worklist.push(WorklistEntry::TVar(
                    fresh_evar.clone(),
                    TyVarKind::Existential,
                ));
                let substituted = self.substitute_type(a, &CoreType::ETVar(fresh_evar), t);
                self.instantiate_right(&substituted, var)
            }
            _ if self.is_monotype(ty) => {
                self.worklist.solve_evar(var, ty.clone())?;
                Ok(())
            }
            _ => Err(TypeError::InstantiationError {
                var: var.to_string(),
                ty: ty.clone(),
                span: None,
            }),
        }
    }

    fn occurs_check(&self, var: &str, ty: &CoreType) -> bool {
        match ty {
            CoreType::ETVar(name) | CoreType::Var(name) => name == var,
            CoreType::Arrow(t1, t2) | CoreType::App(t1, t2) => {
                self.occurs_check(var, t1) || self.occurs_check(var, t2)
            }
            CoreType::Forall(_, t) => self.occurs_check(var, t),
            CoreType::Product(types) => types.iter().any(|t| self.occurs_check(var, t)),
            _ => false,
        }
    }

    fn is_monotype(&self, ty: &CoreType) -> bool {
        match ty {
            CoreType::Con(_) | CoreType::Var(_) | CoreType::ETVar(_) => true,
            CoreType::Arrow(t1, t2) | CoreType::App(t1, t2) => {
                self.is_monotype(t1) && self.is_monotype(t2)
            }
            CoreType::Product(types) => types.iter().all(|t| self.is_monotype(t)),
            CoreType::Forall(_, _) => false,
        }
    }

    fn substitute_type(&self, var: &str, replacement: &CoreType, ty: &CoreType) -> CoreType {
        match ty {
            CoreType::Var(name) if name == var => replacement.clone(),
            CoreType::Arrow(t1, t2) => CoreType::Arrow(
                Box::new(self.substitute_type(var, replacement, t1)),
                Box::new(self.substitute_type(var, replacement, t2)),
            ),
            CoreType::Forall(bound_var, body) if bound_var != var => CoreType::Forall(
                bound_var.clone(),
                Box::new(self.substitute_type(var, replacement, body)),
            ),
            CoreType::App(t1, t2) => CoreType::App(
                Box::new(self.substitute_type(var, replacement, t1)),
                Box::new(self.substitute_type(var, replacement, t2)),
            ),
            CoreType::Product(types) => CoreType::Product(
                types
                    .iter()
                    .map(|t| self.substitute_type(var, replacement, t))
                    .collect(),
            ),
            _ => ty.clone(),
        }
    }

    fn term_to_string(&self, term: &CoreTerm) -> String {
        match term {
            CoreTerm::Var(name) => name.clone(),
            CoreTerm::LitInt(n) => n.to_string(),
            _ => format!("{:?}", term), // Simplified for now
        }
    }

    pub fn get_trace(&self) -> &[String] {
        &self.trace
    }

    fn infer_binop_types(&mut self, op: &CoreBinOp) -> (CoreType, CoreType, CoreType) {
        match op {
            // Arithmetic operations: Int -> Int -> Int
            CoreBinOp::Add | CoreBinOp::Sub | CoreBinOp::Mul | CoreBinOp::Div => (
                CoreType::Con("Int".to_string()),
                CoreType::Con("Int".to_string()),
                CoreType::Con("Int".to_string()),
            ),
            // Comparison operations: Int -> Int -> Bool
            CoreBinOp::Lt | CoreBinOp::Le => (
                CoreType::Con("Int".to_string()),
                CoreType::Con("Int".to_string()),
                CoreType::Con("Bool".to_string()),
            ),
        }
    }

    fn add_etvars_to_worklist(&mut self, ty: &CoreType) {
        match ty {
            CoreType::ETVar(var_name) => {
                self.worklist.push(WorklistEntry::TVar(
                    var_name.clone(),
                    TyVarKind::Existential,
                ));
            }
            CoreType::Arrow(t1, t2) | CoreType::App(t1, t2) => {
                self.add_etvars_to_worklist(t1);
                self.add_etvars_to_worklist(t2);
            }
            CoreType::Forall(_, t) => {
                self.add_etvars_to_worklist(t);
            }
            CoreType::Product(types) => {
                for t in types {
                    self.add_etvars_to_worklist(t);
                }
            }
            CoreType::Var(_) | CoreType::Con(_) => {
                // No existential variables to add
            }
        }
    }

    /// Check a pattern against a type and return variable bindings
    /// This implements pattern type checking with unification
    fn check_pattern(
        &mut self,
        pattern: &CorePattern,
        expected_ty: &CoreType,
    ) -> TypeResult<HashMap<String, CoreType>> {
        let mut bindings = HashMap::new();

        match pattern {
            CorePattern::Wildcard => {
                // Wildcard matches anything, no bindings
                Ok(bindings)
            }

            CorePattern::Var(var_name) => {
                // Variable patterns bind the scrutinee type
                bindings.insert(var_name.clone(), expected_ty.clone());
                Ok(bindings)
            }

            CorePattern::Constructor { name, args } => {
                // Look up constructor type from data constructor environment
                if let Some(constructor_ty) = self.data_constructors.get(name).cloned() {
                    // Constructor type should be of the form: T1 -> T2 -> ... -> DataType
                    // We need to unify the result type with expected_ty and
                    // check argument patterns against parameter types

                    let (param_types, result_type) =
                        self.extract_constructor_signature(&constructor_ty)?;

                    // The result type should unify with the expected type
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: result_type,
                        right: expected_ty.clone(),
                    }));

                    // Check that we have the right number of pattern arguments
                    if args.len() != param_types.len() {
                        return Err(TypeError::ArityMismatch {
                            expected: param_types.len(),
                            actual: args.len(),
                            span: None,
                        });
                    }

                    // Recursively check argument patterns
                    for (arg_pattern, param_ty) in args.iter().zip(param_types.iter()) {
                        let arg_bindings = self.check_pattern(arg_pattern, param_ty)?;
                        // Merge bindings (should check for conflicts in a real implementation)
                        for (var, ty) in arg_bindings {
                            bindings.insert(var, ty);
                        }
                    }

                    Ok(bindings)
                } else {
                    Err(TypeError::UnboundDataConstructor {
                        name: name.clone(),
                        span: None,
                    })
                }
            }
        }
    }

    /// Extract parameter types and result type from a constructor type
    /// signature e.g., Int -> String -> Bool -> MyData => ([Int, String,
    /// Bool], MyData)
    fn extract_constructor_signature(
        &mut self,
        constructor_ty: &CoreType,
    ) -> TypeResult<(Vec<CoreType>, CoreType)> {
        let mut param_types = Vec::new();
        let mut current_ty = constructor_ty.clone();
        let mut substitutions = HashMap::new();

        // Instantiate forall quantifiers with fresh existential variables
        while let CoreType::Forall(var, body) = current_ty {
            let fresh_var_name = self.worklist.fresh_evar();
            let fresh_evar = CoreType::ETVar(fresh_var_name.clone());

            // Add the existential variable to the worklist
            self.worklist.push(WorklistEntry::TVar(
                fresh_var_name.clone(),
                TyVarKind::Existential,
            ));

            substitutions.insert(var.clone(), fresh_evar);
            current_ty = *body;
        }

        // Apply substitutions to the type
        current_ty = self.apply_type_substitutions(&current_ty, &substitutions);

        // Extract parameter types from arrows
        while let CoreType::Arrow(param_ty, result_ty) = current_ty {
            param_types.push(*param_ty.clone());
            current_ty = *result_ty;
        }

        // The final type is the result type
        Ok((param_types, current_ty))
    }

    fn instantiate_constructor_type(&mut self, constructor_ty: CoreType) -> CoreType {
        let mut current_ty = constructor_ty;
        let mut substitutions = HashMap::new();

        // Instantiate forall quantifiers with fresh existential variables
        while let CoreType::Forall(var, body) = current_ty {
            let fresh_var_name = self.worklist.fresh_evar();
            let fresh_evar = CoreType::ETVar(fresh_var_name.clone());

            // Add the existential variable to the worklist
            self.worklist.push(WorklistEntry::TVar(
                fresh_var_name.clone(),
                TyVarKind::Existential,
            ));

            substitutions.insert(var.clone(), fresh_evar);
            current_ty = *body;
        }

        // Apply substitutions to the type
        self.apply_type_substitutions(&current_ty, &substitutions)
    }

    fn apply_type_substitutions(
        &self,
        ty: &CoreType,
        substitutions: &HashMap<String, CoreType>,
    ) -> CoreType {
        match ty {
            CoreType::Var(name) => substitutions
                .get(name)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            CoreType::Con(name) => CoreType::Con(name.clone()),
            CoreType::ETVar(name) => CoreType::ETVar(name.clone()),
            CoreType::Arrow(left, right) => CoreType::Arrow(
                Box::new(self.apply_type_substitutions(left, substitutions)),
                Box::new(self.apply_type_substitutions(right, substitutions)),
            ),
            CoreType::App(left, right) => CoreType::App(
                Box::new(self.apply_type_substitutions(left, substitutions)),
                Box::new(self.apply_type_substitutions(right, substitutions)),
            ),
            CoreType::Forall(var, body) => {
                // Don't substitute under forall bindings - this is a simplification
                CoreType::Forall(
                    var.clone(),
                    Box::new(self.apply_type_substitutions(body, substitutions)),
                )
            }
            CoreType::Product(types) => CoreType::Product(
                types
                    .iter()
                    .map(|t| self.apply_type_substitutions(t, substitutions))
                    .collect(),
            ),
        }
    }
}
