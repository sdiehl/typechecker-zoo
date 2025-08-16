use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;

use crate::ast::{Expr, Lit, Scheme, Type};
use crate::errors::{InferenceError, Result};

pub type TyVar = String;
pub type TmVar = String;
pub type Env = BTreeMap<TmVar, Scheme>; // Now stores schemes, not types
pub type Subst = HashMap<TyVar, Type>;

#[derive(Debug)]
pub struct InferenceTree {
    pub rule: String,
    pub input: String,
    pub output: String,
    pub children: Vec<InferenceTree>,
}

impl InferenceTree {
    fn new(rule: &str, input: &str, output: &str, children: Vec<InferenceTree>) -> Self {
        Self {
            rule: rule.to_string(),
            input: input.to_string(),
            output: output.to_string(),
            children,
        }
    }
}

impl fmt::Display for InferenceTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_with_indent(f, 0)
    }
}

impl InferenceTree {
    fn display_with_indent(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        let prefix = "  ".repeat(indent);
        writeln!(
            f,
            "{}{}: {} => {}",
            prefix, self.rule, self.input, self.output
        )?;
        for child in &self.children {
            child.display_with_indent(f, indent + 1)?;
        }
        Ok(())
    }
}

pub struct TypeInference {
    counter: usize,
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInference {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    fn fresh_tyvar(&mut self) -> TyVar {
        let var = format!("t{}", self.counter);
        self.counter += 1;
        var
    }

    fn pretty_env(&self, env: &Env) -> String {
        if env.is_empty() {
            "{}".to_string()
        } else {
            let entries: Vec<String> = env.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
            format!("{{{}}}", entries.join(", "))
        }
    }

    fn pretty_subst(&self, subst: &Subst) -> String {
        if subst.is_empty() {
            "{}".to_string()
        } else {
            let entries: Vec<String> = subst.iter().map(|(k, v)| format!("{}/{}", v, k)).collect();
            format!("{{{}}}", entries.join(", "))
        }
    }

    fn apply_subst(&self, subst: &Subst, ty: &Type) -> Type {
        match ty {
            Type::Var(name) => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(self.apply_subst(subst, t1)),
                Box::new(self.apply_subst(subst, t2)),
            ),
            Type::Tuple(types) => {
                Type::Tuple(types.iter().map(|t| self.apply_subst(subst, t)).collect())
            }
            Type::Int | Type::Bool => ty.clone(),
        }
    }

    fn apply_subst_scheme(&self, subst: &Subst, scheme: &Scheme) -> Scheme {
        // Remove bindings for quantified variables to avoid capture
        let mut filtered_subst = subst.clone();
        for var in &scheme.vars {
            filtered_subst.remove(var);
        }
        Scheme {
            vars: scheme.vars.clone(),
            ty: self.apply_subst(&filtered_subst, &scheme.ty),
        }
    }

    fn apply_subst_env(&self, subst: &Subst, env: &Env) -> Env {
        env.iter()
            .map(|(k, v)| (k.clone(), self.apply_subst_scheme(subst, v)))
            .collect()
    }

    fn compose_subst(&self, s1: &Subst, s2: &Subst) -> Subst {
        let mut result = s1.clone();
        for (k, v) in s2 {
            result.insert(k.clone(), self.apply_subst(s1, v));
        }
        result
    }

    fn free_type_vars(&self, ty: &Type) -> HashSet<TyVar> {
        match ty {
            Type::Var(name) => {
                let mut set = HashSet::new();
                set.insert(name.clone());
                set
            }
            Type::Arrow(t1, t2) => {
                let mut set = self.free_type_vars(t1);
                set.extend(self.free_type_vars(t2));
                set
            }
            Type::Tuple(types) => {
                let mut set = HashSet::new();
                for t in types {
                    set.extend(self.free_type_vars(t));
                }
                set
            }
            Type::Int | Type::Bool => HashSet::new(),
        }
    }

    fn free_type_vars_scheme(&self, scheme: &Scheme) -> HashSet<TyVar> {
        let mut set = self.free_type_vars(&scheme.ty);
        // Remove quantified variables
        for var in &scheme.vars {
            set.remove(var);
        }
        set
    }

    fn free_type_vars_env(&self, env: &Env) -> HashSet<TyVar> {
        let mut set = HashSet::new();
        for scheme in env.values() {
            set.extend(self.free_type_vars_scheme(scheme));
        }
        set
    }

    fn generalize(&self, env: &Env, ty: &Type) -> Scheme {
        let type_vars = self.free_type_vars(ty);
        let env_vars = self.free_type_vars_env(env);
        let mut free_vars: Vec<_> = type_vars.difference(&env_vars).cloned().collect();
        free_vars.sort(); // Sort for deterministic behavior

        Scheme {
            vars: free_vars,
            ty: ty.clone(),
        }
    }

    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        // Create fresh type variables for each quantified variable
        let mut subst = HashMap::new();
        for var in &scheme.vars {
            let fresh = self.fresh_tyvar();
            subst.insert(var.clone(), Type::Var(fresh));
        }

        self.apply_subst(&subst, &scheme.ty)
    }

    fn occurs_check(&self, var: &TyVar, ty: &Type) -> bool {
        match ty {
            Type::Var(name) => name == var,
            Type::Arrow(t1, t2) => self.occurs_check(var, t1) || self.occurs_check(var, t2),
            Type::Tuple(types) => types.iter().any(|t| self.occurs_check(var, t)),
            Type::Int | Type::Bool => false,
        }
    }

    fn unify(&self, t1: &Type, t2: &Type) -> Result<(Subst, InferenceTree)> {
        let input = format!("{} ~ {}", t1, t2);

        match (t1, t2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => {
                let tree = InferenceTree::new("Unify-Base", &input, "{}", vec![]);
                Ok((HashMap::new(), tree))
            }
            (Type::Var(v), ty) | (ty, Type::Var(v)) => {
                if ty == &Type::Var(v.clone()) {
                    let tree = InferenceTree::new("Unify-Var-Same", &input, "{}", vec![]);
                    Ok((HashMap::new(), tree))
                } else if self.occurs_check(v, ty) {
                    Err(InferenceError::OccursCheck {
                        var: v.clone(),
                        ty: ty.clone(),
                    })
                } else {
                    let mut subst = HashMap::new();
                    subst.insert(v.clone(), ty.clone());
                    let output = format!("{{{}/{}}}", ty, v);
                    let tree = InferenceTree::new("Unify-Var", &input, &output, vec![]);
                    Ok((subst, tree))
                }
            }
            (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => {
                let (s1, tree1) = self.unify(a1, b1)?;
                let a2_subst = self.apply_subst(&s1, a2);
                let b2_subst = self.apply_subst(&s1, b2);
                let (s2, tree2) = self.unify(&a2_subst, &b2_subst)?;
                let final_subst = self.compose_subst(&s2, &s1);
                let output = self.pretty_subst(&final_subst);
                let tree = InferenceTree::new("Unify-Arrow", &input, &output, vec![tree1, tree2]);
                Ok((final_subst, tree))
            }
            (Type::Tuple(ts1), Type::Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    return Err(InferenceError::TupleLengthMismatch {
                        left_len: ts1.len(),
                        right_len: ts2.len(),
                    });
                }

                let mut subst = HashMap::new();
                let mut trees = Vec::new();

                for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                    let t1_subst = self.apply_subst(&subst, t1);
                    let t2_subst = self.apply_subst(&subst, t2);
                    let (s, tree) = self.unify(&t1_subst, &t2_subst)?;
                    subst = self.compose_subst(&s, &subst);
                    trees.push(tree);
                }

                let output = self.pretty_subst(&subst);
                let tree = InferenceTree::new("Unify-Tuple", &input, &output, trees);
                Ok((subst, tree))
            }
            _ => Err(InferenceError::UnificationFailure {
                expected: t1.clone(),
                actual: t2.clone(),
            }),
        }
    }

    pub fn infer(&mut self, env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        match expr {
            Expr::Lit(Lit::Int(_)) => self.infer_lit_int(env, expr),
            Expr::Lit(Lit::Bool(_)) => self.infer_lit_bool(env, expr),
            Expr::Var(name) => self.infer_var(env, expr, name),
            Expr::Abs(param, body) => self.infer_abs(env, expr, param, body),
            Expr::App(func, arg) => self.infer_app(env, expr, func, arg),
            Expr::Let(var, value, body) => self.infer_let(env, expr, var, value, body),
            Expr::Tuple(exprs) => self.infer_tuple(env, expr, exprs),
        }
    }

    /// T-LitInt: ─────────────────
    ///           Γ ⊢ n : Int
    fn infer_lit_int(&mut self, env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let tree = InferenceTree::new("T-Int", &input, "Int", vec![]);
        Ok((HashMap::new(), Type::Int, tree))
    }

    /// T-LitBool: ─────────────────
    ///            Γ ⊢ b : Bool
    fn infer_lit_bool(&mut self, env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let tree = InferenceTree::new("T-Bool", &input, "Bool", vec![]);
        Ok((HashMap::new(), Type::Bool, tree))
    }

    /// T-Var: x : σ ∈ Γ    τ = inst(σ)
    ///        ─────────────────────────
    ///               Γ ⊢ x : τ
    fn infer_var(
        &mut self,
        env: &Env,
        expr: &Expr,
        name: &str,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);

        match env.get(name) {
            Some(scheme) => {
                let instantiated = self.instantiate(scheme);
                let output = format!("{}", instantiated);
                let tree = InferenceTree::new("T-Var", &input, &output, vec![]);
                Ok((HashMap::new(), instantiated, tree))
            }
            None => Err(InferenceError::UnboundVariable {
                name: name.to_string(),
            }),
        }
    }

    /// T-Lam: Γ, x : α ⊢ e : τ    α fresh
    ///        ─────────────────────────────
    ///           Γ ⊢ λx. e : α → τ
    fn infer_abs(
        &mut self,
        env: &Env,
        expr: &Expr,
        param: &str,
        body: &Expr,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);

        let param_type = Type::Var(self.fresh_tyvar());
        let mut new_env = env.clone();
        // Insert a monomorphic scheme for the parameter
        let param_scheme = Scheme {
            vars: vec![],
            ty: param_type.clone(),
        };
        new_env.insert(param.to_string(), param_scheme);

        let (s1, body_type, tree1) = self.infer(&new_env, body)?;
        let param_type_subst = self.apply_subst(&s1, &param_type);
        let result_type = Type::Arrow(Box::new(param_type_subst), Box::new(body_type));

        let output = format!("{}", result_type);
        let tree = InferenceTree::new("T-Abs", &input, &output, vec![tree1]);
        Ok((s1, result_type, tree))
    }

    /// T-App: Γ ⊢ e₁ : τ₁    Γ ⊢ e₂ : τ₂    α fresh    S = unify(τ₁, τ₂ → α)
    ///        ──────────────────────────────────────────────────────────────
    ///                            Γ ⊢ e₁ e₂ : S(α)
    fn infer_app(
        &mut self,
        env: &Env,
        expr: &Expr,
        func: &Expr,
        arg: &Expr,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);

        let result_type = Type::Var(self.fresh_tyvar());

        let (s1, func_type, tree1) = self.infer(env, func)?;
        let env_subst = self.apply_subst_env(&s1, env);
        let (s2, arg_type, tree2) = self.infer(&env_subst, arg)?;

        let func_type_subst = self.apply_subst(&s2, &func_type);
        let expected_func_type = Type::Arrow(Box::new(arg_type), Box::new(result_type.clone()));

        let (s3, tree3) = self.unify(&func_type_subst, &expected_func_type)?;

        let final_subst = self.compose_subst(&s3, &self.compose_subst(&s2, &s1));
        let final_type = self.apply_subst(&s3, &result_type);

        let output = format!("{}", final_type);
        let tree = InferenceTree::new("T-App", &input, &output, vec![tree1, tree2, tree3]);
        Ok((final_subst, final_type, tree))
    }

    /// T-Let: Γ ⊢ e₁ : τ₁    σ = gen(Γ, τ₁)    Γ, x : σ ⊢ e₂ : τ₂
    ///        ──────────────────────────────────────────────────────
    ///                     Γ ⊢ let x = e₁ in e₂ : τ₂
    fn infer_let(
        &mut self,
        env: &Env,
        expr: &Expr,
        var: &str,
        value: &Expr,
        body: &Expr,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);

        let (s1, value_type, tree1) = self.infer(env, value)?;
        let env_subst = self.apply_subst_env(&s1, env);
        let generalized_type = self.generalize(&env_subst, &value_type);

        let mut new_env = env_subst;
        new_env.insert(var.to_string(), generalized_type);

        let (s2, body_type, tree2) = self.infer(&new_env, body)?;

        let final_subst = self.compose_subst(&s2, &s1);
        let output = format!("{}", body_type);
        let tree = InferenceTree::new("T-Let", &input, &output, vec![tree1, tree2]);
        Ok((final_subst, body_type, tree))
    }

    /// T-Tuple: Γ ⊢ e₁ : τ₁    ...    Γ ⊢ eₙ : τₙ
    ///          ─────────────────────────────────────
    ///              Γ ⊢ (e₁, ..., eₙ) : (τ₁, ..., τₙ)
    fn infer_tuple(
        &mut self,
        env: &Env,
        expr: &Expr,
        exprs: &[Expr],
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);

        let mut subst = HashMap::new();
        let mut types = Vec::new();
        let mut trees = Vec::new();
        let mut current_env = env.clone();

        for expr in exprs {
            let (s, ty, tree) = self.infer(&current_env, expr)?;
            subst = self.compose_subst(&s, &subst);
            current_env = self.apply_subst_env(&s, &current_env);
            types.push(ty);
            trees.push(tree);
        }

        let result_type = Type::Tuple(types);
        let output = format!("{}", result_type);
        let tree = InferenceTree::new("T-Tuple", &input, &output, trees);
        Ok((subst, result_type, tree))
    }
}

pub fn run_inference(expr: &Expr) -> Result<InferenceTree> {
    let mut inference = TypeInference::new();
    let env = BTreeMap::new();
    let (_, _, tree) = inference.infer(&env, expr)?;
    Ok(tree)
}

pub fn infer_type_only(expr: &Expr) -> Result<Type> {
    let mut inference = TypeInference::new();
    let env = BTreeMap::new();
    let (_, ty, _) = inference.infer(&env, expr)?;
    Ok(ty)
}
