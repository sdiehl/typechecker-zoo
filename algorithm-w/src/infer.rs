use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;

use crate::ast::{Expr, Lit, Scheme, Type};
use crate::errors::{InferenceError, Result};

pub type TyVar = String;
pub type TmVar = String;
pub type Env = BTreeMap<TmVar, Scheme>;

#[derive(Debug, Clone, Default)]
pub struct Subst {
    pub types: HashMap<String, Type>,
}

impl Subst {
    fn empty() -> Self {
        Self::default()
    }

    fn singleton_type(var: String, ty: Type) -> Self {
        let mut s = Self::empty();
        s.types.insert(var, ty);
        s
    }

    // s2 ∘ s1: apply s1 first, then s2.
    fn compose(&self, s1: &Self) -> Self {
        let mut types: HashMap<String, Type> = s1
            .types
            .iter()
            .map(|(k, v)| (k.clone(), apply_type(self, v)))
            .collect();
        for (k, v) in &self.types {
            types.entry(k.clone()).or_insert_with(|| v.clone());
        }
        Subst { types }
    }
}

fn apply_type(s: &Subst, ty: &Type) -> Type {
    match ty {
        Type::Var(name) => match s.types.get(name) {
            Some(t) => apply_type(s, t),
            None => ty.clone(),
        },
        Type::Arrow(a, b) => Type::Arrow(Box::new(apply_type(s, a)), Box::new(apply_type(s, b))),
        Type::Tuple(types) => Type::Tuple(types.iter().map(|t| apply_type(s, t)).collect()),
        Type::Int | Type::Bool => ty.clone(),
    }
}

fn apply_scheme(s: &Subst, scheme: &Scheme) -> Scheme {
    let mut filtered = s.clone();
    for v in &scheme.type_vars {
        filtered.types.remove(v);
    }
    Scheme {
        type_vars: scheme.type_vars.clone(),
        ty: apply_type(&filtered, &scheme.ty),
    }
}

fn apply_env(s: &Subst, env: &Env) -> Env {
    env.iter()
        .map(|(k, v)| (k.clone(), apply_scheme(s, v)))
        .collect()
}

#[derive(Default)]
struct FreeVars {
    types: HashSet<String>,
}

fn ftv_type(ty: &Type, out: &mut FreeVars) {
    match ty {
        Type::Var(n) => {
            out.types.insert(n.clone());
        }
        Type::Arrow(a, b) => {
            ftv_type(a, out);
            ftv_type(b, out);
        }
        Type::Tuple(types) => {
            for t in types {
                ftv_type(t, out);
            }
        }
        Type::Int | Type::Bool => {}
    }
}

fn ftv_scheme(s: &Scheme) -> FreeVars {
    let mut fv = FreeVars::default();
    ftv_type(&s.ty, &mut fv);
    for v in &s.type_vars {
        fv.types.remove(v);
    }
    fv
}

fn ftv_env(env: &Env) -> FreeVars {
    let mut fv = FreeVars::default();
    for s in env.values() {
        let s_fv = ftv_scheme(s);
        fv.types.extend(s_fv.types);
    }
    fv
}

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
        if subst.types.is_empty() {
            "{}".to_string()
        } else {
            let entries: Vec<String> = subst
                .types
                .iter()
                .map(|(k, v)| format!("{}/{}", v, k))
                .collect();
            format!("{{{}}}", entries.join(", "))
        }
    }

    fn generalize(&self, env: &Env, ty: &Type) -> Scheme {
        let mut fv = FreeVars::default();
        ftv_type(ty, &mut fv);
        let env_fv = ftv_env(env);
        let mut tvars: Vec<_> = fv.types.difference(&env_fv.types).cloned().collect();
        tvars.sort();
        Scheme {
            type_vars: tvars,
            ty: ty.clone(),
        }
    }

    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut s = Subst::empty();
        for v in &scheme.type_vars {
            let fresh = self.fresh_tyvar();
            s.types.insert(v.clone(), Type::Var(fresh));
        }
        apply_type(&s, &scheme.ty)
    }

    fn occurs_check(&self, var: &TyVar, ty: &Type) -> bool {
        let mut fv = FreeVars::default();
        ftv_type(ty, &mut fv);
        fv.types.contains(var)
    }

    fn unify(&self, t1: &Type, t2: &Type) -> Result<(Subst, InferenceTree)> {
        let input = format!("{} ~ {}", t1, t2);

        match (t1, t2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => {
                let tree = InferenceTree::new("Unify-Base", &input, "{}", vec![]);
                Ok((Subst::empty(), tree))
            }
            (Type::Var(v), ty) | (ty, Type::Var(v)) => {
                if ty == &Type::Var(v.clone()) {
                    let tree = InferenceTree::new("Unify-Var-Same", &input, "{}", vec![]);
                    Ok((Subst::empty(), tree))
                } else if self.occurs_check(v, ty) {
                    Err(InferenceError::OccursCheck {
                        var: v.clone(),
                        ty: ty.clone(),
                    })
                } else {
                    let subst = Subst::singleton_type(v.clone(), ty.clone());
                    let output = format!("{{{}/{}}}", ty, v);
                    let tree = InferenceTree::new("Unify-Var", &input, &output, vec![]);
                    Ok((subst, tree))
                }
            }
            (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => {
                let (s1, tree1) = self.unify(a1, b1)?;
                let (s2, tree2) = self.unify(&apply_type(&s1, a2), &apply_type(&s1, b2))?;
                let final_subst = s2.compose(&s1);
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

                let mut subst = Subst::empty();
                let mut trees = Vec::new();

                for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                    let (s, tree) = self.unify(&apply_type(&subst, t1), &apply_type(&subst, t2))?;
                    subst = s.compose(&subst);
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
        Ok((Subst::empty(), Type::Int, tree))
    }

    /// T-LitBool: ─────────────────
    ///            Γ ⊢ b : Bool
    fn infer_lit_bool(&mut self, env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let tree = InferenceTree::new("T-Bool", &input, "Bool", vec![]);
        Ok((Subst::empty(), Type::Bool, tree))
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
                Ok((Subst::empty(), instantiated, tree))
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
        let param_scheme = Scheme {
            type_vars: vec![],
            ty: param_type.clone(),
        };
        new_env.insert(param.to_string(), param_scheme);

        let (s1, body_type, tree1) = self.infer(&new_env, body)?;
        let result_type = Type::Arrow(Box::new(apply_type(&s1, &param_type)), Box::new(body_type));

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
        let env_subst = apply_env(&s1, env);
        let (s2, arg_type, tree2) = self.infer(&env_subst, arg)?;

        let func_type_subst = apply_type(&s2, &func_type);
        let expected_func_type = Type::Arrow(Box::new(arg_type), Box::new(result_type.clone()));

        let (s3, tree3) = self.unify(&func_type_subst, &expected_func_type)?;

        let final_subst = s3.compose(&s2.compose(&s1));
        let final_type = apply_type(&s3, &result_type);

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
        let env_subst = apply_env(&s1, env);
        let generalized_type = self.generalize(&env_subst, &value_type);

        let mut new_env = env_subst;
        new_env.insert(var.to_string(), generalized_type);

        let (s2, body_type, tree2) = self.infer(&new_env, body)?;

        let final_subst = s2.compose(&s1);
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

        let mut subst = Subst::empty();
        let mut types = Vec::new();
        let mut trees = Vec::new();
        let mut current_env = env.clone();

        for expr in exprs {
            let (s, ty, tree) = self.infer(&current_env, expr)?;
            subst = s.compose(&subst);
            current_env = apply_env(&s, &current_env);
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

pub fn infer_type(expr: &Expr) -> Result<Type> {
    let mut inference = TypeInference::new();
    let env = BTreeMap::new();
    let (s, ty, _) = inference.infer(&env, expr)?;
    let final_ty = apply_type(&s, &ty);
    let scheme = inference.generalize(&env, &final_ty);
    Ok(rename_scheme(&scheme))
}

fn rename_scheme(scheme: &Scheme) -> Type {
    let mut subst = Subst::empty();
    let mut letters = ('a'..='z').map(|c| c.to_string());
    for v in &scheme.type_vars {
        let fresh = letters.next().unwrap_or_else(|| v.clone());
        subst.types.insert(v.clone(), Type::Var(fresh));
    }
    apply_type(&subst, &scheme.ty)
}
