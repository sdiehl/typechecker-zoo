use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;

use crate::ast::{Effect, Expr, Lit, Scheme, Type};
use crate::errors::{InferenceError, Result};

pub type Env = BTreeMap<String, Scheme>;

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

#[derive(Debug, Clone, Default)]
pub struct Subst {
    pub types: HashMap<String, Type>,
    pub effects: HashMap<String, Effect>,
}

impl Subst {
    fn empty() -> Self {
        Self::default()
    }
    fn singleton_type(v: String, t: Type) -> Self {
        let mut s = Self::empty();
        s.types.insert(v, t);
        s
    }
    fn singleton_effect(v: String, e: Effect) -> Self {
        let mut s = Self::empty();
        s.effects.insert(v, e);
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
        let mut effects: HashMap<String, Effect> = s1
            .effects
            .iter()
            .map(|(k, v)| (k.clone(), apply_effect(self, v)))
            .collect();
        for (k, v) in &self.effects {
            effects.entry(k.clone()).or_insert_with(|| v.clone());
        }
        Subst { types, effects }
    }
}

fn apply_type(s: &Subst, ty: &Type) -> Type {
    match ty {
        Type::Var(n) => match s.types.get(n) {
            Some(t) => apply_type(s, t),
            None => ty.clone(),
        },
        Type::Int | Type::Bool | Type::Unit => ty.clone(),
        Type::Arrow(a, eff, b) => Type::Arrow(
            Box::new(apply_type(s, a)),
            Box::new(apply_effect(s, eff)),
            Box::new(apply_type(s, b)),
        ),
    }
}

fn apply_effect(s: &Subst, eff: &Effect) -> Effect {
    match eff {
        Effect::Empty => Effect::Empty,
        Effect::Var(n) => match s.effects.get(n) {
            Some(e) => apply_effect(s, e),
            None => eff.clone(),
        },
        Effect::Extend(l, rest) => Effect::Extend(l.clone(), Box::new(apply_effect(s, rest))),
    }
}

fn apply_scheme(s: &Subst, scheme: &Scheme) -> Scheme {
    let mut filtered = s.clone();
    for v in &scheme.type_vars {
        filtered.types.remove(v);
    }
    for v in &scheme.effect_vars {
        filtered.effects.remove(v);
    }
    Scheme {
        type_vars: scheme.type_vars.clone(),
        effect_vars: scheme.effect_vars.clone(),
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
    effects: HashSet<String>,
}

fn ftv_type(ty: &Type, out: &mut FreeVars) {
    match ty {
        Type::Var(n) => {
            out.types.insert(n.clone());
        }
        Type::Int | Type::Bool | Type::Unit => {}
        Type::Arrow(a, eff, b) => {
            ftv_type(a, out);
            ftv_effect(eff, out);
            ftv_type(b, out);
        }
    }
}

fn ftv_effect(eff: &Effect, out: &mut FreeVars) {
    match eff {
        Effect::Empty => {}
        Effect::Var(n) => {
            out.effects.insert(n.clone());
        }
        Effect::Extend(_, rest) => ftv_effect(rest, out),
    }
}

fn ftv_scheme(s: &Scheme) -> FreeVars {
    let mut fv = FreeVars::default();
    ftv_type(&s.ty, &mut fv);
    for v in &s.type_vars {
        fv.types.remove(v);
    }
    for v in &s.effect_vars {
        fv.effects.remove(v);
    }
    fv
}

fn ftv_env(env: &Env) -> FreeVars {
    let mut fv = FreeVars::default();
    for s in env.values() {
        let s_fv = ftv_scheme(s);
        fv.types.extend(s_fv.types);
        fv.effects.extend(s_fv.effects);
    }
    fv
}

// Built-in operations: name -> (param type, return type, effect label).
// `throw` returns a fresh polymorphic type, signalled with `~`.
fn op_signature(op: &str) -> Option<(Type, Type, &'static str)> {
    match op {
        "print" => Some((Type::Int, Type::Unit, "io")),
        "read" => Some((Type::Unit, Type::Int, "io")),
        "throw" => Some((Type::Int, Type::Var("~".to_string()), "exn")),
        "ask" => Some((Type::Unit, Type::Int, "reader")),
        "tell" => Some((Type::Int, Type::Unit, "writer")),
        _ => None,
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

    fn fresh_type(&mut self) -> Type {
        let v = format!("t{}", self.counter);
        self.counter += 1;
        Type::Var(v)
    }

    fn fresh_effect(&mut self) -> Effect {
        let v = format!("e{}", self.counter);
        self.counter += 1;
        Effect::Var(v)
    }

    fn fresh_effect_var(&mut self) -> String {
        let v = format!("e{}", self.counter);
        self.counter += 1;
        v
    }

    fn pretty_env(&self, env: &Env) -> String {
        if env.is_empty() {
            "{}".to_string()
        } else {
            let entries: Vec<String> = env.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
            format!("{{{}}}", entries.join(", "))
        }
    }

    fn pretty_judgment(&self, ty: &Type, eff: &Effect) -> String {
        if matches!(eff, Effect::Empty) {
            format!("{}", ty)
        } else {
            format!("{} ! <{}>", ty, eff)
        }
    }

    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut s = Subst::empty();
        for v in &scheme.type_vars {
            s.types.insert(v.clone(), self.fresh_type());
        }
        for v in &scheme.effect_vars {
            s.effects.insert(v.clone(), self.fresh_effect());
        }
        apply_type(&s, &scheme.ty)
    }

    fn generalize(&self, env: &Env, ty: &Type) -> Scheme {
        let mut fv = FreeVars::default();
        ftv_type(ty, &mut fv);
        let env_fv = ftv_env(env);
        let mut tvars: Vec<_> = fv.types.difference(&env_fv.types).cloned().collect();
        let mut evars: Vec<_> = fv.effects.difference(&env_fv.effects).cloned().collect();
        tvars.sort();
        evars.sort();
        Scheme {
            type_vars: tvars,
            effect_vars: evars,
            ty: ty.clone(),
        }
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(Subst, InferenceTree)> {
        let input = format!("{} ~ {}", t1, t2);
        match (t1, t2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) | (Type::Unit, Type::Unit) => {
                let tree = InferenceTree::new("Unify-Base", &input, "{}", vec![]);
                Ok((Subst::empty(), tree))
            }
            (Type::Var(a), Type::Var(b)) if a == b => {
                let tree = InferenceTree::new("Unify-Var", &input, "{}", vec![]);
                Ok((Subst::empty(), tree))
            }
            (Type::Var(v), other) | (other, Type::Var(v)) => {
                let mut fv = FreeVars::default();
                ftv_type(other, &mut fv);
                if fv.types.contains(v) {
                    Err(InferenceError::OccursCheck {
                        var: v.clone(),
                        ty: other.clone(),
                    })
                } else {
                    let output = format!("{{{}/{}}}", other, v);
                    let tree = InferenceTree::new("Unify-Var", &input, &output, vec![]);
                    Ok((Subst::singleton_type(v.clone(), other.clone()), tree))
                }
            }
            (Type::Arrow(a1, e1, b1), Type::Arrow(a2, e2, b2)) => {
                let (s1, tree1) = self.unify(a1, a2)?;
                let (s2, tree2) =
                    self.unify_effect(&apply_effect(&s1, e1), &apply_effect(&s1, e2))?;
                let s12 = s2.compose(&s1);
                let (s3, tree3) = self.unify(&apply_type(&s12, b1), &apply_type(&s12, b2))?;
                let s_final = s3.compose(&s12);
                let tree =
                    InferenceTree::new("Unify-Arrow", &input, "ok", vec![tree1, tree2, tree3]);
                Ok((s_final, tree))
            }
            _ => Err(InferenceError::UnificationFailure {
                expected: t1.clone(),
                actual: t2.clone(),
            }),
        }
    }

    // Same scoped-row unification as in row-poly: for an effect label l in the
    // LHS, rewrite the RHS to expose l at the head, unify the tails. The side
    // condition `tail(rest1) ∉ dom(θ1)` rules out the Wand-style divergence.
    fn unify_effect(&mut self, e1: &Effect, e2: &Effect) -> Result<(Subst, InferenceTree)> {
        let input = format!("<{}> ~ <{}>", e1, e2);
        match (e1, e2) {
            (Effect::Empty, Effect::Empty) => {
                let tree = InferenceTree::new("Unify-EffEmpty", &input, "{}", vec![]);
                Ok((Subst::empty(), tree))
            }
            (Effect::Var(a), Effect::Var(b)) if a == b => {
                let tree = InferenceTree::new("Unify-EffVar", &input, "{}", vec![]);
                Ok((Subst::empty(), tree))
            }
            (Effect::Var(v), other) | (other, Effect::Var(v)) => {
                let mut fv = FreeVars::default();
                ftv_effect(other, &mut fv);
                if fv.effects.contains(v) {
                    Err(InferenceError::EffectOccursCheck {
                        var: v.clone(),
                        eff: other.clone(),
                    })
                } else {
                    let output = format!("{{<{}>/{}}}", other, v);
                    let tree = InferenceTree::new("Unify-EffVar", &input, &output, vec![]);
                    Ok((Subst::singleton_effect(v.clone(), other.clone()), tree))
                }
            }
            (Effect::Extend(l, rest1), other) => {
                let (rest2, s1, rewrite_tree) = self.rewrite_effect(other, l)?;
                if let Some(tv) = effect_tail(rest1) {
                    if s1.effects.contains_key(tv) {
                        return Err(InferenceError::RecursiveEffect {
                            var: tv.to_string(),
                        });
                    }
                }
                let (s2, tree2) =
                    self.unify_effect(&apply_effect(&s1, rest1), &apply_effect(&s1, &rest2))?;
                let s_final = s2.compose(&s1);
                let mut children = vec![];
                if let Some(t) = rewrite_tree {
                    children.push(t);
                }
                children.push(tree2);
                let tree = InferenceTree::new("Unify-EffExtend", &input, "ok", children);
                Ok((s_final, tree))
            }
            (Effect::Empty, Effect::Extend(l, _)) => Err(InferenceError::MissingEffect {
                label: l.clone(),
                eff: Effect::Empty,
            }),
        }
    }

    // Hoist label `l` to the head of an effect row. Returns the residual row,
    // a substitution, and an optional trace node describing the rewrite step.
    fn rewrite_effect(
        &mut self,
        eff: &Effect,
        label: &str,
    ) -> Result<(Effect, Subst, Option<InferenceTree>)> {
        match eff {
            Effect::Empty => Err(InferenceError::MissingEffect {
                label: label.to_string(),
                eff: Effect::Empty,
            }),
            Effect::Extend(l, rest) if l == label => Ok((*rest.clone(), Subst::empty(), None)),
            Effect::Extend(l, rest) => {
                let (rest2, s, _) = self.rewrite_effect(rest, label)?;
                Ok((Effect::Extend(l.clone(), Box::new(rest2)), s, None))
            }
            Effect::Var(alpha) => {
                let beta = self.fresh_effect();
                let new_eff = Effect::Extend(label.to_string(), Box::new(beta.clone()));
                let s = Subst::singleton_effect(alpha.clone(), new_eff);
                Ok((beta, s, None))
            }
        }
    }

    fn op_instance(&mut self, op: &str) -> Result<(Type, Type, String)> {
        match op_signature(op) {
            Some((p, r, l)) => {
                // `~` marks a polymorphic return slot in a primitive op.
                let r = if matches!(&r, Type::Var(n) if n == "~") {
                    self.fresh_type()
                } else {
                    r
                };
                Ok((p, r, l.to_string()))
            }
            None => Err(InferenceError::UnknownOp { op: op.to_string() }),
        }
    }

    pub fn infer(
        &mut self,
        env: &Env,
        expr: &Expr,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        match expr {
            Expr::Lit(Lit::Int(_)) => self.infer_lit_int(env, expr),
            Expr::Lit(Lit::Bool(_)) => self.infer_lit_bool(env, expr),
            Expr::Lit(Lit::Unit) => self.infer_lit_unit(env, expr),
            Expr::Var(name) => self.infer_var(env, expr, name),
            Expr::Abs(p, body) => self.infer_abs(env, expr, p, body),
            Expr::App(f, a) => self.infer_app(env, expr, f, a),
            Expr::Let(name, value, body) => self.infer_let(env, expr, name, value, body),
            Expr::Perform(op, e) => self.infer_perform(env, expr, op, e),
            Expr::Handle {
                body,
                op,
                param,
                resume,
                handler,
            } => self.infer_handle(env, expr, body, op, param, resume, handler),
        }
    }

    /// T-Int: ─────────────────────
    ///        Γ ⊢ n : Int ! ε
    fn infer_lit_int(
        &mut self,
        env: &Env,
        expr: &Expr,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let eff = self.fresh_effect();
        let output = self.pretty_judgment(&Type::Int, &eff);
        let tree = InferenceTree::new("T-Int", &input, &output, vec![]);
        Ok((Subst::empty(), Type::Int, eff, tree))
    }

    /// T-Bool: ─────────────────────
    ///         Γ ⊢ b : Bool ! ε
    fn infer_lit_bool(
        &mut self,
        env: &Env,
        expr: &Expr,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let eff = self.fresh_effect();
        let output = self.pretty_judgment(&Type::Bool, &eff);
        let tree = InferenceTree::new("T-Bool", &input, &output, vec![]);
        Ok((Subst::empty(), Type::Bool, eff, tree))
    }

    /// T-Unit: ─────────────────────
    ///         Γ ⊢ () : Unit ! ε
    fn infer_lit_unit(
        &mut self,
        env: &Env,
        expr: &Expr,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let eff = self.fresh_effect();
        let output = self.pretty_judgment(&Type::Unit, &eff);
        let tree = InferenceTree::new("T-Unit", &input, &output, vec![]);
        Ok((Subst::empty(), Type::Unit, eff, tree))
    }

    /// T-Var: x : σ ∈ Γ    τ = inst(σ)
    ///        ─────────────────────────
    ///               Γ ⊢ x : τ ! ε
    fn infer_var(
        &mut self,
        env: &Env,
        expr: &Expr,
        name: &str,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        match env.get(name) {
            Some(scheme) => {
                let ty = self.instantiate(scheme);
                let eff = self.fresh_effect();
                let output = self.pretty_judgment(&ty, &eff);
                let tree = InferenceTree::new("T-Var", &input, &output, vec![]);
                Ok((Subst::empty(), ty, eff, tree))
            }
            None => Err(InferenceError::UnboundVariable {
                name: name.to_string(),
            }),
        }
    }

    /// T-Abs: Γ, x : τ_1 ⊢ e : τ_2 ! ε
    ///        ──────────────────────────────
    ///        Γ ⊢ λx. e : τ_1 -[ε]-> τ_2 ! ∅
    fn infer_abs(
        &mut self,
        env: &Env,
        expr: &Expr,
        param: &str,
        body: &Expr,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let p_ty = self.fresh_type();
        let mut env1 = env.clone();
        env1.insert(
            param.to_string(),
            Scheme {
                type_vars: vec![],
                effect_vars: vec![],
                ty: p_ty.clone(),
            },
        );
        let (s, body_ty, body_eff, body_tree) = self.infer(&env1, body)?;
        let arrow = Type::Arrow(
            Box::new(apply_type(&s, &p_ty)),
            Box::new(body_eff),
            Box::new(body_ty),
        );
        // The lambda value itself is pure; the body's effect lives on the
        // arrow.
        let outer_eff = self.fresh_effect();
        let output = self.pretty_judgment(&arrow, &outer_eff);
        let tree = InferenceTree::new("T-Abs", &input, &output, vec![body_tree]);
        Ok((s, arrow, outer_eff, tree))
    }

    /// T-App: Γ ⊢ e_1 : τ_1 -[ε]-> τ_2 ! ε_1    Γ ⊢ e_2 : τ_1 ! ε_2
    ///        ──────────────────────────────────────────────────────
    ///                  Γ ⊢ e_1 e_2 : τ_2 ! ε_1 ∪ ε_2 ∪ ε
    fn infer_app(
        &mut self,
        env: &Env,
        expr: &Expr,
        func: &Expr,
        arg: &Expr,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let result_ty = self.fresh_type();
        let call_eff = self.fresh_effect();
        let (s1, f_ty, e1, tree1) = self.infer(env, func)?;
        let env1 = apply_env(&s1, env);
        let (s2, a_ty, e2, tree2) = self.infer(&env1, arg)?;
        let f_ty = apply_type(&s2, &f_ty);
        let expected = Type::Arrow(
            Box::new(a_ty),
            Box::new(call_eff.clone()),
            Box::new(result_ty.clone()),
        );
        let (s3, unify_tree) = self.unify(&f_ty, &expected)?;
        let s = s3.compose(&s2.compose(&s1));
        let (merged, merge_tree) = self.merge_effects(&[
            apply_effect(&s, &e1),
            apply_effect(&s, &e2),
            apply_effect(&s, &call_eff),
        ])?;
        let s_final = merged.subst.compose(&s);
        let final_ty = apply_type(&s_final, &result_ty);
        let final_eff = apply_effect(&s_final, &merged.effect);
        let output = self.pretty_judgment(&final_ty, &final_eff);
        let mut children = vec![tree1, tree2, unify_tree];
        children.extend(merge_tree);
        let tree = InferenceTree::new("T-App", &input, &output, children);
        Ok((s_final, final_ty, final_eff, tree))
    }

    /// T-Let: Γ ⊢ e_1 : τ_1 ! ε_1    σ = gen(Γ, τ_1)    Γ, x : σ ⊢ e_2 : τ_2 !
    /// ε_2
    ///        ───────────────────────────────────────────────────────────────────
    ///              Γ ⊢ let x = e_1 in e_2 : τ_2 ! ε_1 ∪ ε_2
    ///
    /// Value restriction applies: only syntactic values are generalized.
    fn infer_let(
        &mut self,
        env: &Env,
        expr: &Expr,
        name: &str,
        value: &Expr,
        body: &Expr,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let (s1, v_ty, e1, tree1) = self.infer(env, value)?;
        let env1 = apply_env(&s1, env);
        // Syntactic value restriction: only generalize when the bound
        // expression is a value form (Lit, Var, Abs). Effectful forms
        // (App, Perform, Handle, nested Let) keep a monomorphic type
        // so that re-using the binding doesn't re-execute effects.
        let scheme = if is_value(value) {
            self.generalize(&env1, &v_ty)
        } else {
            Scheme {
                type_vars: vec![],
                effect_vars: vec![],
                ty: v_ty.clone(),
            }
        };
        let mut env2 = env1;
        env2.insert(name.to_string(), scheme);
        let (s2, b_ty, e2, tree2) = self.infer(&env2, body)?;
        let s = s2.compose(&s1);
        let (merged, merge_tree) =
            self.merge_effects(&[apply_effect(&s, &e1), apply_effect(&s, &e2)])?;
        let s_final = merged.subst.compose(&s);
        let final_ty = apply_type(&s_final, &b_ty);
        let final_eff = apply_effect(&s_final, &merged.effect);
        let output = self.pretty_judgment(&final_ty, &final_eff);
        let mut children = vec![tree1, tree2];
        children.extend(merge_tree);
        let tree = InferenceTree::new("T-Let", &input, &output, children);
        Ok((s_final, final_ty, final_eff, tree))
    }

    /// T-Perform: op : τ_1 → τ_2    Γ ⊢ e : τ_1 ! ε
    ///            ────────────────────────────────────
    ///            Γ ⊢ perform op e : τ_2 ! op | ε
    fn infer_perform(
        &mut self,
        env: &Env,
        expr: &Expr,
        op: &str,
        e: &Expr,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let (param_ty, ret_ty, label) = self.op_instance(op)?;
        let (s1, arg_ty, arg_eff, tree1) = self.infer(env, e)?;
        let (s2, unify_tree) = self.unify(&apply_type(&s1, &arg_ty), &param_ty)?;
        let s = s2.compose(&s1);
        let eff = Effect::Extend(label, Box::new(apply_effect(&s, &arg_eff)));
        let ret = apply_type(&s, &ret_ty);
        let output = self.pretty_judgment(&ret, &eff);
        let tree = InferenceTree::new("T-Perform", &input, &output, vec![tree1, unify_tree]);
        Ok((s, ret, eff, tree))
    }

    /// T-Handle: Γ ⊢ e : τ ! op | ε    Γ, x : τ_1, k : τ_2 -[ε]-> τ ⊢ body : τ
    /// ! ε
    ///           ──────────────────────────────────────────────────────────────────
    ///           Γ ⊢ handle e with op x k -> body : τ ! ε
    #[allow(clippy::too_many_arguments)]
    fn infer_handle(
        &mut self,
        env: &Env,
        expr: &Expr,
        body: &Expr,
        op: &str,
        param: &str,
        resume: &str,
        handler: &Expr,
    ) -> Result<(Subst, Type, Effect, InferenceTree)> {
        let input = format!("{} ⊢ {} ⇒", self.pretty_env(env), expr);
        let (param_ty, op_ret_ty, label) = self.op_instance(op)?;
        let (s1, body_ty, body_eff, tree_body) = self.infer(env, body)?;

        // Strip `label` from the body's effect row, leaving the residual
        // `tail` effects.
        let (tail_eff, s_strip, _) = self.rewrite_effect(&body_eff, &label)?;
        let s2 = s_strip.compose(&s1);

        // In the handler body: param has the op's parameter type, and resume
        // is op_ret -[tail]-> body_ty.
        let mut env_h = apply_env(&s2, env);
        env_h.insert(
            param.to_string(),
            Scheme {
                type_vars: vec![],
                effect_vars: vec![],
                ty: apply_type(&s2, &param_ty),
            },
        );
        let resume_ty = Type::Arrow(
            Box::new(apply_type(&s2, &op_ret_ty)),
            Box::new(apply_effect(&s2, &tail_eff)),
            Box::new(apply_type(&s2, &body_ty)),
        );
        env_h.insert(
            resume.to_string(),
            Scheme {
                type_vars: vec![],
                effect_vars: vec![],
                ty: resume_ty,
            },
        );

        let (s3, h_ty, h_eff, tree_handler) = self.infer(&env_h, handler)?;
        let (s4, unify_ty_tree) = self.unify(&h_ty, &apply_type(&s3.compose(&s2), &body_ty))?;
        let s_th = s4.compose(&s3.compose(&s2));
        let (s5, unify_eff_tree) = self.unify_effect(
            &apply_effect(&s_th, &h_eff),
            &apply_effect(&s_th, &tail_eff),
        )?;
        let s = s5.compose(&s_th);
        let final_ty = apply_type(&s, &body_ty);
        let final_eff = apply_effect(&s, &tail_eff);
        let output = self.pretty_judgment(&final_ty, &final_eff);
        let tree = InferenceTree::new(
            "T-Handle",
            &input,
            &output,
            vec![tree_body, tree_handler, unify_ty_tree, unify_eff_tree],
        );
        Ok((s, final_ty, final_eff, tree))
    }

    // Merge n effect rows into one fresh row, unifying each with the merged
    // result. This collapses {ε1, ε2, ε3} into a single ε via repeated
    // unification — with scoped labels, the result preserves duplicates that
    // came from different sources.
    fn merge_effects(&mut self, effs: &[Effect]) -> Result<(MergedEffect, Vec<InferenceTree>)> {
        let merged_var = self.fresh_effect_var();
        let mut subst = Subst::empty();
        let mut current = Effect::Var(merged_var.clone());
        let mut trees = Vec::new();
        for eff in effs {
            let (s, tree) =
                self.unify_effect(&apply_effect(&subst, eff), &apply_effect(&subst, &current))?;
            subst = s.compose(&subst);
            current = apply_effect(&subst, &current);
            trees.push(tree);
        }
        Ok((
            MergedEffect {
                subst,
                effect: current,
            },
            trees,
        ))
    }
}

struct MergedEffect {
    subst: Subst,
    effect: Effect,
}

fn is_value(e: &Expr) -> bool {
    matches!(e, Expr::Lit(_) | Expr::Var(_) | Expr::Abs(_, _))
}

fn effect_tail(eff: &Effect) -> Option<&str> {
    match eff {
        Effect::Empty => None,
        Effect::Var(n) => Some(n),
        Effect::Extend(_, rest) => effect_tail(rest),
    }
}

pub fn run_inference(expr: &Expr) -> Result<InferenceTree> {
    let mut inf = TypeInference::new();
    let env = Env::new();
    let (_, _, _, tree) = inf.infer(&env, expr)?;
    Ok(tree)
}

pub fn infer_type(expr: &Expr) -> Result<(Type, Effect)> {
    let mut inf = TypeInference::new();
    let env = Env::new();
    let (s, ty, eff, _) = inf.infer(&env, expr)?;
    let final_ty = apply_type(&s, &ty);
    let final_eff = apply_effect(&s, &eff);
    // Close any effect variables that survived inference. A free tail variable
    // means "no further labels required"; collapsing it to Empty hides the
    // polymorphic plumbing introduced for sequencing pure subterms.
    let closer = close_effect_vars(&final_ty, &final_eff);
    let closed_ty = apply_type(&closer, &final_ty);
    let closed_eff = apply_effect(&closer, &final_eff);
    let scheme = inf.generalize(&env, &closed_ty);
    let renamed_ty = rename_scheme(&scheme, &closed_eff);
    Ok(renamed_ty)
}

fn close_effect_vars(ty: &Type, eff: &Effect) -> Subst {
    let mut fv = FreeVars::default();
    ftv_type(ty, &mut fv);
    ftv_effect(eff, &mut fv);
    let mut s = Subst::empty();
    for v in fv.effects {
        s.effects.insert(v, Effect::Empty);
    }
    s
}

// Pretty names: a..z for type vars, e..k for effect vars.
fn rename_scheme(scheme: &Scheme, eff: &Effect) -> (Type, Effect) {
    let mut subst = Subst::empty();
    let mut letters = ('a'..='z').map(|c| c.to_string());
    for v in &scheme.type_vars {
        let fresh = letters.next().unwrap_or_else(|| v.clone());
        subst.types.insert(v.clone(), Type::Var(fresh));
    }
    let mut eff_letters = ('e'..='m').map(|c| c.to_string());
    let mut all_eff_vars: Vec<String> = scheme.effect_vars.clone();
    let mut top_eff = FreeVars::default();
    ftv_effect(eff, &mut top_eff);
    for v in top_eff.effects {
        if !all_eff_vars.contains(&v) {
            all_eff_vars.push(v);
        }
    }
    all_eff_vars.sort();
    for v in &all_eff_vars {
        let fresh = eff_letters.next().unwrap_or_else(|| v.clone());
        subst.effects.insert(v.clone(), Effect::Var(fresh));
    }
    (apply_type(&subst, &scheme.ty), apply_effect(&subst, eff))
}
