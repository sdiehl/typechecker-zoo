use std::collections::{BTreeMap, HashMap, HashSet};

use crate::ast::{Expr, Lit, Row, Scheme, Type};
use crate::errors::{InferenceError, Result};

pub type Env = BTreeMap<String, Scheme>;

#[derive(Debug, Clone, Default)]
pub struct Subst {
    pub types: HashMap<String, Type>,
    pub rows: HashMap<String, Row>,
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

    fn singleton_row(var: String, row: Row) -> Self {
        let mut s = Self::empty();
        s.rows.insert(var, row);
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
        let mut rows: HashMap<String, Row> = s1
            .rows
            .iter()
            .map(|(k, v)| (k.clone(), apply_row(self, v)))
            .collect();
        for (k, v) in &self.rows {
            rows.entry(k.clone()).or_insert_with(|| v.clone());
        }
        Subst { types, rows }
    }
}

fn apply_type(s: &Subst, ty: &Type) -> Type {
    match ty {
        Type::Var(name) => match s.types.get(name) {
            Some(t) => apply_type(s, t),
            None => ty.clone(),
        },
        Type::Int | Type::Bool => ty.clone(),
        Type::Arrow(a, b) => Type::Arrow(Box::new(apply_type(s, a)), Box::new(apply_type(s, b))),
        Type::Record(r) => Type::Record(Box::new(apply_row(s, r))),
    }
}

fn apply_row(s: &Subst, row: &Row) -> Row {
    match row {
        Row::Empty => Row::Empty,
        Row::Var(name) => match s.rows.get(name) {
            Some(r) => apply_row(s, r),
            None => row.clone(),
        },
        Row::Extend(l, t, r) => Row::Extend(
            l.clone(),
            Box::new(apply_type(s, t)),
            Box::new(apply_row(s, r)),
        ),
    }
}

fn apply_scheme(s: &Subst, scheme: &Scheme) -> Scheme {
    let mut filtered = s.clone();
    for v in &scheme.type_vars {
        filtered.types.remove(v);
    }
    for v in &scheme.row_vars {
        filtered.rows.remove(v);
    }
    Scheme {
        type_vars: scheme.type_vars.clone(),
        row_vars: scheme.row_vars.clone(),
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
    rows: HashSet<String>,
}

fn ftv_type(ty: &Type, out: &mut FreeVars) {
    match ty {
        Type::Var(n) => {
            out.types.insert(n.clone());
        }
        Type::Int | Type::Bool => {}
        Type::Arrow(a, b) => {
            ftv_type(a, out);
            ftv_type(b, out);
        }
        Type::Record(r) => ftv_row(r, out),
    }
}

fn ftv_row(row: &Row, out: &mut FreeVars) {
    match row {
        Row::Empty => {}
        Row::Var(n) => {
            out.rows.insert(n.clone());
        }
        Row::Extend(_, t, rest) => {
            ftv_type(t, out);
            ftv_row(rest, out);
        }
    }
}

fn ftv_scheme(s: &Scheme) -> FreeVars {
    let mut fv = FreeVars::default();
    ftv_type(&s.ty, &mut fv);
    for v in &s.type_vars {
        fv.types.remove(v);
    }
    for v in &s.row_vars {
        fv.rows.remove(v);
    }
    fv
}

fn ftv_env(env: &Env) -> FreeVars {
    let mut fv = FreeVars::default();
    for s in env.values() {
        let s_fv = ftv_scheme(s);
        fv.types.extend(s_fv.types);
        fv.rows.extend(s_fv.rows);
    }
    fv
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

    fn fresh_row(&mut self) -> Row {
        let v = format!("r{}", self.counter);
        self.counter += 1;
        Row::Var(v)
    }

    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut s = Subst::empty();
        for v in &scheme.type_vars {
            s.types.insert(v.clone(), self.fresh_type());
        }
        for v in &scheme.row_vars {
            s.rows.insert(v.clone(), self.fresh_row());
        }
        apply_type(&s, &scheme.ty)
    }

    fn generalize(&self, env: &Env, ty: &Type) -> Scheme {
        let mut fv = FreeVars::default();
        ftv_type(ty, &mut fv);
        let env_fv = ftv_env(env);
        let mut tvars: Vec<_> = fv.types.difference(&env_fv.types).cloned().collect();
        let mut rvars: Vec<_> = fv.rows.difference(&env_fv.rows).cloned().collect();
        tvars.sort();
        rvars.sort();
        Scheme {
            type_vars: tvars,
            row_vars: rvars,
            ty: ty.clone(),
        }
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Subst> {
        match (t1, t2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => Ok(Subst::empty()),
            (Type::Var(a), Type::Var(b)) if a == b => Ok(Subst::empty()),
            (Type::Var(v), other) | (other, Type::Var(v)) => {
                let mut fv = FreeVars::default();
                ftv_type(other, &mut fv);
                if fv.types.contains(v) {
                    Err(InferenceError::OccursCheck {
                        var: v.clone(),
                        ty: other.clone(),
                    })
                } else {
                    Ok(Subst::singleton_type(v.clone(), other.clone()))
                }
            }
            (Type::Arrow(a1, b1), Type::Arrow(a2, b2)) => {
                let s1 = self.unify(a1, a2)?;
                let s2 = self.unify(&apply_type(&s1, b1), &apply_type(&s1, b2))?;
                Ok(s2.compose(&s1))
            }
            (Type::Record(r1), Type::Record(r2)) => self.unify_row(r1, r2),
            _ => Err(InferenceError::UnificationFailure {
                expected: t1.clone(),
                actual: t2.clone(),
            }),
        }
    }

    // Leijen Fig. 2 (uni-row): rewrite the RHS so the head label of LHS comes
    // first, then unify field types and tails. The side condition `tail(r) ∉
    // dom(θ1)` rules out non-terminating cases like
    //   \r -> if c then {x=1|r} else {y=2|r}
    fn unify_row(&mut self, r1: &Row, r2: &Row) -> Result<Subst> {
        match (r1, r2) {
            (Row::Empty, Row::Empty) => Ok(Subst::empty()),
            (Row::Var(a), Row::Var(b)) if a == b => Ok(Subst::empty()),
            (Row::Var(v), other) | (other, Row::Var(v)) => {
                let mut fv = FreeVars::default();
                ftv_row(other, &mut fv);
                if fv.rows.contains(v) {
                    Err(InferenceError::RowOccursCheck {
                        var: v.clone(),
                        row: other.clone(),
                    })
                } else {
                    Ok(Subst::singleton_row(v.clone(), other.clone()))
                }
            }
            (Row::Extend(l, t1, rest1), other) => {
                let (t2, rest2, s1) = self.rewrite_row(other, l)?;
                if let Some(tv) = row_tail(rest1) {
                    if s1.rows.contains_key(tv) {
                        return Err(InferenceError::RecursiveRow {
                            var: tv.to_string(),
                        });
                    }
                }
                let s2 = self.unify(&apply_type(&s1, t1), &apply_type(&s1, &t2))?;
                let s12 = s2.compose(&s1);
                let s3 = self.unify_row(&apply_row(&s12, rest1), &apply_row(&s12, &rest2))?;
                Ok(s3.compose(&s12))
            }
            (Row::Empty, Row::Extend(l, _, _)) => Err(InferenceError::MissingLabel {
                label: l.clone(),
                row: Row::Empty,
            }),
        }
    }

    // Leijen Fig. 3: r ≃ (l :: τ | s) : θ. Hoists label `l` to the head of
    // `r`, returning the field type, the remainder, and the substitution.
    fn rewrite_row(&mut self, row: &Row, label: &str) -> Result<(Type, Row, Subst)> {
        match row {
            Row::Empty => Err(InferenceError::MissingLabel {
                label: label.to_string(),
                row: Row::Empty,
            }),
            Row::Extend(l, t, rest) if l == label => {
                Ok((*t.clone(), *rest.clone(), Subst::empty()))
            }
            Row::Extend(l, t, rest) => {
                let (t2, rest2, s) = self.rewrite_row(rest, label)?;
                Ok((t2, Row::Extend(l.clone(), t.clone(), Box::new(rest2)), s))
            }
            Row::Var(alpha) => {
                let beta = self.fresh_row();
                let gamma = self.fresh_type();
                let new_row = Row::Extend(
                    label.to_string(),
                    Box::new(gamma.clone()),
                    Box::new(beta.clone()),
                );
                let s = Subst::singleton_row(alpha.clone(), new_row);
                Ok((gamma, beta, s))
            }
        }
    }

    pub fn infer(&mut self, env: &Env, expr: &Expr) -> Result<(Subst, Type)> {
        match expr {
            Expr::Lit(Lit::Int(_)) => Ok((Subst::empty(), Type::Int)),
            Expr::Lit(Lit::Bool(_)) => Ok((Subst::empty(), Type::Bool)),
            Expr::Var(name) => match env.get(name) {
                Some(scheme) => Ok((Subst::empty(), self.instantiate(scheme))),
                None => Err(InferenceError::UnboundVariable { name: name.clone() }),
            },
            Expr::Lam(param, body) => {
                let param_ty = self.fresh_type();
                let mut new_env = env.clone();
                new_env.insert(
                    param.clone(),
                    Scheme {
                        type_vars: vec![],
                        row_vars: vec![],
                        ty: param_ty.clone(),
                    },
                );
                let (s, body_ty) = self.infer(&new_env, body)?;
                let result = Type::Arrow(Box::new(apply_type(&s, &param_ty)), Box::new(body_ty));
                Ok((s, result))
            }
            Expr::App(f, a) => {
                let result_ty = self.fresh_type();
                let (s1, f_ty) = self.infer(env, f)?;
                let env1 = apply_env(&s1, env);
                let (s2, a_ty) = self.infer(&env1, a)?;
                let expected = Type::Arrow(Box::new(a_ty), Box::new(result_ty.clone()));
                let s3 = self.unify(&apply_type(&s2, &f_ty), &expected)?;
                let s = s3.compose(&s2.compose(&s1));
                Ok((s.clone(), apply_type(&s, &result_ty)))
            }
            Expr::Let(name, value, body) => {
                let (s1, value_ty) = self.infer(env, value)?;
                let env1 = apply_env(&s1, env);
                let scheme = self.generalize(&env1, &value_ty);
                let mut env2 = env1;
                env2.insert(name.clone(), scheme);
                let (s2, body_ty) = self.infer(&env2, body)?;
                Ok((s2.compose(&s1), body_ty))
            }
            Expr::EmptyRecord => Ok((Subst::empty(), Type::Record(Box::new(Row::Empty)))),
            Expr::Extend(label, value, rest) => {
                let row_var = match self.fresh_row() {
                    Row::Var(v) => v,
                    _ => unreachable!(),
                };
                let (s1, v_ty) = self.infer(env, value)?;
                let env1 = apply_env(&s1, env);
                let (s2, r_ty) = self.infer(&env1, rest)?;
                let expected = Type::Record(Box::new(Row::Var(row_var.clone())));
                let s3 = self.unify(&apply_type(&s2, &r_ty), &expected)?;
                let s = s3.compose(&s2.compose(&s1));
                let v_final = apply_type(&s, &v_ty);
                let row = Row::Extend(
                    label.clone(),
                    Box::new(v_final),
                    Box::new(apply_row(&s, &Row::Var(row_var))),
                );
                Ok((s, Type::Record(Box::new(row))))
            }
            // (.l) : ∀rα. {l :: α | r} → α  (Leijen §3.1)
            Expr::Select(record, label) => {
                let field_ty = self.fresh_type();
                let rest = self.fresh_row();
                let (s1, r_ty) = self.infer(env, record)?;
                let expected = Type::Record(Box::new(Row::Extend(
                    label.clone(),
                    Box::new(field_ty.clone()),
                    Box::new(rest),
                )));
                let s2 = self.unify(&r_ty, &expected)?;
                let s = s2.compose(&s1);
                Ok((s.clone(), apply_type(&s, &field_ty)))
            }
            // (- l) : ∀rα. {l :: α | r} → {r}
            Expr::Restrict(record, label) => {
                let field_ty = self.fresh_type();
                let rest_var = match self.fresh_row() {
                    Row::Var(v) => v,
                    _ => unreachable!(),
                };
                let (s1, r_ty) = self.infer(env, record)?;
                let expected = Type::Record(Box::new(Row::Extend(
                    label.clone(),
                    Box::new(field_ty),
                    Box::new(Row::Var(rest_var.clone())),
                )));
                let s2 = self.unify(&r_ty, &expected)?;
                let s = s2.compose(&s1);
                let result = Type::Record(Box::new(apply_row(&s, &Row::Var(rest_var))));
                Ok((s, result))
            }
        }
    }
}

fn row_tail(row: &Row) -> Option<&str> {
    match row {
        Row::Empty => None,
        Row::Var(name) => Some(name),
        Row::Extend(_, _, rest) => row_tail(rest),
    }
}

pub fn infer_type(expr: &Expr) -> Result<Type> {
    let mut inf = TypeInference::new();
    let env = Env::new();
    let (s, ty) = inf.infer(&env, expr)?;
    let final_ty = apply_type(&s, &ty);
    let scheme = inf.generalize(&env, &final_ty);
    Ok(rename_scheme(&scheme))
}

// Pretty-friendly variable names for the final scheme (a, b, c, ... and r, s,
// t, ...).
fn rename_scheme(scheme: &Scheme) -> Type {
    let mut subst = Subst::empty();
    let mut letters = ('a'..='z').map(|c| c.to_string());
    for v in &scheme.type_vars {
        let fresh = letters.next().unwrap_or_else(|| v.clone());
        subst.types.insert(v.clone(), Type::Var(fresh));
    }
    let mut rletters = ('r'..='z').map(|c| c.to_string());
    for v in &scheme.row_vars {
        let fresh = rletters.next().unwrap_or_else(|| v.clone());
        subst.rows.insert(v.clone(), Row::Var(fresh));
    }
    apply_type(&subst, &scheme.ty)
}
