use std::collections::{HashMap, HashSet};

use crate::ast::{Name, Pred, Qual, Scheme, Type};
use crate::errors::{InferenceError, Result};

#[derive(Debug, Clone, Default)]
pub struct Subst(pub HashMap<Name, Type>);

impl Subst {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn singleton(v: Name, t: Type) -> Self {
        let mut m = HashMap::new();
        m.insert(v, t);
        Self(m)
    }

    pub fn compose(&self, other: &Self) -> Self {
        let mut out: HashMap<Name, Type> = other
            .0
            .iter()
            .map(|(k, v)| (k.clone(), apply_type(self, v)))
            .collect();
        for (k, v) in &self.0 {
            out.entry(k.clone()).or_insert_with(|| v.clone());
        }
        Subst(out)
    }

    pub fn merge(&self, other: &Self) -> Result<Self> {
        for (k, v) in &self.0 {
            if let Some(w) = other.0.get(k) {
                if v != w {
                    return Err(InferenceError::UnificationFailure {
                        expected: v.clone(),
                        actual: w.clone(),
                    });
                }
            }
        }
        let mut out = self.0.clone();
        for (k, v) in &other.0 {
            out.entry(k.clone()).or_insert_with(|| v.clone());
        }
        Ok(Subst(out))
    }
}

pub fn apply_type(s: &Subst, ty: &Type) -> Type {
    match ty {
        Type::Var(n) => match s.0.get(n) {
            Some(t) => t.clone(),
            None => ty.clone(),
        },
        Type::Con(n, args) => Type::Con(n.clone(), args.iter().map(|a| apply_type(s, a)).collect()),
        Type::Arrow(a, b) => Type::Arrow(Box::new(apply_type(s, a)), Box::new(apply_type(s, b))),
    }
}

pub fn apply_pred(s: &Subst, p: &Pred) -> Pred {
    Pred {
        class: p.class.clone(),
        ty: apply_type(s, &p.ty),
    }
}

pub fn apply_preds(s: &Subst, ps: &[Pred]) -> Vec<Pred> {
    ps.iter().map(|p| apply_pred(s, p)).collect()
}

pub fn apply_qual(s: &Subst, q: &Qual) -> Qual {
    Qual {
        preds: apply_preds(s, &q.preds),
        ty: apply_type(s, &q.ty),
    }
}

pub fn apply_scheme(s: &Subst, sch: &Scheme) -> Scheme {
    let mut filtered = s.clone();
    for v in &sch.vars {
        filtered.0.remove(v);
    }
    Scheme {
        vars: sch.vars.clone(),
        qual: apply_qual(&filtered, &sch.qual),
    }
}

pub fn ftv_type(ty: &Type, out: &mut HashSet<Name>) {
    match ty {
        Type::Var(n) => {
            out.insert(n.clone());
        }
        Type::Con(_, args) => {
            for a in args {
                ftv_type(a, out);
            }
        }
        Type::Arrow(a, b) => {
            ftv_type(a, out);
            ftv_type(b, out);
        }
    }
}

pub fn ftv_pred(p: &Pred, out: &mut HashSet<Name>) {
    ftv_type(&p.ty, out);
}

pub fn ftv_qual(q: &Qual) -> HashSet<Name> {
    let mut s = HashSet::new();
    ftv_type(&q.ty, &mut s);
    for p in &q.preds {
        ftv_pred(p, &mut s);
    }
    s
}

pub fn ftv_scheme(sch: &Scheme) -> HashSet<Name> {
    let mut s = ftv_qual(&sch.qual);
    for v in &sch.vars {
        s.remove(v);
    }
    s
}

fn occurs(v: &Name, ty: &Type) -> bool {
    let mut s = HashSet::new();
    ftv_type(ty, &mut s);
    s.contains(v)
}

pub fn unify(a: &Type, b: &Type) -> Result<Subst> {
    match (a, b) {
        (Type::Var(v), t) | (t, Type::Var(v)) => bind(v, t),
        (Type::Con(n1, a1), Type::Con(n2, a2)) => {
            if n1 != n2 {
                return Err(InferenceError::ConMismatch {
                    left: n1.clone(),
                    right: n2.clone(),
                });
            }
            if a1.len() != a2.len() {
                return Err(InferenceError::ArityMismatch {
                    name: n1.clone(),
                    expected: a1.len(),
                    actual: a2.len(),
                });
            }
            unify_zip(a1, a2)
        }
        (Type::Arrow(p1, r1), Type::Arrow(p2, r2)) => {
            let s1 = unify(p1, p2)?;
            let s2 = unify(&apply_type(&s1, r1), &apply_type(&s1, r2))?;
            Ok(s2.compose(&s1))
        }
        _ => Err(InferenceError::UnificationFailure {
            expected: a.clone(),
            actual: b.clone(),
        }),
    }
}

fn unify_zip(xs: &[Type], ys: &[Type]) -> Result<Subst> {
    let mut s = Subst::empty();
    for (x, y) in xs.iter().zip(ys.iter()) {
        let s1 = unify(&apply_type(&s, x), &apply_type(&s, y))?;
        s = s1.compose(&s);
    }
    Ok(s)
}

fn bind(v: &Name, t: &Type) -> Result<Subst> {
    if let Type::Var(u) = t {
        if u == v {
            return Ok(Subst::empty());
        }
    }
    if occurs(v, t) {
        return Err(InferenceError::OccursCheck {
            var: v.clone(),
            ty: t.clone(),
        });
    }
    Ok(Subst::singleton(v.clone(), t.clone()))
}

pub fn match_ty(pat: &Type, target: &Type) -> Result<Subst> {
    match (pat, target) {
        (Type::Var(v), t) => Ok(Subst::singleton(v.clone(), t.clone())),
        (Type::Con(n1, a1), Type::Con(n2, a2)) if n1 == n2 && a1.len() == a2.len() => {
            let mut s = Subst::empty();
            for (x, y) in a1.iter().zip(a2.iter()) {
                let s1 = match_ty(x, y)?;
                s = s.merge(&s1)?;
            }
            Ok(s)
        }
        (Type::Arrow(p1, r1), Type::Arrow(p2, r2)) => {
            let s1 = match_ty(p1, p2)?;
            let s2 = match_ty(r1, r2)?;
            s1.merge(&s2)
        }
        _ => Err(InferenceError::UnificationFailure {
            expected: pat.clone(),
            actual: target.clone(),
        }),
    }
}

pub fn match_pred(pat: &Pred, target: &Pred) -> Result<Subst> {
    if pat.class != target.class {
        return Err(InferenceError::UnificationFailure {
            expected: Type::Con(pat.class.clone(), vec![]),
            actual: Type::Con(target.class.clone(), vec![]),
        });
    }
    match_ty(&pat.ty, &target.ty)
}
