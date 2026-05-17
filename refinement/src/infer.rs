use std::collections::HashMap;

use crate::ast::{BaseTy, BinOp, Expr, Pred, Type};
use crate::errors::{InferenceError, Result};
use crate::smt::{check_valid, Sort, Validity};

#[derive(Debug, Clone, Default)]
pub struct Env {
    bindings: Vec<(String, Type)>,
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    fn extend(&self, x: String, t: Type) -> Self {
        let mut e = self.clone();
        e.bindings.push((x, t));
        e
    }

    fn lookup(&self, x: &str) -> Option<&Type> {
        self.bindings
            .iter()
            .rev()
            .find(|(n, _)| n == x)
            .map(|(_, t)| t)
    }

    fn smt_bindings(&self) -> Vec<(String, Sort, Pred)> {
        let mut out = Vec::new();
        for (name, t) in &self.bindings {
            if let Type::Refine(v, b, p) = t {
                let sort = match b {
                    BaseTy::Int => Sort::Int,
                    BaseTy::Bool => Sort::Bool,
                };
                let renamed = subst_pred(p, v, &Pred::Var(name.clone()));
                out.push((name.clone(), sort, renamed));
            }
        }
        out
    }
}

pub fn subst_pred(p: &Pred, x: &str, with: &Pred) -> Pred {
    match p {
        Pred::Var(y) if y == x => with.clone(),
        Pred::Var(_) | Pred::Bool(_) | Pred::Int(_) => p.clone(),
        Pred::Not(a) => Pred::Not(Box::new(subst_pred(a, x, with))),
        Pred::Neg(a) => Pred::Neg(Box::new(subst_pred(a, x, with))),
        Pred::Bin(op, a, b) => Pred::Bin(
            *op,
            Box::new(subst_pred(a, x, with)),
            Box::new(subst_pred(b, x, with)),
        ),
    }
}

fn subst_ty(t: &Type, x: &str, with: &Pred) -> Type {
    match t {
        Type::Refine(v, b, p) => {
            if v == x {
                t.clone()
            } else {
                Type::Refine(v.clone(), b.clone(), subst_pred(p, x, with))
            }
        }
        Type::Fun(y, t1, t2) => {
            let t1n = subst_ty(t1, x, with);
            let t2n = if y == x {
                (**t2).clone()
            } else {
                subst_ty(t2, x, with)
            };
            Type::Fun(y.clone(), Box::new(t1n), Box::new(t2n))
        }
    }
}

fn and_pred(a: Pred, b: Pred) -> Pred {
    match (a, b) {
        (Pred::Bool(true), p) | (p, Pred::Bool(true)) => p,
        (a, b) => Pred::Bin(BinOp::And, Box::new(a), Box::new(b)),
    }
}

// Try to translate a pure expression to a logical predicate term.
// Returns None for expressions we cannot reflect (e.g. function applications,
// lambdas, lets, or unknown variables of non-base sort).
pub fn reflect(env: &Env, e: &Expr) -> Option<Pred> {
    match e {
        Expr::Int(n) => Some(Pred::Int(*n)),
        Expr::Bool(b) => Some(Pred::Bool(*b)),
        Expr::Var(x) => match env.lookup(x)? {
            Type::Refine(_, _, _) => Some(Pred::Var(x.clone())),
            _ => None,
        },
        Expr::Neg(a) => Some(Pred::Neg(Box::new(reflect(env, a)?))),
        Expr::Not(a) => Some(Pred::Not(Box::new(reflect(env, a)?))),
        Expr::Bin(op, a, b) => {
            let ap = reflect(env, a)?;
            let bp = reflect(env, b)?;
            Some(Pred::Bin(*op, Box::new(ap), Box::new(bp)))
        }
        Expr::If(c, t, f) => {
            // if c then t else f  ===  (c && t) || (!c && f)  when reflected
            let cp = reflect(env, c)?;
            let tp = reflect(env, t)?;
            let fp = reflect(env, f)?;
            let left = Pred::Bin(BinOp::And, Box::new(cp.clone()), Box::new(tp));
            let right = Pred::Bin(BinOp::And, Box::new(Pred::Not(Box::new(cp))), Box::new(fp));
            Some(Pred::Bin(BinOp::Or, Box::new(left), Box::new(right)))
        }
        _ => None,
    }
}

pub struct Checker {
    fresh: usize,
    prims: HashMap<String, Type>,
}

impl Default for Checker {
    fn default() -> Self {
        Self::new()
    }
}

impl Checker {
    pub fn new() -> Self {
        let mut prims = HashMap::new();
        // div : (x : Int) -> { y : Int | y != 0 } -> Int
        let nonzero = Type::Refine(
            "y".into(),
            BaseTy::Int,
            Pred::Bin(
                BinOp::Ne,
                Box::new(Pred::Var("y".into())),
                Box::new(Pred::Int(0)),
            ),
        );
        let div_ty = Type::Fun(
            "_x".into(),
            Box::new(Type::base(BaseTy::Int)),
            Box::new(Type::Fun(
                "_y".into(),
                Box::new(nonzero.clone()),
                Box::new(Type::base(BaseTy::Int)),
            )),
        );
        prims.insert("div".to_string(), div_ty);
        let mod_ty = Type::Fun(
            "_x".into(),
            Box::new(Type::base(BaseTy::Int)),
            Box::new(Type::Fun(
                "_y".into(),
                Box::new(nonzero),
                Box::new(Type::base(BaseTy::Int)),
            )),
        );
        prims.insert("mod".to_string(), mod_ty);
        Self { fresh: 0, prims }
    }

    fn gensym(&mut self, base: &str) -> String {
        let n = self.fresh;
        self.fresh += 1;
        format!("{}_{}", base, n)
    }

    pub fn synth(&mut self, env: &Env, path: &Pred, e: &Expr) -> Result<Type> {
        match e {
            Expr::Int(n) => Ok(singleton_int(*n)),
            Expr::Bool(b) => Ok(singleton_bool(*b)),
            Expr::Var(x) => {
                if let Some(t) = env.lookup(x) {
                    Ok(self_refine(x, t))
                } else if let Some(t) = self.prims.get(x) {
                    Ok(t.clone())
                } else {
                    Err(InferenceError::UnboundVariable { name: x.clone() })
                }
            }
            Expr::Ann(inner, t) => {
                self.check(env, path, inner, t)?;
                Ok(t.clone())
            }
            Expr::App(f, a) => {
                let ft = self.synth(env, path, f)?;
                let (x, t1, t2) = match ft {
                    Type::Fun(x, t1, t2) => (x, *t1, *t2),
                    other => return Err(InferenceError::NotAFunction { found: other }),
                };
                self.check(env, path, a, &t1)?;
                if let Some(arg_pred) = reflect(env, a) {
                    Ok(subst_ty(&t2, &x, &arg_pred))
                } else {
                    Ok(t2)
                }
            }
            Expr::Lam(x, t1, body) => {
                let body_env = env.extend(x.clone(), t1.clone());
                let t2 = self.synth(&body_env, path, body)?;
                Ok(Type::Fun(x.clone(), Box::new(t1.clone()), Box::new(t2)))
            }
            Expr::Let(x, v, body) => {
                let tv = self.synth(env, path, v)?;
                let new_env = env.extend(x.clone(), tv.clone());
                let new_path = if let (Type::Refine(_, _, _), Some(vp)) = (&tv, reflect(env, v)) {
                    and_pred(
                        path.clone(),
                        Pred::Bin(BinOp::Eq, Box::new(Pred::Var(x.clone())), Box::new(vp)),
                    )
                } else {
                    path.clone()
                };
                self.synth(&new_env, &new_path, body)
            }
            Expr::If(c, a, b) => {
                self.check(env, path, c, &Type::base(BaseTy::Bool))?;
                let cp = reflect(env, c).unwrap_or(Pred::Bool(true));
                let path_a = and_pred(path.clone(), cp.clone());
                let path_b = and_pred(path.clone(), Pred::Not(Box::new(cp)));
                let ta = self.synth(env, &path_a, a)?;
                let tb = self.synth(env, &path_b, b)?;
                // Return whichever is the more general base type; require both are base.
                match (&ta, &tb) {
                    (Type::Refine(_, ba, _), Type::Refine(_, bb, _)) if ba == bb => {
                        Ok(Type::base(ba.clone()))
                    }
                    _ => Err(InferenceError::Mismatch {
                        expected: ta,
                        found: tb,
                    }),
                }
            }
            Expr::Bin(op, a, b) => self.synth_bin(env, path, *op, a, b),
            Expr::Not(a) => {
                self.check(env, path, a, &Type::base(BaseTy::Bool))?;
                if let Some(ap) = reflect(env, a) {
                    let v = "v".to_string();
                    Ok(Type::Refine(
                        v.clone(),
                        BaseTy::Bool,
                        Pred::Bin(
                            BinOp::Eq,
                            Box::new(Pred::Var(v)),
                            Box::new(Pred::Not(Box::new(ap))),
                        ),
                    ))
                } else {
                    Ok(Type::base(BaseTy::Bool))
                }
            }
            Expr::Neg(a) => {
                self.check(env, path, a, &Type::base(BaseTy::Int))?;
                if let Some(ap) = reflect(env, a) {
                    let v = "v".to_string();
                    Ok(Type::Refine(
                        v.clone(),
                        BaseTy::Int,
                        Pred::Bin(
                            BinOp::Eq,
                            Box::new(Pred::Var(v)),
                            Box::new(Pred::Neg(Box::new(ap))),
                        ),
                    ))
                } else {
                    Ok(Type::base(BaseTy::Int))
                }
            }
        }
    }

    fn synth_bin(&mut self, env: &Env, path: &Pred, op: BinOp, a: &Expr, b: &Expr) -> Result<Type> {
        let (arg_b, res_b) = if op.is_arith() {
            (BaseTy::Int, BaseTy::Int)
        } else if op.is_cmp() {
            (BaseTy::Int, BaseTy::Bool)
        } else {
            (BaseTy::Bool, BaseTy::Bool)
        };
        self.check(env, path, a, &Type::base(arg_b.clone()))?;
        self.check(env, path, b, &Type::base(arg_b))?;
        let v = "v".to_string();
        if let (Some(ap), Some(bp)) = (reflect(env, a), reflect(env, b)) {
            let rhs = Pred::Bin(op, Box::new(ap), Box::new(bp));
            Ok(Type::Refine(
                v.clone(),
                res_b,
                Pred::Bin(BinOp::Eq, Box::new(Pred::Var(v)), Box::new(rhs)),
            ))
        } else {
            Ok(Type::base(res_b))
        }
    }

    pub fn check(&mut self, env: &Env, path: &Pred, e: &Expr, t: &Type) -> Result<()> {
        match (e, t) {
            (Expr::Lam(x, t1, body), Type::Fun(y, s1, s2)) => {
                self.subtype(env, path, s1, t1)?;
                let body_env = env.extend(x.clone(), t1.clone());
                let s2n = subst_ty(s2, y, &Pred::Var(x.clone()));
                self.check(&body_env, path, body, &s2n)
            }
            (Expr::Let(x, v, body), _) => {
                let tv = self.synth(env, path, v)?;
                let new_env = env.extend(x.clone(), tv.clone());
                let new_path = if let (Type::Refine(_, _, _), Some(vp)) = (&tv, reflect(env, v)) {
                    and_pred(
                        path.clone(),
                        Pred::Bin(BinOp::Eq, Box::new(Pred::Var(x.clone())), Box::new(vp)),
                    )
                } else {
                    path.clone()
                };
                self.check(&new_env, &new_path, body, t)
            }
            (Expr::If(c, a, b), _) => {
                self.check(env, path, c, &Type::base(BaseTy::Bool))?;
                let cp = reflect(env, c).unwrap_or(Pred::Bool(true));
                let path_a = and_pred(path.clone(), cp.clone());
                let path_b = and_pred(path.clone(), Pred::Not(Box::new(cp)));
                self.check(env, &path_a, a, t)?;
                self.check(env, &path_b, b, t)
            }
            _ => {
                let t_syn = self.synth(env, path, e)?;
                self.subtype(env, path, &t_syn, t)
            }
        }
    }

    pub fn subtype(&mut self, env: &Env, path: &Pred, sub: &Type, sup: &Type) -> Result<()> {
        match (sub, sup) {
            (Type::Refine(v1, b1, p1), Type::Refine(v2, b2, p2)) => {
                if b1 != b2 {
                    return Err(InferenceError::Mismatch {
                        expected: sup.clone(),
                        found: sub.clone(),
                    });
                }
                let nm = self.gensym("v");
                let p1r = subst_pred(p1, v1, &Pred::Var(nm.clone()));
                let p2r = subst_pred(p2, v2, &Pred::Var(nm.clone()));
                let sort = match b1 {
                    BaseTy::Int => Sort::Int,
                    BaseTy::Bool => Sort::Bool,
                };
                let mut binds = env.smt_bindings();
                binds.push((nm.clone(), sort, p1r.clone()));
                let obligation = format!("{} => {}", show_pred(&p1r), show_pred(&p2r));
                match check_valid(&binds, path, &p2r)? {
                    Validity::Valid => Ok(()),
                    Validity::Invalid { model } => {
                        Err(InferenceError::RefinementFailed { obligation, model })
                    }
                    Validity::Unknown => Err(InferenceError::RefinementFailed {
                        obligation: format!("{} (z3 returned unknown)", obligation),
                        model: None,
                    }),
                }
            }
            (Type::Fun(x1, a1, r1), Type::Fun(x2, a2, r2)) => {
                self.subtype(env, path, a2, a1)?;
                let body_env = env.extend(x2.clone(), (**a2).clone());
                let r1n = subst_ty(r1, x1, &Pred::Var(x2.clone()));
                self.subtype(&body_env, path, &r1n, r2)
            }
            _ => Err(InferenceError::Mismatch {
                expected: sup.clone(),
                found: sub.clone(),
            }),
        }
    }
}

fn singleton_int(n: i64) -> Type {
    let v = "v".to_string();
    Type::Refine(
        v.clone(),
        BaseTy::Int,
        Pred::Bin(BinOp::Eq, Box::new(Pred::Var(v)), Box::new(Pred::Int(n))),
    )
}

fn singleton_bool(b: bool) -> Type {
    let v = "v".to_string();
    Type::Refine(
        v.clone(),
        BaseTy::Bool,
        Pred::Bin(BinOp::Eq, Box::new(Pred::Var(v)), Box::new(Pred::Bool(b))),
    )
}

// When a variable is looked up, sharpen its refinement with v = x so the
// caller has both the original predicate and a name for the value.
fn self_refine(name: &str, t: &Type) -> Type {
    match t {
        Type::Refine(v, b, p) => {
            let nm = "v".to_string();
            let p_renamed = subst_pred(p, v, &Pred::Var(nm.clone()));
            let eq = Pred::Bin(
                BinOp::Eq,
                Box::new(Pred::Var(nm.clone())),
                Box::new(Pred::Var(name.to_string())),
            );
            let combined = and_pred(p_renamed, eq);
            Type::Refine(nm, b.clone(), combined)
        }
        _ => t.clone(),
    }
}

fn show_pred(p: &Pred) -> String {
    format!("{}", p)
}

pub fn infer_type(e: &Expr) -> Result<Type> {
    let mut c = Checker::new();
    let env = Env::new();
    c.synth(&env, &Pred::Bool(true), e)
}
