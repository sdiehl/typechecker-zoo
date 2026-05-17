use std::collections::BTreeMap;

use crate::ast::{Comp, CompType, Term, ValType, Value};
use crate::errors::{InferenceError, Result};

pub type Env = BTreeMap<String, ValType>;

pub struct Checker;

impl Checker {
    // Synthesize the type of a value.
    pub fn synth_value(env: &Env, v: &Value) -> Result<ValType> {
        match v {
            Value::Int(_, _) => Ok(ValType::Int),
            Value::Bool(_, _) => Ok(ValType::Bool),
            Value::Unit(_) => Ok(ValType::Unit),
            Value::Var(n, s) => {
                env.get(n)
                    .cloned()
                    .ok_or_else(|| InferenceError::UnboundVariable {
                        name: n.clone(),
                        span: s.clone(),
                    })
            }
            Value::Pair(a, b, _) => {
                let ta = Self::synth_value(env, a)?;
                let tb = Self::synth_value(env, b)?;
                Ok(ValType::Pair(Box::new(ta), Box::new(tb)))
            }
            Value::Thunk(c, _) => {
                let ct = Self::synth_comp(env, c)?;
                Ok(ValType::U(Box::new(ct)))
            }
        }
    }

    // Check a value against an expected type.
    pub fn check_value(env: &Env, v: &Value, expected: &ValType) -> Result<()> {
        match (v, expected) {
            (Value::Thunk(c, _), ValType::U(ct)) => Self::check_comp(env, c, ct),
            (Value::Pair(a, b, _), ValType::Pair(ta, tb)) => {
                Self::check_value(env, a, ta)?;
                Self::check_value(env, b, tb)
            }
            _ => {
                let found = Self::synth_value(env, v)?;
                if &found == expected {
                    Ok(())
                } else {
                    Err(InferenceError::ValueMismatch {
                        expected: expected.clone(),
                        found,
                        span: v.span(),
                    })
                }
            }
        }
    }

    // Synthesize the type of a computation when possible.
    pub fn synth_comp(env: &Env, m: &Comp) -> Result<CompType> {
        match m {
            Comp::Return(v, _) => {
                let t = Self::synth_value(env, v)?;
                Ok(CompType::F(Box::new(t)))
            }
            Comp::Force(v, s) => {
                let t = Self::synth_value(env, v)?;
                match t {
                    ValType::U(c) => Ok(*c),
                    _ => Err(InferenceError::NotAThunk {
                        found: t,
                        span: s.clone(),
                    }),
                }
            }
            Comp::App(f, arg, s) => {
                let ft = Self::synth_comp(env, f)?;
                match ft {
                    CompType::Arrow(a, c) => {
                        Self::check_value(env, arg, &a)?;
                        Ok(*c)
                    }
                    _ => Err(InferenceError::NotAFunction {
                        found: ft,
                        span: s.clone(),
                    }),
                }
            }
            Comp::Abs(x, t, body, _) => {
                let mut env1 = env.clone();
                env1.insert(x.clone(), t.clone());
                let c = Self::synth_comp(&env1, body)?;
                Ok(CompType::Arrow(Box::new(t.clone()), Box::new(c)))
            }
            Comp::To(m1, x, m2, _) => {
                let t1 = Self::synth_comp(env, m1)?;
                match t1 {
                    CompType::F(a) => {
                        let mut env1 = env.clone();
                        env1.insert(x.clone(), *a);
                        Self::synth_comp(&env1, m2)
                    }
                    _ => Err(InferenceError::NotAProducer {
                        found: t1,
                        span: m1.span(),
                    }),
                }
            }
            Comp::Let(x, v, m, _) => {
                let t = Self::synth_value(env, v)?;
                let mut env1 = env.clone();
                env1.insert(x.clone(), t);
                Self::synth_comp(&env1, m)
            }
            Comp::If(v, m1, m2, _) => {
                Self::check_value(env, v, &ValType::Bool)?;
                let c1 = Self::synth_comp(env, m1)?;
                let c2 = Self::synth_comp(env, m2)?;
                if c1 == c2 {
                    Ok(c1)
                } else {
                    Err(InferenceError::CompMismatch {
                        expected: c1,
                        found: c2,
                        span: m2.span(),
                    })
                }
            }
            Comp::Prim(op, a, b, _) => {
                Self::check_value(env, a, &ValType::Int)?;
                Self::check_value(env, b, &ValType::Int)?;
                Ok(CompType::F(Box::new(op.result())))
            }
        }
    }

    pub fn check_comp(env: &Env, m: &Comp, expected: &CompType) -> Result<()> {
        match (m, expected) {
            (Comp::Abs(x, t, body, span), CompType::Arrow(a, c)) => {
                if t != a.as_ref() {
                    return Err(InferenceError::ValueMismatch {
                        expected: (**a).clone(),
                        found: t.clone(),
                        span: span.clone(),
                    });
                }
                let mut env1 = env.clone();
                env1.insert(x.clone(), t.clone());
                Self::check_comp(&env1, body, c)
            }
            (Comp::Return(v, _), CompType::F(a)) => Self::check_value(env, v, a),
            (Comp::If(v, m1, m2, _), c) => {
                Self::check_value(env, v, &ValType::Bool)?;
                Self::check_comp(env, m1, c)?;
                Self::check_comp(env, m2, c)
            }
            (Comp::To(m1, x, m2, _), c) => {
                let t1 = Self::synth_comp(env, m1)?;
                match t1 {
                    CompType::F(a) => {
                        let mut env1 = env.clone();
                        env1.insert(x.clone(), *a);
                        Self::check_comp(&env1, m2, c)
                    }
                    _ => Err(InferenceError::NotAProducer {
                        found: t1,
                        span: m1.span(),
                    }),
                }
            }
            (Comp::Let(x, v, m, _), c) => {
                let t = Self::synth_value(env, v)?;
                let mut env1 = env.clone();
                env1.insert(x.clone(), t);
                Self::check_comp(&env1, m, c)
            }
            _ => {
                let found = Self::synth_comp(env, m)?;
                if &found == expected {
                    Ok(())
                } else {
                    Err(InferenceError::CompMismatch {
                        expected: expected.clone(),
                        found,
                        span: m.span(),
                    })
                }
            }
        }
    }
}

pub enum TypeResult {
    Val(ValType),
    Comp(CompType),
}

impl std::fmt::Display for TypeResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeResult::Val(t) => write!(f, "{}", t),
            TypeResult::Comp(c) => write!(f, "{}", c),
        }
    }
}

pub fn infer_type(term: &Term) -> Result<TypeResult> {
    let env = Env::new();
    match term {
        Term::Val(v) => Checker::synth_value(&env, v).map(TypeResult::Val),
        Term::Comp(c) => Checker::synth_comp(&env, c).map(TypeResult::Comp),
    }
}
