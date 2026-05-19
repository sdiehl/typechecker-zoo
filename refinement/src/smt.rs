use std::collections::HashMap;

use z3::ast::{Bool, Int};
use z3::{SatResult, Solver};

use crate::ast::{BinOp, Pred};
use crate::errors::Result;

#[derive(Debug, Clone)]
pub enum Sort {
    Int,
    Bool,
}

#[derive(Debug)]
pub enum Validity {
    Valid,
    Invalid { model: Option<String> },
    Unknown,
}

pub fn check_valid(
    bindings: &[(String, Sort, Pred)],
    path: &Pred,
    goal: &Pred,
) -> Result<Validity> {
    let solver = Solver::new();

    let mut env: HashMap<String, Val> = HashMap::with_capacity(bindings.len());
    for (name, sort, _) in bindings {
        let v = match sort {
            Sort::Int => Val::I(Int::new_const(name.as_str())),
            Sort::Bool => Val::B(Bool::new_const(name.as_str())),
        };
        env.insert(name.clone(), v);
    }
    for (_, _, p) in bindings {
        solver.assert(as_bool(&env, p));
    }
    if !matches!(path, Pred::Bool(true)) {
        solver.assert(as_bool(&env, path));
    }
    solver.assert(as_bool(&env, goal).not());

    match solver.check() {
        SatResult::Unsat => Ok(Validity::Valid),
        SatResult::Unknown => Ok(Validity::Unknown),
        SatResult::Sat => {
            let model = solver.get_model().map(|m| render_model(&m, bindings, &env));
            Ok(Validity::Invalid { model })
        }
    }
}

#[derive(Clone)]
enum Val {
    I(Int),
    B(Bool),
}

fn as_int(env: &HashMap<String, Val>, p: &Pred) -> Int {
    match translate(env, p) {
        Val::I(i) => i,
        Val::B(_) => panic!("expected integer predicate, found boolean"),
    }
}

fn as_bool(env: &HashMap<String, Val>, p: &Pred) -> Bool {
    match translate(env, p) {
        Val::B(b) => b,
        Val::I(_) => panic!("expected boolean predicate, found integer"),
    }
}

fn translate(env: &HashMap<String, Val>, p: &Pred) -> Val {
    match p {
        Pred::Bool(b) => Val::B(Bool::from_bool(*b)),
        Pred::Int(n) => Val::I(Int::from_i64(*n)),
        Pred::Var(x) => env
            .get(x)
            .cloned()
            .unwrap_or_else(|| panic!("unbound variable in predicate: {x}")),
        Pred::Not(a) => Val::B(as_bool(env, a).not()),
        Pred::Neg(a) => Val::I(as_int(env, a).unary_minus()),
        Pred::Bin(op, a, b) => translate_bin(env, *op, a, b),
    }
}

fn translate_bin(env: &HashMap<String, Val>, op: BinOp, a: &Pred, b: &Pred) -> Val {
    match op {
        BinOp::Add => Val::I(Int::add(&[as_int(env, a), as_int(env, b)])),
        BinOp::Sub => Val::I(Int::sub(&[as_int(env, a), as_int(env, b)])),
        BinOp::Mul => Val::I(Int::mul(&[as_int(env, a), as_int(env, b)])),
        BinOp::Lt => Val::B(as_int(env, a).lt(as_int(env, b))),
        BinOp::Le => Val::B(as_int(env, a).le(as_int(env, b))),
        BinOp::Gt => Val::B(as_int(env, a).gt(as_int(env, b))),
        BinOp::Ge => Val::B(as_int(env, a).ge(as_int(env, b))),
        BinOp::Eq => Val::B(eq_dispatch(env, a, b)),
        BinOp::Ne => Val::B(eq_dispatch(env, a, b).not()),
        BinOp::And => Val::B(Bool::and(&[as_bool(env, a), as_bool(env, b)])),
        BinOp::Or => Val::B(Bool::or(&[as_bool(env, a), as_bool(env, b)])),
        BinOp::Implies => Val::B(as_bool(env, a).implies(as_bool(env, b))),
    }
}

fn eq_dispatch(env: &HashMap<String, Val>, a: &Pred, b: &Pred) -> Bool {
    match (translate(env, a), translate(env, b)) {
        (Val::I(x), Val::I(y)) => x.eq(&y),
        (Val::B(x), Val::B(y)) => x.eq(&y),
        _ => panic!("equality between incompatible sorts"),
    }
}

fn render_model(
    model: &z3::Model,
    bindings: &[(String, Sort, Pred)],
    env: &HashMap<String, Val>,
) -> String {
    let mut entries: Vec<(String, String)> = Vec::new();
    for (name, _, _) in bindings {
        let rendered = match env.get(name) {
            Some(Val::I(i)) => model
                .eval(i, true)
                .and_then(|v| v.as_i64())
                .map(|n| n.to_string()),
            Some(Val::B(b)) => model
                .eval(b, true)
                .and_then(|v| v.as_bool())
                .map(|x| x.to_string()),
            None => None,
        };
        if let Some(v) = rendered {
            entries.push((name.clone(), v));
        }
    }
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    entries
        .into_iter()
        .map(|(k, v)| format!("{k} = {v}"))
        .collect::<Vec<_>>()
        .join(", ")
}
