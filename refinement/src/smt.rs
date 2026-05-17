use std::collections::HashMap;

use z3::ast::{Ast, Bool, Int};
use z3::{Config, Context, SatResult, Solver};

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
    let cfg = Config::new();
    let ctx = Context::new(&cfg);
    let solver = Solver::new(&ctx);

    let mut env: HashMap<String, Val> = HashMap::with_capacity(bindings.len());
    for (name, sort, _) in bindings {
        let v = match sort {
            Sort::Int => Val::I(Int::new_const(&ctx, name.as_str())),
            Sort::Bool => Val::B(Bool::new_const(&ctx, name.as_str())),
        };
        env.insert(name.clone(), v);
    }
    for (_, _, p) in bindings {
        solver.assert(&as_bool(&ctx, &env, p));
    }
    if !matches!(path, Pred::Bool(true)) {
        solver.assert(&as_bool(&ctx, &env, path));
    }
    solver.assert(&as_bool(&ctx, &env, goal).not());

    match solver.check() {
        SatResult::Unsat => Ok(Validity::Valid),
        SatResult::Unknown => Ok(Validity::Unknown),
        SatResult::Sat => {
            let model = solver
                .get_model()
                .map(|m| render_model(&m, bindings, &env));
            Ok(Validity::Invalid { model })
        }
    }
}

#[derive(Clone)]
enum Val<'c> {
    I(Int<'c>),
    B(Bool<'c>),
}

fn as_int<'c>(ctx: &'c Context, env: &HashMap<String, Val<'c>>, p: &Pred) -> Int<'c> {
    match translate(ctx, env, p) {
        Val::I(i) => i,
        Val::B(_) => panic!("expected integer predicate, found boolean"),
    }
}

fn as_bool<'c>(ctx: &'c Context, env: &HashMap<String, Val<'c>>, p: &Pred) -> Bool<'c> {
    match translate(ctx, env, p) {
        Val::B(b) => b,
        Val::I(_) => panic!("expected boolean predicate, found integer"),
    }
}

fn translate<'c>(ctx: &'c Context, env: &HashMap<String, Val<'c>>, p: &Pred) -> Val<'c> {
    match p {
        Pred::Bool(b) => Val::B(Bool::from_bool(ctx, *b)),
        Pred::Int(n) => Val::I(Int::from_i64(ctx, *n)),
        Pred::Var(x) => env
            .get(x)
            .cloned()
            .unwrap_or_else(|| panic!("unbound variable in predicate: {x}")),
        Pred::Not(a) => Val::B(as_bool(ctx, env, a).not()),
        Pred::Neg(a) => Val::I(as_int(ctx, env, a).unary_minus()),
        Pred::Bin(op, a, b) => translate_bin(ctx, env, *op, a, b),
    }
}

fn translate_bin<'c>(
    ctx: &'c Context,
    env: &HashMap<String, Val<'c>>,
    op: BinOp,
    a: &Pred,
    b: &Pred,
) -> Val<'c> {
    match op {
        BinOp::Add => Val::I(Int::add(ctx, &[&as_int(ctx, env, a), &as_int(ctx, env, b)])),
        BinOp::Sub => Val::I(Int::sub(ctx, &[&as_int(ctx, env, a), &as_int(ctx, env, b)])),
        BinOp::Mul => Val::I(Int::mul(ctx, &[&as_int(ctx, env, a), &as_int(ctx, env, b)])),
        BinOp::Lt => Val::B(as_int(ctx, env, a).lt(&as_int(ctx, env, b))),
        BinOp::Le => Val::B(as_int(ctx, env, a).le(&as_int(ctx, env, b))),
        BinOp::Gt => Val::B(as_int(ctx, env, a).gt(&as_int(ctx, env, b))),
        BinOp::Ge => Val::B(as_int(ctx, env, a).ge(&as_int(ctx, env, b))),
        BinOp::Eq => Val::B(eq_dispatch(ctx, env, a, b)),
        BinOp::Ne => Val::B(eq_dispatch(ctx, env, a, b).not()),
        BinOp::And => Val::B(Bool::and(
            ctx,
            &[&as_bool(ctx, env, a), &as_bool(ctx, env, b)],
        )),
        BinOp::Or => Val::B(Bool::or(
            ctx,
            &[&as_bool(ctx, env, a), &as_bool(ctx, env, b)],
        )),
        BinOp::Implies => Val::B(as_bool(ctx, env, a).implies(&as_bool(ctx, env, b))),
    }
}

fn eq_dispatch<'c>(
    ctx: &'c Context,
    env: &HashMap<String, Val<'c>>,
    a: &Pred,
    b: &Pred,
) -> Bool<'c> {
    match (translate(ctx, env, a), translate(ctx, env, b)) {
        (Val::I(x), Val::I(y)) => x._eq(&y),
        (Val::B(x), Val::B(y)) => x._eq(&y),
        _ => panic!("equality between incompatible sorts"),
    }
}

fn render_model(
    model: &z3::Model<'_>,
    bindings: &[(String, Sort, Pred)],
    env: &HashMap<String, Val<'_>>,
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
