use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::ast::{ClassDecl, Core, Decl, Expr, InstanceDecl, Lit, Name, Pred, Qual, Scheme, Type};
use crate::classes::{
    reduce, resolve, simplify_context, super_field, Binder, ClassEnv, ClassInfo, InstanceInfo,
};
use crate::errors::{InferenceError, Result};
use crate::subst::{
    apply_pred, apply_preds, apply_scheme, apply_type, ftv_qual, ftv_scheme, ftv_type, unify, Subst,
};

pub type Pending = Vec<Binder>;
pub type Inferred = (Subst, Type, Pending, Core);
pub type Env = BTreeMap<Name, Scheme>;

fn primitives() -> Vec<(&'static str, Type)> {
    let int_to_int_to_bool = Type::arrow(Type::int(), Type::arrow(Type::int(), Type::bool()));
    let int_to_int_to_int = Type::arrow(Type::int(), Type::arrow(Type::int(), Type::int()));
    let bool_to_bool_to_bool = Type::arrow(Type::bool(), Type::arrow(Type::bool(), Type::bool()));
    let bool_to_bool = Type::arrow(Type::bool(), Type::bool());
    vec![
        ("primEqInt", int_to_int_to_bool.clone()),
        ("primLtInt", int_to_int_to_bool),
        ("primAddInt", int_to_int_to_int.clone()),
        ("primMulInt", int_to_int_to_int),
        ("primAndBool", bool_to_bool_to_bool.clone()),
        ("primEqBool", bool_to_bool_to_bool),
        ("primNotBool", bool_to_bool),
    ]
}

pub struct Checker {
    pub env: Env,
    pub class_env: ClassEnv,
    tv_counter: usize,
    dv_counter: usize,
}

impl Default for Checker {
    fn default() -> Self {
        Self::new()
    }
}

impl Checker {
    pub fn new() -> Self {
        let mut ck = Checker {
            env: BTreeMap::new(),
            class_env: ClassEnv::new(),
            tv_counter: 0,
            dv_counter: 0,
        };
        ck.add_primitives();
        ck
    }

    fn add_primitives(&mut self) {
        for (n, ty) in primitives() {
            self.env.insert(
                n.to_string(),
                Scheme {
                    vars: vec![],
                    qual: Qual { preds: vec![], ty },
                },
            );
        }
    }

    fn fresh_tyvar(&mut self) -> Name {
        let n = format!("t{}", self.tv_counter);
        self.tv_counter += 1;
        n
    }

    fn fresh_dictvar(&mut self) -> Name {
        let n = format!("d{}", self.dv_counter);
        self.dv_counter += 1;
        n
    }

    fn instantiate(&mut self, sch: &Scheme) -> (Type, Pending) {
        let mut s = Subst::empty();
        for v in &sch.vars {
            s.0.insert(v.clone(), Type::Var(self.fresh_tyvar()));
        }
        let ty = apply_type(&s, &sch.qual.ty);
        let preds: Vec<(Name, Pred)> = sch
            .qual
            .preds
            .iter()
            .map(|p| (self.fresh_dictvar(), apply_pred(&s, p)))
            .collect();
        (ty, preds)
    }

    fn env_ftv(&self) -> HashSet<Name> {
        let mut s = HashSet::new();
        for sch in self.env.values() {
            s.extend(ftv_scheme(sch));
        }
        s
    }

    pub fn infer_expr(&mut self, expr: &Expr) -> Result<Inferred> {
        match expr {
            Expr::Lit(Lit::Int(_)) => Ok((
                Subst::empty(),
                Type::int(),
                vec![],
                Core::Lit(match expr {
                    Expr::Lit(l) => l.clone(),
                    _ => unreachable!(),
                }),
            )),
            Expr::Lit(Lit::Bool(_)) => Ok((
                Subst::empty(),
                Type::bool(),
                vec![],
                Core::Lit(match expr {
                    Expr::Lit(l) => l.clone(),
                    _ => unreachable!(),
                }),
            )),
            Expr::Var(name) => self.infer_var(name),
            Expr::Abs(param, body) => self.infer_abs(param, body),
            Expr::App(f, x) => self.infer_app(f, x),
            Expr::Let(v, e1, e2) => self.infer_let(v, e1, e2),
        }
    }

    /// T-Var: x : ∀ᾱ. Q ⇒ τ ∈ Γ    β̄ fresh    σ = [β̄/ᾱ]
    ///        ───────────────────────────────────────────
    ///                  σ(Q) | Γ ⊢ x : σ(τ)
    fn infer_var(&mut self, name: &str) -> Result<Inferred> {
        let sch = self
            .env
            .get(name)
            .cloned()
            .ok_or_else(|| InferenceError::UnboundVariable {
                name: name.to_string(),
            })?;
        let (ty, pending) = self.instantiate(&sch);
        let core = if let Some((ci, _)) = self.class_env.lookup_method(&name.to_string()) {
            let class_name = ci.name.clone();
            let dict_var = pending
                .iter()
                .find(|(_, p)| p.class == class_name)
                .map(|(d, _)| d.clone())
                .ok_or_else(|| InferenceError::UnknownMethod {
                    name: name.to_string(),
                })?;
            Core::DictProj(Box::new(Core::Var(dict_var)), name.to_string())
        } else if pending.is_empty() {
            Core::Var(name.to_string())
        } else {
            let args: Vec<Core> = pending.iter().map(|(d, _)| Core::Var(d.clone())).collect();
            Core::DictApp(Box::new(Core::Var(name.to_string())), args)
        };
        Ok((Subst::empty(), ty, pending, core))
    }

    /// T-Abs: P | Γ, x:α ⊢ e : τ    α fresh
    ///        ────────────────────────────
    ///            P | Γ ⊢ λx.e : α → τ
    fn infer_abs(&mut self, param: &str, body: &Expr) -> Result<Inferred> {
        let param_ty = Type::Var(self.fresh_tyvar());
        let prev = self.env.insert(
            param.to_string(),
            Scheme {
                vars: vec![],
                qual: Qual {
                    preds: vec![],
                    ty: param_ty.clone(),
                },
            },
        );
        let (s, body_ty, preds, body_core) = self.infer_expr(body)?;
        match prev {
            Some(old) => {
                self.env.insert(param.to_string(), old);
            }
            None => {
                self.env.remove(param);
            }
        }
        let arrow = Type::arrow(apply_type(&s, &param_ty), body_ty);
        let core = Core::Abs(param.to_string(), Box::new(body_core));
        Ok((s, arrow, preds, core))
    }

    /// T-App: P | Γ ⊢ e₁ : τ₁    Q | Γ ⊢ e₂ : τ₂    S = unify(τ₁, τ₂ → α)
    ///        ─────────────────────────────────────────────────────────
    ///                    S(P ∪ Q) | Γ ⊢ e₁ e₂ : S(α)
    fn infer_app(&mut self, f: &Expr, x: &Expr) -> Result<Inferred> {
        let result_ty = Type::Var(self.fresh_tyvar());
        let (s1, f_ty, p1, f_core) = self.infer_expr(f)?;
        self.apply_env(&s1);
        let (s2, x_ty, p2, x_core) = self.infer_expr(x)?;
        let f_ty2 = apply_type(&s2, &f_ty);
        let expected = Type::arrow(x_ty, result_ty.clone());
        let s3 = unify(&f_ty2, &expected)?;
        let s = s3.compose(&s2.compose(&s1));
        let ty = apply_type(&s3, &result_ty);
        let mut preds = apply_preds_pending(&s, &p1);
        preds.extend(apply_preds_pending(&s, &p2));
        let core = Core::App(Box::new(f_core), Box::new(x_core));
        Ok((s, ty, preds, core))
    }

    /// T-Let: P | Γ ⊢ e₁ : τ    σ = gen(Γ, P ⇒ τ)    Q | Γ, x:σ ⊢ e₂ : τ'
    ///        ────────────────────────────────────────────────────────
    ///                   Q | Γ ⊢ let x = e₁ in e₂ : τ'
    fn infer_let(&mut self, v: &str, e1: &Expr, e2: &Expr) -> Result<Inferred> {
        let (s1, t1, p1, c1) = self.infer_expr(e1)?;
        self.apply_env(&s1);
        let p1 = apply_preds_pending(&s1, &p1);
        let (scheme, c1_final, deferred) = self.generalize_binding(&t1, &p1, &c1)?;
        let prev = self.env.insert(v.to_string(), scheme);
        let (s2, t2, p2, c2) = self.infer_expr(e2)?;
        match prev {
            Some(old) => {
                self.env.insert(v.to_string(), old);
            }
            None => {
                self.env.remove(v);
            }
        }
        let mut preds = apply_preds_pending(&s2, &deferred);
        preds.extend(p2);
        let s = s2.compose(&s1);
        let core = Core::Let(v.to_string(), Box::new(c1_final), Box::new(c2));
        Ok((s, t2, preds, core))
    }

    fn apply_env(&mut self, s: &Subst) {
        let updated: Vec<(Name, Scheme)> = self
            .env
            .iter()
            .map(|(k, v)| (k.clone(), apply_scheme(s, v)))
            .collect();
        for (k, v) in updated {
            self.env.insert(k, v);
        }
    }

    fn generalize_binding(
        &mut self,
        ty: &Type,
        pending: &[Binder],
        core: &Core,
    ) -> Result<(Scheme, Core, Pending)> {
        let env_ftv = self.env_ftv();
        let mut ty_ftv = HashSet::new();
        ftv_type(ty, &mut ty_ftv);
        let pending_preds: Vec<Pred> = pending.iter().map(|(_, p)| p.clone()).collect();
        let (discharged, residual) = reduce(&self.class_env, &[], &pending_preds)?;
        let mut sub_map: HashMap<Name, Core> = HashMap::new();
        for (p, c) in &discharged {
            for (d, q) in pending {
                if q == p {
                    sub_map.insert(d.clone(), c.clone());
                }
            }
        }
        let (residual_binders, deferred): (Vec<_>, Vec<_>) = residual
            .iter()
            .filter_map(|p| {
                pending
                    .iter()
                    .find(|(_, q)| q == p)
                    .map(|(d, _)| (d.clone(), p.clone()))
            })
            .partition(|(_, p)| {
                let mut pf = HashSet::new();
                ftv_type(&p.ty, &mut pf);
                !pf.is_disjoint(&ty_ftv) || !pf.is_subset(&env_ftv)
            });
        for (canonical_d, p) in &residual_binders {
            for (d, q) in pending {
                if q == p && d != canonical_d {
                    sub_map.insert(d.clone(), Core::Var(canonical_d.clone()));
                }
            }
        }
        let binders = residual_binders;
        let (binders, removed) = simplify_context(&self.class_env, &binders);
        for (d, c) in &removed {
            sub_map.insert(d.clone(), c.clone());
        }
        let kept_preds: Vec<Pred> = binders.iter().map(|(_, p)| p.clone()).collect();
        let core1 = subst_core(core, &sub_map);
        let core_final = if binders.is_empty() {
            core1
        } else {
            Core::DictAbs(binders.clone(), Box::new(core1))
        };
        let mut ambig = HashSet::new();
        for p in &kept_preds {
            ftv_type(&p.ty, &mut ambig);
        }
        for v in &ambig {
            if !ty_ftv.contains(v) && !env_ftv.contains(v) {
                return Err(InferenceError::Ambiguous {
                    pred: kept_preds
                        .iter()
                        .find(|p| {
                            let mut s = HashSet::new();
                            ftv_type(&p.ty, &mut s);
                            s.contains(v)
                        })
                        .cloned()
                        .unwrap(),
                });
            }
        }
        let qual = Qual {
            preds: kept_preds,
            ty: ty.clone(),
        };
        let quant: Vec<Name> = ftv_qual(&qual).difference(&env_ftv).cloned().collect();
        let (scheme, core_final) =
            canonical_scheme_and_core(Scheme { vars: quant, qual }, core_final);
        Ok((scheme, core_final, deferred))
    }

    pub fn process_class_decl(&mut self, d: &ClassDecl) -> Result<()> {
        for s in &d.supers {
            if !self.class_env.classes.contains_key(s) {
                return Err(InferenceError::MissingSuperclass {
                    class: d.name.clone(),
                    superclass: s.clone(),
                });
            }
        }
        let info = ClassInfo {
            name: d.name.clone(),
            tyvar: d.tyvar.clone(),
            supers: d.supers.clone(),
            sigs: d.sigs.clone(),
            defaults: d.defaults.clone(),
        };
        self.class_env.add_class(info)?;
        for (m, t) in &d.sigs {
            let head_pred = Pred {
                class: d.name.clone(),
                ty: Type::Var(d.tyvar.clone()),
            };
            let qual = Qual {
                preds: vec![head_pred],
                ty: t.clone(),
            };
            let vars: Vec<Name> = ftv_qual(&qual).into_iter().collect();
            let mut vars = vars;
            vars.sort();
            let scheme = Scheme { vars, qual };
            self.env.insert(m.clone(), scheme);
        }
        Ok(())
    }

    pub fn process_instance_decl(&mut self, d: &InstanceDecl) -> Result<()> {
        let class_info = self
            .class_env
            .class(&d.head.class)
            .cloned()
            .ok_or_else(|| InferenceError::UnknownClass {
                name: d.head.class.clone(),
            })?;
        let head_subst = Subst::singleton(class_info.tyvar.clone(), d.head.ty.clone());
        let context_binders: Vec<(Name, Pred)> = d
            .context
            .iter()
            .enumerate()
            .map(|(i, p)| (format!("d_ctx_{}_{}", d.head.class, i), p.clone()))
            .collect();
        let givens: Vec<(Core, Pred)> = context_binders
            .iter()
            .map(|(n, p)| (Core::Var(n.clone()), p.clone()))
            .collect();
        let mut method_cores: Vec<(Name, Core)> = Vec::new();
        for (mname, body) in &d.methods {
            let sig_ty = class_info
                .sigs
                .iter()
                .find(|(n, _)| n == mname)
                .map(|(_, t)| t.clone())
                .ok_or_else(|| InferenceError::UnknownMethod {
                    name: mname.clone(),
                })?;
            let expected_ty = apply_type(&head_subst, &sig_ty);
            let saved_tv = self.tv_counter;
            let saved_dv = self.dv_counter;
            let (s, body_ty, body_preds, body_core) = self.infer_expr(body)?;
            let s_u = unify(&apply_type(&s, &body_ty), &apply_type(&s, &expected_ty))?;
            let s_final = s_u.compose(&s);
            let body_preds = apply_preds_pending(&s_final, &body_preds);
            let preds_only: Vec<Pred> = body_preds.iter().map(|(_, p)| p.clone()).collect();
            let (discharged, residual) = reduce(&self.class_env, &givens, &preds_only)?;
            if !residual.is_empty() {
                let pred = residual[0].clone();
                return Err(InferenceError::NoInstance { pred });
            }
            let mut sub_map: HashMap<Name, Core> = HashMap::new();
            for (p, c) in &discharged {
                if let Some((d, _)) = body_preds.iter().find(|(_, q)| q == p) {
                    sub_map.insert(d.clone(), c.clone());
                }
            }
            let body_core = subst_core(&body_core, &sub_map);
            method_cores.push((mname.clone(), body_core));
            let _ = saved_tv;
            let _ = saved_dv;
        }
        for m in &class_info.sigs {
            if !d.methods.iter().any(|(n, _)| n == &m.0) {
                if let Some((_, def_body)) = class_info.defaults.iter().find(|(n, _)| n == &m.0) {
                    let (_, _, dpreds, dc) = self.infer_expr(def_body)?;
                    let mut sub_map: HashMap<Name, Core> = HashMap::new();
                    for (dv, p) in &dpreds {
                        if p.class == class_info.name {
                            sub_map.insert(dv.clone(), Core::Var("self".to_string()));
                        }
                    }
                    let dc = subst_core(&dc, &sub_map);
                    method_cores.push((m.0.clone(), dc));
                } else {
                    return Err(InferenceError::MissingMethod {
                        class: class_info.name.clone(),
                        ty: d.head.ty.clone(),
                        method: m.0.clone(),
                    });
                }
            }
        }
        for s in &class_info.supers {
            let sp = Pred {
                class: s.clone(),
                ty: d.head.ty.clone(),
            };
            let super_dict = resolve(&self.class_env, &givens, &sp)?;
            method_cores.push((super_field(&class_info.name, s), super_dict));
        }
        let inst = InstanceInfo {
            context: d.context.clone(),
            head: d.head.clone(),
            methods: method_cores,
        };
        let _ = context_binders;
        self.class_env.add_instance(inst)?;
        Ok(())
    }

    pub fn process_let_decl(&mut self, name: &str, body: &Expr) -> Result<Scheme> {
        let (s, t, p, c) = self.infer_expr(body)?;
        let p = apply_preds_pending(&s, &p);
        let t = apply_type(&s, &t);
        let (scheme, _core, deferred) = self.generalize_binding(&t, &p, &c)?;
        if !deferred.is_empty() {
            return Err(InferenceError::NoInstance {
                pred: deferred[0].1.clone(),
            });
        }
        self.env.insert(name.to_string(), scheme.clone());
        Ok(scheme)
    }

    pub fn check_expr(&mut self, expr: &Expr) -> Result<(Qual, Core)> {
        let (s, t, p, c) = self.infer_expr(expr)?;
        let t = apply_type(&s, &t);
        let p = apply_preds_pending(&s, &p);
        let (qual, core, deferred) = finalize_expr(self, &t, &p, &c)?;
        if !deferred.is_empty() {
            return Err(InferenceError::NoInstance {
                pred: deferred[0].1.clone(),
            });
        }
        Ok((qual, core))
    }

    pub fn process_decl(&mut self, d: &Decl) -> Result<String> {
        match d {
            Decl::Class(c) => {
                self.process_class_decl(c)?;
                Ok(format!("class {} {} : ok", c.name, c.tyvar))
            }
            Decl::Instance(i) => {
                self.process_instance_decl(i)?;
                Ok(format!("instance {} : ok", i.head))
            }
            Decl::Let(n, e) => {
                let sch = self.process_let_decl(n, e)?;
                Ok(format!("let {} : {}", n, sch))
            }
        }
    }
}

fn finalize_expr(
    ck: &Checker,
    ty: &Type,
    pending: &[Binder],
    core: &Core,
) -> Result<(Qual, Core, Pending)> {
    let mut ty_ftv = HashSet::new();
    ftv_type(ty, &mut ty_ftv);
    let retained: Vec<(Name, Pred)> = pending.to_vec();
    let deferred: Vec<(Name, Pred)> = Vec::new();
    let retained_preds: Vec<Pred> = retained.iter().map(|(_, p)| p.clone()).collect();
    let (discharged, residual) = reduce(&ck.class_env, &[], &retained_preds)?;
    let mut sub_map: HashMap<Name, Core> = HashMap::new();
    for (p, c) in &discharged {
        for (d, q) in &retained {
            if q == p {
                sub_map.insert(d.clone(), c.clone());
            }
        }
    }
    let mut binders: Vec<(Name, Pred)> = Vec::new();
    for p in &residual {
        if let Some((d, _)) = retained.iter().find(|(_, q)| q == p) {
            binders.push((d.clone(), p.clone()));
        }
    }
    for (canonical_d, p) in &binders {
        for (d, q) in &retained {
            if q == p && d != canonical_d {
                sub_map.insert(d.clone(), Core::Var(canonical_d.clone()));
            }
        }
    }
    let (binders, removed) = simplify_context(&ck.class_env, &binders);
    for (d, c) in &removed {
        sub_map.insert(d.clone(), c.clone());
    }
    let kept_preds: Vec<Pred> = binders.iter().map(|(_, p)| p.clone()).collect();
    let core1 = subst_core(core, &sub_map);
    let core_final = if binders.is_empty() {
        core1
    } else {
        Core::DictAbs(binders, Box::new(core1))
    };
    let qual = Qual {
        preds: kept_preds,
        ty: ty.clone(),
    };
    let (qual, core_final) = canonicalize_pair(&qual, &core_final);
    Ok((qual, core_final, deferred))
}

fn apply_preds_pending(s: &Subst, ps: &[Binder]) -> Pending {
    ps.iter()
        .map(|(d, p)| (d.clone(), apply_pred(s, p)))
        .collect()
}

fn subst_core(core: &Core, sub: &HashMap<Name, Core>) -> Core {
    match core {
        Core::Var(n) => sub.get(n).cloned().unwrap_or_else(|| core.clone()),
        Core::Lit(_) => core.clone(),
        Core::Abs(p, b) => Core::Abs(p.clone(), Box::new(subst_core(b, sub))),
        Core::App(g, x) => Core::App(Box::new(subst_core(g, sub)), Box::new(subst_core(x, sub))),
        Core::Let(v, e1, e2) => Core::Let(
            v.clone(),
            Box::new(subst_core(e1, sub)),
            Box::new(subst_core(e2, sub)),
        ),
        Core::DictAbs(binders, body) => {
            let names: HashSet<Name> = binders.iter().map(|(n, _)| n.clone()).collect();
            let sub2: HashMap<Name, Core> = sub
                .iter()
                .filter(|(k, _)| !names.contains(*k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Core::DictAbs(binders.clone(), Box::new(subst_core(body, &sub2)))
        }
        Core::DictApp(g, args) => Core::DictApp(
            Box::new(subst_core(g, sub)),
            args.iter().map(|a| subst_core(a, sub)).collect(),
        ),
        Core::DictProj(d, m) => Core::DictProj(Box::new(subst_core(d, sub)), m.clone()),
        Core::DictRec(c, t, methods) => Core::DictRec(
            c.clone(),
            t.clone(),
            methods
                .iter()
                .map(|(n, e)| (n.clone(), subst_core(e, sub)))
                .collect(),
        ),
    }
}

fn canonical_scheme_and_core(s: Scheme, c: Core) -> (Scheme, Core) {
    let mut ordered: Vec<Name> = s.vars.clone();
    ordered.sort_by(internal_name_order);
    let renamer = canonical_renamer(&ordered);
    let subst = Subst(renamer.clone());
    let qual = apply_qual_to(&renamer, &s.qual);
    let mut vars: Vec<Name> = s
        .vars
        .iter()
        .map(|v| match &renamer[v] {
            Type::Var(n) => n.clone(),
            _ => unreachable!(),
        })
        .collect();
    vars.sort();
    vars.dedup();
    let core = apply_subst_to_core(&subst, &c);
    (Scheme { vars, qual }, core)
}

fn canonicalize_pair(q: &Qual, c: &Core) -> (Qual, Core) {
    let mut vs: Vec<Name> = ftv_qual(q).into_iter().collect();
    vs.sort_by(internal_name_order);
    let renamer = canonical_renamer(&vs);
    let s = Subst(renamer);
    let qual = Qual {
        preds: apply_preds(&s, &q.preds),
        ty: apply_type(&s, &q.ty),
    };
    let core = apply_subst_to_core(&s, c);
    (qual, core)
}

fn internal_name_order(a: &Name, b: &Name) -> Ordering {
    fn key(s: &str) -> Option<usize> {
        s.strip_prefix('t').and_then(|n| n.parse().ok())
    }
    match (key(a), key(b)) {
        (Some(x), Some(y)) => x.cmp(&y),
        _ => a.cmp(b),
    }
}

fn apply_subst_to_core(s: &Subst, c: &Core) -> Core {
    match c {
        Core::Var(_) | Core::Lit(_) => c.clone(),
        Core::Abs(p, b) => Core::Abs(p.clone(), Box::new(apply_subst_to_core(s, b))),
        Core::App(g, x) => Core::App(
            Box::new(apply_subst_to_core(s, g)),
            Box::new(apply_subst_to_core(s, x)),
        ),
        Core::Let(v, e1, e2) => Core::Let(
            v.clone(),
            Box::new(apply_subst_to_core(s, e1)),
            Box::new(apply_subst_to_core(s, e2)),
        ),
        Core::DictAbs(bs, body) => {
            let bs = bs
                .iter()
                .map(|(d, p)| (d.clone(), apply_pred(s, p)))
                .collect();
            Core::DictAbs(bs, Box::new(apply_subst_to_core(s, body)))
        }
        Core::DictApp(g, args) => Core::DictApp(
            Box::new(apply_subst_to_core(s, g)),
            args.iter().map(|a| apply_subst_to_core(s, a)).collect(),
        ),
        Core::DictProj(d, m) => Core::DictProj(Box::new(apply_subst_to_core(s, d)), m.clone()),
        Core::DictRec(cls, ty, methods) => Core::DictRec(
            cls.clone(),
            apply_type(s, ty),
            methods
                .iter()
                .map(|(n, e)| (n.clone(), apply_subst_to_core(s, e)))
                .collect(),
        ),
    }
}

fn canonical_renamer(vars: &[Name]) -> HashMap<Name, Type> {
    let mut out = HashMap::new();
    let mut letters = ('a'..='z').map(|c| c.to_string());
    for v in vars {
        let fresh = letters.next().unwrap_or_else(|| v.clone());
        out.insert(v.clone(), Type::Var(fresh));
    }
    out
}

fn apply_qual_to(m: &HashMap<Name, Type>, q: &Qual) -> Qual {
    let s = Subst(m.clone());
    Qual {
        preds: apply_preds(&s, &q.preds),
        ty: apply_type(&s, &q.ty),
    }
}
