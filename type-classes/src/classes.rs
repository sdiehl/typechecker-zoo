// Intuition pump: read class resolution as miniature Prolog. Each
// `instance C τ where { ... }` is a Horn clause `C τ :- context`, and
// `resolve` is SLD-style depth-first search through those clauses, with
// superclass projection as a shortcut for goals already in scope.
use std::collections::BTreeMap;

use crate::ast::{Core, Expr, Name, Pred, Type};
use crate::errors::{InferenceError, Result};
use crate::subst::{apply_pred, apply_type, match_pred, match_ty, Subst};

pub type Given = (Core, Pred);
pub type Binder = (Name, Pred);
pub type Discharged = (Pred, Core);
pub type Removed = (Name, Core);

#[derive(Debug, Clone)]
pub struct ClassInfo {
    pub name: Name,
    pub tyvar: Name,
    pub supers: Vec<Name>,
    pub sigs: Vec<(Name, Type)>,
    pub defaults: Vec<(Name, Expr)>,
}

#[derive(Debug, Clone)]
pub struct InstanceInfo {
    pub context: Vec<Pred>,
    pub head: Pred,
    pub methods: Vec<(Name, Core)>,
}

#[derive(Debug, Default, Clone)]
pub struct ClassEnv {
    pub classes: BTreeMap<Name, ClassInfo>,
    pub method_of: BTreeMap<Name, Name>,
    pub instances: Vec<InstanceInfo>,
}

impl ClassEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_class(&mut self, info: ClassInfo) -> Result<()> {
        for s in &info.supers {
            if !self.classes.contains_key(s) {
                return Err(InferenceError::MissingSuperclass {
                    class: info.name.clone(),
                    superclass: s.clone(),
                });
            }
        }
        for (m, _) in &info.sigs {
            self.method_of.insert(m.clone(), info.name.clone());
        }
        self.classes.insert(info.name.clone(), info);
        Ok(())
    }

    pub fn add_instance(&mut self, inst: InstanceInfo) -> Result<()> {
        if !self.classes.contains_key(&inst.head.class) {
            return Err(InferenceError::UnknownClass {
                name: inst.head.class.clone(),
            });
        }
        for existing in &self.instances {
            if existing.head.class == inst.head.class && overlaps(&existing.head.ty, &inst.head.ty)
            {
                return Err(InferenceError::DuplicateInstance {
                    pred: inst.head.clone(),
                });
            }
        }
        self.instances.push(inst);
        Ok(())
    }

    pub fn lookup_method(&self, m: &Name) -> Option<(&ClassInfo, &Type)> {
        let cname = self.method_of.get(m)?;
        let ci = self.classes.get(cname)?;
        let sig = ci.sigs.iter().find(|(n, _)| n == m)?;
        Some((ci, &sig.1))
    }

    pub fn class(&self, name: &Name) -> Option<&ClassInfo> {
        self.classes.get(name)
    }
}

fn overlaps(a: &Type, b: &Type) -> bool {
    match_ty(a, b).is_ok() || match_ty(b, a).is_ok()
}

pub fn by_super(ce: &ClassEnv, d: &Core, p: &Pred) -> Vec<(Core, Pred)> {
    let mut out = vec![(d.clone(), p.clone())];
    if let Some(ci) = ce.class(&p.class) {
        for s in &ci.supers {
            let proj = Core::DictProj(Box::new(d.clone()), super_field(&p.class, s));
            let sp = Pred {
                class: s.clone(),
                ty: p.ty.clone(),
            };
            out.extend(by_super(ce, &proj, &sp));
        }
    }
    out
}

pub fn super_field(class: &str, sup: &str) -> Name {
    format!("__super_{}__{}", class, sup)
}

pub fn by_inst(ce: &ClassEnv, p: &Pred) -> Result<(Vec<Pred>, usize)> {
    for (idx, inst) in ce.instances.iter().enumerate() {
        if inst.head.class != p.class {
            continue;
        }
        if let Ok(s) = match_pred(&inst.head, p) {
            let subgoals = inst.context.iter().map(|q| apply_pred(&s, q)).collect();
            return Ok((subgoals, idx));
        }
    }
    Err(InferenceError::NoInstance { pred: p.clone() })
}

pub fn resolve(ce: &ClassEnv, givens: &[Given], p: &Pred) -> Result<Core> {
    for (d, q) in givens {
        for (d2, q2) in by_super(ce, d, q) {
            if &q2 == p {
                return Ok(d2);
            }
        }
    }
    let (subgoals, idx) = by_inst(ce, p)?;
    let inst = &ce.instances[idx];
    let s = match_pred(&inst.head, p)?;
    let sub_dicts: Result<Vec<Core>> = subgoals.iter().map(|sg| resolve(ce, givens, sg)).collect();
    let sub_dicts = sub_dicts?;
    let instance_dict = build_instance_dict(inst, &s);
    if sub_dicts.is_empty() {
        Ok(instance_dict)
    } else {
        Ok(Core::DictApp(Box::new(instance_dict), sub_dicts))
    }
}

fn build_instance_dict(inst: &InstanceInfo, s: &Subst) -> Core {
    let head_ty = apply_type(s, &inst.head.ty);
    Core::DictRec(inst.head.class.clone(), head_ty, inst.methods.clone())
}

pub fn reduce(
    ce: &ClassEnv,
    givens: &[Given],
    preds: &[Pred],
) -> Result<(Vec<Discharged>, Vec<Pred>)> {
    let mut residual: Vec<Pred> = Vec::new();
    let mut discharged: Vec<(Pred, Core)> = Vec::new();
    for p in preds {
        if is_head_normal(p) {
            match resolve_from_givens(ce, givens, p) {
                Some(d) => discharged.push((p.clone(), d)),
                None => {
                    if !residual.contains(p) {
                        residual.push(p.clone());
                    }
                }
            }
        } else {
            let d = resolve(ce, givens, p)?;
            discharged.push((p.clone(), d));
        }
    }
    Ok((discharged, residual))
}

pub fn simplify_context(ce: &ClassEnv, binders: &[Binder]) -> (Vec<Binder>, Vec<Removed>) {
    let mut kept: Vec<Binder> = Vec::new();
    let mut removed: Vec<Removed> = Vec::new();
    for (i, (d, p)) in binders.iter().enumerate() {
        let context: Vec<Binder> = kept
            .iter()
            .cloned()
            .chain(binders.iter().skip(i + 1).cloned())
            .collect();
        match find_super_proof(ce, &context, p) {
            Some(core) => removed.push((d.clone(), core)),
            None => kept.push((d.clone(), p.clone())),
        }
    }
    (kept, removed)
}

fn find_super_proof(ce: &ClassEnv, context: &[Binder], goal: &Pred) -> Option<Core> {
    for (d, q) in context {
        let root = Core::Var(d.clone());
        for (proj, sp) in by_super(ce, &root, q) {
            if &sp == goal && proj != root {
                return Some(proj);
            }
        }
    }
    None
}

fn is_head_normal(p: &Pred) -> bool {
    matches!(p.ty, Type::Var(_))
}

fn resolve_from_givens(ce: &ClassEnv, givens: &[Given], p: &Pred) -> Option<Core> {
    for (d, q) in givens {
        for (d2, q2) in by_super(ce, d, q) {
            if &q2 == p {
                return Some(d2);
            }
        }
    }
    None
}
