use std::collections::HashSet;
use std::fmt;

use crate::ast::{BinOp, Expr, Type};
use crate::errors::{TypeError, TypeResult};

pub type TyVar = String;
pub type TmVar = String;

#[derive(Debug, Clone)]
pub enum Entry {
    VarBnd(TmVar, Type),    // x: A
    TVarBnd(TyVar),         // α
    ETVarBnd(TyVar),        // ^α
    SETVarBnd(TyVar, Type), // ^α = τ
    Mark(TyVar),            // $α
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Entry::VarBnd(x, ty) => write!(f, "{}: {}", x, ty),
            Entry::TVarBnd(a) => write!(f, "{}", a),
            Entry::ETVarBnd(a) => write!(f, "^{}", a),
            Entry::SETVarBnd(a, ty) => write!(f, "^{} = {}", a, ty),
            Entry::Mark(a) => write!(f, "${}", a),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context(Vec<Entry>);

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Context(Vec::new())
    }

    pub fn push(&mut self, entry: Entry) {
        self.0.push(entry);
    }

    pub fn find<F>(&self, predicate: F) -> Option<&Entry>
    where
        F: Fn(&Entry) -> bool, {
        self.0.iter().find(|entry| predicate(entry))
    }

    pub fn break3<F>(&self, predicate: F) -> (Vec<Entry>, Option<Entry>, Vec<Entry>)
    where
        F: Fn(&Entry) -> bool, {
        if let Some(pos) = self.0.iter().position(predicate) {
            let left = self.0[..pos].to_vec();
            let middle = self.0[pos].clone();
            let right = self.0[pos + 1..].to_vec();
            (left, Some(middle), right)
        } else {
            (self.0.clone(), None, Vec::new())
        }
    }

    pub fn from_parts(left: Vec<Entry>, middle: Entry, right: Vec<Entry>) -> Self {
        let mut ctx = left;
        ctx.push(middle);
        ctx.extend(right);
        Context(ctx)
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let entries: Vec<String> = self.0.iter().rev().map(|e| e.to_string()).collect();
        write!(f, "{}", entries.join(", "))
    }
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

pub struct BiDirectional {
    counter: usize,
}

impl Default for BiDirectional {
    fn default() -> Self {
        Self::new()
    }
}

impl BiDirectional {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    fn fresh_tyvar(&mut self) -> TyVar {
        let var = format!("α{}", self.counter);
        self.counter += 1;
        var
    }

    fn is_mono(&self, ty: &Type) -> bool {
        match ty {
            Type::Int | Type::Bool | Type::Var(_) | Type::ETVar(_) => true,
            Type::Arrow(t1, t2) => self.is_mono(t1) && self.is_mono(t2),
            Type::Forall(_, _) => false,
        }
    }

    fn free_vars(&self, ty: &Type) -> HashSet<TyVar> {
        match ty {
            Type::Var(name) | Type::ETVar(name) => {
                let mut set = HashSet::new();
                set.insert(name.clone());
                set
            }
            Type::Arrow(t1, t2) => {
                let mut set = self.free_vars(t1);
                set.extend(self.free_vars(t2));
                set
            }
            Type::Forall(var, ty) => {
                let mut set = self.free_vars(ty);
                set.remove(var);
                set
            }
            Type::Int | Type::Bool => HashSet::new(),
        }
    }

    fn subst_type(&self, var: &TyVar, replacement: &Type, ty: &Type) -> Type {
        match ty {
            Type::Var(name) if name == var => replacement.clone(),
            Type::ETVar(name) if name == var => replacement.clone(),
            Type::Var(_) | Type::ETVar(_) | Type::Int | Type::Bool => ty.clone(),
            Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(self.subst_type(var, replacement, t1)),
                Box::new(self.subst_type(var, replacement, t2)),
            ),
            Type::Forall(bound_var, body) => {
                if bound_var == var {
                    ty.clone() // Variable is shadowed
                } else {
                    Type::Forall(
                        bound_var.clone(),
                        Box::new(self.subst_type(var, replacement, body)),
                    )
                }
            }
        }
    }

    pub fn apply_ctx_type(&self, ctx: &Context, ty: &Type) -> Type {
        let mut current = ty.clone();
        let mut changed = true;

        // Keep applying substitutions until no more changes occur
        while changed {
            changed = false;
            let new_type = self.apply_ctx_type_once(ctx, &current);
            if new_type != current {
                changed = true;
                current = new_type;
            }
        }

        current
    }

    fn apply_ctx_type_once(&self, ctx: &Context, ty: &Type) -> Type {
        match ty {
            Type::ETVar(a) => {
                if let Some(Entry::SETVarBnd(_, replacement)) =
                    ctx.find(|entry| matches!(entry, Entry::SETVarBnd(name, _) if name == a))
                {
                    // Recursively apply substitutions to the replacement type
                    self.apply_ctx_type_once(ctx, replacement)
                } else {
                    ty.clone()
                }
            }
            Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(self.apply_ctx_type_once(ctx, t1)),
                Box::new(self.apply_ctx_type_once(ctx, t2)),
            ),
            Type::Forall(var, body) => {
                Type::Forall(var.clone(), Box::new(self.apply_ctx_type_once(ctx, body)))
            }
            _ => ty.clone(),
        }
    }

    fn before(&self, ctx: &Context, a: &TyVar, b: &TyVar) -> bool {
        let pos_a = ctx
            .0
            .iter()
            .position(|entry| matches!(entry, Entry::ETVarBnd(name) if name == a));
        let pos_b = ctx
            .0
            .iter()
            .position(|entry| matches!(entry, Entry::ETVarBnd(name) if name == b));

        match (pos_a, pos_b) {
            (Some(pa), Some(pb)) => pa > pb, // Later in the context means earlier in ordering
            _ => false,
        }
    }

    /// Check if a type variable occurs in a type (occurs check for unification)
    /// This prevents creating infinite types when solving ^α := τ by ensuring α
    /// ∉ τ
    fn occurs_check(&self, var: &TyVar, ty: &Type) -> bool {
        match ty {
            Type::Var(name) | Type::ETVar(name) => name == var,
            Type::Arrow(t1, t2) => self.occurs_check(var, t1) || self.occurs_check(var, t2),
            Type::Forall(bound_var, body) => {
                // If the variable is shadowed by the forall binding, it doesn't occur
                if bound_var == var {
                    false
                } else {
                    self.occurs_check(var, body)
                }
            }
            Type::Int | Type::Bool => false,
        }
    }

    // ========== TYPING RULE METHODS ==========
    // Each method implements a specific typing rule from System F

    /// T-Var: Variable lookup rule
    /// Γ, x:A ⊢ x ⇒ A
    fn infer_var(
        &self,
        ctx: &Context,
        x: &str,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        if let Some(Entry::VarBnd(_, ty)) =
            ctx.find(|entry| matches!(entry, Entry::VarBnd(name, _) if name == x))
        {
            let output = format!("{} ⇒ {} ⊣ {}", input, ty, ctx);
            Ok((
                ty.clone(),
                ctx.clone(),
                InferenceTree::new("InfVar", input, &output, vec![]),
            ))
        } else {
            Err(TypeError::UnboundVariable {
                name: x.to_string(),
                expr: None,
            })
        }
    }

    /// T-LitInt: Integer literal rule
    /// ⊢ n ⇒ Int
    fn infer_lit_int(
        &self,
        ctx: &Context,
        _n: i64,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let output = format!("{} ⇒ Int ⊣ {}", input, ctx);
        Ok((
            Type::Int,
            ctx.clone(),
            InferenceTree::new("InfLitInt", input, &output, vec![]),
        ))
    }

    /// T-LitBool: Boolean literal rule
    /// ⊢ b ⇒ Bool
    fn infer_lit_bool(
        &self,
        ctx: &Context,
        _b: bool,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let output = format!("{} ⇒ Bool ⊣ {}", input, ctx);
        Ok((
            Type::Bool,
            ctx.clone(),
            InferenceTree::new("InfLitBool", input, &output, vec![]),
        ))
    }

    /// T-Abs: Lambda abstraction rule
    /// Γ,x:A ⊢ e ⇐ B
    /// ─────────────────────
    /// Γ ⊢ λx:A.e ⇒ A → B
    fn infer_abs(
        &mut self,
        ctx: &Context,
        x: &str,
        param_ty: &Type,
        body: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let b = self.fresh_tyvar();
        let mut new_ctx = ctx.clone();
        new_ctx.push(Entry::VarBnd(x.to_string(), param_ty.clone()));
        new_ctx.push(Entry::ETVarBnd(b.clone()));

        let (ctx1, tree) = self.check(&new_ctx, body, &Type::ETVar(b.clone()))?;
        let (left, _, right) =
            ctx1.break3(|entry| matches!(entry, Entry::VarBnd(name, _) if name == x));
        // Preserve solved existential variable bindings from the left context
        let mut final_ctx_entries = left
            .into_iter()
            .filter(|entry| matches!(entry, Entry::SETVarBnd(_, _)))
            .collect::<Vec<_>>();
        final_ctx_entries.extend(right);
        let final_ctx = Context(final_ctx_entries);
        let result_ty = Type::Arrow(Box::new(param_ty.clone()), Box::new(Type::ETVar(b)));
        let output = format!("{} ⇒ {} ⊣ {}", input, result_ty, final_ctx);
        Ok((
            result_ty,
            final_ctx,
            InferenceTree::new("InfLam", input, &output, vec![tree]),
        ))
    }

    /// T-App: Function application rule
    /// Γ ⊢ e1 ⇒ A   Γ ⊢ e2 • A ⇒⇒ C
    /// ────────────────────────────────
    /// Γ ⊢ e1 e2 ⇒ C
    fn infer_application(
        &mut self,
        ctx: &Context,
        func: &Expr,
        arg: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (func_ty, ctx1, tree1) = self.infer(ctx, func)?;
        let func_ty_applied = self.apply_ctx_type(&ctx1, &func_ty);
        let (result_ty, ctx2, tree2) = self.infer_app(&ctx1, &func_ty_applied, arg)?;
        let output = format!("{} ⇒ {} ⊣ {}", input, result_ty, ctx2);
        Ok((
            result_ty,
            ctx2,
            InferenceTree::new("InfApp", input, &output, vec![tree1, tree2]),
        ))
    }

    /// T-Let: Let binding rule
    /// Γ ⊢ e1 ⇒ A   Γ,x:A ⊢ e2 ⇒ B
    /// ─────────────────────────────
    /// Γ ⊢ let x = e1 in e2 ⇒ B
    fn infer_let(
        &mut self,
        ctx: &Context,
        x: &str,
        e1: &Expr,
        e2: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (ty1, ctx1, tree1) = self.infer(ctx, e1)?;
        let mut new_ctx = ctx1.clone();
        new_ctx.push(Entry::VarBnd(x.to_string(), ty1));
        let (ty2, ctx2, tree2) = self.infer(&new_ctx, e2)?;
        let (left, _, right) =
            ctx2.break3(|entry| matches!(entry, Entry::VarBnd(name, _) if name == x));
        // Preserve solved existential variable bindings from the left context
        let mut final_ctx_entries = left
            .into_iter()
            .filter(|entry| matches!(entry, Entry::SETVarBnd(_, _)))
            .collect::<Vec<_>>();
        final_ctx_entries.extend(right);
        let final_ctx = Context(final_ctx_entries);
        let output = format!("{} ⇒ {} ⊣ {}", input, ty2, final_ctx);
        Ok((
            ty2,
            final_ctx,
            InferenceTree::new("InfLet", input, &output, vec![tree1, tree2]),
        ))
    }

    /// T-If: Conditional rule
    /// Γ ⊢ e1 ⇐ Bool   Γ ⊢ e2 ⇒ A   Γ ⊢ e3 ⇒ A
    /// ──────────────────────────────────────────
    /// Γ ⊢ if e1 then e2 else e3 ⇒ A
    fn infer_if(
        &mut self,
        ctx: &Context,
        e1: &Expr,
        e2: &Expr,
        e3: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (ctx1, tree1) = self.check(ctx, e1, &Type::Bool)?;
        let (ty2, ctx2, tree2) = self.infer(&ctx1, e2)?;
        let (ty3, ctx3, tree3) = self.infer(&ctx2, e3)?;
        // Ensure both branches have the same type
        let (unified_ctx, tree_unify) = self.subtype(&ctx3, &ty2, &ty3)?;
        let output = format!("{} ⇒ {} ⊣ {}", input, ty2, unified_ctx);
        Ok((
            ty2,
            unified_ctx,
            InferenceTree::new(
                "InfIf",
                input,
                &output,
                vec![tree1, tree2, tree3, tree_unify],
            ),
        ))
    }

    /// T-BinOp: Binary operation rules
    fn infer_binop(
        &mut self,
        ctx: &Context,
        op: &BinOp,
        e1: &Expr,
        e2: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        match op {
            // T-Arith: Int → Int → Int
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                let (ctx1, tree1) = self.check(ctx, e1, &Type::Int)?;
                let (ctx2, tree2) = self.check(&ctx1, e2, &Type::Int)?;
                let output = format!("{} ⇒ Int ⊣ {}", input, ctx2);
                Ok((
                    Type::Int,
                    ctx2,
                    InferenceTree::new("InfArith", input, &output, vec![tree1, tree2]),
                ))
            }
            // T-Bool: Bool → Bool → Bool
            BinOp::And | BinOp::Or => {
                let (ctx1, tree1) = self.check(ctx, e1, &Type::Bool)?;
                let (ctx2, tree2) = self.check(&ctx1, e2, &Type::Bool)?;
                let output = format!("{} ⇒ Bool ⊣ {}", input, ctx2);
                Ok((
                    Type::Bool,
                    ctx2,
                    InferenceTree::new("InfBool", input, &output, vec![tree1, tree2]),
                ))
            }
            // T-Cmp: Int → Int → Bool
            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                let (ctx1, tree1) = self.check(ctx, e1, &Type::Int)?;
                let (ctx2, tree2) = self.check(&ctx1, e2, &Type::Int)?;
                let output = format!("{} ⇒ Bool ⊣ {}", input, ctx2);
                Ok((
                    Type::Bool,
                    ctx2,
                    InferenceTree::new("InfCmp", input, &output, vec![tree1, tree2]),
                ))
            }
            // T-Eq: ∀α. α → α → Bool
            BinOp::Eq | BinOp::Ne => {
                let (ty1, ctx1, tree1) = self.infer(ctx, e1)?;
                let (ctx2, tree2) = self.check(&ctx1, e2, &ty1)?;
                let output = format!("{} ⇒ Bool ⊣ {}", input, ctx2);
                Ok((
                    Type::Bool,
                    ctx2,
                    InferenceTree::new("InfEq", input, &output, vec![tree1, tree2]),
                ))
            }
        }
    }

    fn inst_l(
        &mut self,
        ctx: &Context,
        a: &TyVar,
        ty: &Type,
    ) -> TypeResult<(Context, InferenceTree)> {
        let input = format!("{} ⊢ ^{} :=< {}", ctx, a, ty);

        match ty {
            Type::ETVar(b) if self.before(ctx, a, b) => {
                let (left, _, right) =
                    ctx.break3(|entry| matches!(entry, Entry::ETVarBnd(name) if name == b));
                let new_ctx = Context::from_parts(
                    left,
                    Entry::SETVarBnd(b.clone(), Type::ETVar(a.clone())),
                    right,
                );
                let output = format!("{}", new_ctx);
                Ok((
                    new_ctx,
                    InferenceTree::new("InstLReach", &input, &output, vec![]),
                ))
            }
            Type::Arrow(t1, t2) => {
                let a1 = self.fresh_tyvar();
                let a2 = self.fresh_tyvar();
                let (left, _, right) =
                    ctx.break3(|entry| matches!(entry, Entry::ETVarBnd(name) if name == a));
                let arrow_type = Type::Arrow(
                    Box::new(Type::ETVar(a1.clone())),
                    Box::new(Type::ETVar(a2.clone())),
                );
                let mut new_ctx = left;
                new_ctx.push(Entry::SETVarBnd(a.clone(), arrow_type));
                new_ctx.push(Entry::ETVarBnd(a1.clone()));
                new_ctx.push(Entry::ETVarBnd(a2.clone()));
                new_ctx.extend(right);
                let ctx1 = Context(new_ctx);

                let (ctx2, tree1) = self.inst_r(&ctx1, t1, &a1)?;
                let t2_applied = self.apply_ctx_type(&ctx2, t2);
                let (ctx3, tree2) = self.inst_l(&ctx2, &a2, &t2_applied)?;

                let output = format!("{}", ctx3);
                Ok((
                    ctx3,
                    InferenceTree::new("InstLArr", &input, &output, vec![tree1, tree2]),
                ))
            }
            Type::Forall(b, t) => {
                let mut new_ctx = ctx.clone();
                new_ctx.push(Entry::TVarBnd(b.clone()));
                let (ctx1, tree) = self.inst_l(&new_ctx, a, t)?;
                let (_, _, right) =
                    ctx1.break3(|entry| matches!(entry, Entry::TVarBnd(name) if name == b));
                let final_ctx = Context(right);
                let output = format!("{}", final_ctx);
                Ok((
                    final_ctx,
                    InferenceTree::new("InstLAllR", &input, &output, vec![tree]),
                ))
            }
            _ if self.is_mono(ty) => {
                // Occurs check: ensure ^α doesn't occur in τ to prevent infinite types
                if self.occurs_check(a, ty) {
                    return Err(TypeError::OccursCheck {
                        var: a.clone(),
                        ty: ty.clone(),
                        expr: None,
                    });
                }
                let (left, _, right) =
                    ctx.break3(|entry| matches!(entry, Entry::ETVarBnd(name) if name == a));
                let new_ctx =
                    Context::from_parts(left, Entry::SETVarBnd(a.clone(), ty.clone()), right);
                let output = format!("{}", new_ctx);
                Ok((
                    new_ctx,
                    InferenceTree::new("InstLSolve", &input, &output, vec![]),
                ))
            }
            _ => Err(TypeError::InstantiationError {
                var: a.clone(),
                ty: ty.clone(),
                expr: None,
            }),
        }
    }

    fn inst_r(
        &mut self,
        ctx: &Context,
        ty: &Type,
        a: &TyVar,
    ) -> TypeResult<(Context, InferenceTree)> {
        let input = format!("{} ⊢ {} :=< ^{}", ctx, ty, a);

        match ty {
            Type::ETVar(b) if self.before(ctx, a, b) => {
                let (left, _, right) =
                    ctx.break3(|entry| matches!(entry, Entry::ETVarBnd(name) if name == b));
                let new_ctx = Context::from_parts(
                    left,
                    Entry::SETVarBnd(b.clone(), Type::ETVar(a.clone())),
                    right,
                );
                let output = format!("{}", new_ctx);
                Ok((
                    new_ctx,
                    InferenceTree::new("InstRReach", &input, &output, vec![]),
                ))
            }
            Type::Arrow(t1, t2) => {
                let a1 = self.fresh_tyvar();
                let a2 = self.fresh_tyvar();
                let (left, _, right) =
                    ctx.break3(|entry| matches!(entry, Entry::ETVarBnd(name) if name == a));
                let arrow_type = Type::Arrow(
                    Box::new(Type::ETVar(a1.clone())),
                    Box::new(Type::ETVar(a2.clone())),
                );
                let mut new_ctx = left;
                new_ctx.push(Entry::SETVarBnd(a.clone(), arrow_type));
                new_ctx.push(Entry::ETVarBnd(a1.clone()));
                new_ctx.push(Entry::ETVarBnd(a2.clone()));
                new_ctx.extend(right);
                let ctx1 = Context(new_ctx);

                let (ctx2, tree1) = self.inst_l(&ctx1, &a1, t1)?;
                let t2_applied = self.apply_ctx_type(&ctx2, t2);
                let (ctx3, tree2) = self.inst_r(&ctx2, &t2_applied, &a2)?;

                let output = format!("{}", ctx3);
                Ok((
                    ctx3,
                    InferenceTree::new("InstRArr", &input, &output, vec![tree1, tree2]),
                ))
            }
            Type::Forall(b, t) => {
                let subst_t = self.subst_type(b, &Type::ETVar(b.clone()), t);
                let mut new_ctx = ctx.clone();
                new_ctx.push(Entry::ETVarBnd(b.clone()));
                new_ctx.push(Entry::Mark(b.clone()));
                let (ctx1, tree) = self.inst_r(&new_ctx, &subst_t, a)?;
                let (_, _, right) =
                    ctx1.break3(|entry| matches!(entry, Entry::Mark(name) if name == b));
                let final_ctx = Context(right);
                let output = format!("{}", final_ctx);
                Ok((
                    final_ctx,
                    InferenceTree::new("InstRAllL", &input, &output, vec![tree]),
                ))
            }
            _ if self.is_mono(ty) => {
                // Occurs check: ensure ^α doesn't occur in τ to prevent infinite types
                if self.occurs_check(a, ty) {
                    return Err(TypeError::OccursCheck {
                        var: a.clone(),
                        ty: ty.clone(),
                        expr: None,
                    });
                }
                let (left, _, right) =
                    ctx.break3(|entry| matches!(entry, Entry::ETVarBnd(name) if name == a));
                let new_ctx =
                    Context::from_parts(left, Entry::SETVarBnd(a.clone(), ty.clone()), right);
                let output = format!("{}", new_ctx);
                Ok((
                    new_ctx,
                    InferenceTree::new("InstRSolve", &input, &output, vec![]),
                ))
            }
            _ => Err(TypeError::InstantiationError {
                var: a.clone(),
                ty: ty.clone(),
                expr: None,
            }),
        }
    }

    fn subtype(
        &mut self,
        ctx: &Context,
        t1: &Type,
        t2: &Type,
    ) -> TypeResult<(Context, InferenceTree)> {
        let input = format!("{} ⊢ {} <: {}", ctx, t1, t2);

        match (t1, t2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => Ok((
                ctx.clone(),
                InferenceTree::new("SubRefl", &input, &format!("{}", ctx), vec![]),
            )),
            (Type::Var(a), Type::Var(b)) if a == b => Ok((
                ctx.clone(),
                InferenceTree::new("SubReflTVar", &input, &format!("{}", ctx), vec![]),
            )),
            (Type::ETVar(a), Type::ETVar(b)) if a == b => Ok((
                ctx.clone(),
                InferenceTree::new("SubReflETVar", &input, &format!("{}", ctx), vec![]),
            )),
            (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => {
                let (ctx1, tree1) = self.subtype(ctx, b1, a1)?; // Contravariant in argument
                let (ctx2, tree2) = self.subtype(&ctx1, a2, b2)?; // Covariant in result
                let output = format!("{}", ctx2);
                Ok((
                    ctx2,
                    InferenceTree::new("SubArr", &input, &output, vec![tree1, tree2]),
                ))
            }
            (_, Type::Forall(b, t2_body)) => {
                let mut new_ctx = ctx.clone();
                new_ctx.push(Entry::TVarBnd(b.clone()));
                let (ctx1, tree) = self.subtype(&new_ctx, t1, t2_body)?;
                let (_, _, right) =
                    ctx1.break3(|entry| matches!(entry, Entry::TVarBnd(name) if name == b));
                let final_ctx = Context(right);
                let output = format!("{}", final_ctx);
                Ok((
                    final_ctx,
                    InferenceTree::new("SubAllR", &input, &output, vec![tree]),
                ))
            }
            (Type::Forall(a, t1_body), _) => {
                let subst_t1 = self.subst_type(a, &Type::ETVar(a.clone()), t1_body);
                let mut new_ctx = ctx.clone();
                new_ctx.push(Entry::ETVarBnd(a.clone()));
                new_ctx.push(Entry::Mark(a.clone()));
                let (ctx1, tree) = self.subtype(&new_ctx, &subst_t1, t2)?;
                let (_, _, right) =
                    ctx1.break3(|entry| matches!(entry, Entry::Mark(name) if name == a));
                let final_ctx = Context(right);
                let output = format!("{}", final_ctx);
                Ok((
                    final_ctx,
                    InferenceTree::new("SubAllL", &input, &output, vec![tree]),
                ))
            }
            (Type::ETVar(a), _) if !self.free_vars(t2).contains(a) => {
                let (ctx1, tree) = self.inst_l(ctx, a, t2)?;
                let output = format!("{}", ctx1);
                Ok((
                    ctx1,
                    InferenceTree::new("SubInstL", &input, &output, vec![tree]),
                ))
            }
            (_, Type::ETVar(a)) if !self.free_vars(t1).contains(a) => {
                let (ctx1, tree) = self.inst_r(ctx, t1, a)?;
                let output = format!("{}", ctx1);
                Ok((
                    ctx1,
                    InferenceTree::new("SubInstR", &input, &output, vec![tree]),
                ))
            }
            _ => Err(TypeError::SubtypingError {
                left: t1.clone(),
                right: t2.clone(),
                expr: None,
            }),
        }
    }

    fn check(
        &mut self,
        ctx: &Context,
        expr: &Expr,
        ty: &Type,
    ) -> TypeResult<(Context, InferenceTree)> {
        let input = format!("{} ⊢ {:?} ⇐ {}", ctx, expr, ty);

        match (expr, ty) {
            (Expr::LitInt(_), Type::Int) => Ok((
                ctx.clone(),
                InferenceTree::new("ChkLitInt", &input, &format!("{}", ctx), vec![]),
            )),
            (Expr::LitBool(_), Type::Bool) => Ok((
                ctx.clone(),
                InferenceTree::new("ChkLitBool", &input, &format!("{}", ctx), vec![]),
            )),
            (Expr::Abs(x, _param_ty, body), Type::Arrow(expected_param, result_ty)) => {
                // For simplicity, we assume the parameter type matches
                let mut new_ctx = ctx.clone();
                new_ctx.push(Entry::VarBnd(x.clone(), *expected_param.clone()));
                let (ctx1, tree) = self.check(&new_ctx, body, result_ty)?;
                let (_, _, right) =
                    ctx1.break3(|entry| matches!(entry, Entry::VarBnd(name, _) if name == x));
                let final_ctx = Context(right);
                let output = format!("{}", final_ctx);
                Ok((
                    final_ctx,
                    InferenceTree::new("ChkLam", &input, &output, vec![tree]),
                ))
            }
            (_, Type::Forall(a, ty_body)) => {
                let mut new_ctx = ctx.clone();
                new_ctx.push(Entry::TVarBnd(a.clone()));
                let (ctx1, tree) = self.check(&new_ctx, expr, ty_body)?;
                let (_, _, right) =
                    ctx1.break3(|entry| matches!(entry, Entry::TVarBnd(name) if name == a));
                let final_ctx = Context(right);
                let output = format!("{}", final_ctx);
                Ok((
                    final_ctx,
                    InferenceTree::new("ChkAll", &input, &output, vec![tree]),
                ))
            }
            _ => {
                // Fallback to inference + subtyping
                let (inferred_ty, ctx1, tree1) = self.infer(ctx, expr)?;
                let inferred_applied = self.apply_ctx_type(&ctx1, &inferred_ty);
                let ty_applied = self.apply_ctx_type(&ctx1, ty);
                let (ctx2, tree2) = self.subtype(&ctx1, &inferred_applied, &ty_applied)?;
                let output = format!("{}", ctx2);
                Ok((
                    ctx2,
                    InferenceTree::new("ChkSub", &input, &output, vec![tree1, tree2]),
                ))
            }
        }
    }

    pub fn infer(
        &mut self,
        ctx: &Context,
        expr: &Expr,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let input = format!("{} ⊢ {:?}", ctx, expr);

        match expr {
            Expr::Var(x) => self.infer_var(ctx, x, &input),
            Expr::Ann(expr, ty) => self.infer_ann(ctx, expr, ty, &input),
            Expr::LitInt(n) => self.infer_lit_int(ctx, *n, &input),
            Expr::LitBool(b) => self.infer_lit_bool(ctx, *b, &input),
            Expr::Abs(x, param_ty, body) => self.infer_abs(ctx, x, param_ty, body, &input),
            Expr::App(func, arg) => self.infer_application(ctx, func, arg, &input),
            Expr::TAbs(a, body) => self.infer_tabs(ctx, a, body, &input),
            Expr::TApp(func, ty_arg) => self.infer_tapp(ctx, func, ty_arg, &input),
            Expr::Let(x, e1, e2) => self.infer_let(ctx, x, e1, e2, &input),
            Expr::IfThenElse(e1, e2, e3) => self.infer_if(ctx, e1, e2, e3, &input),
            Expr::BinOp(op, e1, e2) => self.infer_binop(ctx, op, e1, e2, &input),
        }
    }

    /// T-Instr: Type annotation rule
    /// Γ ⊢ e ⇐ A
    /// ──────────────────
    /// Γ ⊢ (e : A) ⇒ A
    fn infer_ann(
        &mut self,
        ctx: &Context,
        expr: &Expr,
        ty: &Type,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (ctx1, tree) = self.check(ctx, expr, ty)?;
        let output = format!("{} ⇒ {} ⊣ {}", input, ty, ctx1);
        Ok((
            ty.clone(),
            ctx1,
            InferenceTree::new("InfAnn", input, &output, vec![tree]),
        ))
    }

    /// T-ForallI: Type abstraction rule
    /// Γ, α ⊢ e ⇒ A
    /// ──────────────────────
    /// Γ ⊢ Λα. e ⇒ ∀α. A
    fn infer_tabs(
        &mut self,
        ctx: &Context,
        a: &str,
        body: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let mut new_ctx = ctx.clone();
        new_ctx.push(Entry::TVarBnd(a.to_string()));
        let (body_ty, ctx1, tree) = self.infer(&new_ctx, body)?;

        // Apply context substitutions to resolve existential variables before removing
        // type binding
        let resolved_body_ty = self.apply_ctx_type(&ctx1, &body_ty);

        let (left, _, right) =
            ctx1.break3(|entry| matches!(entry, Entry::TVarBnd(name) if name == a));
        // Preserve solved existential variable bindings from the left context
        let mut final_ctx_entries = left
            .into_iter()
            .filter(|entry| matches!(entry, Entry::SETVarBnd(_, _)))
            .collect::<Vec<_>>();
        final_ctx_entries.extend(right);
        let final_ctx = Context(final_ctx_entries);
        let result_ty = Type::Forall(a.to_string(), Box::new(resolved_body_ty));
        let output = format!("{} ⇒ {} ⊣ {}", input, result_ty, final_ctx);
        Ok((
            result_ty,
            final_ctx,
            InferenceTree::new("InfTAbs", input, &output, vec![tree]),
        ))
    }

    /// T-ForallE: Type application rule
    /// Γ ⊢ e ⇒ ∀α. A
    /// ──────────────────────
    /// Γ ⊢ e[B] ⇒ [B/α]A
    fn infer_tapp(
        &mut self,
        ctx: &Context,
        func: &Expr,
        ty_arg: &Type,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (func_ty, ctx1, tree1) = self.infer(ctx, func)?;
        match func_ty {
            Type::Forall(a, body_ty) => {
                let result_ty = self.subst_type(&a, ty_arg, &body_ty);
                let output = format!("{} ⇒ {} ⊣ {}", input, result_ty, ctx1);
                Ok((
                    result_ty,
                    ctx1,
                    InferenceTree::new("InfTApp", input, &output, vec![tree1]),
                ))
            }
            _ => Err(TypeError::TypeApplicationError {
                actual: func_ty.clone(),
                expr: None,
            }),
        }
    }

    fn infer_app(
        &mut self,
        ctx: &Context,
        func_ty: &Type,
        arg: &Expr,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let input = format!("{} ⊢ {:?} • {}", ctx, arg, func_ty);

        match func_ty {
            Type::Arrow(param_ty, result_ty) => {
                let (ctx1, tree) = self.check(ctx, arg, param_ty)?;
                let output = format!("{} ⇒⇒ {} ⊣ {}", input, result_ty, ctx1);
                Ok((
                    result_ty.as_ref().clone(),
                    ctx1,
                    InferenceTree::new("InfAppArr", &input, &output, vec![tree]),
                ))
            }
            Type::ETVar(a) => {
                let a1 = self.fresh_tyvar();
                let a2 = self.fresh_tyvar();
                let (left, _, right) =
                    ctx.break3(|entry| matches!(entry, Entry::ETVarBnd(name) if name == a));
                let arrow_type = Type::Arrow(
                    Box::new(Type::ETVar(a1.clone())),
                    Box::new(Type::ETVar(a2.clone())),
                );
                let mut new_ctx = left;
                new_ctx.push(Entry::SETVarBnd(a.clone(), arrow_type));
                new_ctx.push(Entry::ETVarBnd(a1.clone()));
                new_ctx.push(Entry::ETVarBnd(a2.clone()));
                new_ctx.extend(right);
                let ctx1 = Context(new_ctx);

                let (ctx2, tree) = self.check(&ctx1, arg, &Type::ETVar(a1))?;
                let output = format!("{} ⇒⇒ ^{} ⊣ {}", input, a2, ctx2);
                Ok((
                    Type::ETVar(a2),
                    ctx2,
                    InferenceTree::new("InfAppETVar", &input, &output, vec![tree]),
                ))
            }
            _ => Err(TypeError::ApplicationTypeError {
                actual: func_ty.clone(),
                expr: None,
            }),
        }
    }
}

pub fn run_bidirectional(expr: &Expr) -> TypeResult<(Type, Context, InferenceTree)> {
    let mut bi = BiDirectional::new();
    let ctx = Context::new();
    let (ty, final_ctx, tree) = bi.infer(&ctx, expr)?;
    // Apply the final context to resolve existential variables
    let resolved_ty = bi.apply_ctx_type(&final_ctx, &ty);
    Ok((resolved_ty, final_ctx, tree))
}
