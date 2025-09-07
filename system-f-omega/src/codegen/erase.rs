//! Type erasure pass - converts typed Core to untyped representation
//!
//! This removes all type annotations, type abstractions, and type applications,
//! leaving only the computational content.

use crate::core::{CoreBinOp, CoreTerm};

/// Untyped lambda calculus after type erasure
#[derive(Debug, Clone, PartialEq)]
pub enum Erased {
    /// Variable
    Var(String),
    /// Lambda abstraction (no type annotation)
    Lam(String, Box<Erased>),
    /// Application
    App(Box<Erased>, Box<Erased>),
    /// Integer literal
    Int(i64),
    /// Binary operation
    BinOp(CoreBinOp, Box<Erased>, Box<Erased>),
    /// Conditional
    If(Box<Erased>, Box<Erased>, Box<Erased>),
    /// Call a runtime intrinsic
    IntrinsicCall { name: String, args: Vec<Erased> },
}

/// Erase types from Core representation
pub fn erase(term: &CoreTerm) -> Erased {
    match term {
        CoreTerm::Var(name) => Erased::Var(name.clone()),

        CoreTerm::LitInt(n) => Erased::Int(*n),

        CoreTerm::Lambda {
            param,
            param_ty: _,
            body,
        } => {
            // Erase type annotation
            Erased::Lam(param.clone(), Box::new(erase(body)))
        }

        CoreTerm::App { func, arg } => Erased::App(Box::new(erase(func)), Box::new(erase(arg))),

        CoreTerm::TypeLambda { param: _, body } => {
            // Type abstractions are erased - just keep the body
            erase(body)
        }

        CoreTerm::Constructor { .. } => {
            // For now, we don't handle constructors
            panic!("Constructor not yet supported in code generation")
        }

        CoreTerm::Case { .. } => {
            panic!("Pattern matching not yet supported in code generation")
        }

        CoreTerm::BinOp { op, left, right } => {
            Erased::BinOp(op.clone(), Box::new(erase(left)), Box::new(erase(right)))
        }

        CoreTerm::If {
            cond,
            then_branch,
            else_branch,
        } => Erased::If(
            Box::new(erase(cond)),
            Box::new(erase(then_branch)),
            Box::new(erase(else_branch)),
        ),

        CoreTerm::IntrinsicCall { name, args } => Erased::IntrinsicCall {
            name: name.clone(),
            args: args.iter().map(erase).collect(),
        },
    }
}

impl Erased {
    /// Pretty print erased terms
    pub fn pretty(&self) -> String {
        match self {
            Erased::Var(name) => name.to_string(),
            Erased::Lam(name, body) => format!("λ{}. {}", name, body.pretty()),
            Erased::App(f, x) => {
                let f_str = match &**f {
                    Erased::Lam(_, _) => format!("({})", f.pretty()),
                    _ => f.pretty(),
                };
                let x_str = match &**x {
                    Erased::App(_, _) | Erased::Lam(_, _) => format!("({})", x.pretty()),
                    _ => x.pretty(),
                };
                format!("{} {}", f_str, x_str)
            }
            Erased::Int(n) => n.to_string(),
            Erased::BinOp(op, l, r) => format!("{} {:?} {}", l.pretty(), op, r.pretty()),
            Erased::If(c, t, e) => {
                format!("if {} then {} else {}", c.pretty(), t.pretty(), e.pretty())
            }
            Erased::IntrinsicCall { name, args } => {
                let args_str = args
                    .iter()
                    .map(|a| a.pretty())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", name, args_str)
            }
        }
    }

    /// Collect all free variables
    pub fn free_vars(&self) -> Vec<String> {
        self.free_vars_impl(&mut Vec::new())
    }

    fn free_vars_impl(&self, bound: &mut Vec<String>) -> Vec<String> {
        match self {
            Erased::Var(name) => {
                if bound.contains(name) {
                    vec![]
                } else {
                    vec![name.clone()]
                }
            }
            Erased::Lam(name, body) => {
                bound.push(name.clone());
                let result = body.free_vars_impl(bound);
                bound.pop();
                result
            }
            Erased::App(f, x) => {
                let mut fvs = f.free_vars_impl(bound);
                let x_fvs = x.free_vars_impl(bound);
                for fv in x_fvs {
                    if !fvs.contains(&fv) {
                        fvs.push(fv);
                    }
                }
                fvs
            }
            Erased::Int(_) => vec![],
            Erased::BinOp(_, l, r) => {
                let mut fvs = l.free_vars_impl(bound);
                let r_fvs = r.free_vars_impl(bound);
                for fv in r_fvs {
                    if !fvs.contains(&fv) {
                        fvs.push(fv);
                    }
                }
                fvs
            }
            Erased::If(c, t, e) => {
                let mut fvs = c.free_vars_impl(bound);
                for term in [t, e] {
                    let term_fvs = term.free_vars_impl(bound);
                    for fv in term_fvs {
                        if !fvs.contains(&fv) {
                            fvs.push(fv);
                        }
                    }
                }
                fvs
            }
            Erased::IntrinsicCall { args, .. } => {
                let mut fvs = vec![];
                for arg in args {
                    let arg_fvs = arg.free_vars_impl(bound);
                    for fv in arg_fvs {
                        if !fvs.contains(&fv) {
                            fvs.push(fv);
                        }
                    }
                }
                fvs
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::CoreType;

    #[test]
    fn test_erase_simple() {
        // λx:Int. x
        let term = CoreTerm::Lambda {
            param: "x".to_string(),
            param_ty: CoreType::Con("Int".to_string()),
            body: Box::new(CoreTerm::Var("x".to_string())),
        };

        let erased = erase(&term);
        assert_eq!(
            erased,
            Erased::Lam("x".to_string(), Box::new(Erased::Var("x".to_string())))
        );
    }

    #[test]
    fn test_erase_type_abstraction() {
        // Λα. λx:α. x
        let term = CoreTerm::TypeLambda {
            param: "α".to_string(),
            body: Box::new(CoreTerm::Lambda {
                param: "x".to_string(),
                param_ty: CoreType::Var("α".to_string()),
                body: Box::new(CoreTerm::Var("x".to_string())),
            }),
        };

        let erased = erase(&term);
        assert_eq!(
            erased,
            Erased::Lam("x".to_string(), Box::new(Erased::Var("x".to_string())))
        );
    }
}
