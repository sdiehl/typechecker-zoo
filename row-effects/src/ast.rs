#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Lit(Lit),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Perform(String, Box<Expr>),
    // handle e with op x k -> body
    Handle {
        body: Box<Expr>,
        op: String,
        param: String,
        resume: String,
        handler: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    Unit,
}

// Effect-annotated arrow: τ1 -[ε]-> τ2.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(String),
    Int,
    Bool,
    Unit,
    Arrow(Box<Type>, Box<Effect>, Box<Type>),
}

// Effect rows are scoped-label rows over labels with no payload.
#[derive(Debug, Clone, PartialEq)]
pub enum Effect {
    Empty,
    Var(String),
    Extend(String, Box<Effect>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    pub type_vars: Vec<String>,
    pub effect_vars: Vec<String>,
    pub ty: Type,
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lit(Lit::Int(n)) => write!(f, "{}", n),
            Expr::Lit(Lit::Bool(b)) => write!(f, "{}", b),
            Expr::Lit(Lit::Unit) => write!(f, "()"),
            Expr::Lam(p, body) => write!(f, "\\{} -> {}", p, body),
            Expr::App(g, a) => match (g.as_ref(), a.as_ref()) {
                (Expr::Lam(_, _), _) => write!(f, "({}) {}", g, a),
                (_, Expr::App(_, _)) | (_, Expr::Lam(_, _)) => write!(f, "{} ({})", g, a),
                _ => write!(f, "{} {}", g, a),
            },
            Expr::Let(x, v, b) => write!(f, "let {} = {} in {}", x, v, b),
            Expr::Perform(op, e) => write!(f, "perform {} {}", op, e),
            Expr::Handle {
                body,
                op,
                param,
                resume,
                handler,
            } => write!(
                f,
                "handle {} with {} {} {} -> {}",
                body, op, param, resume, handler
            ),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(n) => write!(f, "{}", n),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Unit => write!(f, "Unit"),
            Type::Arrow(a, eff, b) => {
                let lhs = match a.as_ref() {
                    Type::Arrow(_, _, _) => format!("({})", a),
                    _ => format!("{}", a),
                };
                if matches!(eff.as_ref(), Effect::Empty) {
                    write!(f, "{} -> {}", lhs, b)
                } else {
                    write!(f, "{} -<{}>-> {}", lhs, eff, b)
                }
            }
        }
    }
}

impl std::fmt::Display for Effect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (labels, tail) = collect_effect(self);
        for (i, l) in labels.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", l)?;
        }
        match tail {
            Effect::Empty => Ok(()),
            Effect::Var(name) => {
                if labels.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(f, " | {}", name)
                }
            }
            Effect::Extend(_, _) => unreachable!(),
        }
    }
}

fn collect_effect(mut e: &Effect) -> (Vec<&str>, &Effect) {
    let mut labels = Vec::new();
    while let Effect::Extend(l, rest) = e {
        labels.push(l.as_str());
        e = rest;
    }
    (labels, e)
}

impl std::fmt::Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.type_vars.is_empty() && self.effect_vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "forall")?;
            for v in &self.type_vars {
                write!(f, " {}", v)?;
            }
            for v in &self.effect_vars {
                write!(f, " {}", v)?;
            }
            write!(f, ". {}", self.ty)
        }
    }
}
