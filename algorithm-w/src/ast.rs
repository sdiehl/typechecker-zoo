// Value-level AST
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Abs(String, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Lit(Lit),
    Tuple(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

// Type-level AST
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(String),
    Arrow(Box<Type>, Box<Type>),
    Int,
    Bool,
    Tuple(Vec<Type>),
}

// Type schemes for polymorphic types
#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    pub vars: Vec<String>, // Quantified type variables
    pub ty: Type,          // The type being quantified over
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lit(Lit::Int(n)) => write!(f, "{}", n),
            Expr::Lit(Lit::Bool(b)) => write!(f, "{}", b),
            Expr::Abs(param, body) => write!(f, "λ{}.{}", param, body),
            Expr::App(func, arg) => match (func.as_ref(), arg.as_ref()) {
                (Expr::Abs(_, _), _) => write!(f, "({}) {}", func, arg),
                (_, Expr::App(_, _)) => write!(f, "{} ({})", func, arg),
                (_, Expr::Abs(_, _)) => write!(f, "{} ({})", func, arg),
                _ => write!(f, "{} {}", func, arg),
            },
            Expr::Let(var, value, body) => {
                write!(f, "let {} = {} in {}", var, value, body)
            }
            Expr::Tuple(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(name) => write!(f, "{}", name),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Arrow(t1, t2) => {
                // Add parentheses around left side if it's an arrow to avoid ambiguity
                match t1.as_ref() {
                    Type::Arrow(_, _) => write!(f, "({}) → {}", t1, t2),
                    _ => write!(f, "{} → {}", t1, t2),
                }
            }
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl std::fmt::Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "forall {}. {}", self.vars.join(" "), self.ty)
        }
    }
}
