use std::fmt;

pub type Name = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(Name),
    Lit(Lit),
    Abs(Name, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(Name, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Var(Name),
    Con(Name, Vec<Type>),
    Arrow(Box<Type>, Box<Type>),
}

impl Type {
    pub fn int() -> Type {
        Type::Con("Int".into(), vec![])
    }
    pub fn bool() -> Type {
        Type::Con("Bool".into(), vec![])
    }
    pub fn arrow(a: Type, b: Type) -> Type {
        Type::Arrow(Box::new(a), Box::new(b))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pred {
    pub class: Name,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Qual {
    pub preds: Vec<Pred>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    pub vars: Vec<Name>,
    pub qual: Qual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub name: Name,
    pub tyvar: Name,
    pub supers: Vec<Name>,
    pub sigs: Vec<(Name, Type)>,
    pub defaults: Vec<(Name, Expr)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstanceDecl {
    pub context: Vec<Pred>,
    pub head: Pred,
    pub methods: Vec<(Name, Expr)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Class(ClassDecl),
    Instance(InstanceDecl),
    Let(Name, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelItem {
    Decl(Decl),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassItemAst {
    Sig(Name, Type),
    Default(Name, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Core {
    Var(Name),
    Lit(Lit),
    Abs(Name, Box<Core>),
    App(Box<Core>, Box<Core>),
    Let(Name, Box<Core>, Box<Core>),
    DictAbs(Vec<(Name, Pred)>, Box<Core>),
    DictApp(Box<Core>, Vec<Core>),
    DictProj(Box<Core>, Name),
    DictRec(Name, Type, Vec<(Name, Core)>),
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Int(n) => write!(f, "{}", n),
            Lit::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(n) => write!(f, "{}", n),
            Expr::Lit(l) => write!(f, "{}", l),
            Expr::Abs(p, b) => write!(f, "\\{} -> {}", p, b),
            Expr::App(g, x) => match (g.as_ref(), x.as_ref()) {
                (Expr::Abs(_, _), _) => write!(f, "({}) {}", g, x),
                (_, Expr::App(_, _)) | (_, Expr::Abs(_, _)) => write!(f, "{} ({})", g, x),
                _ => write!(f, "{} {}", g, x),
            },
            Expr::Let(v, e1, e2) => write!(f, "let {} = {} in {}", v, e1, e2),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Var(n) => write!(f, "{}", n),
            Type::Con(n, args) if args.is_empty() => write!(f, "{}", n),
            Type::Con(n, args) => {
                write!(f, "{}", n)?;
                for a in args {
                    match a {
                        Type::Con(_, inner) if inner.is_empty() => write!(f, " {}", a)?,
                        Type::Var(_) => write!(f, " {}", a)?,
                        _ => write!(f, " ({})", a)?,
                    }
                }
                Ok(())
            }
            Type::Arrow(a, b) => match a.as_ref() {
                Type::Arrow(_, _) => write!(f, "({}) -> {}", a, b),
                _ => write!(f, "{} -> {}", a, b),
            },
        }
    }
}

impl fmt::Display for Pred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.ty {
            Type::Con(_, args) if !args.is_empty() => write!(f, "{} ({})", self.class, self.ty),
            Type::Arrow(_, _) => write!(f, "{} ({})", self.class, self.ty),
            _ => write!(f, "{} {}", self.class, self.ty),
        }
    }
}

impl fmt::Display for Qual {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.preds.is_empty() {
            write!(f, "{}", self.ty)
        } else if self.preds.len() == 1 {
            write!(f, "{} => {}", self.preds[0], self.ty)
        } else {
            write!(f, "(")?;
            for (i, p) in self.preds.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", p)?;
            }
            write!(f, ") => {}", self.ty)
        }
    }
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.qual)
        } else {
            write!(f, "forall {}. {}", self.vars.join(" "), self.qual)
        }
    }
}

impl fmt::Display for Core {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Core::Var(n) => write!(f, "{}", n),
            Core::Lit(l) => write!(f, "{}", l),
            Core::Abs(p, b) => write!(f, "\\{} -> {}", p, b),
            Core::App(g, x) => match (g.as_ref(), x.as_ref()) {
                (Core::Abs(_, _) | Core::DictAbs(_, _), _) => write!(f, "({}) {}", g, x),
                (_, Core::App(_, _) | Core::Abs(_, _) | Core::DictAbs(_, _)) => {
                    write!(f, "{} ({})", g, x)
                }
                _ => write!(f, "{} {}", g, x),
            },
            Core::Let(v, e1, e2) => write!(f, "let {} = {} in {}", v, e1, e2),
            Core::DictAbs(binders, body) => {
                write!(f, "\\")?;
                for (i, (d, p)) in binders.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "({} : {})", d, p)?;
                }
                write!(f, " -> {}", body)
            }
            Core::DictApp(g, args) => {
                write!(f, "{}", g)?;
                for a in args {
                    match a {
                        Core::Var(_) | Core::DictProj(_, _) => write!(f, " @{}", a)?,
                        _ => write!(f, " @({})", a)?,
                    }
                }
                Ok(())
            }
            Core::DictProj(d, m) => write!(f, "{}.{}", d, m),
            Core::DictRec(cls, ty, methods) => {
                write!(f, "<{} {}: ", cls, ty)?;
                for (i, (m, e)) in methods.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} = {}", m, e)?;
                }
                write!(f, ">")
            }
        }
    }
}
