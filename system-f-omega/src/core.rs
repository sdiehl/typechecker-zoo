use std::fmt;

/// Core language for System-F-ω with explicit type applications and
/// abstractions
#[derive(Debug, Clone, PartialEq)]
pub struct CoreModule {
    pub type_defs: Vec<TypeDef>,
    pub term_defs: Vec<TermDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub name: String,
    pub kind: Kind,
    pub constructors: Vec<DataConstructor>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataConstructor {
    pub name: String,
    pub ty: CoreType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TermDef {
    pub name: String,
    pub ty: CoreType,
    pub body: CoreTerm,
}

/// Kinds for types
#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    /// Base kind: *
    Star,
    /// Function kind: k1 -> k2
    Arrow(Box<Kind>, Box<Kind>),
}

/// Core types (System-F-ω)
#[derive(Debug, Clone, PartialEq)]
pub enum CoreType {
    /// Type variable: a
    Var(String),
    /// Existential type variable: ^a
    ETVar(String),
    /// Type constructor: Int, Bool
    Con(String),
    /// Function type: T1 -> T2
    Arrow(Box<CoreType>, Box<CoreType>),
    /// Universal quantification: ∀a. T
    Forall(String, Box<CoreType>),
    /// Type application: F T
    App(Box<CoreType>, Box<CoreType>),
    /// Product type: T1 × T2
    Product(Vec<CoreType>),
}

/// Core terms (System-F)
#[derive(Debug, Clone, PartialEq)]
pub enum CoreTerm {
    /// Variable: x
    Var(String),
    /// Integer literal: 42
    LitInt(i64),
    /// Lambda abstraction: λx:T. e
    Lambda {
        param: String,
        param_ty: CoreType,
        body: Box<CoreTerm>,
    },
    /// Application: e1 e2
    App {
        func: Box<CoreTerm>,
        arg: Box<CoreTerm>,
    },
    /// Type abstraction: Λα. e
    TypeLambda { param: String, body: Box<CoreTerm> },
    /// Constructor: C e1 ... en
    Constructor { name: String, args: Vec<CoreTerm> },
    /// Pattern matching: case e of { p1 -> e1; ... }
    Case {
        scrutinee: Box<CoreTerm>,
        arms: Vec<CaseArm>,
    },
    /// Built-in operations
    BinOp {
        op: CoreBinOp,
        left: Box<CoreTerm>,
        right: Box<CoreTerm>,
    },
    /// Conditional: if e1 then e2 else e3
    If {
        cond: Box<CoreTerm>,
        then_branch: Box<CoreTerm>,
        else_branch: Box<CoreTerm>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseArm {
    pub pattern: CorePattern,
    pub body: CoreTerm,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CorePattern {
    /// Wildcard: _
    Wildcard,
    /// Variable: x
    Var(String),
    /// Constructor: C p1 ... pn
    Constructor {
        name: String,
        args: Vec<CorePattern>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum CoreBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Le,
}

// Display implementations

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Kind::Star => write!(f, "*"),
            Kind::Arrow(k1, k2) => write!(f, "{} -> {}", k1, k2),
        }
    }
}

impl fmt::Display for CoreType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CoreType::Var(name) => write!(f, "{}", name),
            CoreType::ETVar(name) => write!(f, "^{}", name),
            CoreType::Con(name) => write!(f, "{}", name),
            CoreType::Arrow(t1, t2) => write!(f, "{} -> {}", t1, t2),
            CoreType::Forall(var, ty) => write!(f, "∀{}. {}", var, ty),
            CoreType::App(t1, t2) => write!(f, "({} {})", t1, t2),
            CoreType::Product(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " × ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for CoreBinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CoreBinOp::Add => write!(f, "+"),
            CoreBinOp::Sub => write!(f, "-"),
            CoreBinOp::Mul => write!(f, "*"),
            CoreBinOp::Div => write!(f, "/"),
            CoreBinOp::Lt => write!(f, "<"),
            CoreBinOp::Le => write!(f, "<="),
        }
    }
}

impl fmt::Display for CoreTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CoreTerm::Var(name) => write!(f, "{}", name),
            CoreTerm::LitInt(n) => write!(f, "{}", n),
            CoreTerm::Lambda {
                param,
                param_ty,
                body,
            } => {
                write!(f, "λ{} : {}. {}", param, param_ty, body)
            }
            CoreTerm::App { func, arg } => write!(f, "{} {}", func, arg),
            CoreTerm::Constructor { name, args } => {
                if args.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}", name)?;
                    for arg in args {
                        write!(f, " {}", arg)?;
                    }
                    Ok(())
                }
            }
            CoreTerm::If {
                cond,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {} then {} else {}", cond, then_branch, else_branch)
            }
            CoreTerm::Case { scrutinee, arms: _ } => {
                write!(f, "match {} {{ ... }}", scrutinee)
            }
            CoreTerm::BinOp { op, left, right } => {
                write!(f, "{} {} {}", left, op, right)
            }
            _ => write!(f, "<term>"),
        }
    }
}
