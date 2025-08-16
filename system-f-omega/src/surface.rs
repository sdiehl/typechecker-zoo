use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Data type declaration: data Color = Red | Blue
    Data {
        name: String,
        type_params: Vec<String>,
        constructors: Vec<Constructor>,
    },
    /// Function type signature: fib :: Int -> Int
    TypeSig {
        name: String,
        type_scheme: TypeScheme,
    },
    /// Function definition: fib n = match n { 0 -> 0, ... }
    FunDef {
        name: String,
        params: Vec<String>,
        body: Expr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    pub name: String,
    pub fields: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeScheme {
    pub quantified: Vec<String>, // forall a b. ...
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Type variable: a, b
    Var(String),
    /// Type constructor: Int, Bool, Color
    Con(String),
    /// Function type: a -> b
    Arrow(Box<Type>, Box<Type>),
    /// Type application: List a, Maybe Int
    App(Box<Type>, Box<Type>),
    /// Record type: { x :: Int, y :: Int }
    Record(Vec<(String, Type)>),
    /// Forall type: forall a b. a -> b
    Forall(Vec<String>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Variable: x, y
    Var(String),
    /// Integer literal: 42
    LitInt(i64),
    /// Lambda: \x -> e
    Lambda { param: String, body: Box<Expr> },
    /// Function application: f x
    App { func: Box<Expr>, arg: Box<Expr> },
    /// Conditional: if cond then e1 else e2
    If {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    /// Pattern matching: match e { p1 -> e1, p2 -> e2 }
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    /// Binary operation: e1 + e2
    BinOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard pattern: _
    Wildcard,
    /// Variable pattern: x
    Var(String),
    /// Constructor pattern: Red, Cons x xs
    Constructor { name: String, args: Vec<Pattern> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Le,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Var(name) | Type::Con(name) => write!(f, "{}", name),
            Type::Arrow(t1, t2) => write!(f, "{} -> {}", t1, t2),
            Type::App(t1, t2) => write!(f, "{} {}", t1, t2),
            Type::Record(fields) => {
                write!(f, "{{ ")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} :: {}", name, ty)?;
                }
                write!(f, " }}")
            }
            Type::Forall(vars, ty) => {
                write!(f, "forall ")?;
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", var)?;
                }
                write!(f, ". {}", ty)
            }
        }
    }
}

impl fmt::Display for TypeScheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.quantified.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "forall {}. {}", self.quantified.join(" "), self.ty)
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
        }
    }
}
