use crate::errors::Span;

#[derive(Debug, Clone)]
pub enum Term {
    Val(Box<Value>),
    Comp(Box<Comp>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Var(String, Span),
    Int(i64, Span),
    Bool(bool, Span),
    Unit(Span),
    Pair(Box<Value>, Box<Value>, Span),
    Thunk(Box<Comp>, Span),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comp {
    Return(Box<Value>, Span),
    Force(Box<Value>, Span),
    Abs(String, ValType, Box<Comp>, Span),
    App(Box<Comp>, Box<Value>, Span),
    To(Box<Comp>, String, Box<Comp>, Span),
    Let(String, Box<Value>, Box<Comp>, Span),
    If(Box<Value>, Box<Comp>, Box<Comp>, Span),
    Prim(PrimOp, Box<Value>, Box<Value>, Span),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum PrimOp {
    Add,
    Sub,
    Mul,
    Eq,
    Lt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValType {
    Int,
    Bool,
    Unit,
    Pair(Box<ValType>, Box<ValType>),
    U(Box<CompType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompType {
    F(Box<ValType>),
    Arrow(Box<ValType>, Box<CompType>),
}

impl PrimOp {
    pub fn name(self) -> &'static str {
        match self {
            PrimOp::Add => "add",
            PrimOp::Sub => "sub",
            PrimOp::Mul => "mul",
            PrimOp::Eq => "eq",
            PrimOp::Lt => "lt",
        }
    }

    pub fn result(self) -> ValType {
        match self {
            PrimOp::Add | PrimOp::Sub | PrimOp::Mul => ValType::Int,
            PrimOp::Eq | PrimOp::Lt => ValType::Bool,
        }
    }
}

impl Value {
    pub fn span(&self) -> Span {
        match self {
            Value::Var(_, s)
            | Value::Int(_, s)
            | Value::Bool(_, s)
            | Value::Unit(s)
            | Value::Pair(_, _, s)
            | Value::Thunk(_, s) => s.clone(),
        }
    }
}

impl Comp {
    pub fn span(&self) -> Span {
        match self {
            Comp::Return(_, s)
            | Comp::Force(_, s)
            | Comp::Abs(_, _, _, s)
            | Comp::App(_, _, s)
            | Comp::To(_, _, _, s)
            | Comp::Let(_, _, _, s)
            | Comp::If(_, _, _, s)
            | Comp::Prim(_, _, _, s) => s.clone(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Var(n, _) => write!(f, "{}", n),
            Value::Int(n, _) => write!(f, "{}", n),
            Value::Bool(b, _) => write!(f, "{}", b),
            Value::Unit(_) => write!(f, "()"),
            Value::Pair(a, b, _) => write!(f, "({}, {})", a, b),
            Value::Thunk(c, _) => write!(f, "thunk ({})", c),
        }
    }
}

impl std::fmt::Display for Comp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Comp::Return(v, _) => write!(f, "return {}", v),
            Comp::Force(v, _) => write!(f, "force {}", v),
            Comp::Abs(x, t, m, _) => write!(f, "\\{} : {}. {}", x, t, m),
            Comp::App(m, v, _) => write!(f, "{} {}", m, v),
            Comp::To(m, x, n, _) => write!(f, "{} to {}. {}", m, x, n),
            Comp::Let(x, v, m, _) => write!(f, "let {} = {} in {}", x, v, m),
            Comp::If(v, m, n, _) => write!(f, "if {} then {} else {}", v, m, n),
            Comp::Prim(op, a, b, _) => write!(f, "{} {} {}", op.name(), a, b),
        }
    }
}

impl std::fmt::Display for ValType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValType::Int => write!(f, "Int"),
            ValType::Bool => write!(f, "Bool"),
            ValType::Unit => write!(f, "Unit"),
            ValType::Pair(a, b) => {
                write_val_factor(f, a)?;
                write!(f, " * ")?;
                write_val_factor(f, b)
            }
            ValType::U(c) => write!(f, "U ({})", c),
        }
    }
}

fn write_val_factor(f: &mut std::fmt::Formatter<'_>, t: &ValType) -> std::fmt::Result {
    match t {
        ValType::Pair(_, _) => write!(f, "({})", t),
        _ => write!(f, "{}", t),
    }
}

impl std::fmt::Display for CompType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompType::F(a) => match a.as_ref() {
                ValType::Pair(_, _) => write!(f, "F ({})", a),
                _ => write!(f, "F {}", a),
            },
            CompType::Arrow(a, c) => {
                write_val_factor(f, a)?;
                write!(f, " -> {}", c)
            }
        }
    }
}
