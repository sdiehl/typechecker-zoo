use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum BaseTy {
    Int,
    Bool,
}

impl fmt::Display for BaseTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BaseTy::Int => write!(f, "Int"),
            BaseTy::Bool => write!(f, "Bool"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // { v : B | phi }
    Refine(String, BaseTy, Pred),
    // (x : T1) -> T2
    Fun(String, Box<Type>, Box<Type>),
}

impl Type {
    pub fn base(b: BaseTy) -> Type {
        Type::Refine("v".to_string(), b, Pred::Bool(true))
    }

    pub fn is_trivial(&self) -> bool {
        matches!(self, Type::Refine(_, _, Pred::Bool(true)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pred {
    Bool(bool),
    Int(i64),
    Var(String),
    Bin(BinOp, Box<Pred>, Box<Pred>),
    Not(Box<Pred>),
    Neg(Box<Pred>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Implies,
}

impl BinOp {
    pub fn is_arith(self) -> bool {
        matches!(self, BinOp::Add | BinOp::Sub | BinOp::Mul)
    }

    pub fn is_cmp(self) -> bool {
        matches!(
            self,
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
        )
    }

    pub fn is_bool(self) -> bool {
        matches!(self, BinOp::And | BinOp::Or | BinOp::Implies)
    }

    pub fn as_str(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Eq => "=",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::Implies => "==>",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Let(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Lam(String, Type, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Ann(Box<Expr>, Type),
    Bin(BinOp, Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    Neg(Box<Expr>),
}

impl fmt::Display for Pred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_pred(f, self, 0)
    }
}

fn prec(op: BinOp) -> u8 {
    match op {
        BinOp::Or | BinOp::Implies => 1,
        BinOp::And => 2,
        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => 3,
        BinOp::Add | BinOp::Sub => 4,
        BinOp::Mul => 5,
    }
}

fn write_pred(f: &mut fmt::Formatter<'_>, p: &Pred, ctx: u8) -> fmt::Result {
    match p {
        Pred::Bool(b) => write!(f, "{}", b),
        Pred::Int(n) => write!(f, "{}", n),
        Pred::Var(x) => write!(f, "{}", x),
        Pred::Not(inner) => {
            write!(f, "!")?;
            write_pred(f, inner, 6)
        }
        Pred::Neg(inner) => {
            write!(f, "-")?;
            write_pred(f, inner, 6)
        }
        Pred::Bin(op, a, b) => {
            let p = prec(*op);
            let need = p < ctx;
            if need {
                write!(f, "(")?;
            }
            write_pred(f, a, p)?;
            write!(f, " {} ", op.as_str())?;
            write_pred(f, b, p + 1)?;
            if need {
                write!(f, ")")?;
            }
            Ok(())
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Refine(v, b, Pred::Bool(true)) => {
                let _ = v;
                write!(f, "{}", b)
            }
            Type::Refine(v, b, p) => write!(f, "{{ {} : {} | {} }}", v, b, p),
            Type::Fun(x, t1, t2) => {
                let dep = mentions(t2, x);
                let left = match t1.as_ref() {
                    Type::Fun(_, _, _) => format!("({})", t1),
                    _ => format!("{}", t1),
                };
                if dep {
                    write!(f, "({} : {}) -> {}", x, left, t2)
                } else {
                    write!(f, "{} -> {}", left, t2)
                }
            }
        }
    }
}

pub fn mentions(t: &Type, x: &str) -> bool {
    match t {
        Type::Refine(v, _, p) => v != x && pred_mentions(p, x),
        Type::Fun(y, t1, t2) => mentions(t1, x) || (y != x && mentions(t2, x)),
    }
}

pub fn pred_mentions(p: &Pred, x: &str) -> bool {
    match p {
        Pred::Var(y) => y == x,
        Pred::Bin(_, a, b) => pred_mentions(a, x) || pred_mentions(b, x),
        Pred::Not(a) | Pred::Neg(a) => pred_mentions(a, x),
        _ => false,
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_expr(f, self, 0)
    }
}

fn expr_prec(e: &Expr) -> u8 {
    match e {
        Expr::Let(_, _, _) | Expr::If(_, _, _) | Expr::Lam(_, _, _) | Expr::Ann(_, _) => 0,
        Expr::Bin(op, _, _) => prec(*op),
        Expr::App(_, _) | Expr::Not(_) | Expr::Neg(_) => 7,
        _ => 8,
    }
}

fn write_expr(f: &mut fmt::Formatter<'_>, e: &Expr, ctx: u8) -> fmt::Result {
    let p = expr_prec(e);
    let need = p < ctx;
    if need {
        write!(f, "(")?;
    }
    match e {
        Expr::Int(n) => write!(f, "{}", n)?,
        Expr::Bool(b) => write!(f, "{}", b)?,
        Expr::Var(x) => write!(f, "{}", x)?,
        Expr::Let(x, v, body) => write!(f, "let {} = {} in {}", x, v, body)?,
        Expr::If(c, a, b) => write!(f, "if {} then {} else {}", c, a, b)?,
        Expr::Lam(x, t, body) => write!(f, "\\{} : {} -> {}", x, t, body)?,
        Expr::App(g, a) => {
            write_expr(f, g, 7)?;
            write!(f, " ")?;
            write_expr(f, a, 8)?;
        }
        Expr::Ann(e, t) => write!(f, "({} : {})", e, t)?,
        Expr::Bin(op, a, b) => {
            write_expr(f, a, p)?;
            write!(f, " {} ", op.as_str())?;
            write_expr(f, b, p + 1)?;
        }
        Expr::Not(a) => {
            write!(f, "!")?;
            write_expr(f, a, 7)?;
        }
        Expr::Neg(a) => {
            write!(f, "-")?;
            write_expr(f, a, 7)?;
        }
    }
    if need {
        write!(f, ")")?;
    }
    Ok(())
}
