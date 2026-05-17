#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Lit(Lit),
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    EmptyRecord,
    Extend(String, Box<Expr>, Box<Expr>),
    Select(Box<Expr>, String),
    Restrict(Box<Expr>, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(String),
    Int,
    Bool,
    Arrow(Box<Type>, Box<Type>),
    Record(Box<Row>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Row {
    Empty,
    Var(String),
    Extend(String, Box<Type>, Box<Row>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    pub type_vars: Vec<String>,
    pub row_vars: Vec<String>,
    pub ty: Type,
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lit(Lit::Int(n)) => write!(f, "{}", n),
            Expr::Lit(Lit::Bool(b)) => write!(f, "{}", b),
            Expr::Abs(param, body) => write!(f, "\\{} -> {}", param, body),
            Expr::App(func, arg) => match (func.as_ref(), arg.as_ref()) {
                (Expr::Abs(_, _), _) => write!(f, "({}) {}", func, arg),
                (_, Expr::App(_, _)) => write!(f, "{} ({})", func, arg),
                (_, Expr::Abs(_, _)) => write!(f, "{} ({})", func, arg),
                _ => write!(f, "{} {}", func, arg),
            },
            Expr::Let(var, value, body) => {
                write!(f, "let {} = {} in {}", var, value, body)
            }
            Expr::EmptyRecord => write!(f, "{{}}"),
            Expr::Extend(_, _, _) => {
                let (fields, tail) = collect_extend(self);
                write!(f, "{{")?;
                for (i, (label, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} = {}", label, value)?;
                }
                match tail {
                    Expr::EmptyRecord => write!(f, "}}"),
                    other => write!(f, " | {}}}", other),
                }
            }
            Expr::Select(e, label) => write!(f, "{}.{}", e, label),
            Expr::Restrict(e, label) => write!(f, "{{{} - {}}}", e, label),
        }
    }
}

fn collect_extend(mut e: &Expr) -> (Vec<(String, &Expr)>, &Expr) {
    let mut fields = Vec::new();
    while let Expr::Extend(l, v, rest) = e {
        fields.push((l.clone(), v.as_ref()));
        e = rest.as_ref();
    }
    (fields, e)
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(name) => write!(f, "{}", name),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Arrow(t1, t2) => match t1.as_ref() {
                Type::Arrow(_, _) => write!(f, "({}) -> {}", t1, t2),
                _ => write!(f, "{} -> {}", t1, t2),
            },
            Type::Record(row) => write!(f, "{{{}}}", row),
        }
    }
}

impl std::fmt::Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (fields, tail) = collect_row(self);
        for (i, (label, ty)) in fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} : {}", label, ty)?;
        }
        match tail {
            Row::Empty => Ok(()),
            Row::Var(name) => {
                if fields.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(f, " | {}", name)
                }
            }
            Row::Extend(_, _, _) => unreachable!("collect_row drains all extensions"),
        }
    }
}

fn collect_row(mut r: &Row) -> (Vec<(String, &Type)>, &Row) {
    let mut fields = Vec::new();
    while let Row::Extend(l, t, rest) = r {
        fields.push((l.clone(), t.as_ref()));
        r = rest.as_ref();
    }
    (fields, r)
}

impl std::fmt::Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.type_vars.is_empty() && self.row_vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "forall")?;
            for v in &self.type_vars {
                write!(f, " {}", v)?;
            }
            for v in &self.row_vars {
                write!(f, " {}", v)?;
            }
            write!(f, ". {}", self.ty)
        }
    }
}

pub fn build_extend(fields: Vec<(String, Box<Expr>)>, tail: Box<Expr>) -> Box<Expr> {
    let mut acc = tail;
    for (label, value) in fields.into_iter().rev() {
        acc = Box::new(Expr::Extend(label, value, acc));
    }
    acc
}
