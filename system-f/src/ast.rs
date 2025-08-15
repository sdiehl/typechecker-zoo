// Value-level AST
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Abs(String, Box<Type>, Box<Expr>),
    TApp(Box<Expr>, Box<Type>),
    TAbs(String, Box<Expr>),
    Ann(Box<Expr>, Box<Type>), // Type annotation: e : T
    LitInt(i64),
    LitBool(bool),
    // New constructs
    Let(String, Box<Expr>, Box<Expr>),           // let x = e1 in e2
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>), // if e1 then e2 else e3
    BinOp(BinOp, Box<Expr>, Box<Expr>),          // Binary operations
}

// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    // Arithmetic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    // Boolean
    And, // &&
    Or,  // ||
    // Comparison
    Eq, // ==
    Ne, // !=
    Lt, // <
    Le, // <=
    Gt, // >
    Ge, // >=
}

// Type-level AST
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(String),                 // α (ordinary type variable)
    ETVar(String),               // ^α (existential type variable)
    Arrow(Box<Type>, Box<Type>), // A -> B
    Forall(String, Box<Type>),   // ∀α. A
    Int,                         // Int
    Bool,                        // Bool
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(name) => write!(f, "{}", name),
            Type::ETVar(name) => write!(f, "^{}", name),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Arrow(t1, t2) => {
                // Add parentheses around left side if it's an arrow to avoid ambiguity
                match t1.as_ref() {
                    Type::Arrow(_, _) => write!(f, "({}) -> {}", t1, t2),
                    _ => write!(f, "{} -> {}", t1, t2),
                }
            }
            Type::Forall(var, ty) => write!(f, "∀{}. {}", var, ty),
        }
    }
}
