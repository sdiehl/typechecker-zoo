// Value-level AST
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),                                 // Variable: x
    App(Box<Expr>, Box<Expr>),                   // Application: e1 e2
    Abs(String, Box<Type>, Box<Expr>),           // Lambda abstraction: λx: T. e
    TApp(Box<Expr>, Box<Type>),                  // Type application: e [T]
    TAbs(String, Box<Expr>),                     // Type abstraction: Λα. e
    Ann(Box<Expr>, Box<Type>),                   // Type annotation: e : T
    LitInt(i64),                                 // Integer literal
    LitBool(bool),                               // Boolean literal
    Let(String, Box<Expr>, Box<Expr>),           // Let binding: let x = e1 in e2
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>), // Conditional: if e1 then e2 else e3
    BinOp(BinOp, Box<Expr>, Box<Expr>),          // Binary operation: e1 op e2
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
