// Calculus of Constructions AST with Lean 4 syntax conventions

use std::fmt;

/// Core terms in the Calculus of Constructions
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// Variables: x, y, z
    Var(String),

    /// Application: f a
    App(Box<Term>, Box<Term>),

    /// Lambda abstraction: λ x : A, t  (written as fun x : A => t)
    Abs(String, Box<Term>, Box<Term>),

    /// Dependent product: Π x : A, B  (written as (x : A) → B or {x : A} → B)
    Pi(String, Box<Term>, Box<Term>, bool), // bool = implicit

    /// Sort/Type: Sort u, Type, Prop
    Sort(Universe),

    /// Let binding: let x := t in s
    Let(String, Box<Term>, Box<Term>, Box<Term>),

    /// Match expression with patterns
    Match(Box<Term>, Vec<MatchArm>),

    /// Inductive type constructor
    Constructor(String, Vec<Term>),

    /// Local constant (for definitions and axioms)
    Const(String),

    /// Meta-variable for type inference
    Meta(String),

    /// Field projection: term.field
    Proj(Box<Term>, String),

    /// Dependent pair type (Sigma type): Σ (x : A), B
    Sigma(String, Box<Term>, Box<Term>),

    /// Pair constructor: ⟨a, b⟩
    Pair(Box<Term>, Box<Term>),

    /// First projection: π₁
    Fst(Box<Term>),

    /// Second projection: π₂
    Snd(Box<Term>),
}

/// Universe levels for type theory
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Universe {
    Const(u32),                         // Concrete level: 0, 1, 2, ...
    ScopedVar(String, String),          // Scoped universe variable: (scope_name, var_name)
    Meta(u32),                          // Universe metavariable: ?u₀, ?u₁, ...
    Add(Box<Universe>, u32),            // Level + n
    Max(Box<Universe>, Box<Universe>),  // max(u, v)
    IMax(Box<Universe>, Box<Universe>), // imax(u, v)
}

/// Universe level constraints for polymorphism
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UniverseConstraint {
    /// u ≤ v
    LessEq(Universe, Universe),
    /// u = v
    Equal(Universe, Universe),
}

/// Context for universe level variables
#[derive(Debug, Clone, PartialEq)]
pub struct UniverseContext {
    /// Currently bound universe variables
    pub variables: Vec<String>,
    /// Constraints on universe levels
    pub constraints: Vec<UniverseConstraint>,
}

/// Pattern matching arms
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Term,
}

/// Patterns for match expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Variable pattern: x
    Var(String),

    /// Constructor pattern: Cons x xs
    Constructor(String, Vec<Pattern>),

    /// Wildcard pattern: _
    Wildcard,
}

/// Top-level declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Constant definition: def name {u...} (params) : type := body
    Definition {
        name: String,
        universe_params: Vec<String>, // Universe level parameters
        params: Vec<Parameter>,
        ty: Term,
        body: Term,
    },

    /// Axiom: axiom name {u...} : type
    Axiom {
        name: String,
        universe_params: Vec<String>, // Universe level parameters
        ty: Term,
    },

    /// Inductive type: inductive Name {u...} (params) : Type := constructors
    Inductive {
        name: String,
        universe_params: Vec<String>, // Universe level parameters
        params: Vec<Parameter>,
        ty: Term,
        constructors: Vec<Constructor>,
    },

    /// Structure (single constructor inductive): structure Name {u...} :=
    /// (fields)
    Structure {
        name: String,
        universe_params: Vec<String>, // Universe level parameters
        params: Vec<Parameter>,
        ty: Term,
        fields: Vec<Field>,
    },
}

/// Function parameters
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub ty: Term,
    pub implicit: bool, // for {x : A} vs (x : A)
}

/// Constructor for inductive types
#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    pub name: String,
    pub ty: Term,
}

/// Structure fields
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: Term,
}

/// Module containing multiple declarations
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

// Display implementations for pretty printing
impl fmt::Display for Universe {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Universe::Const(n) => write!(f, "{}", n),
            Universe::ScopedVar(scope, var) => write!(f, "{}::{}", scope, var),
            Universe::Meta(id) => write!(f, "?u{}", id),
            Universe::Add(u, n) => write!(f, "{}+{}", u, n),
            Universe::Max(u, v) => write!(f, "max({}, {})", u, v),
            Universe::IMax(u, v) => write!(f, "imax({}, {})", u, v),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Var(x) => write!(f, "{}", x),
            Term::App(t1, t2) => write!(f, "({} {})", t1, t2),
            Term::Abs(x, ty, body) => write!(f, "(fun {} : {} => {})", x, ty, body),
            Term::Pi(x, ty, body, implicit) => {
                if Self::occurs_free(x, body) {
                    if *implicit {
                        write!(f, "{{{}  : {}}} → {}", x, ty, body)
                    } else {
                        write!(f, "({} : {}) → {}", x, ty, body)
                    }
                } else {
                    write!(f, "{} → {}", ty, body)
                }
            }
            Term::Sort(u) => match u {
                Universe::Const(0) => write!(f, "Prop"),
                Universe::Const(n) => {
                    if *n == 1 {
                        write!(f, "Type")
                    } else {
                        write!(f, "Type {}", n - 1)
                    }
                }
                Universe::Add(ref base, n) => {
                    if let Universe::Const(1) = **base {
                        write!(f, "Type {}", n)
                    } else {
                        write!(f, "Sort {}", u)
                    }
                }
                _ => write!(f, "Sort {}", u),
            },
            Term::Let(x, ty, val, body) => write!(f, "(let {} : {} := {} in {})", x, ty, val, body),
            Term::Match(scrutinee, arms) => {
                writeln!(f, "match {} with", scrutinee)?;
                for arm in arms {
                    writeln!(f, "| {} => {}", arm.pattern, arm.body)?;
                }
                Ok(())
            }
            Term::Constructor(name, args) => {
                write!(f, "{}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                Ok(())
            }
            Term::Const(name) => write!(f, "{}", name),
            Term::Meta(name) => write!(f, "?{}", name),
            Term::Proj(term, field) => write!(f, "{}.{}", term, field),
            Term::Sigma(x, domain, codomain) => write!(f, "Σ ({} : {}), {}", x, domain, codomain),
            Term::Pair(fst, snd) => write!(f, "⟨{}, {}⟩", fst, snd),
            Term::Fst(pair) => write!(f, "π₁({})", pair),
            Term::Snd(pair) => write!(f, "π₂({})", pair),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Var(x) => write!(f, "{}", x),
            Pattern::Constructor(name, args) => {
                write!(f, "{}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                Ok(())
            }
            Pattern::Wildcard => write!(f, "_"),
        }
    }
}

impl Term {
    /// Check if variable occurs free in term
    pub fn occurs_free(var: &str, term: &Term) -> bool {
        match term {
            Term::Var(x) => x == var,
            Term::App(t1, t2) => Self::occurs_free(var, t1) || Self::occurs_free(var, t2),
            Term::Abs(x, ty, body) => {
                Self::occurs_free(var, ty) || (x != var && Self::occurs_free(var, body))
            }
            Term::Pi(x, ty, body, _) => {
                Self::occurs_free(var, ty) || (x != var && Self::occurs_free(var, body))
            }
            Term::Sort(_) => false,
            Term::Let(x, ty, val, body) => {
                Self::occurs_free(var, ty)
                    || Self::occurs_free(var, val)
                    || (x != var && Self::occurs_free(var, body))
            }
            Term::Match(scrutinee, arms) => {
                Self::occurs_free(var, scrutinee)
                    || arms.iter().any(|arm| Self::occurs_free(var, &arm.body))
            }
            Term::Constructor(_, args) => args.iter().any(|arg| Self::occurs_free(var, arg)),
            Term::Const(_) => false,
            Term::Meta(_) => false,
            Term::Proj(term, _) => Self::occurs_free(var, term),
            Term::Sigma(x, domain, codomain) => {
                Self::occurs_free(var, domain) || (x != var && Self::occurs_free(var, codomain))
            }
            Term::Pair(fst, snd) => Self::occurs_free(var, fst) || Self::occurs_free(var, snd),
            Term::Fst(pair) | Term::Snd(pair) => Self::occurs_free(var, pair),
        }
    }
}

impl UniverseContext {
    pub fn new() -> Self {
        UniverseContext {
            variables: Vec::new(),
            constraints: Vec::new(),
        }
    }

    /// Add a universe variable to the context
    pub fn add_var(&mut self, name: String) {
        if !self.variables.contains(&name) {
            self.variables.push(name);
        }
    }

    /// Add a constraint to the context
    pub fn add_constraint(&mut self, constraint: UniverseConstraint) {
        self.constraints.push(constraint);
    }

    /// Check if a universe variable is bound in this context
    pub fn contains(&self, name: &str) -> bool {
        self.variables.contains(&name.to_string())
    }

    /// Extend context with variables from another context
    pub fn extend(&self, other: &UniverseContext) -> Self {
        let mut new_ctx = self.clone();
        for var in &other.variables {
            new_ctx.add_var(var.clone());
        }
        for constraint in &other.constraints {
            new_ctx.add_constraint(constraint.clone());
        }
        new_ctx
    }

    /// Generate a fresh universe variable name
    pub fn fresh_var(&self, base: &str) -> String {
        let mut counter = 0;
        loop {
            let name = if counter == 0 {
                base.to_string()
            } else {
                format!("{}{}", base, counter)
            };
            if !self.contains(&name) {
                return name;
            }
            counter += 1;
        }
    }
}

impl Default for UniverseContext {
    fn default() -> Self {
        Self::new()
    }
}

impl Universe {
    /// Create a scoped universe variable
    pub fn scoped_var(scope: String, name: String) -> Self {
        Universe::ScopedVar(scope, name)
    }
}
