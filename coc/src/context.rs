use std::collections::HashMap;

use crate::ast::{Term, Universe, UniverseConstraint, UniverseContext};

/// Definition with universe parameters
#[derive(Debug, Clone, PartialEq)]
pub struct Definition {
    /// Universe parameters for the definition
    pub universe_params: Vec<String>,
    /// The type of the definition
    pub ty: Term,
    /// Name of the definition (for scoping universe parameters)
    pub name: String,
}

/// Type checking context containing variable bindings
#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    /// Variable to type bindings
    bindings: HashMap<String, Term>,
    /// Universe variable constraints
    universe_vars: HashMap<String, Universe>,
    /// Constructor definitions
    constructors: HashMap<String, Term>,
    /// Axiom definitions (without universe params - for backwards compat)
    axioms: HashMap<String, Term>,
    /// Definitions with universe parameters
    definitions: HashMap<String, Definition>,
    /// Universe level context for polymorphism
    universe_context: UniverseContext,
}

impl Context {
    pub fn new() -> Self {
        Context {
            bindings: HashMap::new(),
            universe_vars: HashMap::new(),
            constructors: HashMap::new(),
            axioms: HashMap::new(),
            definitions: HashMap::new(),
            universe_context: UniverseContext::new(),
        }
    }

    /// Extend context with variable binding
    pub fn extend(&self, var: String, ty: Term) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.bindings.insert(var, ty);
        new_ctx
    }

    /// Extend context with multiple bindings
    pub fn extend_many(&self, bindings: Vec<(String, Term)>) -> Self {
        let mut new_ctx = self.clone();
        for (var, ty) in bindings {
            new_ctx.bindings.insert(var, ty);
        }
        new_ctx
    }

    /// Look up variable type
    pub fn lookup(&self, var: &str) -> Option<&Term> {
        self.bindings.get(var)
    }

    /// Check if context has a binding for the given variable
    pub fn has_binding(&self, var: &str) -> bool {
        self.bindings.contains_key(var)
    }

    /// Get all variable bindings
    pub fn get_all_bindings(&self) -> impl Iterator<Item = (&String, &Term)> {
        self.bindings.iter()
    }

    /// Look up constructor type
    pub fn lookup_constructor(&self, name: &str) -> Option<&Term> {
        self.constructors.get(name)
    }

    /// Look up axiom type
    pub fn lookup_axiom(&self, name: &str) -> Option<&Term> {
        self.axioms.get(name)
    }

    /// Add constructor definition
    pub fn add_constructor(&mut self, name: String, ty: Term) {
        self.constructors.insert(name, ty);
    }

    /// Add axiom definition
    pub fn add_axiom(&mut self, name: String, ty: Term) {
        self.axioms.insert(name, ty);
    }

    /// Add definition with universe parameters
    pub fn add_definition(&mut self, name: String, universe_params: Vec<String>, ty: Term) {
        self.definitions.insert(
            name.clone(),
            Definition {
                universe_params,
                ty,
                name: name.clone(),
            },
        );
    }

    /// Look up definition with universe parameters
    pub fn lookup_definition(&self, name: &str) -> Option<&Definition> {
        self.definitions.get(name)
    }

    /// Add universe variable constraint
    pub fn add_universe_var(&mut self, var: String, constraint: Universe) {
        self.universe_vars.insert(var, constraint);
    }

    /// Check if variable is bound in context
    pub fn contains(&self, var: &str) -> bool {
        self.bindings.contains_key(var)
    }

    /// Get all variable names
    pub fn variables(&self) -> Vec<&String> {
        self.bindings.keys().collect()
    }

    /// Remove variable from context
    pub fn remove(&self, var: &str) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.bindings.remove(var);
        new_ctx
    }

    /// Get fresh variable name not in context
    pub fn fresh_var(&self, base: &str) -> String {
        let mut counter = 0;
        loop {
            let candidate = if counter == 0 {
                base.to_string()
            } else {
                format!("{}{}", base, counter)
            };

            if !self.contains(&candidate) {
                return candidate;
            }
            counter += 1;
        }
    }

    /// Access universe context
    pub fn universe_context(&self) -> &UniverseContext {
        &self.universe_context
    }

    /// Extend context with universe variable
    pub fn extend_universe(&self, name: String) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.universe_context.add_var(name);
        new_ctx
    }

    /// Extend context with multiple universe variables
    pub fn extend_universe_many(&self, names: Vec<String>) -> Self {
        let mut new_ctx = self.clone();
        for name in names {
            new_ctx.universe_context.add_var(name);
        }
        new_ctx
    }

    /// Add universe constraint
    pub fn add_universe_constraint(&mut self, constraint: UniverseConstraint) {
        self.universe_context.add_constraint(constraint);
    }

    /// Generate fresh universe variable
    pub fn fresh_universe_var(&self, base: &str) -> String {
        self.universe_context.fresh_var(base)
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

/// Type checking errors related to context
#[derive(Debug, Clone, PartialEq)]
pub enum ContextError {
    UnboundVariable { name: String },
    UnboundConstructor { name: String },
    UnboundAxiom { name: String },
    VariableAlreadyBound { name: String },
}

impl std::fmt::Display for ContextError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ContextError::UnboundVariable { name } => {
                write!(f, "Unbound variable: {}", name)
            }
            ContextError::UnboundConstructor { name } => {
                write!(f, "Unbound constructor: {}", name)
            }
            ContextError::UnboundAxiom { name } => {
                write!(f, "Unbound axiom: {}", name)
            }
            ContextError::VariableAlreadyBound { name } => {
                write!(f, "Variable already bound: {}", name)
            }
        }
    }
}

impl std::error::Error for ContextError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Universe;

    #[test]
    fn test_context_operations() {
        let ctx = Context::new();

        let var_type = Term::Sort(Universe::Const(0));
        let ctx2 = ctx.extend("x".to_string(), var_type.clone());

        assert!(ctx2.contains("x"));
        assert_eq!(ctx2.lookup("x"), Some(&var_type));
        assert!(!ctx.contains("x")); // Original context unchanged
    }

    #[test]
    fn test_fresh_var() {
        let mut ctx = Context::new();
        ctx.bindings
            .insert("x".to_string(), Term::Sort(Universe::Const(0)));
        ctx.bindings
            .insert("x1".to_string(), Term::Sort(Universe::Const(0)));

        let fresh = ctx.fresh_var("x");
        assert_eq!(fresh, "x2");

        let fresh2 = ctx.fresh_var("y");
        assert_eq!(fresh2, "y");
    }

    #[test]
    fn test_extend_many() {
        let ctx = Context::new();
        let bindings = vec![
            ("x".to_string(), Term::Sort(Universe::Const(0))),
            ("y".to_string(), Term::Sort(Universe::Const(1))),
        ];

        let ctx2 = ctx.extend_many(bindings);
        assert!(ctx2.contains("x"));
        assert!(ctx2.contains("y"));
    }
}
