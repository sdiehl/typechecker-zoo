//! Closure conversion - transform functions to explicit closures
//!
//! This pass converts all lambda abstractions into explicit closure
//! allocations with captured environments.

use super::erase::Erased;
use crate::core::CoreBinOp;

/// A unique identifier for each function
pub type FunctionId = usize;

/// Closure-converted representation
#[derive(Debug, Clone, PartialEq)]
pub enum Closed {
    /// Variable reference
    Var(String),
    /// Environment projection: project(closure, index)
    Proj(Box<Closed>, usize),
    /// Create closure: closure(function_id, [captured_vars])
    MakeClosure(FunctionId, Vec<Closed>),
    /// Direct function call: call(closure, arg)
    Call(Box<Closed>, Box<Closed>),
    /// Integer literal
    Int(i64),
    /// Binary operation
    BinOp(CoreBinOp, Box<Closed>, Box<Closed>),
    /// Conditional
    If(Box<Closed>, Box<Closed>, Box<Closed>),
    /// Print integer builtin
    PrintInt(Box<Closed>),
}

/// A top-level function definition after closure conversion
#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub param: String,
    pub body: Closed,
    pub free_vars: Vec<String>,
}

/// Result of closure conversion
#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub main: Closed,
}

/// State for closure conversion
struct ClosureConverter {
    next_id: FunctionId,
    functions: Vec<Function>,
}

impl ClosureConverter {
    fn new() -> Self {
        Self {
            next_id: 0,
            functions: Vec::new(),
        }
    }

    fn fresh_id(&mut self) -> FunctionId {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    /// Convert with knowledge of module-level functions
    fn convert_with_modules(
        &mut self,
        term: &Erased,
        env: &[(String, usize)],
        module_funcs: &std::collections::HashMap<String, FunctionId>,
    ) -> Closed {
        match term {
            Erased::Var(name) => {
                // Check if it's a module function
                if let Some(&func_id) = module_funcs.get(name) {
                    // Check if this is a thunk (non-lambda definition)
                    // by looking at the function we've already created
                    let is_thunk = self
                        .functions
                        .iter()
                        .any(|f| f.id == func_id && f.param == "_unit");

                    if is_thunk {
                        // Call the thunk with a dummy argument (0)
                        Closed::Call(
                            Box::new(Closed::MakeClosure(func_id, vec![])),
                            Box::new(Closed::Int(0)),
                        )
                    } else {
                        // Regular function - create a closure with no captured variables
                        Closed::MakeClosure(func_id, vec![])
                    }
                } else if let Some((_, idx)) = env.iter().find(|(n, _)| n == name) {
                    // This is a captured variable - project from closure
                    Closed::Proj(Box::new(Closed::Var("$closure".into())), *idx)
                } else {
                    // Free variable or parameter
                    Closed::Var(name.clone())
                }
            }

            Erased::Lam(param, body) => {
                // Collect free variables (excluding the parameter and module functions)
                let mut free_vars = body.free_vars();
                free_vars.retain(|v| v != param && !module_funcs.contains_key(v));

                // Create environment mapping for the body
                let mut body_env = vec![("$closure".into(), 0)];
                for (i, fv) in free_vars.iter().enumerate() {
                    body_env.push((fv.clone(), i));
                }

                // Convert the body with the new environment
                let body_closed = self.convert_with_modules(body, &body_env, module_funcs);

                // Create a new function
                let id = self.fresh_id();
                self.functions.push(Function {
                    id,
                    param: param.clone(),
                    body: body_closed,
                    free_vars: free_vars.clone(),
                });

                // Create closure capturing the free variables
                let captured = free_vars
                    .into_iter()
                    .map(|fv| {
                        if let Some((_, idx)) = env.iter().find(|(n, _)| n == &fv) {
                            Closed::Proj(Box::new(Closed::Var("$closure".into())), *idx)
                        } else {
                            Closed::Var(fv)
                        }
                    })
                    .collect();

                Closed::MakeClosure(id, captured)
            }

            Erased::App(f, x) => {
                let f_closed = self.convert_with_modules(f, env, module_funcs);
                let x_closed = self.convert_with_modules(x, env, module_funcs);
                Closed::Call(Box::new(f_closed), Box::new(x_closed))
            }

            Erased::Int(n) => Closed::Int(*n),

            Erased::BinOp(op, l, r) => {
                let l_closed = self.convert_with_modules(l, env, module_funcs);
                let r_closed = self.convert_with_modules(r, env, module_funcs);
                Closed::BinOp(op.clone(), Box::new(l_closed), Box::new(r_closed))
            }

            Erased::If(c, t, e) => {
                let c_closed = self.convert_with_modules(c, env, module_funcs);
                let t_closed = self.convert_with_modules(t, env, module_funcs);
                let e_closed = self.convert_with_modules(e, env, module_funcs);
                Closed::If(Box::new(c_closed), Box::new(t_closed), Box::new(e_closed))
            }

            Erased::PrintInt(arg) => {
                let arg_closed = self.convert_with_modules(arg, env, module_funcs);
                Closed::PrintInt(Box::new(arg_closed))
            }
        }
    }
}

/// Perform closure conversion with module context
pub fn closure_convert_module(functions: Vec<(&str, &Erased)>, main: &Erased) -> Program {
    let mut converter = ClosureConverter::new();

    // First, assign IDs to all module functions
    let mut module_funcs = std::collections::HashMap::new();
    for (name, _) in &functions {
        let id = converter.fresh_id();
        module_funcs.insert(name.to_string(), id);
    }

    // Convert each function body
    for (name, body) in functions {
        match body {
            Erased::Lam(param, func_body) => {
                // Get free variables
                let mut free_vars = func_body.free_vars();
                free_vars.retain(|v| v != param && !module_funcs.contains_key(v));

                // Create environment mapping
                let mut body_env = vec![("$closure".into(), 0)];
                for (i, fv) in free_vars.iter().enumerate() {
                    body_env.push((fv.clone(), i));
                }

                // Convert the body
                let body_closed =
                    converter.convert_with_modules(func_body, &body_env, &module_funcs);

                converter.functions.push(Function {
                    id: module_funcs[name],
                    param: param.clone(),
                    body: body_closed,
                    free_vars,
                });
            }
            _ => {
                // For non-lambda definitions (like add5 = add 5),
                // convert them to thunks (lambdas that take a dummy parameter)
                let body_closed = converter.convert_with_modules(body, &[], &module_funcs);

                converter.functions.push(Function {
                    id: module_funcs[name],
                    param: "_unit".to_string(), // Dummy parameter
                    body: body_closed,
                    free_vars: vec![],
                });
            }
        }
    }

    // Convert main
    let main_closed = converter.convert_with_modules(main, &[], &module_funcs);

    Program {
        functions: converter.functions,
        main: main_closed,
    }
}

impl Closed {
    /// Pretty print closed terms
    pub fn pretty(&self) -> String {
        match self {
            Closed::Var(name) => name.to_string(),
            Closed::Proj(closure, idx) => format!("{}[{}]", closure.pretty(), idx),
            Closed::MakeClosure(id, captured) => {
                let caps: Vec<String> = captured.iter().map(|c| c.pretty()).collect();
                format!("closure({}, [{}])", id, caps.join(", "))
            }
            Closed::Call(f, x) => format!("{}({})", f.pretty(), x.pretty()),
            Closed::Int(n) => n.to_string(),
            Closed::BinOp(op, l, r) => format!("{} {:?} {}", l.pretty(), op, r.pretty()),
            Closed::If(c, t, e) => {
                format!("if {} then {} else {}", c.pretty(), t.pretty(), e.pretty())
            }
            Closed::PrintInt(arg) => format!("printInt({})", arg.pretty()),
        }
    }
}
