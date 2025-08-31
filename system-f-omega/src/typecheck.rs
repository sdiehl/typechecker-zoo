use std::collections::HashMap;

use crate::builtins;
use crate::core::{
    CaseArm, CoreBinOp, CoreModule, CorePattern, CoreTerm, CoreType, DataConstructor, Kind,
    TermDef, TypeDef,
};
use crate::errors::{CompilerError, CompilerResult, TypeError};
use crate::surface::{self as surface};

/// Environment for compilation from surface to core language
#[derive(Debug, Clone)]
pub struct CompileEnv {
    /// Type constructors and their kinds
    pub type_constructors: HashMap<String, (Kind, Vec<DataConstructor>)>,
    /// Data constructors and their types
    pub data_constructors: HashMap<String, CoreType>,
    /// Term variables and their types
    pub term_vars: HashMap<String, CoreType>,
}

impl Default for CompileEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl CompileEnv {
    pub fn new() -> Self {
        let mut env = CompileEnv {
            type_constructors: HashMap::new(),
            data_constructors: HashMap::new(),
            term_vars: HashMap::new(),
        };

        // Add built-in types
        builtins::add_builtin_types(&mut env.type_constructors, &mut env.data_constructors);
        env
    }

    pub fn add_type_constructor(
        &mut self,
        name: String,
        kind: Kind,
        constructors: Vec<DataConstructor>,
    ) {
        self.type_constructors.insert(name, (kind, constructors));
    }

    pub fn add_data_constructor(&mut self, name: String, ty: CoreType) {
        self.data_constructors.insert(name, ty);
    }

    pub fn lookup_data_constructor(&self, name: &str) -> Option<&CoreType> {
        self.data_constructors.get(name)
    }

    pub fn get_data_constructors(&self) -> &HashMap<String, CoreType> {
        &self.data_constructors
    }
}

#[allow(clippy::result_large_err)]
pub struct Compiler {
    env: CompileEnv,
}

#[allow(clippy::result_large_err)]
impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            env: CompileEnv::new(),
        }
    }

    pub fn get_data_constructors(&self) -> &HashMap<String, CoreType> {
        self.env.get_data_constructors()
    }

    pub fn compile_module(&mut self, module: &surface::Module) -> CompilerResult<CoreModule> {
        // First pass: collect all type and data constructor definitions
        for decl in &module.declarations {
            if let surface::Declaration::Data {
                name,
                type_params,
                constructors,
            } = decl
            {
                self.collect_data_declaration(name, type_params, constructors)?;
            }
        }

        // Second pass: compile function signatures and bodies
        let mut type_defs = Vec::new();
        let mut term_defs = Vec::new();

        // Collect type signatures first
        let mut signatures: HashMap<String, surface::TypeScheme> = HashMap::new();
        for decl in &module.declarations {
            if let surface::Declaration::TypeSig { name, type_scheme } = decl {
                signatures.insert(name.clone(), type_scheme.clone());
            }
        }

        // Add all function signatures to term_vars for mutual recursion
        for (name, type_scheme) in &signatures {
            let core_type = self.compile_type_scheme(type_scheme)?;
            self.env.term_vars.insert(name.clone(), core_type);
        }

        // Process function definitions
        for decl in &module.declarations {
            match decl {
                surface::Declaration::Data {
                    name,
                    type_params,
                    constructors,
                } => {
                    let kind = self.infer_data_kind(type_params);
                    let core_constructors =
                        self.compile_constructors(name, type_params, constructors)?;
                    type_defs.push(TypeDef {
                        name: name.clone(),
                        kind,
                        constructors: core_constructors,
                    });
                }
                surface::Declaration::FunDef { name, params, body } => {
                    let type_scheme =
                        signatures
                            .get(name)
                            .ok_or_else(|| TypeError::MissingTypeSignature {
                                name: name.clone(),
                                span: None,
                            })?;

                    let core_type = self.compile_type_scheme(type_scheme)?;
                    let core_body = self.compile_function_body(params, body, &core_type)?;

                    term_defs.push(TermDef {
                        name: name.clone(),
                        ty: core_type,
                        body: core_body,
                    });
                }
                surface::Declaration::TypeSig { .. } => {
                    // Already processed above
                }
            }
        }

        Ok(CoreModule {
            type_defs,
            term_defs,
        })
    }

    fn collect_data_declaration(
        &mut self,
        name: &str,
        type_params: &[String],
        constructors: &[surface::Constructor],
    ) -> CompilerResult<()> {
        let kind = self.infer_data_kind(type_params);

        // Create the data type
        let data_type = if type_params.is_empty() {
            CoreType::Con(name.to_string())
        } else {
            // For parametric types, create applications
            let base = CoreType::Con(name.to_string());
            type_params.iter().fold(base, |acc, param| {
                CoreType::App(Box::new(acc), Box::new(CoreType::Var(param.clone())))
            })
        };

        let mut core_constructors = Vec::new();
        for constructor in constructors {
            let constructor_type =
                self.compile_constructor_type(name, type_params, constructor, &data_type)?;

            core_constructors.push(DataConstructor {
                name: constructor.name.clone(),
                ty: constructor_type.clone(),
            });

            self.env
                .add_data_constructor(constructor.name.clone(), constructor_type.clone());

            // Also add constructors that need arguments as term variables for function use
            if !self.is_nullary_constructor(&constructor_type) {
                self.env
                    .term_vars
                    .insert(constructor.name.clone(), constructor_type);
            }
        }

        self.env
            .add_type_constructor(name.to_string(), kind, core_constructors);
        Ok(())
    }

    fn infer_data_kind(&self, type_params: &[String]) -> Kind {
        type_params.iter().rev().fold(Kind::Star, |acc, _| {
            Kind::Arrow(Box::new(Kind::Star), Box::new(acc))
        })
    }

    fn compile_constructors(
        &mut self,
        type_name: &str,
        type_params: &[String],
        constructors: &[surface::Constructor],
    ) -> CompilerResult<Vec<DataConstructor>> {
        constructors
            .iter()
            .map(|c| {
                let data_type = if type_params.is_empty() {
                    CoreType::Con(type_name.to_string())
                } else {
                    let base = CoreType::Con(type_name.to_string());
                    type_params.iter().fold(base, |acc, param| {
                        CoreType::App(Box::new(acc), Box::new(CoreType::Var(param.clone())))
                    })
                };

                let ty = self.compile_constructor_type(type_name, type_params, c, &data_type)?;
                Ok(DataConstructor {
                    name: c.name.clone(),
                    ty,
                })
            })
            .collect()
    }

    fn compile_constructor_type(
        &self,
        _type_name: &str,
        type_params: &[String],
        constructor: &surface::Constructor,
        result_type: &CoreType,
    ) -> CompilerResult<CoreType> {
        let field_types: Result<Vec<_>, _> = constructor
            .fields
            .iter()
            .map(|f| Self::compile_type(f))
            .collect();
        let field_types = field_types?;

        // Build function type: field1 -> field2 -> ... -> result_type
        let constructor_type = field_types
            .into_iter()
            .rev()
            .fold(result_type.clone(), |acc, field_ty| {
                CoreType::Arrow(Box::new(field_ty), Box::new(acc))
            });

        // Add forall quantification for type parameters
        let quantified_type = type_params
            .iter()
            .rev()
            .fold(constructor_type, |acc, param| {
                CoreType::Forall(param.clone(), Box::new(acc))
            });

        Ok(quantified_type)
    }

    fn compile_type_scheme(&self, scheme: &surface::TypeScheme) -> CompilerResult<CoreType> {
        let core_type = Self::compile_type(&scheme.ty)?;

        // Add forall quantification
        let quantified_type = scheme.quantified.iter().rev().fold(core_type, |acc, var| {
            CoreType::Forall(var.clone(), Box::new(acc))
        });

        Ok(quantified_type)
    }

    fn compile_type(ty: &surface::Type) -> CompilerResult<CoreType> {
        match ty {
            surface::Type::Var(name) => Ok(CoreType::Var(name.clone())),
            surface::Type::Con(name) => Ok(CoreType::Con(name.clone())),
            surface::Type::Arrow(t1, t2) => {
                let core_t1 = Self::compile_type(t1)?;
                let core_t2 = Self::compile_type(t2)?;
                Ok(CoreType::Arrow(Box::new(core_t1), Box::new(core_t2)))
            }
            surface::Type::App(t1, t2) => {
                let core_t1 = Self::compile_type(t1)?;
                let core_t2 = Self::compile_type(t2)?;
                Ok(CoreType::App(Box::new(core_t1), Box::new(core_t2)))
            }
            surface::Type::Record(fields) => {
                // For now, compile records as products with field names in a map
                let field_types: Result<Vec<_>, _> =
                    fields.iter().map(|(_, t)| Self::compile_type(t)).collect();
                Ok(CoreType::Product(field_types?))
            }
            surface::Type::Forall(vars, ty) => {
                // Compile forall types to CoreType::Forall (universal quantification)
                let inner_type = Self::compile_type(ty)?;
                vars.iter().rev().try_fold(inner_type, |acc, var| {
                    Ok(CoreType::Forall(var.clone(), Box::new(acc)))
                })
            }
        }
    }

    fn compile_function_body(
        &mut self,
        params: &[String],
        body: &surface::Expr,
        func_type: &CoreType,
    ) -> CompilerResult<CoreTerm> {
        // Extract parameter types from function type
        let param_types = self.extract_parameter_types(func_type, params.len())?;

        // Create lambda abstractions for each parameter
        let mut core_body = self.compile_expr(body)?;

        for (param, param_ty) in params.iter().zip(param_types.iter()).rev() {
            core_body = CoreTerm::Lambda {
                param: param.clone(),
                param_ty: param_ty.clone(),
                body: Box::new(core_body),
            };
        }

        // Add type lambda abstractions if the function is polymorphic
        self.add_type_lambda_abstractions(core_body, func_type)
    }

    fn extract_parameter_types(
        &self,
        ty: &CoreType,
        param_count: usize,
    ) -> CompilerResult<Vec<CoreType>> {
        let mut current_type = ty;
        let mut param_types = Vec::new();

        // Skip forall quantifiers
        while let CoreType::Forall(_, body) = current_type {
            current_type = body;
        }

        // Extract parameter types from arrow types
        for _ in 0..param_count {
            match current_type {
                CoreType::Arrow(param_ty, result_ty) => {
                    param_types.push(*param_ty.clone());
                    current_type = result_ty;
                }
                _ => {
                    return Err(CompilerError::Type(TypeError::ArityMismatch {
                        expected: param_count,
                        actual: param_types.len(),
                        span: None,
                    }));
                }
            }
        }

        Ok(param_types)
    }

    fn add_type_lambda_abstractions(
        &self,
        mut term: CoreTerm,
        ty: &CoreType,
    ) -> CompilerResult<CoreTerm> {
        let mut current_type = ty;
        let mut type_vars = Vec::new();

        // Collect forall-bound variables
        while let CoreType::Forall(var, body) = current_type {
            type_vars.push(var.clone());
            current_type = body;
        }

        // Add type lambda abstractions (in reverse order)
        for var in type_vars.into_iter().rev() {
            term = CoreTerm::TypeLambda {
                param: var,
                body: Box::new(term),
            };
        }

        Ok(term)
    }

    fn is_nullary_constructor(&self, constructor_type: &CoreType) -> bool {
        let mut current_type = constructor_type;

        // Skip forall quantifiers
        while let CoreType::Forall(_, body) = current_type {
            current_type = body;
        }

        // Check if we have arrow types (indicating parameters)
        // Nullary constructors have no arrow types, just the result type
        !matches!(current_type, CoreType::Arrow(_, _))
    }

    fn compile_expr(&mut self, expr: &surface::Expr) -> CompilerResult<CoreTerm> {
        match expr {
            surface::Expr::Var(name) => {
                // Check if this is the printInt builtin
                if name == "printInt" {
                    // For now, treat it as a regular variable - we'll handle it specially during
                    // application
                    Ok(CoreTerm::Var(name.clone()))
                } else if let Some(constructor_type) = self.env.lookup_data_constructor(name) {
                    // Check if this is a nullary constructor (no arguments required)
                    if self.is_nullary_constructor(constructor_type) {
                        Ok(CoreTerm::Constructor {
                            name: name.clone(),
                            args: vec![],
                        })
                    } else {
                        // Constructor needs arguments, treat as a variable for partial application
                        Ok(CoreTerm::Var(name.clone()))
                    }
                } else {
                    Ok(CoreTerm::Var(name.clone()))
                }
            }

            surface::Expr::LitInt(n) => Ok(CoreTerm::LitInt(*n)),

            surface::Expr::Lambda { param, body } => {
                // For surface lambdas without type annotations, use a placeholder type that
                // will be inferred during type checking. Use 'a' as a generic type variable.
                let param_ty = CoreType::Var("a".to_string());
                let core_body = self.compile_expr(body)?;
                Ok(CoreTerm::Lambda {
                    param: param.clone(),
                    param_ty,
                    body: Box::new(core_body),
                })
            }

            surface::Expr::App { func, arg } => {
                // Check if this is an application of printInt
                if let surface::Expr::Var(name) = &**func {
                    if name == "printInt" {
                        let core_arg = self.compile_expr(arg)?;
                        return Ok(CoreTerm::PrintInt(Box::new(core_arg)));
                    }
                }

                let core_func = self.compile_expr(func)?;
                let core_arg = self.compile_expr(arg)?;
                Ok(CoreTerm::App {
                    func: Box::new(core_func),
                    arg: Box::new(core_arg),
                })
            }

            surface::Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let core_cond = self.compile_expr(cond)?;
                let core_then = self.compile_expr(then_branch)?;
                let core_else = self.compile_expr(else_branch)?;
                Ok(CoreTerm::If {
                    cond: Box::new(core_cond),
                    then_branch: Box::new(core_then),
                    else_branch: Box::new(core_else),
                })
            }

            surface::Expr::Match { expr, arms } => {
                let core_expr = self.compile_expr(expr)?;
                let core_arms: Result<Vec<_>, _> =
                    arms.iter().map(|arm| self.compile_match_arm(arm)).collect();
                Ok(CoreTerm::Case {
                    scrutinee: Box::new(core_expr),
                    arms: core_arms?,
                })
            }

            surface::Expr::BinOp { op, left, right } => {
                let core_left = self.compile_expr(left)?;
                let core_right = self.compile_expr(right)?;
                let core_op = self.compile_binop(op);
                Ok(CoreTerm::BinOp {
                    op: core_op,
                    left: Box::new(core_left),
                    right: Box::new(core_right),
                })
            }
        }
    }

    fn compile_match_arm(&mut self, arm: &surface::MatchArm) -> CompilerResult<CaseArm> {
        let pattern = self.compile_pattern(&arm.pattern)?;
        let body = self.compile_expr(&arm.body)?;
        Ok(CaseArm { pattern, body })
    }

    fn compile_pattern(&self, pattern: &surface::Pattern) -> CompilerResult<CorePattern> {
        match pattern {
            surface::Pattern::Wildcard => Ok(CorePattern::Wildcard),
            surface::Pattern::Var(name) => Ok(CorePattern::Var(name.clone())),
            surface::Pattern::Constructor { name, args } => {
                let core_args: Result<Vec<_>, _> =
                    args.iter().map(|arg| self.compile_pattern(arg)).collect();
                Ok(CorePattern::Constructor {
                    name: name.clone(),
                    args: core_args?,
                })
            }
        }
    }

    fn compile_binop(&self, op: &surface::BinOp) -> CoreBinOp {
        match op {
            surface::BinOp::Add => CoreBinOp::Add,
            surface::BinOp::Sub => CoreBinOp::Sub,
            surface::BinOp::Mul => CoreBinOp::Mul,
            surface::BinOp::Div => CoreBinOp::Div,
            surface::BinOp::Lt => CoreBinOp::Lt,
            surface::BinOp::Le => CoreBinOp::Le,
        }
    }
}
