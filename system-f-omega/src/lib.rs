pub mod builtins;
pub mod codegen;
pub mod core;
pub mod errors;
pub mod lexer;
pub mod parse;
pub mod surface;
pub mod typecheck;
pub mod worklist;

use std::collections::HashMap;
use std::fmt;

use ariadne::Source;
use errors::CompilerError;
use parse::Parser;
use typecheck::Compiler;

#[derive(Debug)]
pub enum LibError {
    ParseError,
    CompilationError,
    TypeCheckError,
    IoError,
}

impl fmt::Display for LibError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LibError::ParseError => write!(f, "Parse error"),
            LibError::CompilationError => write!(f, "Compilation error"),
            LibError::TypeCheckError => write!(f, "Type check error"),
            LibError::IoError => write!(f, "IO error"),
        }
    }
}

impl std::error::Error for LibError {}

/// Typecheck a module from source code - mirrors main.rs implementation
pub fn typecheck_module(source: &str, filename: &str) -> Result<(), LibError> {
    typecheck_module_impl(source, filename, true)
}

/// Silent version for testing that doesn't print errors
pub fn typecheck_module_silent(source: &str, filename: &str) -> Result<(), LibError> {
    typecheck_module_impl(source, filename, false)
}

fn typecheck_module_impl(source: &str, filename: &str, verbose: bool) -> Result<(), LibError> {
    // Parse the module
    let parser = Parser::new();
    let surface_module = parser
        .parse_module_with_diagnostics(source, filename)
        .map_err(|_| LibError::ParseError)?;

    // Compile to core language
    let mut compiler = Compiler::new();
    let core_module = match compiler.compile_module(&surface_module) {
        Ok(module) => module,
        Err(CompilerError::Type(type_error)) => {
            if verbose {
                let report = type_error.report(source, filename);
                let _ = report.eprint((filename, Source::from(source)));
            }
            return Err(LibError::TypeCheckError);
        }
        Err(CompilerError::Parse(parse_error)) => {
            if verbose {
                let report = parse_error.report(source, filename);
                let _ = report.eprint((filename, Source::from(source)));
            }
            return Err(LibError::CompilationError);
        }
    };

    // Collect all function types for mutual recursion
    let mut function_types = HashMap::new();
    for term_def in &core_module.term_defs {
        function_types.insert(term_def.name.clone(), term_def.ty.clone());
    }

    // Type-check each function definition with the collected types
    for term_def in &core_module.term_defs {
        let mut inference = worklist::DKInference::with_context(
            compiler.get_data_constructors().clone(),
            function_types.clone(),
        );

        match inference.check_type(&term_def.body, &term_def.ty) {
            Ok(_) => {
                if verbose {
                    println!("Checking {} : {} ... ✓", term_def.name, term_def.ty);
                }
            }
            Err(type_error) => {
                if verbose {
                    println!("Checking {} : {} ... ✗", term_def.name, term_def.ty);
                    let report = type_error.report(source, filename);
                    let _ = report.eprint((filename, Source::from(source)));
                }
                return Err(LibError::TypeCheckError);
            }
        }
    }

    Ok(())
}

/// Check if a specific example file typechecks successfully
pub fn check_example(example_path: &str) -> Result<(), LibError> {
    let source = std::fs::read_to_string(example_path).map_err(|_| LibError::IoError)?;

    typecheck_module(&source, example_path)
}
