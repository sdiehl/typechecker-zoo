pub mod alpha;
pub mod ast;
pub mod context;
pub mod diagnostics;
pub mod errors;
pub mod lexer;
pub mod parse;
pub mod solver;
pub mod term_utils;
pub mod typecheck;
pub mod unification;
pub mod universe_solver;

use context::Context;
use errors::CocResult;
use parse::Parser;
use typecheck::TypeChecker;

/// Type check a module from source code
pub fn typecheck_module(source: &str, _filename: &str) -> CocResult<()> {
    let parser = Parser::new();
    let module = parser.parse_module(source)?;

    let mut type_checker = TypeChecker::new();
    let _final_context = type_checker.check_module(&module)?;

    Ok(())
}

/// Silent version for testing that doesn't print errors
pub fn typecheck_module_silent(source: &str, filename: &str) -> CocResult<()> {
    typecheck_module(source, filename)
}

/// Parse and type check a single term
pub fn check_term(source: &str) -> CocResult<String> {
    let parser = Parser::new();
    let term = parser.parse_term(source)?;

    let mut type_checker = TypeChecker::new();
    let ctx = Context::new();
    let inferred_type = type_checker.infer(&term, &ctx)?;

    Ok(format!("{} : {}", term, inferred_type))
}

/// Process term-level input (one term per line) into per-line output.
pub fn process_term_lines(input_content: &str) -> Vec<String> {
    let mut results = Vec::new();
    for line in input_content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            results.push(line.to_string());
            continue;
        }
        let result = match check_term(line) {
            Ok(type_info) => type_info,
            Err(e) => format!("{} : ERROR: {}", line, e),
        };
        results.push(result);
    }
    results
}

/// Type-check a module from source text and return a single-line summary.
pub fn check_module_string(source: &str, filename: &str) -> String {
    let parser = Parser::new();
    let module = match parser.parse_module(source) {
        Ok(m) => m,
        Err(e) => return format!("ERROR: {}", e),
    };
    let mut type_checker = TypeChecker::new();
    match type_checker.check_module(&module) {
        Ok(_) => format!(
            "Parsed module with {} declarations\n✓ Module '{}' type checks successfully!",
            module.declarations.len(),
            filename
        ),
        Err(e) => format!("ERROR: {}", e),
    }
}
