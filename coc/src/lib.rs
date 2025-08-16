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

/// Check a module file and return success/failure message
pub fn check_module_file(filename: &str) -> CocResult<String> {
    use std::fs;

    let content = fs::read_to_string(filename)?;

    let parser = Parser::new();
    let module = parser.parse_module(&content)?;

    let mut type_checker = TypeChecker::new();
    let _final_context = type_checker.check_module(&module)?;

    Ok(format!(
        "Parsed module with {} declarations\n✓ Module '{}' type checks successfully!",
        module.declarations.len(),
        filename
    ))
}

/// Run golden test for modules
pub fn run_module_golden_test(input_file: &str, expected_file: &str) -> Result<(), String> {
    use std::fs;

    // Read expected output file
    let expected_content = fs::read_to_string(expected_file)
        .map_err(|e| format!("Error reading expected file {}: {}", expected_file, e))?;

    // Generate actual result by checking the module
    let actual_output = match check_module_file(input_file) {
        Ok(result) => result,
        Err(e) => format!("ERROR: {}", e),
    };

    if actual_output.trim() == expected_content.trim() {
        Ok(())
    } else {
        Err(format!(
            "Module golden test failed!\nExpected:\n{}\nActual:\n{}",
            expected_content, actual_output
        ))
    }
}

/// Run golden test - compare actual output with expected output
pub fn run_golden_test(input_file: &str, expected_file: &str) -> Result<(), String> {
    use std::fs;

    // Read input file
    let input_content = fs::read_to_string(input_file)
        .map_err(|e| format!("Error reading input file {}: {}", input_file, e))?;

    // Read expected output file
    let expected_content = fs::read_to_string(expected_file)
        .map_err(|e| format!("Error reading expected file {}: {}", expected_file, e))?;

    // Generate actual results
    let mut actual_results = Vec::new();

    for line in input_content.lines() {
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('#') {
            actual_results.push(line.to_string());
            continue;
        }

        let result = match check_term(line) {
            Ok(type_info) => type_info,
            Err(e) => format!("{} : ERROR: {}", line, e),
        };

        actual_results.push(result);
    }

    let actual_output = actual_results.join("\n") + "\n";
    let expected_output = expected_content;

    if actual_output.trim() == expected_output.trim() {
        Ok(())
    } else {
        Err(format!(
            "Golden test failed!\nExpected:\n{}\nActual:\n{}",
            expected_output, actual_output
        ))
    }
}
