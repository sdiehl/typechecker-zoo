pub mod ast;
pub mod errors;
pub mod infer;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}
use std::fs;

use infer::infer_type_only;
pub use parser_impl::parser;

/// Result of running a golden test with detailed line-by-line comparison
#[derive(Debug)]
pub struct GoldenTestResult {
    pub passed: bool,
    pub line_results: Vec<LineResult>,
    pub expected_lines: Vec<String>,
    pub actual_lines: Vec<String>,
}

#[derive(Debug)]
pub struct LineResult {
    pub line_number: usize,
    pub passed: bool,
    pub expected: String,
    pub actual: String,
}

/// Process input lines and generate type inference results
pub fn process_test_lines(input_content: &str) -> Vec<String> {
    let mut results = Vec::new();

    for line in input_content.lines() {
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('#') {
            results.push(line.to_string());
            continue;
        }

        let result = match parser::ExprParser::new().parse(line) {
            Ok(expr) => match infer_type_only(&expr) {
                Ok(ty) => format!("{} : {}", line, ty),
                Err(e) => format!("{} : ERROR: {}", line, e),
            },
            Err(e) => format!("{} : PARSE ERROR: {}", line, e),
        };

        results.push(result);
    }

    results
}

/// Run a detailed golden test comparison
pub fn run_golden_test_detailed(
    input_file: &str,
    expected_file: &str,
) -> Result<GoldenTestResult, String> {
    // Read input file
    let input_content = fs::read_to_string(input_file)
        .map_err(|e| format!("Error reading input file {}: {}", input_file, e))?;

    // Read expected output file
    let expected_content = fs::read_to_string(expected_file)
        .map_err(|e| format!("Error reading expected file {}: {}", expected_file, e))?;

    // Generate actual results
    let actual_results = process_test_lines(&input_content);
    let expected_lines: Vec<String> = expected_content.lines().map(|s| s.to_string()).collect();
    let actual_lines = actual_results;

    // Compare line by line
    let mut line_results = Vec::new();
    let mut all_passed = true;
    let max_lines = expected_lines.len().max(actual_lines.len());

    for i in 0..max_lines {
        let expected_line = expected_lines
            .get(i)
            .cloned()
            .unwrap_or_else(|| "<missing>".to_string());
        let actual_line = actual_lines
            .get(i)
            .cloned()
            .unwrap_or_else(|| "<missing>".to_string());

        let passed = expected_line == actual_line;
        if !passed {
            all_passed = false;
        }

        line_results.push(LineResult {
            line_number: i + 1,
            passed,
            expected: expected_line,
            actual: actual_line,
        });
    }

    Ok(GoldenTestResult {
        passed: all_passed,
        line_results,
        expected_lines,
        actual_lines,
    })
}

/// Simple golden test function for backwards compatibility
pub fn run_golden_test(input_file: &str, expected_file: &str) -> Result<(), String> {
    let result = run_golden_test_detailed(input_file, expected_file)?;
    if result.passed {
        Ok(())
    } else {
        let expected_output = result.expected_lines.join("\n");
        let actual_output = result.actual_lines.join("\n");
        Err(format!(
            "Golden test failed!\nExpected:\n{}\nActual:\n{}",
            expected_output, actual_output
        ))
    }
}
