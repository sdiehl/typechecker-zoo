use std::fs;
use std::path::Path;

use system_f::{parser, testing};

/// Run a golden test for integration testing
fn run_local_golden_test(input_file: &str, expected_file: &str) -> Result<(), String> {
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

        let result = match parser::ExprParser::new().parse(line) {
            Ok(expr) => match testing::infer_type_only(&expr) {
                Ok(ty) => format!("{} : {}", line, ty),
                Err(e) => format!("{} : ERROR: {}", line, e),
            },
            Err(e) => format!("{} : PARSE ERROR: {}", line, e),
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

/// Test that all test files pass their golden tests
#[test]
fn test_all_golden_files() {
    let test_files = vec![
        ("tests/basic.fun", "tests/basic.out"),
        ("tests/edge_cases.fun", "tests/edge_cases.out"),
        (
            "tests/impredicative_cases.fun",
            "tests/impredicative_cases.out",
        ),
        (
            "tests/subtyping_edge_cases.fun",
            "tests/subtyping_edge_cases.out",
        ),
    ];

    for (input_file, expected_file) in test_files {
        // Check if both files exist
        if !Path::new(input_file).exists() {
            panic!("Input file '{}' does not exist", input_file);
        }
        if !Path::new(expected_file).exists() {
            panic!("Expected file '{}' does not exist", expected_file);
        }

        // Run the golden test
        match run_local_golden_test(input_file, expected_file) {
            Ok(()) => {
                println!("✓ Golden test passed: {}", input_file);
            }
            Err(e) => {
                panic!("✗ Golden test failed for {}: {}", input_file, e);
            }
        }
    }
}
