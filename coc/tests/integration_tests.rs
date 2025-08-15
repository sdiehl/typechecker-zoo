use std::path::Path;

use coc::{run_golden_test, run_module_golden_test};

/// Test that all term-level test files pass their golden tests
#[test]
fn test_term_golden_files() {
    let test_files = vec![("tests/basic.coc", "tests/basic.out")];

    for (input_file, expected_file) in test_files {
        // Check if both files exist
        if !Path::new(input_file).exists() {
            panic!("Input file '{}' does not exist", input_file);
        }
        if !Path::new(expected_file).exists() {
            panic!("Expected file '{}' does not exist", expected_file);
        }

        // Run the golden test
        match run_golden_test(input_file, expected_file) {
            Ok(()) => {
                println!("✓ Term golden test passed: {}", input_file);
            }
            Err(e) => {
                panic!("✗ Term golden test failed for {}: {}", input_file, e);
            }
        }
    }
}

/// Test that all module files pass their golden tests
#[test]
fn test_module_golden_files() {
    let test_files = vec![
        ("tests/final_demo.coc", "tests/final_demo.out"),
        ("tests/target_compose.coc", "tests/target_compose.out"),
        ("tests/working_square.coc", "tests/working_square.out"),
        ("tests/nat.coc", "tests/nat.out"),
    ];

    for (input_file, expected_file) in test_files {
        // Check if both files exist
        if !Path::new(input_file).exists() {
            panic!("Input file '{}' does not exist", input_file);
        }
        if !Path::new(expected_file).exists() {
            panic!("Expected file '{}' does not exist", expected_file);
        }

        // Run the module golden test
        match run_module_golden_test(input_file, expected_file) {
            Ok(()) => {
                println!("✓ Module golden test passed: {}", input_file);
            }
            Err(e) => {
                panic!("✗ Module golden test failed for {}: {}", input_file, e);
            }
        }
    }
}

#[test]
fn test_basic_terms() {
    use coc::check_term;

    // Test basic terms
    let tests = vec![("Type", "Type : Type 1"), ("Prop", "Prop : Type")];

    for (input, expected) in tests {
        match check_term(input) {
            Ok(result) => {
                assert_eq!(result, expected);
                println!("✓ {}", result);
            }
            Err(e) => {
                panic!("✗ Failed to check term '{}': {}", input, e);
            }
        }
    }
}
