use std::path::Path;

use algorithm_w::run_golden_test;

/// Test that all test files pass their golden tests
#[test]
fn test_all_golden_files() {
    let test_files = vec![("tests/basic.fun", "tests/basic.out")];

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
                println!("✓ Golden test passed: {}", input_file);
            }
            Err(e) => {
                panic!("✗ Golden test failed for {}: {}", input_file, e);
            }
        }
    }
}
