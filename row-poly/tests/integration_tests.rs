use std::path::Path;

use row_poly::run_golden_test;

#[test]
fn test_all_golden_files() {
    let test_files = vec![
        ("tests/basic.fun", "tests/basic.out"),
        ("tests/01_basics.fun", "tests/01_basics.out"),
        ("tests/02_scoped_labels.fun", "tests/02_scoped_labels.out"),
        ("tests/03_selection.fun", "tests/03_selection.out"),
        ("tests/04_extension.fun", "tests/04_extension.out"),
        ("tests/05_restriction.fun", "tests/05_restriction.out"),
        ("tests/06_update.fun", "tests/06_update.out"),
        ("tests/07_polymorphism.fun", "tests/07_polymorphism.out"),
        ("tests/08_higher_order.fun", "tests/08_higher_order.out"),
        ("tests/09_termination.fun", "tests/09_termination.out"),
        ("tests/10_errors.fun", "tests/10_errors.out"),
    ];

    for (input_file, expected_file) in test_files {
        if !Path::new(input_file).exists() {
            panic!("Input file '{}' does not exist", input_file);
        }
        if !Path::new(expected_file).exists() {
            panic!("Expected file '{}' does not exist", expected_file);
        }
        match run_golden_test(input_file, expected_file) {
            Ok(()) => println!("Golden test passed: {}", input_file),
            Err(e) => panic!("Golden test failed for {}: {}", input_file, e),
        }
    }
}
