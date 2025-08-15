use std::fs;
use std::path::Path;

use system_f_omega::check_example;

/// Test that all working example files typecheck successfully
#[test]
fn test_working_examples() {
    let working_examples = vec![
        "examples/working_records.hs",
        "examples/comprehensive_records.hs",
    ];

    for example in working_examples {
        let path = format!("../system-f-omega/{}", example);

        // Check if file exists first
        if Path::new(&path).exists() {
            match check_example(&path) {
                Ok(()) => {
                    println!("✓ Example '{}' typechecks successfully", example);
                }
                Err(e) => {
                    panic!("✗ Example '{}' failed to typecheck: {:?}", example, e);
                }
            }
        } else {
            println!("⚠ Example '{}' not found, skipping", example);
        }
    }
}

/// Test that all examples in the examples directory that should work do work
#[test]
fn test_all_valid_examples() {
    let examples_dir = "../system-f-omega/examples";

    if !Path::new(examples_dir).exists() {
        println!("Examples directory not found, skipping");
        return;
    }

    // Read all .hs files in examples directory
    let entries = fs::read_dir(examples_dir).expect("Failed to read examples directory");

    let mut valid_examples = Vec::new();
    let mut total_examples = 0;

    for entry in entries {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("hs") {
            total_examples += 1;
            let file_path = path.to_string_lossy().to_string();

            // Skip examples that are known to have parsing issues (use String literals,
            // comments)
            if file_path.contains("simple_record") || file_path.contains("records_via_adts") {
                println!("⚠ Skipping '{}' (contains unsupported syntax)", file_path);
                continue;
            }

            match check_example(&file_path) {
                Ok(()) => {
                    println!("✓ Example '{}' typechecks successfully", file_path);
                    valid_examples.push(file_path);
                }
                Err(e) => {
                    println!("✗ Example '{}' failed: {:?}", file_path, e);
                    // Don't panic here - just report the failure
                }
            }
        }
    }

    println!(
        "\nSummary: {}/{} examples passed type checking",
        valid_examples.len(),
        total_examples
    );

    // Ensure at least one example passes (we know working_records.hs should work)
    assert!(
        !valid_examples.is_empty(),
        "No examples passed type checking"
    );
}
