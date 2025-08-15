use std::fs;
use std::path::Path;

fn run_example(path: &Path) -> Result<(), String> {
    let content = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;

    let filename = path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");

    match coc::typecheck_module(&content, filename) {
        Ok(_) => Ok(()),
        Err(e) => Err(format!("{}", e)),
    }
}

#[test]
fn test_all_examples() {
    let examples_dir = Path::new("examples");
    let mut results = Vec::new();

    // Get all .coc files in examples directory
    let mut entries: Vec<_> = fs::read_dir(examples_dir)
        .expect("Failed to read examples directory")
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| ext == "coc")
                .unwrap_or(false)
        })
        .collect();

    // Sort entries for consistent output
    entries.sort_by_key(|e| e.file_name());

    println!("\n=== Running Integration Tests on Examples ===\n");

    let mut passed = 0;
    let mut failed = 0;

    for entry in entries {
        let path = entry.path();
        let filename = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");

        print!("Testing {:40} ", filename);

        match run_example(&path) {
            Ok(_) => {
                println!("✓ PASS");
                passed += 1;
                results.push((filename.to_string(), true, String::new()));
            }
            Err(e) => {
                println!("✗ FAIL");
                failed += 1;

                // Extract just the error type for summary
                let error_summary = if e.contains("Type error:") {
                    e.split("Type error:")
                        .nth(1)
                        .unwrap_or(&e)
                        .lines()
                        .next()
                        .unwrap_or(&e)
                        .trim()
                        .to_string()
                } else if e.contains("Parse error:") {
                    "Parse error".to_string()
                } else {
                    e.lines().next().unwrap_or(&e).to_string()
                };

                results.push((filename.to_string(), false, error_summary));
            }
        }
    }

    println!("\n=== Summary ===");
    println!("Passed: {}/{}", passed, passed + failed);
    println!("Failed: {}/{}", failed, passed + failed);

    if failed > 0 {
        println!("\n=== Failed Tests ===");
        for (name, success, error) in &results {
            if !success {
                println!("  {} - {}", name, error);
            }
        }
    }

    // Print statistics
    let pass_rate = if passed + failed > 0 {
        (passed as f64 / (passed + failed) as f64) * 100.0
    } else {
        0.0
    };

    println!("\nPass rate: {:.1}%", pass_rate);

    // Fail the test if any files failed
    assert_eq!(failed, 0, "{} examples failed type checking", failed);
}

#[test]
fn test_implicit_examples() {
    println!("\n=== Testing Implicit Argument Examples ===\n");

    let implicit_examples = vec![
        "debug_implicit.coc",
        "implicit_args_test.coc",
        "implicit_basic.coc",
        "implicit_final_demo.coc",
        "test_const.coc",
        "test_compose.coc",
        "test_implicit_simple.coc",
    ];

    let mut passed = 0;
    let mut failed = 0;

    for filename in implicit_examples {
        let path = Path::new("examples").join(filename);

        if !path.exists() {
            continue;
        }

        print!("Testing {:40} ", filename);

        match run_example(&path) {
            Ok(_) => {
                println!("✓ PASS");
                passed += 1;
            }
            Err(e) => {
                println!("✗ FAIL: {}", e.lines().next().unwrap_or(&e));
                failed += 1;
            }
        }
    }

    println!("\n=== Implicit Arguments Summary ===");
    println!("Passed: {}/{}", passed, passed + failed);
    println!("Failed: {}/{}", failed, passed + failed);

    // Fail the test if any implicit examples failed
    assert_eq!(
        failed, 0,
        "{} implicit examples failed type checking",
        failed
    );
}
