use std::fs;

use crate::{ast, errors, parser, typecheck};

pub fn infer_type_only(expr: &ast::Expr) -> Result<String, errors::TypeError> {
    let mut bi = typecheck::BiDirectional::new();
    let ctx = typecheck::Context::new();
    let (ty, final_ctx, _) = bi.infer(&ctx, expr)?;
    let resolved_ty = bi.apply_ctx_type(&final_ctx, &ty);
    Ok(format!("{}", resolved_ty))
}

pub fn run_tests(args: &[String]) {
    if args.len() < 3 {
        eprintln!("Usage: {} --test <input_file> [output_file]", args[0]);
        std::process::exit(1);
    }

    let input_file = &args[2];
    let output_file = args.get(3);

    let content = match fs::read_to_string(input_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {}: {}", input_file, e);
            std::process::exit(1);
        }
    };

    let mut results = Vec::new();

    for (line_num, line) in content.lines().enumerate() {
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
        println!("Line {}: {}", line_num + 1, results.last().unwrap());
    }

    if let Some(output_file) = output_file {
        match fs::write(output_file, results.join("\n") + "\n") {
            Ok(()) => println!("Results written to {}", output_file),
            Err(e) => eprintln!("Error writing to {}: {}", output_file, e),
        }
    }
}

pub fn run_golden_test(args: &[String]) {
    if args.len() != 4 {
        eprintln!("Usage: {} --golden <input_file> <expected_file>", args[0]);
        std::process::exit(1);
    }

    let input_file = &args[2];
    let expected_file = &args[3];

    // Read input file
    let input_content = match fs::read_to_string(input_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading input file {}: {}", input_file, e);
            std::process::exit(1);
        }
    };

    // Read expected output file
    let expected_content = match fs::read_to_string(expected_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading expected file {}: {}", expected_file, e);
            std::process::exit(1);
        }
    };

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
            Ok(expr) => match infer_type_only(&expr) {
                Ok(ty) => format!("{} : {}", line, ty),
                Err(e) => format!("{} : ERROR: {}", line, e),
            },
            Err(e) => format!("{} : PARSE ERROR: {}", line, e),
        };

        actual_results.push(result);
    }

    let actual_output = actual_results.join("\n");
    let expected_lines: Vec<&str> = expected_content.lines().collect();
    let actual_lines: Vec<&str> = actual_output.lines().collect();

    let mut all_passed = true;
    let max_lines = expected_lines.len().max(actual_lines.len());

    println!("Running golden test: {} vs {}", input_file, expected_file);
    println!("=======================================");

    for i in 0..max_lines {
        let expected_line = expected_lines.get(i).unwrap_or(&"<missing>");
        let actual_line = actual_lines.get(i).unwrap_or(&"<missing>");

        if expected_line == actual_line {
            println!("PASS Line {}", i + 1);
        } else {
            println!("FAIL Line {}", i + 1);
            println!("  Expected: {}", expected_line);
            println!("  Actual:   {}", actual_line);
            all_passed = false;
        }
    }

    println!("=======================================");
    if all_passed {
        println!("All tests PASSED!");
        std::process::exit(0);
    } else {
        println!("Some tests FAILED!");
        std::process::exit(1);
    }
}
