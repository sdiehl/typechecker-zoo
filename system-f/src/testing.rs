use std::fs;

use crate::{ast, errors, parser, typecheck};

pub fn infer_type_only(expr: &ast::Expr) -> Result<String, errors::TypeError> {
    let mut bi = typecheck::BiDirectional::new();
    let ctx = typecheck::Context::new();
    let (ty, final_ctx, _) = bi.infer(&ctx, expr)?;
    let resolved_ty = bi.apply_ctx_type(&final_ctx, &ty);
    Ok(format!("{}", resolved_ty))
}

pub fn process_test_lines(input_content: &str) -> Vec<String> {
    let mut results = Vec::new();
    for line in input_content.lines() {
        let line = line.trim();
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

    let results = process_test_lines(&content);
    for (line_num, result) in results.iter().enumerate() {
        println!("Line {}: {}", line_num + 1, result);
    }

    if let Some(output_file) = output_file {
        match fs::write(output_file, results.join("\n") + "\n") {
            Ok(()) => println!("Results written to {}", output_file),
            Err(e) => eprintln!("Error writing to {}: {}", output_file, e),
        }
    }
}
