mod ast;
mod errors;
mod infer;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}

use std::fs;

use clap::{Parser, Subcommand};
use infer::infer_type;
pub use parser_impl::parser;
use row_poly::{process_test_lines, run_golden_test_detailed};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[derive(Parser)]
#[command(name = "row-poly")]
#[command(about = "Koka-style row polymorphism with scoped labels")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
    expression: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    Repl,
    Test {
        input_file: String,
        output_file: Option<String>,
    },
    Golden {
        input_file: String,
        expected_file: String,
    },
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Some(Commands::Repl) => run_repl(),
        Some(Commands::Test {
            input_file,
            output_file,
        }) => run_tests(input_file, output_file.as_deref()),
        Some(Commands::Golden {
            input_file,
            expected_file,
        }) => run_golden(input_file, expected_file),
        None => {
            if let Some(expr) = &cli.expression {
                run_single_expression(expr);
            } else {
                run_repl();
            }
        }
    }
}

fn run_single_expression(input: &str) {
    let expr = match parser::ExprParser::new().parse(input) {
        Ok(expr) => expr,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };
    match infer_type(&expr) {
        Ok(ty) => println!("{} : {}", expr, ty),
        Err(e) => {
            eprintln!("Type error: {}", e);
            std::process::exit(1);
        }
    }
}

fn run_tests(input_file: &str, output_file: Option<&str>) {
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

fn run_golden(input_file: &str, expected_file: &str) {
    let result = match run_golden_test_detailed(input_file, expected_file) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };
    println!("Running golden test: {} vs {}", input_file, expected_file);
    println!("=======================================");
    for line_result in &result.line_results {
        if line_result.passed {
            println!("[ok] Line {}: PASS", line_result.line_number);
        } else {
            println!("[fail] Line {}: FAIL", line_result.line_number);
            println!("  Expected: {}", line_result.expected);
            println!("  Actual:   {}", line_result.actual);
        }
    }
    println!("=======================================");
    if result.passed {
        println!("All tests PASSED!");
    } else {
        println!("Some tests FAILED!");
        std::process::exit(1);
    }
}

fn run_repl() {
    println!("row-poly REPL: extensible records with scoped labels.");
    println!("Examples: {{x = 1, y = 2}}, \\r -> r.x, {{r - x}}, {{x := 1 | r}}");
    println!("Press Ctrl+C or Ctrl+D to exit.\n");

    let mut rl = DefaultEditor::new().unwrap();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                rl.add_history_entry(line).unwrap();
                match parser::ExprParser::new().parse(line) {
                    Ok(expr) => match infer_type(&expr) {
                        Ok(ty) => println!("{} : {}", line, ty),
                        Err(e) => println!("Type error: {}", e),
                    },
                    Err(e) => println!("Parse error: {}", e),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
