mod ast;
mod errors;
mod infer;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}
pub use parser_impl::parser;

use std::fs;

use algorithm_w::{process_test_lines, run_golden_test_detailed};
use clap::{Parser, Subcommand};
use infer::{infer_type_only, run_inference};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[derive(Parser)]
#[command(name = "algorithm-w")]
#[command(about = "Algorithm W Example")]
#[command(
    long_about = "A type inference system implementing Algorithm W for Hindley-Milner type system"
)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Expression to evaluate (if no subcommand provided)
    expression: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    /// Start interactive REPL (default behavior)
    Repl,
    /// Run test file and optionally save results
    Test {
        /// Input file containing expressions to test
        input_file: String,
        /// Optional output file to save results
        output_file: Option<String>,
    },
    /// Run golden test comparing actual vs expected output
    Golden {
        /// Input file containing expressions
        input_file: String,
        /// Expected output file
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
        }) => run_golden_test(input_file, expected_file),
        None => {
            if let Some(expr) = &cli.expression {
                run_single_expression(expr);
            } else {
                // Default: start REPL
                run_repl();
            }
        }
    }
}

fn run_single_expression(input: &str) {
    // Parse the expression
    let expr = match parser::ExprParser::new().parse(input) {
        Ok(expr) => expr,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    println!("Parsed expression: {}", expr);
    println!();

    // Run type inference
    match (infer_type_only(&expr), run_inference(&expr)) {
        (Ok(final_type), Ok(tree)) => {
            println!("Type inference successful!");
            println!("Final type: {}", final_type);
            println!();
            println!("Inference trace:");
            println!("{}", tree);
        }
        (Err(e), _) | (_, Err(e)) => {
            eprintln!("Type inference error: {}", e);
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

fn run_golden_test(input_file: &str, expected_file: &str) {
    // Use the detailed golden test function
    let result = match run_golden_test_detailed(input_file, expected_file) {
        Ok(result) => result,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };

    // Print results with callback-like functionality
    println!("Running golden test: {} vs {}", input_file, expected_file);
    println!("═══════════════════════════════════════");

    for line_result in &result.line_results {
        if line_result.passed {
            println!("✓ Line {}: PASS", line_result.line_number);
        } else {
            println!("✗ Line {}: FAIL", line_result.line_number);
            println!("  Expected: {}", line_result.expected);
            println!("  Actual:   {}", line_result.actual);
        }
    }

    println!("═══════════════════════════════════════");
    if result.passed {
        println!("All tests PASSED!");
        std::process::exit(0);
    } else {
        println!("Some tests FAILED!");
        std::process::exit(1);
    }
}

fn run_repl() {
    println!("Algorithm W Example");
    println!("Type expressions to see their inferred types.");
    println!("Examples: \\x -> x, let f = \\x -> x in f 42, (42, true)");
    println!("Press Ctrl+C or Ctrl+D to exit.\n");

    let mut rl = DefaultEditor::new().unwrap();

    loop {
        let readline = rl.readline("λ> ");
        match readline {
            Ok(line) => {
                let line = line.trim();

                // Skip empty lines
                if line.is_empty() {
                    continue;
                }

                // Add to history
                rl.add_history_entry(line).unwrap();

                // Parse and infer type
                match parser::ExprParser::new().parse(line) {
                    Ok(expr) => match infer_type_only(&expr) {
                        Ok(ty) => println!("{} : {}", line, ty),
                        Err(e) => println!("Type error: {}", e),
                    },
                    Err(e) => println!("Parse error: {}", e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Goodbye!");
                break;
            }
            Err(ReadlineError::Eof) => {
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
