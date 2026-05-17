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

use algorithm_w::process_test_lines;
use clap::{Parser, Subcommand};
use errors::{ParseError, Span};
use infer::{infer_type, run_inference};
pub use parser_impl::parser;
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
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Repl) => run_repl(),
        Some(Commands::Test {
            input_file,
            output_file,
        }) => run_tests(input_file, output_file.as_deref()),
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
            let parse_error = lalrpop_error_to_parse_error(e);
            let report = parse_error.to_ariadne_report("<input>");
            report
                .eprint(("<input>", ariadne::Source::from(input)))
                .unwrap();
            std::process::exit(1);
        }
    };

    println!("Parsed expression: {}", expr);
    println!();

    // Run type inference
    match (infer_type(&expr), run_inference(&expr)) {
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
                    Ok(expr) => match infer_type(&expr) {
                        Ok(ty) => println!("{} : {}", line, ty),
                        Err(e) => println!("Type error: {}", e),
                    },
                    Err(e) => {
                        let parse_error = lalrpop_error_to_parse_error(e);
                        let report = parse_error.to_ariadne_report("<input>");
                        let _ = report.eprint(("<input>", ariadne::Source::from(line)));
                    }
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

fn lalrpop_error_to_parse_error(
    err: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, &str>,
) -> ParseError {
    let (message, span) = match err {
        lalrpop_util::ParseError::InvalidToken { location } => {
            ("Invalid token".to_string(), Span::point(location))
        }
        lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
            let msg = if expected.is_empty() {
                "Unexpected end of input".to_string()
            } else {
                format!(
                    "Unexpected end of input. Expected one of {}",
                    expected.join(", ")
                )
            };
            (msg, Span::point(location))
        }
        lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
            let msg = if expected.is_empty() {
                format!(
                    "Unrecognized token `{}` found at {}:{}",
                    token.1, token.0, token.2
                )
            } else {
                format!(
                    "Unrecognized token `{}` found at {}:{}\nExpected one of {}",
                    token.1,
                    token.0,
                    token.2,
                    expected.join(", ")
                )
            };
            (msg, Span::new(token.0, token.2))
        }
        lalrpop_util::ParseError::ExtraToken { token } => (
            format!("Extra token `{}` found at {}:{}", token.1, token.0, token.2),
            Span::new(token.0, token.2),
        ),
        lalrpop_util::ParseError::User { error: _ } => ("User error".to_string(), Span::point(0)),
    };

    ParseError::LalrpopError { message, span }
}
