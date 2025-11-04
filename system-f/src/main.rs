mod ast;
mod errors;
mod testing;
mod typecheck;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}
pub use parser_impl::parser;

use clap::{Parser, Subcommand};
use errors::{ParseError, Span};
use testing::{run_golden_test, run_tests};
use typecheck::run_bidirectional;

#[derive(Parser)]
#[command(name = "system-f")]
#[command(about = "A System F type checker with bidirectional type inference")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Expression to type check (if no subcommand is given)
    #[arg(value_name = "EXPRESSION")]
    expression: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run tests on a file
    Test {
        /// Input file containing expressions to test
        input_file: String,
        /// Optional output file to write results
        output_file: Option<String>,
    },
    /// Run golden tests comparing actual vs expected output
    Golden {
        /// Input file containing expressions
        input_file: String,
        /// Expected output file
        expected_file: String,
    },
    /// Type check a single expression
    Check {
        /// Expression to type check
        expression: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Test {
            input_file,
            output_file,
        }) => {
            let mut args = vec![String::from("system-f"), String::from("--test"), input_file];
            if let Some(output) = output_file {
                args.push(output);
            }
            run_tests(&args);
        }
        Some(Commands::Golden {
            input_file,
            expected_file,
        }) => {
            let args = vec![
                String::from("system-f"),
                String::from("--golden"),
                input_file,
                expected_file,
            ];
            run_golden_test(&args);
        }
        Some(Commands::Check { expression }) => {
            run_single_expression(&expression);
        }
        None => {
            if let Some(expression) = cli.expression {
                run_single_expression(&expression);
            } else {
                eprintln!("Error: No expression provided");
                eprintln!("Use --help for usage information");
                std::process::exit(1);
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

fn run_single_expression(expression: &str) {
    // Parse the expression
    let expr = match parser::ExprParser::new().parse(expression) {
        Ok(expr) => expr,
        Err(e) => {
            let parse_error = lalrpop_error_to_parse_error(e);
            let report = parse_error.to_ariadne_report("<input>");
            report
                .eprint(("<input>", ariadne::Source::from(expression)))
                .unwrap();
            std::process::exit(1);
        }
    };

    println!("Parsed expression: {:?}", expr);
    println!();

    match run_bidirectional(&expr) {
        Ok((final_type, _final_ctx, tree)) => {
            println!("Type checking successful!");
            println!("Final type: {}", final_type);
            println!();
            println!("{}", tree);
        }
        Err(e) => {
            eprintln!("Type checking error: {}", e);
            std::process::exit(1);
        }
    }
}
