mod ast;
mod errors;
mod infer;
mod smt;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}

use std::fs;

use clap::{Parser, Subcommand};
use errors::{ParseError, Span};
use infer::infer_type;
pub use parser_impl::parser;
use refinement::process_test_lines;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[derive(Parser)]
#[command(name = "refinement")]
#[command(about = "Refinement Types via Z3")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// File containing expressions to typecheck
    input_file: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    /// Start interactive REPL
    Repl,
    /// Run test file
    Test {
        input_file: String,
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
            if let Some(path) = &cli.input_file {
                run_tests(path, None);
            } else {
                run_repl();
            }
        }
    }
}

fn run_tests(input_file: &str, output_file: Option<&str>) {
    let content = match fs::read_to_string(input_file) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading {}: {}", input_file, e);
            std::process::exit(1);
        }
    };
    let results = process_test_lines(&content);
    for r in &results {
        println!("{}", r);
    }
    if let Some(out) = output_file {
        let _ = fs::write(out, results.join("\n") + "\n");
    }
}

fn run_repl() {
    println!("Refinement Types REPL");
    println!("Examples: 5, \\x : Int -> x, (\\x : {{ n : Int | n != 0 }} -> div 10 x) 2");
    println!("Press Ctrl-C or Ctrl-D to exit.\n");

    let mut rl = DefaultEditor::new().unwrap();
    loop {
        match rl.readline("rt> ") {
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
                    Err(e) => {
                        let pe = lalrpop_error_to_parse_error(e);
                        let _ = pe
                            .to_ariadne_report("<input>")
                            .eprint(("<input>", ariadne::Source::from(line)));
                    }
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                println!("Goodbye.");
                break;
            }
            Err(e) => {
                println!("Error: {:?}", e);
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
                format!("Unexpected end of input. Expected: {}", expected.join(", "))
            };
            (msg, Span::point(location))
        }
        lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
            let msg = if expected.is_empty() {
                format!("Unrecognized token `{}`", token.1)
            } else {
                format!(
                    "Unrecognized token `{}`. Expected: {}",
                    token.1,
                    expected.join(", ")
                )
            };
            (msg, Span::new(token.0, token.2))
        }
        lalrpop_util::ParseError::ExtraToken { token } => (
            format!("Extra token `{}`", token.1),
            Span::new(token.0, token.2),
        ),
        lalrpop_util::ParseError::User { error: _ } => ("User error".to_string(), Span::point(0)),
    };
    ParseError::LalrpopError { message, span }
}
