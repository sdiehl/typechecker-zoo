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

use cbpv::process_test_lines;
use clap::{Parser, Subcommand};
use errors::{ParseError, Span};
use infer::infer_type;
pub use parser_impl::parser;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[derive(Parser)]
#[command(name = "cbpv")]
#[command(about = "Call-by-Push-Value bidirectional typechecker")]
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
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Some(Commands::Repl) => run_repl(),
        Some(Commands::Test {
            input_file,
            output_file,
        }) => run_tests(input_file, output_file.as_deref()),
        None => match &cli.expression {
            Some(expr) if std::path::Path::new(expr).is_file() => run_tests(expr, None),
            Some(expr) => run_single(expr),
            None => run_repl(),
        },
    }
}

fn run_single(input: &str) {
    match parser::TopParser::new().parse(input) {
        Ok(term) => match infer_type(&term) {
            Ok(ty) => println!("{} : {}", input, ty),
            Err(e) => {
                eprintln!("Type error: {}", e);
                std::process::exit(1);
            }
        },
        Err(e) => {
            let parse_error = lalrpop_error_to_parse_error(e);
            let report = parse_error.to_ariadne_report("<input>");
            report
                .eprint(("<input>", ariadne::Source::from(input)))
                .unwrap();
            std::process::exit(1);
        }
    }
}

fn run_tests(input_file: &str, output_file: Option<&str>) {
    let content = match fs::read_to_string(input_file) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading file {}: {}", input_file, e);
            std::process::exit(1);
        }
    };
    let results = process_test_lines(&content);
    for r in &results {
        println!("{}", r);
    }
    if let Some(out) = output_file {
        if let Err(e) = fs::write(out, results.join("\n") + "\n") {
            eprintln!("Error writing {}: {}", out, e);
        }
    }
}

fn run_repl() {
    println!("Call-by-Push-Value typechecker.");
    println!("Examples: return 5, thunk (return 5), \\x : Int. return x");
    println!("Press Ctrl+C or Ctrl+D to exit.\n");
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                rl.add_history_entry(line).unwrap();
                match parser::TopParser::new().parse(line) {
                    Ok(term) => match infer_type(&term) {
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
