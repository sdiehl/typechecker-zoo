mod ast;
mod classes;
mod errors;
mod infer;
mod subst;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}

use std::fs;

use ast::TopLevelItem;
use clap::{Parser, Subcommand};
use errors::{ParseError, Span};
use infer::Checker;
pub use parser_impl::parser;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use type_classes::process_test_lines;

#[derive(Parser)]
#[command(name = "type-classes")]
#[command(about = "Type classes with dictionary-passing elaboration")]
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
            Some(expr) => run_one(expr),
            None => run_repl(),
        },
    }
}

fn run_one(input: &str) {
    let mut checker = Checker::new();
    match parser::TopLevelParser::new().parse(input) {
        Ok(TopLevelItem::Decl(d)) => match checker.process_decl(&d) {
            Ok(msg) => println!("{}", msg),
            Err(e) => {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        },
        Ok(TopLevelItem::Expr(e)) => match checker.check_expr(&e) {
            Ok((qual, core)) => {
                println!("{} : {}", e, qual);
                println!("  ~> {}", core);
            }
            Err(err) => {
                eprintln!("Type error: {}", err);
                std::process::exit(1);
            }
        },
        Err(e) => {
            let parse_error = lalrpop_to_parse_error(e);
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
    for (i, r) in results.iter().enumerate() {
        println!("Line {}: {}", i + 1, r);
    }
    if let Some(out) = output_file {
        if let Err(e) = fs::write(out, results.join("\n") + "\n") {
            eprintln!("Error writing to {}: {}", out, e);
        }
    }
}

fn run_repl() {
    println!("type-classes REPL: Haskell-style classes with dictionary elaboration.");
    println!("Examples: class Eq a where {{ eq : a -> a -> Bool }}, instance Eq Int where {{ eq = ... }}");
    println!("Press Ctrl+C or Ctrl+D to exit.\n");
    let mut rl = DefaultEditor::new().unwrap();
    let mut checker = Checker::new();
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                rl.add_history_entry(line).unwrap();
                match parser::TopLevelParser::new().parse(line) {
                    Ok(TopLevelItem::Decl(d)) => match checker.process_decl(&d) {
                        Ok(msg) => println!("{}", msg),
                        Err(e) => println!("Error: {}", e),
                    },
                    Ok(TopLevelItem::Expr(e)) => match checker.check_expr(&e) {
                        Ok((qual, core)) => {
                            println!("{} : {}", e, qual);
                            println!("  ~> {}", core);
                        }
                        Err(err) => println!("Type error: {}", err),
                    },
                    Err(e) => {
                        let pe = lalrpop_to_parse_error(e);
                        let report = pe.to_ariadne_report("<input>");
                        let _ = report.eprint(("<input>", ariadne::Source::from(line)));
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

fn lalrpop_to_parse_error(
    err: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, &str>,
) -> ParseError {
    let (message, span) = match err {
        lalrpop_util::ParseError::InvalidToken { location } => {
            ("Invalid token".to_string(), Span::point(location))
        }
        lalrpop_util::ParseError::UnrecognizedEof { location, expected } => (
            if expected.is_empty() {
                "Unexpected end of input".to_string()
            } else {
                format!("Unexpected end of input. Expected: {}", expected.join(", "))
            },
            Span::point(location),
        ),
        lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
            let msg = if expected.is_empty() {
                format!(
                    "Unrecognized token `{}` at {}:{}",
                    token.1, token.0, token.2
                )
            } else {
                format!(
                    "Unrecognized token `{}` at {}:{}\nExpected one of {}",
                    token.1,
                    token.0,
                    token.2,
                    expected.join(", ")
                )
            };
            (msg, Span::new(token.0, token.2))
        }
        lalrpop_util::ParseError::ExtraToken { token } => (
            format!("Extra token `{}` at {}:{}", token.1, token.0, token.2),
            Span::new(token.0, token.2),
        ),
        lalrpop_util::ParseError::User { error: _ } => ("User error".to_string(), Span::point(0)),
    };
    ParseError::LalrpopError { message, span }
}
