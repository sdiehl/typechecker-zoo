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

use ast::Effect;
use clap::{Parser, Subcommand};
use errors::{ParseError, Span};
use infer::{infer_type, run_inference};
pub use parser_impl::parser;
use row_effects::process_test_lines;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[derive(Parser)]
#[command(name = "row-effects")]
#[command(about = "Row polymorphism for algebraic effects")]
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
        None => {
            if let Some(expr) = &cli.expression {
                run_single_expression(expr);
            } else {
                run_repl();
            }
        }
    }
}

fn format_result(line: &str, ty: &ast::Type, eff: &Effect) -> String {
    if matches!(eff, Effect::Empty) {
        format!("{} : {}", line, ty)
    } else {
        format!("{} : {} ! <{}>", line, ty, eff)
    }
}

fn run_single_expression(input: &str) {
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
    match (infer_type(&expr), run_inference(&expr)) {
        (Ok((ty, eff)), Ok(tree)) => {
            println!("{}", format_result(&format!("{}", expr), &ty, &eff));
            println!();
            println!("Inference trace:");
            println!("{}", tree);
        }
        (Err(e), _) | (_, Err(e)) => {
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

fn run_repl() {
    println!("row-effects REPL: row polymorphism for algebraic effects.");
    println!("Examples: perform print 1, \\x -> perform read (), handle (perform throw 1) with throw x k -> 0");
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
                        Ok((ty, eff)) => println!("{}", format_result(line, &ty, &eff)),
                        Err(e) => println!("Type error: {}", e),
                    },
                    Err(e) => {
                        let parse_error = lalrpop_error_to_parse_error(e);
                        let report = parse_error.to_ariadne_report("<input>");
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
