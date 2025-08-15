use std::{env, fs, process};

use coc::diagnostics::DiagnosticReporter;
use coc::errors::CocResult;
use coc::parse::Parser;
use coc::typecheck::TypeChecker;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <command> [args...]", args[0]);
        eprintln!("Commands:");
        eprintln!("  check <file>     - Type check a file");
        eprintln!("  parse <file>     - Parse a file and show AST");
        eprintln!("  repl            - Start interactive REPL");
        eprintln!("  term <expr>     - Parse and type check a term");
        process::exit(1);
    }

    match args[1].as_str() {
        "check" => {
            if args.len() != 3 {
                eprintln!("Usage: {} check <file>", args[0]);
                process::exit(1);
            }
            if let Err(e) = check_file(&args[2]) {
                eprintln!("Error: {}", e);
                process::exit(1);
            }
        }
        "parse" => {
            if args.len() != 3 {
                eprintln!("Usage: {} parse <file>", args[0]);
                process::exit(1);
            }
            if let Err(e) = parse_file(&args[2]) {
                eprintln!("Error: {}", e);
                process::exit(1);
            }
        }
        "term" => {
            if args.len() != 3 {
                eprintln!("Usage: {} term <expression>", args[0]);
                process::exit(1);
            }
            if let Err(e) = check_term(&args[2]) {
                eprintln!("Error: {}", e);
                process::exit(1);
            }
        }
        "repl" => {
            println!("Starting CoC REPL...");
            println!("Enter Lean-style expressions. Type :quit to exit.");
            repl();
        }
        _ => {
            eprintln!("Unknown command: {}", args[1]);
            process::exit(1);
        }
    }
}

fn check_file(filename: &str) -> CocResult<()> {
    let content = fs::read_to_string(filename)?;
    let reporter = DiagnosticReporter::new(filename, &content);

    let parser = Parser::new();
    let module = match parser.parse_module(&content) {
        Ok(m) => m,
        Err(e) => {
            reporter.report_error(&e);
            return Err(e);
        }
    };

    println!(
        "Parsed module with {} declarations",
        module.declarations.len()
    );

    let mut type_checker = TypeChecker::new();
    match type_checker.check_module(&module) {
        Ok(_) => {
            println!("✓ Module '{}' type checks successfully!", filename);
            Ok(())
        }
        Err(type_err) => {
            let coc_err = coc::errors::CocError::Type(type_err);
            reporter.report_error(&coc_err);
            Err(coc_err)
        }
    }
}

fn parse_file(filename: &str) -> CocResult<()> {
    let content = fs::read_to_string(filename)?;
    let parser = Parser::new();
    let module = parser.parse_module(&content)?;

    println!("Parsed AST:");
    println!("{:#?}", module);
    Ok(())
}

fn check_term(expr: &str) -> CocResult<()> {
    let parser = Parser::new();
    let term = parser.parse_term(expr)?;

    println!("Parsed term: {}", term);

    let mut type_checker = TypeChecker::new();
    let ctx = coc::context::Context::new();
    let inferred_type = type_checker.infer(&term, &ctx)?;

    println!("Type: {}", inferred_type);
    Ok(())
}

fn repl() {
    use std::io::{self, Write};

    let parser = Parser::new();
    let mut type_checker = TypeChecker::new();
    let mut ctx = coc::context::Context::new();

    loop {
        print!("coc> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim();
                if input == ":quit" || input == ":q" {
                    println!("Goodbye!");
                    break;
                }

                if input.is_empty() {
                    continue;
                }

                // Try parsing as a declaration first, then as a term
                if let Ok(module) = parser.parse_module(input) {
                    match type_checker.check_module(&module) {
                        Ok(new_ctx) => {
                            ctx = new_ctx;
                            println!("✓ Declaration accepted");
                        }
                        Err(e) => println!("Type error: {}", e),
                    }
                } else if let Ok(term) = parser.parse_term(input) {
                    match type_checker.infer(&term, &ctx) {
                        Ok(ty) => println!("{} : {}", term, ty),
                        Err(e) => println!("Type error: {}", e),
                    }
                } else {
                    println!("Parse error: could not parse input");
                }
            }
            Err(e) => {
                println!("Error reading input: {}", e);
                break;
            }
        }
    }
}
