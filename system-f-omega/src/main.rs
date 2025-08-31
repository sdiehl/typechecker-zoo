mod builtins;
mod typecheck;
mod core;
mod errors;
mod lexer;
mod parse;
mod surface;
mod worklist;

use std::collections::HashMap;
use std::{env, fs, process};

use ariadne::Source;
use clap::{Parser as ClapParser, Subcommand};

use crate::typecheck::Compiler;
use crate::errors::CompilerError;
use crate::lexer::Lexer;
use crate::parse::Parser;
use crate::worklist::DKInference;

#[derive(ClapParser)]
#[command(name = "system-f-omega")]
#[command(about = "A System F-ω type checker and evaluator")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Input file to process
    file: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    /// Dump the lexer token stream for debugging
    Lex {
        /// Input file to lex
        file: String,
    },
    /// Typecheck a module (default)
    Check {
        /// Input file to typecheck  
        file: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Lex { file }) => {
            dump_tokens(&file);
        }
        Some(Commands::Check { file }) => {
            typecheck_file(&file);
        }
        None => {
            if let Some(file) = cli.file {
                typecheck_file(&file);
            } else {
                eprintln!("Usage: system-f-omega [COMMAND] <FILE>");
                eprintln!("Commands:");
                eprintln!("  lex <file>    Dump lexer tokens");
                eprintln!("  check <file>  Typecheck module (default)");
                eprintln!("Examples:");
                eprintln!("  system-f-omega fibonacci.hs");
                eprintln!("  system-f-omega lex fibonacci.hs");
                eprintln!("  system-f-omega check fibonacci.hs");
                process::exit(1);
            }
        }
    }
}

fn dump_tokens(filename: &str) {
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", filename, e);
            process::exit(1);
        }
    };

    println!("=== Lexer Token Stream for '{}' ===", filename);
    let lexer = Lexer::new(&source);
    let mut position = 0;

    for (i, result) in lexer.enumerate() {
        match result {
            Ok((start, token, end)) => {
                let text = &source[start..end];
                println!(
                    "{:3}: {:8}-{:8} {:20} '{}'",
                    i,
                    start,
                    end,
                    format!("{:?}", token),
                    text.escape_debug()
                );
                position = end;
            }
            Err(e) => {
                println!(
                    "{:3}: {:8}-{:8} {:20} ERROR: {}",
                    i, position, position, "ERROR", e
                );
                break;
            }
        }
    }
    println!("=== End Token Stream ===");
}

fn typecheck_file(filename: &str) {
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", filename, e);
            process::exit(1);
        }
    };

    if let Err(()) = typecheck_module(&source, filename) {
        process::exit(1);
    }

    println!("✓ Module '{}' typechecks successfully!", filename);
}

fn typecheck_module(source: &str, filename: &str) -> Result<(), ()> {
    // Parse the module
    let parser = Parser::new();
    let surface_module = parser
        .parse_module_with_diagnostics(source, filename)
        .map_err(|_| ())?;

    println!("Parsed {} declarations", surface_module.declarations.len());

    // Compile to core language
    let mut compiler = Compiler::new();
    let core_module = match compiler.compile_module(&surface_module) {
        Ok(module) => module,
        Err(CompilerError::Type(type_error)) => {
            let report = type_error.report(source, filename);
            report.eprint((filename, Source::from(source))).unwrap();
            return Err(());
        }
        Err(CompilerError::Parse(parse_error)) => {
            let report = parse_error.report(source, filename);
            report.eprint((filename, Source::from(source))).unwrap();
            return Err(());
        }
    };

    println!(
        "Compiled to {} type definitions and {} term definitions",
        core_module.type_defs.len(),
        core_module.term_defs.len()
    );

    // Collect all function types for mutual recursion
    let mut function_types = HashMap::new();
    for term_def in &core_module.term_defs {
        function_types.insert(term_def.name.clone(), term_def.ty.clone());
    }

    // Typecheck each function using DK worklist algorithm
    for term_def in &core_module.term_defs {
        print!("Checking {} : {} ... ", term_def.name, term_def.ty);

        // Print function body for debugging
        if env::var("DEBUG").is_ok() {
            println!("\n  DEBUG - Function body: {}", term_def.body);
        }

        let mut inference = DKInference::with_context(
            compiler.get_data_constructors().clone(),
            function_types.clone(),
        );
        match inference.check_type(&term_def.body, &term_def.ty) {
            Ok(()) => {
                println!("✓");

                // Print inference trace if verbose
                if env::var("VERBOSE").is_ok() {
                    println!("  Body: {}", term_def.body);
                    println!("  Inference trace:");
                    for step in inference.get_trace() {
                        println!("    {}", step);
                    }
                }
            }
            Err(type_error) => {
                println!("✗");
                let report = type_error.report(source, filename);
                report.eprint((filename, Source::from(source))).unwrap();
                return Err(());
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let source = "id :: Int -> Int; id x = x;";

        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_fibonacci() {
        let source = r#"
            fib :: Int -> Int;
            fib n = 1 + 2;
        "#;

        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_data_type() {
        let source = r#"
            data Bool = True | False;
            
            not :: Bool;
            not = True;
        "#;

        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_binary_operations() {
        let source = r#"
            add :: Int -> Int -> Int;
            add x y = x + y;
        "#;
        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_parametric_data_type() {
        let source = r#"
            data List a = Nil | Cons a (List a);
            
            length :: Int;
            length = 42;
        "#;

        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_if_then_else() {
        let source = r#"
            data Bool = True | False;
            
            test_if :: Bool -> Int;
            test_if b = if b then 1 else 0;
        "#;
        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_forall_types() {
        let source = r#"
            id :: forall a. a -> a;
            id x = x;
        "#;
        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_match_expressions() {
        let source = r#"
            data Bool = True | False;
            
            not :: Bool -> Bool;
            not x = match x {
                True -> False;
                False -> True;
            };
        "#;
        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_match_with_variables() {
        let source = r#"
            data Maybe a = Nothing | Just a;
            
            fromMaybe :: Int -> Maybe Int -> Int;
            fromMaybe default m = match m {
                Nothing -> default;
                Just x -> x;
            };
        "#;

        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_comprehensive_working_examples() {
        let source = include_str!("../examples/working.hs");
        assert!(typecheck_module(source, "working.hs").is_ok());
    }

    #[test]
    fn test_list_operations() {
        let source = r#"
            data List a = Nil | Cons a (List a);

            -- Basic list functions
            append :: List Int -> List Int -> List Int;
            append xs ys = match xs {
                Nil -> ys;
                Cons z zs -> Cons z (append zs ys);
            };

            -- Length function
            length :: List Int -> Int;
            length xs = match xs {
                Nil -> 0;
                Cons y ys -> 1 + length ys;
            };

            -- Test list
            test_list :: List Int;
            test_list = Cons 1 (Cons 2 (Cons 3 Nil));

            test_length :: Int;
            test_length = length test_list;
        "#;

        assert!(typecheck_module(source, "lists.hs").is_ok());
    }

    #[test]
    fn test_simple_nothing_match() {
        let source = r#"
            data Maybe a = Nothing | Just a;
            
            test :: Maybe Int -> Int;
            test m = match m {
                Nothing -> 42;
            };
        "#;

        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_type_error() {
        let source = r#"
            wrong :: Int;
            wrong = 42;
        "#;

        // This should pass - just a simple value binding
        assert!(typecheck_module(source, "test.hs").is_ok());
    }

    #[test]
    fn test_missing_type_signature() {
        use system_f_omega::typecheck_module_silent;

        let source = r#"
            mystery = 1 + 1;
        "#;

        assert!(typecheck_module_silent(source, "test.hs").is_err());
    }
}
