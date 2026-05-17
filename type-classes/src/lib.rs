pub mod ast;
pub mod classes;
pub mod errors;
pub mod infer;
pub mod subst;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}

use ast::TopLevelItem;
use infer::Checker;
pub use parser_impl::parser;

pub fn process_test_lines(input: &str) -> Vec<String> {
    let mut results = Vec::new();
    let mut checker = Checker::new();
    for line in input.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            results.push(trimmed.to_string());
            continue;
        }
        let result = match parser::TopLevelParser::new().parse(trimmed) {
            Ok(TopLevelItem::Decl(d)) => match checker.process_decl(&d) {
                Ok(msg) => msg,
                Err(e) => format!("ERROR: {}", e),
            },
            Ok(TopLevelItem::Expr(e)) => match checker.check_expr(&e) {
                Ok((qual, core)) => format!("{} : {}\n  ~> {}", e, qual, core),
                Err(err) => format!("{} : ERROR: {}", e, err),
            },
            Err(e) => format!("PARSE ERROR: {}", e),
        };
        results.push(result);
    }
    results
}
