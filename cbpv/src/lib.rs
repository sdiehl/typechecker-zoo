pub mod ast;
pub mod errors;
pub mod infer;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}

pub use errors::{InferenceError, ParseError, Span};
pub use infer::{infer_type, TypeResult};
pub use parser_impl::parser;

pub fn process_test_lines(input_content: &str) -> Vec<String> {
    let mut results = Vec::new();
    for line in input_content.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            results.push(trimmed.to_string());
            continue;
        }
        let result = match parser::TopParser::new().parse(trimmed) {
            Ok(term) => match infer_type(&term) {
                Ok(ty) => format!("{} : {}", trimmed, ty),
                Err(e) => format!("{} : ERROR: {}", trimmed, e),
            },
            Err(e) => format!("{} : PARSE ERROR: {}", trimmed, e),
        };
        results.push(result);
    }
    results
}
