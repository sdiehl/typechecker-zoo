pub mod ast;
pub mod errors;
pub mod infer;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}
use ast::Effect;
pub use errors::{InferenceError, ParseError, Span};
pub use infer::{infer_type, run_inference, InferenceTree};
pub use parser_impl::parser;

pub fn process_test_lines(input_content: &str) -> Vec<String> {
    let mut results = Vec::new();
    for line in input_content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            results.push(line.to_string());
            continue;
        }
        let result = match parser::ExprParser::new().parse(line) {
            Ok(expr) => match infer_type(&expr) {
                Ok((ty, eff)) => {
                    if matches!(eff, Effect::Empty) {
                        format!("{} : {}", line, ty)
                    } else {
                        format!("{} : {} ! <{}>", line, ty, eff)
                    }
                }
                Err(e) => format!("{} : ERROR: {}", line, e),
            },
            Err(e) => format!("{} : PARSE ERROR: {}", line, e),
        };
        results.push(result);
    }
    results
}
