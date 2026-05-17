pub mod ast;
pub mod errors;
pub mod typecheck;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}
pub use parser_impl::parser;

pub fn infer_type_only(expr: &ast::Expr) -> Result<String, errors::TypeError> {
    let mut bi = typecheck::BiDirectional::new();
    let ctx = typecheck::Context::new();
    let (ty, final_ctx, _) = bi.infer(&ctx, expr)?;
    let resolved_ty = bi.apply_ctx_type(&final_ctx, &ty);
    Ok(format!("{}", resolved_ty))
}

pub fn process_test_lines(input_content: &str) -> Vec<String> {
    let mut results = Vec::new();
    for line in input_content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            results.push(line.to_string());
            continue;
        }
        let result = match parser::ExprParser::new().parse(line) {
            Ok(expr) => match infer_type_only(&expr) {
                Ok(ty) => format!("{} : {}", line, ty),
                Err(e) => format!("{} : ERROR: {}", line, e),
            },
            Err(e) => format!("{} : PARSE ERROR: {}", line, e),
        };
        results.push(result);
    }
    results
}
