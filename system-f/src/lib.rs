pub mod ast;
pub mod errors;
pub mod testing;
pub mod typecheck;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}
pub use parser_impl::parser;
