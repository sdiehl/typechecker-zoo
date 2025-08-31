//! Cranelift backend for System F-ω

pub mod closure;
pub mod codegen;
pub mod driver;
pub mod erase;
pub mod executable;
pub mod runtime;
pub mod value;

pub use driver::{compile, compile_to_executable};
pub use executable::compile_executable;
