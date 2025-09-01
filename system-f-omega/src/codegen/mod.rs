//! Cranelift backend for System F-ω

pub mod closure;
pub mod compile;
pub mod erase;
pub mod executable;
pub mod runtime;

pub use executable::compile_executable;
