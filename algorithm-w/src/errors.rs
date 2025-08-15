use thiserror::Error;

use crate::ast::Type;

#[derive(Error, Debug)]
pub enum InferenceError {
    #[error("Variable '{name}' not found in environment")]
    UnboundVariable { name: String },

    #[error("Cannot unify types: expected {expected}, found {actual}")]
    UnificationFailure { expected: Type, actual: Type },

    #[error("Occurs check failed: variable '{var}' occurs in type {ty}")]
    OccursCheck { var: String, ty: Type },

    #[error("Cannot unify tuples of different lengths: {left_len} vs {right_len}")]
    TupleLengthMismatch { left_len: usize, right_len: usize },
}

pub type Result<T> = std::result::Result<T, InferenceError>;
