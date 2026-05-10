use thiserror::Error;

use crate::ast::{Row, Type};

#[derive(Error, Debug)]
pub enum InferenceError {
    #[error("Variable '{name}' not found in environment")]
    UnboundVariable { name: String },

    #[error("Cannot unify types: {expected} vs {actual}")]
    UnificationFailure { expected: Type, actual: Type },

    #[error("Occurs check failed: type variable '{var}' occurs in {ty}")]
    OccursCheck { var: String, ty: Type },

    #[error("Row occurs check failed: row variable '{var}' occurs in {row}")]
    RowOccursCheck { var: String, row: Row },

    #[error("Recursive row unification: tail variable '{var}' would be bound twice")]
    RecursiveRow { var: String },

    #[error("Label '{label}' missing from row {{{row}}}")]
    MissingLabel { label: String, row: Row },
}

pub type Result<T> = std::result::Result<T, InferenceError>;
