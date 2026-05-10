use thiserror::Error;

use crate::ast::{Effect, Type};

#[derive(Debug, Error)]
pub enum InferenceError {
    #[error("Unbound variable: {name}")]
    UnboundVariable { name: String },
    #[error("Unknown operation: {op}")]
    UnknownOp { op: String },
    #[error("Cannot unify types: {expected} vs {actual}")]
    UnificationFailure { expected: Type, actual: Type },
    #[error("Occurs check failed: type variable '{var}' occurs in {ty}")]
    OccursCheck { var: String, ty: Type },
    #[error("Occurs check failed: effect variable '{var}' occurs in <{eff}>")]
    EffectOccursCheck { var: String, eff: Effect },
    #[error("Effect label '{label}' missing from effect row <{eff}>")]
    MissingEffect { label: String, eff: Effect },
    #[error("Recursive effect unification: tail variable '{var}' would be bound twice")]
    RecursiveEffect { var: String },
}

pub type Result<T> = std::result::Result<T, InferenceError>;
