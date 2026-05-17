use ariadne::{Color, Label, Report, ReportKind};
use thiserror::Error;

use crate::ast::{Pred, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    pub fn point(pos: usize) -> Self {
        Self {
            start: pos,
            end: pos + 1,
        }
    }
}

#[derive(Error, Debug)]
pub enum InferenceError {
    #[error("Variable '{name}' not found in environment")]
    UnboundVariable { name: String },

    #[error("Cannot unify types: {expected} vs {actual}")]
    UnificationFailure { expected: Type, actual: Type },

    #[error("Occurs check failed: variable '{var}' occurs in type {ty}")]
    OccursCheck { var: String, ty: Type },

    #[error("Cannot unify type constructors {left} and {right}")]
    ConMismatch { left: String, right: String },

    #[error("Constructor arity mismatch for {name}: {expected} vs {actual}")]
    ArityMismatch {
        name: String,
        expected: usize,
        actual: usize,
    },

    #[error("No instance for {pred}")]
    NoInstance { pred: Pred },

    #[error("Ambiguous type: {pred} mentions a variable not in the head type")]
    Ambiguous { pred: Pred },

    #[error("Duplicate instance: {pred} overlaps with an existing instance")]
    DuplicateInstance { pred: Pred },

    #[error("Unknown class '{name}'")]
    UnknownClass { name: String },

    #[error("Unknown method '{name}'")]
    UnknownMethod { name: String },

    #[error("Class '{class}' missing superclass '{superclass}'")]
    MissingSuperclass { class: String, superclass: String },

    #[error("Instance of {class} {ty} is missing method '{method}'")]
    MissingMethod {
        class: String,
        ty: Type,
        method: String,
    },
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Parse error: {message}")]
    LalrpopError { message: String, span: Span },
}

impl ParseError {
    pub fn to_ariadne_report<'a>(
        &self,
        filename: &'a str,
    ) -> Report<'a, (&'a str, std::ops::Range<usize>)> {
        match self {
            ParseError::LalrpopError { message, span } => {
                Report::build(ReportKind::Error, (filename, span.start..span.end))
                    .with_message("Parse Error")
                    .with_label(
                        Label::new((filename, span.start..span.end))
                            .with_message(message)
                            .with_color(Color::Red),
                    )
                    .finish()
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, InferenceError>;
