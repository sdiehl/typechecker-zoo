use ariadne::{Color, Label, Report, ReportKind};
use thiserror::Error;

use crate::ast::{Effect, Type};

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
