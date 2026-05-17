use ariadne::{Color, Label, Report, ReportKind};
use thiserror::Error;

use crate::ast::{Row, Type};

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

    #[error("Occurs check failed: type variable '{var}' occurs in {ty}")]
    OccursCheck { var: String, ty: Type },

    #[error("Row occurs check failed: row variable '{var}' occurs in {row}")]
    RowOccursCheck { var: String, row: Row },

    #[error("Recursive row unification: tail variable '{var}' would be bound twice")]
    RecursiveRow { var: String },

    #[error("Label '{label}' missing from row {{{row}}}")]
    MissingLabel { label: String, row: Row },
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
