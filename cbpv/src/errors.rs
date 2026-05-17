use ariadne::{Color, Label, Report, ReportKind};
use thiserror::Error;

use crate::ast::{CompType, ValType};

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
    #[error("Unbound variable '{name}'")]
    UnboundVariable { name: String, span: Span },

    #[error("Type mismatch: expected {expected}, found {found}")]
    ValueMismatch {
        expected: ValType,
        found: ValType,
        span: Span,
    },

    #[error("Computation type mismatch: expected {expected}, found {found}")]
    CompMismatch {
        expected: CompType,
        found: CompType,
        span: Span,
    },

    #[error("Expected a function type, found {found}")]
    NotAFunction { found: CompType, span: Span },

    #[error("Expected a producer (F A), found {found}")]
    NotAProducer { found: CompType, span: Span },

    #[error("Expected a thunk type (U C), found {found}")]
    NotAThunk { found: ValType, span: Span },
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
