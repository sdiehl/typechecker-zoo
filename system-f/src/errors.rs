use ariadne::{Color, Label, Report, ReportKind};
use thiserror::Error;

use crate::ast::Type;

/// Source span for error reporting
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

#[derive(Debug)]
#[allow(dead_code, clippy::enum_variant_names)]
pub enum TypeError {
    UnboundVariable {
        name: String,
        expr: Option<String>,
    },

    ApplicationTypeError {
        actual: Type,
        expr: Option<String>,
    },

    TypeApplicationError {
        actual: Type,
        expr: Option<String>,
    },

    OccursCheck {
        var: String,
        ty: Type,
        expr: Option<String>,
    },

    SubtypingError {
        left: Type,
        right: Type,
        expr: Option<String>,
    },

    InstantiationError {
        var: String,
        ty: Type,
        expr: Option<String>,
    },

    CheckingError {
        expected: Type,
        expr: Option<String>,
    },

    InferenceError {
        expr: Option<String>,
    },

    ContextError {
        message: String,
        expr: Option<String>,
    },
}

impl TypeError {}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnboundVariable { name, expr } => {
                write!(f, "Variable '{}' not found in context", name)?;
                if let Some(expr) = expr {
                    write!(f, "\n  When typing expression: {}", expr)?;
                }
                Ok(())
            }
            TypeError::ApplicationTypeError { actual, expr } => {
                write!(f, "Expected function type in application, got {}", actual)?;
                if let Some(expr) = expr {
                    write!(f, "\n  When typing expression: {}", expr)?;
                }
                Ok(())
            }
            TypeError::TypeApplicationError { actual, expr } => {
                write!(
                    f,
                    "Expected forall type in type application, got {}",
                    actual
                )?;
                if let Some(expr) = expr {
                    write!(f, "\n  When typing expression: {}", expr)?;
                }
                Ok(())
            }
            TypeError::OccursCheck { var, ty, expr } => {
                write!(
                    f,
                    "Occurs check failed: variable '{}' occurs in type {}",
                    var, ty
                )?;
                if let Some(expr) = expr {
                    write!(f, "\n  When typing expression: {}", expr)?;
                }
                Ok(())
            }
            TypeError::SubtypingError { left, right, expr } => {
                write!(f, "No matching rule for subtyping {} <: {}", left, right)?;
                if let Some(expr) = expr {
                    write!(f, "\n  When typing expression: {}", expr)?;
                }
                Ok(())
            }
            TypeError::InstantiationError { var, ty, expr } => {
                write!(f, "No matching rule for instantiation {} :=< {}", var, ty)?;
                if let Some(expr) = expr {
                    write!(f, "\n  When typing expression: {}", expr)?;
                }
                Ok(())
            }
            TypeError::CheckingError { expected, expr } => {
                write!(f, "No matching rule for checking against type {}", expected)?;
                if let Some(expr) = expr {
                    write!(f, "\n  When typing expression: {}", expr)?;
                }
                Ok(())
            }
            TypeError::InferenceError { expr } => {
                write!(f, "No matching rule for inference")?;
                if let Some(expr) = expr {
                    write!(f, "\n  When typing expression: {}", expr)?;
                }
                Ok(())
            }
            TypeError::ContextError { message, expr } => {
                write!(f, "Context error: {}", message)?;
                if let Some(expr) = expr {
                    write!(f, "\n  When typing expression: {}", expr)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Error, Debug)]
#[allow(dead_code)]
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
                Report::build(ReportKind::Error, filename, span.start)
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

pub type TypeResult<T> = Result<T, TypeError>;
#[allow(dead_code)]
pub type ParseResult<T> = Result<T, ParseError>;
