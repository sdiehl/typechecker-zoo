use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind};
use thiserror::Error;

use crate::core::CoreType;

pub type Span = Range<usize>;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unexpected token '{token}' at position {span:?}")]
    UnexpectedToken { token: String, span: Span },

    #[error("Expected {expected} but found '{found}' at position {span:?}")]
    Expected {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("Unexpected end of file, expected {expected}")]
    UnexpectedEof { expected: String },
}

#[derive(Error, Debug, Clone)]
pub enum TypeError {
    #[error("Variable '{name}' not found in scope")]
    UnboundVariable { name: String, span: Option<Span> },

    #[error("Data constructor '{name}' not found")]
    UnboundDataConstructor { name: String, span: Option<Span> },

    #[error("Type '{ty}' is not a function type")]
    NotAFunction { ty: CoreType, span: Option<Span> },

    #[error("Arity mismatch: expected {expected} arguments, got {actual}")]
    ArityMismatch {
        expected: usize,
        actual: usize,
        span: Option<Span>,
    },

    #[error("Function '{name}' has definition but no type signature")]
    MissingTypeSignature { name: String, span: Option<Span> },

    #[error("Subtyping failure: '{left}' is not a subtype of '{right}'")]
    SubtypingError {
        left: CoreType,
        right: CoreType,
        span: Option<Span>,
    },

    #[error("Type unification error: cannot unify type '{left}' with '{right}'")]
    TypeUnificationError { left: CoreType, right: CoreType },

    #[error("Instantiation failure: cannot instantiate '{var}' with '{ty}'")]
    InstantiationError {
        var: String,
        ty: CoreType,
        span: Option<Span>,
    },

    #[error("Intrinsic '{name}' expects {expected} arguments, got {actual}")]
    IntrinsicArityMismatch {
        name: String,
        expected: usize,
        actual: usize,
        span: Option<Span>,
    },

    #[error("Unknown intrinsic function: '{name}'")]
    UnknownIntrinsic { name: String, span: Option<Span> },
}

#[derive(Error, Debug, Clone)]
pub enum CompilerError {
    #[error("Parse error")]
    Parse(#[from] ParseError),

    #[error("Type error")]
    Type(#[from] TypeError),
}

pub type ParseResult<T> = Result<T, ParseError>;
pub type TypeResult<T> = Result<T, TypeError>;
pub type CompilerResult<T> = Result<T, CompilerError>;

impl ParseError {
    pub fn report<'a>(
        &self,
        source: &str,
        filename: &'a str,
    ) -> Report<'a, (&'a str, Range<usize>)> {
        match self {
            ParseError::UnexpectedToken { token, span } => {
                Report::build(ReportKind::Error, (filename, span.clone()))
                    .with_message(format!("Unexpected token '{}'", token))
                    .with_label(
                        Label::new((filename, span.clone()))
                            .with_message("unexpected token here")
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            ParseError::Expected {
                expected,
                found,
                span,
            } => Report::build(ReportKind::Error, (filename, span.clone()))
                .with_message(format!("Expected {}, found '{}'", expected, found))
                .with_label(
                    Label::new((filename, span.clone()))
                        .with_message(format!("expected {}", expected))
                        .with_color(Color::Red),
                )
                .finish(),
            ParseError::UnexpectedEof { expected } => {
                Report::build(ReportKind::Error, (filename, source.len()..source.len()))
                    .with_message(format!("Unexpected end of file, expected {}", expected))
                    .finish()
            }
        }
    }
}

impl TypeError {
    pub fn report<'a>(
        &self,
        source: &str,
        filename: &'a str,
    ) -> Report<'a, (&'a str, Range<usize>)> {
        match self {
            TypeError::UnboundVariable { name, span } => {
                let span = span.clone().unwrap_or(0..source.len());
                Report::build(ReportKind::Error, (filename, span.clone()))
                    .with_message(format!("Variable '{}' not found in scope", name))
                    .with_label(
                        Label::new((filename, span))
                            .with_message("undefined variable")
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            // Add more specific error reporting as needed
            _ => Report::build(ReportKind::Error, (filename, 0..1))
                .with_message(self.to_string())
                .finish(),
        }
    }
}
