use thiserror::Error;

use crate::ast::{Pattern, Term, Universe};
use crate::context::ContextError;
use crate::lexer::LexicalError;
use crate::unification::UnificationError;

/// Unified error type for the COC implementation
#[derive(Debug, Error)]
pub enum CocError {
    #[error("Lexical error: {0}")]
    Lexical(#[from] LexicalError),
    #[error("Parse error: {0}")]
    Parse(#[from] ParseError),
    #[error("Type error: {0}")]
    Type(#[from] TypeError),
    #[error("IO error: {0}")]
    IO(#[from] std::io::Error),
}

/// Type checking errors with detailed context
#[derive(Debug, Error, Clone)]
pub enum TypeError {
    #[error("Context error: {0}")]
    Context(#[from] ContextError),

    #[error("Unification error: {0}")]
    Unification(#[from] UnificationError),

    #[error("Expected function type, got {term} : {ty}")]
    NotAFunction { term: Term, ty: Term },

    #[error("Expected type, got {term} : {ty}")]
    NotAType { term: Term, ty: Term },

    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: Term, actual: Term },

    #[error("Universe error: {universe}")]
    UniverseError { universe: Universe },

    #[error("Invalid inductive type '{name}': {reason}")]
    InvalidInductive { name: String, reason: String },

    #[error("Pattern {pattern} cannot match type {term_type}")]
    PatternMatchError { pattern: Pattern, term_type: Term },

    #[error("Missing case for pattern {pattern}")]
    MissingCase { pattern: Pattern },

    // More specific error types
    #[error("Unknown constructor '{name}'")]
    UnknownConstructor { name: String },

    #[error("Constructor '{name}' expects {expected} arguments, got {actual}")]
    ConstructorArityMismatch {
        name: String,
        expected: usize,
        actual: usize,
    },

    #[error("Field '{field}' not found for type '{ty}'")]
    FieldNotFound { field: String, ty: Term },

    #[error("Cannot project field '{field}' from non-structure type '{ty}'")]
    InvalidProjection { field: String, ty: Term },

    #[error("Match expression must have at least one arm")]
    EmptyMatch,

    #[error("Unexpected implicit parameter '{param}' after processing")]
    UnexpectedImplicitParameter { param: String },

    #[error("Internal compiler error: {message}")]
    Internal { message: String },

    #[error("While type-checking expression: {expr}\n{source}")]
    WithContext {
        expr: Term,
        #[source]
        source: Box<TypeError>,
    },
}

/// Parse errors
#[derive(Debug, Error, Clone)]
pub enum ParseError {
    #[error("Unexpected token '{token}', expected one of: {}", expected.join(", "))]
    UnexpectedToken {
        token: String,
        expected: Vec<String>,
        location: Option<(usize, usize)>, // (start, end)
    },

    #[error("Unexpected end of input, expected one of: {}", expected.join(", "))]
    UnexpectedEndOfInput {
        expected: Vec<String>,
        location: Option<usize>,
    },

    #[error("Invalid syntax: {message}")]
    InvalidSyntax {
        message: String,
        location: Option<usize>,
    },

    #[error("Lexical error: {0}")]
    LexicalError(#[from] LexicalError),
}

pub type CocResult<T> = Result<T, CocError>;
pub type TypeResult<T> = Result<T, TypeError>;
