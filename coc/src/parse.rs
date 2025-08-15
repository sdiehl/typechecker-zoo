use lalrpop_util::lalrpop_mod;

use crate::ast::{Module, Term};
use crate::errors::{CocResult, ParseError};
use crate::lexer::{Lexer, LexicalError, Token};

lalrpop_mod!(pub parser);

/// Parser wrapper that bridges the LALRPOP parser with our lexer
pub struct Parser {
    module_parser: parser::ModuleParser,
    term_parser: parser::TermParser,
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            module_parser: parser::ModuleParser::new(),
            term_parser: parser::TermParser::new(),
        }
    }

    /// Parse a complete module
    pub fn parse_module(&self, input: &str) -> CocResult<Module> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        let mut _position = 0;

        while let Some(result) = lexer.next() {
            match result {
                Ok(token) => {
                    let span = lexer.span();
                    tokens.push(Ok((span.start, token, span.end)));
                    _position = span.end;
                }
                Err(err) => {
                    tokens.push(Err(err));
                }
            }
        }

        match self.module_parser.parse(tokens) {
            Ok(module) => Ok(module),
            Err(err) => {
                let parse_err = self.convert_lalrpop_error(err);
                Err(parse_err.into())
            }
        }
    }

    /// Parse a single term
    pub fn parse_term(&self, input: &str) -> CocResult<Term> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        let mut _position = 0;

        while let Some(result) = lexer.next() {
            match result {
                Ok(token) => {
                    let span = lexer.span();
                    tokens.push(Ok((span.start, token, span.end)));
                    _position = span.end;
                }
                Err(err) => {
                    tokens.push(Err(err));
                }
            }
        }

        match self.term_parser.parse(tokens) {
            Ok(term) => Ok(term),
            Err(err) => {
                let parse_err = self.convert_lalrpop_error(err);
                Err(parse_err.into())
            }
        }
    }

    /// Convert LALRPOP parse error to our ParseError
    fn convert_lalrpop_error(
        &self,
        err: lalrpop_util::ParseError<usize, Token, LexicalError>,
    ) -> ParseError {
        match err {
            lalrpop_util::ParseError::InvalidToken { location } => ParseError::InvalidSyntax {
                message: "Invalid token".to_string(),
                location: Some(location),
            },
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                ParseError::UnexpectedEndOfInput {
                    expected,
                    location: Some(location),
                }
            }
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (start, tok, end),
                expected,
            } => ParseError::UnexpectedToken {
                token: format!("{:?}", tok),
                expected,
                location: Some((start, end)),
            },
            lalrpop_util::ParseError::ExtraToken {
                token: (start, tok, end),
            } => ParseError::UnexpectedToken {
                token: format!("{:?}", tok),
                expected: vec!["end of input".to_string()],
                location: Some((start, end)),
            },
            lalrpop_util::ParseError::User { error } => ParseError::LexicalError(error),
        }
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}
