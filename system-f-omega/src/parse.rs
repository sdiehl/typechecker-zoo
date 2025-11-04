use ariadne::Source;

use crate::errors::{ParseError, ParseResult};
use crate::lexer::{Lexer, LexicalError, Token};
use crate::surface::Module;

#[allow(clippy::all)]
#[allow(clippy::unwrap_in_result)]
mod parser_impl {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser);
}
use parser_impl::parser;

pub struct Parser {
    module_parser: parser::ModuleParser,
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            module_parser: parser::ModuleParser::new(),
        }
    }

    pub fn parse_module(&self, input: &str) -> ParseResult<Module> {
        let lexer = Lexer::new(input);

        self.module_parser
            .parse(lexer)
            .map_err(|e| self.convert_lalrpop_error(e, input))
    }

    fn convert_lalrpop_error(
        &self,
        error: lalrpop_util::ParseError<usize, Token, LexicalError>,
        _input: &str,
    ) -> ParseError {
        match error {
            lalrpop_util::ParseError::InvalidToken { location } => ParseError::UnexpectedToken {
                token: "invalid token".to_string(),
                span: location..location + 1,
            },
            lalrpop_util::ParseError::UnrecognizedEof {
                location: _,
                expected,
            } => ParseError::UnexpectedEof {
                expected: expected.join(", "),
            },
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                let (start, tok, end) = token;
                ParseError::Expected {
                    expected: expected.join(", "),
                    found: tok.to_string(),
                    span: start..end,
                }
            }
            lalrpop_util::ParseError::ExtraToken { token } => {
                let (start, tok, end) = token;
                ParseError::UnexpectedToken {
                    token: tok.to_string(),
                    span: start..end,
                }
            }
            lalrpop_util::ParseError::User { error } => ParseError::UnexpectedToken {
                token: error.to_string(),
                span: 0..1,
            },
        }
    }

    pub fn parse_module_with_diagnostics(
        &self,
        input: &str,
        filename: &str,
    ) -> Result<Module, ParseError> {
        match self.parse_module(input) {
            Ok(module) => Ok(module),
            Err(error) => {
                let report = error.report(input, filename);
                let _ = report.eprint((filename, Source::from(input)));
                Err(error)
            }
        }
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::surface::Declaration;

    #[test]
    fn test_parse_simple_data() {
        let parser = Parser::new();
        let input = "data Bool = True | False;";
        let result = parser.parse_module(input);
        if let Err(e) = &result {
            eprintln!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());

        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);

        match &module.declarations[0] {
            Declaration::Data {
                name, constructors, ..
            } => {
                assert_eq!(name, "Bool");
                assert_eq!(constructors.len(), 2);
                assert_eq!(constructors[0].name, "True");
                assert_eq!(constructors[1].name, "False");
            }
            _ => panic!("Expected data declaration"),
        }
    }

    #[test]
    fn test_parse_function_signature() {
        let parser = Parser::new();
        let input = "fib :: Int -> Int;";
        let result = parser.parse_module(input);
        assert!(result.is_ok());

        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);

        match &module.declarations[0] {
            Declaration::TypeSig { name, .. } => {
                assert_eq!(name, "fib");
            }
            _ => panic!("Expected type signature"),
        }
    }

    #[test]
    fn test_parse_function_definition() {
        let parser = Parser::new();
        let input = "fib n = 42;";
        let result = parser.parse_module(input);
        assert!(result.is_ok());

        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);

        match &module.declarations[0] {
            Declaration::FunDef { name, params, .. } => {
                assert_eq!(name, "fib");
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], "n");
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_complete_module() {
        let parser = Parser::new();
        let input = r#"
            data Bool = True | False;
            fib :: Int -> Int;
            fib n = 42;
        "#;
        let result = parser.parse_module(input);
        if let Err(e) = &result {
            eprintln!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());

        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 3);
    }

    #[test]
    fn test_parse_arithmetic() {
        let parser = Parser::new();
        let input = "test n = 1 + 2 * 3;";
        let result = parser.parse_module(input);
        assert!(result.is_ok());

        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);
    }

    #[test]
    fn test_parse_type_sig_and_def() {
        let parser = Parser::new();

        // Test just function definition with one parameter
        let result = parser.parse_module("id x = x;");
        if let Err(e) = &result {
            eprintln!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());

        let module = result.unwrap();
        assert_eq!(module.declarations.len(), 1);
    }
}
