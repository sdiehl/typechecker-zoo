use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::errors::{CocError, ParseError, TypeError};

/// Enhanced error reporting with Ariadne
pub struct DiagnosticReporter<'a> {
    filename: &'a str,
    source: &'a str,
}

impl<'a> DiagnosticReporter<'a> {
    pub fn new(filename: &'a str, source: &'a str) -> Self {
        Self { filename, source }
    }

    /// Report a parsing error with nice formatting
    pub fn report_parse_error(&self, error: &ParseError) {
        match error {
            ParseError::UnexpectedToken {
                token,
                expected,
                location,
            } => {
                if let Some((start, end)) = location {
                    Report::build(ReportKind::Error, (self.filename, *start..*end))
                        .with_message(format!("Unexpected token '{}'", token))
                        .with_label(
                            Label::new((self.filename, *start..*end))
                                .with_message(format!("Expected one of: {}", expected.join(", ")))
                                .with_color(Color::Red),
                        )
                        .finish()
                        .print((self.filename, Source::from(self.source)))
                        .unwrap();
                } else {
                    eprintln!(
                        "Parse error: Unexpected token '{}', expected: {}",
                        token,
                        expected.join(", ")
                    );
                }
            }
            ParseError::UnexpectedEndOfInput { expected, location } => {
                let loc = location.unwrap_or(self.source.len());
                Report::build(ReportKind::Error, (self.filename, loc..loc))
                    .with_message("Unexpected end of input")
                    .with_label(
                        Label::new((self.filename, loc..loc))
                            .with_message(format!("Expected one of: {}", expected.join(", ")))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print((self.filename, Source::from(self.source)))
                    .unwrap();
            }
            ParseError::InvalidSyntax { message, location } => {
                if let Some(loc) = location {
                    Report::build(ReportKind::Error, (self.filename, *loc..*loc + 1))
                        .with_message("Invalid syntax")
                        .with_label(
                            Label::new((self.filename, *loc..*loc + 1))
                                .with_message(message)
                                .with_color(Color::Red),
                        )
                        .finish()
                        .print((self.filename, Source::from(self.source)))
                        .unwrap();
                } else {
                    eprintln!("Parse error: {}", message);
                }
            }
            ParseError::LexicalError(err) => {
                eprintln!("Lexical error: {}", err);
            }
        }
    }

    /// Report a type error with nice formatting
    pub fn report_type_error(&self, error: &TypeError, location: Option<usize>) {
        let loc = location.unwrap_or(0);
        let message = format!("{}", error);

        Report::build(ReportKind::Error, (self.filename, loc..loc + 1))
            .with_message("Type error")
            .with_label(
                Label::new((self.filename, loc..loc + 1))
                    .with_message(message)
                    .with_color(Color::Red),
            )
            .finish()
            .print((self.filename, Source::from(self.source)))
            .unwrap();
    }

    /// Report a generic error
    pub fn report_error(&self, error: &CocError) {
        match error {
            CocError::Parse(e) => self.report_parse_error(e),
            CocError::Type(e) => self.report_type_error(e, None),
            CocError::Lexical(e) => {
                eprintln!("Lexical error: {}", e);
            }
            CocError::IO(e) => {
                eprintln!("IO error: {}", e);
            }
        }
    }
}

/// Create an error report with a specific location
pub fn create_error_at(filename: &str, source: &str, location: usize, message: &str) {
    Report::build(ReportKind::Error, (filename, location..location + 1))
        .with_message("Error")
        .with_label(
            Label::new((filename, location..location + 1))
                .with_message(message)
                .with_color(Color::Red),
        )
        .finish()
        .print((filename, Source::from(source)))
        .unwrap();
}
