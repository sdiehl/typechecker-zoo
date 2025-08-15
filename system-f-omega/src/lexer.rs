use std::fmt;

use logos::Logos;

pub struct Lexer<'input> {
    token_stream: logos::SpannedIter<'input, Token>,
    last_token: Option<Token>,
    pending_token: Option<(usize, Token, usize)>, // Token to emit after semicolon
    just_inserted_semicolon: bool,                // Track if we just inserted a semicolon
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            token_stream: Token::lexer(input).spanned(),
            last_token: None,
            pending_token: None,
            just_inserted_semicolon: false,
        }
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    #[token("data")]
    Data,
    #[token("forall")]
    Forall,
    #[token("match")]
    Match,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,

    // Operators and delimiters
    #[token("->")]
    Arrow,
    #[token("::")]
    DoubleColon,
    #[token("=")]
    Equal,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("<=")]
    LessEqual,
    #[token("<")]
    Less,
    #[token("|")]
    Pipe,
    #[token("_", priority = 2)]
    Underscore,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("\\")]
    Backslash,

    // Delimiters
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,

    // Identifiers and literals
    #[regex(r"[a-zA-Z][a-zA-Z0-9_']*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap_or(0))]
    Integer(i64),

    // Automatic semicolon insertion
    #[token(";")]
    Semicolon,

    // Special tokens
    Newline,

    // Skip whitespace and comments
    #[regex(r"[ \t\f]+", logos::skip)]
    #[regex(r"--[^\r\n]*", logos::skip)]
    #[token("\r\n", |_| Token::Newline)]
    #[token("\n", |_| Token::Newline)]
    Error,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Data => write!(f, "data"),
            Token::Forall => write!(f, "forall"),
            Token::Match => write!(f, "match"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Arrow => write!(f, "->"),
            Token::DoubleColon => write!(f, "::"),
            Token::Equal => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::LessEqual => write!(f, "<="),
            Token::Less => write!(f, "<"),
            Token::Pipe => write!(f, "|"),
            Token::Underscore => write!(f, "_"),
            Token::Dot => write!(f, "."),
            Token::Comma => write!(f, ","),
            Token::Backslash => write!(f, "\\"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Integer(n) => write!(f, "{}", n),
            Token::Semicolon => write!(f, ";"),
            Token::Newline => write!(f, "\\n"),
            Token::Error => write!(f, "ERROR"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidToken,
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexicalError::InvalidToken => write!(f, "Invalid token"),
        }
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        // If we have a pending token, return it
        if let Some((start, token, end)) = self.pending_token.take() {
            self.last_token = Some(token.clone());
            self.just_inserted_semicolon = false;
            return Some(Ok((start, token, end)));
        }

        match self.token_stream.next()? {
            (Ok(Token::Newline), span) => {
                // Check if we should insert a semicolon before this newline
                // But don't insert if we just inserted one
                if !self.just_inserted_semicolon {
                    if let Some(ref last) = self.last_token {
                        if should_insert_semicolon(last) {
                            self.just_inserted_semicolon = true;
                            return Some(Ok((span.start, Token::Semicolon, span.start)));
                        }
                    }
                }
                // Skip newlines and continue
                self.just_inserted_semicolon = false;
                self.next()
            }
            (Ok(token), span) => {
                // Check for semicolon insertion at statement boundaries
                // But don't insert if we just inserted one
                if !self.just_inserted_semicolon {
                    if let Some(ref last) = self.last_token {
                        if should_insert_semicolon(last) && needs_semicolon_before(&token) {
                            // Store the current token for the next call, and return a semicolon
                            self.pending_token = Some((span.start, token, span.end));
                            self.just_inserted_semicolon = true;
                            return Some(Ok((span.start, Token::Semicolon, span.start)));
                        }
                    }
                }

                self.last_token = Some(token.clone());
                self.just_inserted_semicolon = false;
                Some(Ok((span.start, token, span.end)))
            }
            (Err(_), _span) => Some(Err(LexicalError::InvalidToken)),
        }
    }
}

fn should_insert_semicolon(token: &Token) -> bool {
    matches!(
        token,
        Token::Ident(_) | Token::Integer(_) | Token::RightParen | Token::RightBrace
    )
}

fn needs_semicolon_before(token: &Token) -> bool {
    matches!(token, Token::Data | Token::If | Token::Match)
    // Note: Removed Token::Ident(_) to prevent semicolon insertion in function
    // parameters
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let lexer = Lexer::new("data Bool = True | False");
        let tokens: Vec<_> = lexer.collect();

        // Check that we get the expected tokens without errors
        assert!(tokens.iter().all(|t| t.is_ok()));

        let token_values: Vec<_> = tokens.into_iter().map(|t| t.unwrap().1).collect();
        assert_eq!(
            token_values,
            vec![
                Token::Data,
                Token::Ident("Bool".to_string()),
                Token::Equal,
                Token::Ident("True".to_string()),
                Token::Pipe,
                Token::Ident("False".to_string()),
            ]
        );
    }

    #[test]
    fn test_function_definition() {
        let lexer = Lexer::new("fib :: Int -> Int");
        let tokens: Vec<_> = lexer.map(|t| t.unwrap().1).collect();

        assert_eq!(
            tokens,
            vec![
                Token::Ident("fib".to_string()),
                Token::DoubleColon,
                Token::Ident("Int".to_string()),
                Token::Arrow,
                Token::Ident("Int".to_string()),
            ]
        );
    }

    #[test]
    fn test_arithmetic() {
        let lexer = Lexer::new("1 + 2 * 3 <= 42");
        let tokens: Vec<_> = lexer.map(|t| t.unwrap().1).collect();

        assert_eq!(
            tokens,
            vec![
                Token::Integer(1),
                Token::Plus,
                Token::Integer(2),
                Token::Star,
                Token::Integer(3),
                Token::LessEqual,
                Token::Integer(42),
            ]
        );
    }

    #[test]
    fn test_comments_ignored() {
        let lexer = Lexer::new("fib -- This is a comment\n:: Int");
        let tokens: Vec<_> = lexer.map(|t| t.unwrap().1).collect();

        assert_eq!(
            tokens,
            vec![
                Token::Ident("fib".to_string()),
                Token::Semicolon, // Auto-inserted at newline
                Token::DoubleColon,
                Token::Ident("Int".to_string()),
            ]
        );
    }

    #[test]
    fn test_auto_semicolon_insertion() {
        let lexer = Lexer::new("data Bool = True | False\nfib n = 42");
        let tokens: Vec<_> = lexer.map(|t| t.unwrap().1).collect();

        // Should insert semicolon after "False" before "fib"
        assert!(tokens.contains(&Token::Semicolon));

        // Check that we have the right tokens in order
        assert_eq!(tokens[0], Token::Data);
        assert_eq!(tokens[1], Token::Ident("Bool".to_string()));
        assert_eq!(tokens[2], Token::Equal);
        assert_eq!(tokens[3], Token::Ident("True".to_string()));
        assert_eq!(tokens[4], Token::Pipe);
        assert_eq!(tokens[5], Token::Ident("False".to_string()));
        assert_eq!(tokens[6], Token::Semicolon); // Auto-inserted
        assert_eq!(tokens[7], Token::Ident("fib".to_string()));
        assert_eq!(tokens[8], Token::Ident("n".to_string()));
        assert_eq!(tokens[9], Token::Equal);
        assert_eq!(tokens[10], Token::Integer(42));
    }

    #[test]
    fn test_function_parameter_tokens() {
        let lexer = Lexer::new("id x = x;");
        let tokens: Vec<_> = lexer.map(|t| t.unwrap().1).collect();

        println!("Function parameter tokens: {:?}", tokens);

        assert_eq!(tokens[0], Token::Ident("id".to_string()));
        assert_eq!(tokens[1], Token::Ident("x".to_string()));
        assert_eq!(tokens[2], Token::Equal);
        assert_eq!(tokens[3], Token::Ident("x".to_string()));
        assert_eq!(tokens[4], Token::Semicolon);
    }
}
