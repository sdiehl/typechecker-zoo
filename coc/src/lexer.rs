use std::fmt;

use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords - Lean 4 style
    #[token("def")]
    Def,
    #[token("axiom")]
    Axiom,
    #[token("inductive")]
    Inductive,
    #[token("structure")]
    Structure,
    #[token("Σ")]
    Sigma,
    #[token("exists")]
    Exists,
    #[token("fun")]
    Fun,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,

    // Type system keywords
    #[token("Type")]
    Type,
    #[token("Prop")]
    Prop,
    #[token("Sort")]
    Sort,
    #[token("max")]
    Max,
    #[token("imax")]
    IMax,

    // Operators and symbols
    #[token("=>")]
    FatArrow,
    #[token("->")]
    Arrow,
    #[token("→")]
    UnicodeArrow,
    #[token(":=")]
    ColonEqual,
    #[token(":")]
    Colon,
    #[token("=")]
    Equal,
    #[token("|")]
    Pipe,
    #[token("case")]
    Case,
    #[token("_", priority = 2)]
    Underscore,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,

    // Delimiters
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("⟨")]
    LeftAngle,
    #[token("⟩")]
    RightAngle,

    // Identifiers and literals
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_']*", |lex| lex.slice().to_owned(), priority = 1)]
    Identifier(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Number(Option<i64>),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_owned()
    })]
    String(String),

    #[regex(r"'([^'\\]|\\.)'", |lex| {
        let s = lex.slice();
        s.chars().nth(1)
    })]
    Char(Option<char>),

    // Whitespace and comments
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"--[^\n\r]*", logos::skip)]
    #[regex(r"/\*([^*]|\*+[^*/])*\*+/", logos::skip)]
    Whitespace,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidToken,
    InvalidNumber,
    InvalidString,
    InvalidChar,
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexicalError::InvalidToken => write!(f, "Invalid token"),
            LexicalError::InvalidNumber => write!(f, "Invalid number literal"),
            LexicalError::InvalidString => write!(f, "Invalid string literal"),
            LexicalError::InvalidChar => write!(f, "Invalid character literal"),
        }
    }
}

impl std::error::Error for LexicalError {}

pub struct Lexer<'input> {
    logos_lexer: logos::Lexer<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            logos_lexer: Token::lexer(input),
        }
    }

    pub fn next_token(&mut self) -> Option<Result<Token, LexicalError>> {
        match self.logos_lexer.next() {
            Some(Ok(Token::Number(None))) => Some(Err(LexicalError::InvalidNumber)),
            Some(Ok(Token::Char(None))) => Some(Err(LexicalError::InvalidChar)),
            Some(Ok(token)) => Some(Ok(token)),
            Some(Err(_)) => Some(Err(LexicalError::InvalidToken)),
            None => None,
        }
    }

    pub fn span(&self) -> std::ops::Range<usize> {
        self.logos_lexer.span()
    }

    pub fn slice(&self) -> &'input str {
        self.logos_lexer.slice()
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Token, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let input = "def fun match Type Prop";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Ok(Token::Def)));
        assert_eq!(lexer.next(), Some(Ok(Token::Fun)));
        assert_eq!(lexer.next(), Some(Ok(Token::Match)));
        assert_eq!(lexer.next(), Some(Ok(Token::Type)));
        assert_eq!(lexer.next(), Some(Ok(Token::Prop)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_arrows_and_symbols() {
        let input = "=> -> → := : = |";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Ok(Token::FatArrow)));
        assert_eq!(lexer.next(), Some(Ok(Token::Arrow)));
        assert_eq!(lexer.next(), Some(Ok(Token::UnicodeArrow)));
        assert_eq!(lexer.next(), Some(Ok(Token::ColonEqual)));
        assert_eq!(lexer.next(), Some(Ok(Token::Colon)));
        assert_eq!(lexer.next(), Some(Ok(Token::Equal)));
        assert_eq!(lexer.next(), Some(Ok(Token::Pipe)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_delimiters() {
        let input = "() {} ⟨⟩";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Ok(Token::LeftParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::RightParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::LeftBrace)));
        assert_eq!(lexer.next(), Some(Ok(Token::RightBrace)));
        assert_eq!(lexer.next(), Some(Ok(Token::LeftAngle)));
        assert_eq!(lexer.next(), Some(Ok(Token::RightAngle)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_identifiers() {
        let input = "x var_name f' camelCase";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("x".to_string()))));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::Identifier("var_name".to_string())))
        );
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("f'".to_string()))));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::Identifier("camelCase".to_string())))
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_literals() {
        let input = r#"42 "hello world" 'x'"#;
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Ok(Token::Number(Some(42)))));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::String("hello world".to_string())))
        );
        assert_eq!(lexer.next(), Some(Ok(Token::Char(Some('x')))));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_lean_function() {
        let input = "def id (x : Type) : Type := x";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Ok(Token::Def)));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("id".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::LeftParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("x".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Colon)));
        assert_eq!(lexer.next(), Some(Ok(Token::Type)));
        assert_eq!(lexer.next(), Some(Ok(Token::RightParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::Colon)));
        assert_eq!(lexer.next(), Some(Ok(Token::Type)));
        assert_eq!(lexer.next(), Some(Ok(Token::ColonEqual)));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("x".to_string()))));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_comments_ignored() {
        let input = "def -- this is a comment\n x";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Ok(Token::Def)));
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier("x".to_string()))));
        assert_eq!(lexer.next(), None);
    }
}
