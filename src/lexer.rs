use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Let,
    Mut,
    Fn,
    Struct,
    Enum,
    Import,
    Pub,
    Return,
    If,
    Else,
    Match,
    Loop,
    While,
    Fori,
    For,
    In,
    Break,
    Continue,

    // Literals & Identifiers
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    // Symbols & Delimiters
    Semicolon,
    Colon,
    DoubleColon,
    Comma,
    Dot,
    Equals,
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Lexer<'a> {
    pub input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }
}
