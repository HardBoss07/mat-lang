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

pub fn read_string_literal(chars: &mut std::str::Chars) -> Result<String, String> {
    let mut string_val = String::new();

    while let Some(ch) = chars.next() {
        match ch {
            '"' => return Ok(string_val),
            '\\' => match chars.next() {
                Some('n') => string_val.push('\n'),
                Some('t') => string_val.push('\t'),
                Some('\\') => string_val.push('\\'),
                Some('"') => string_val.push('"'),
                Some('\'') => string_val.push('\''),
                Some(escaped) => {
                    string_val.push('\\');
                    string_val.push(escaped);
                }
                None => {
                    return Err("Unterminated escape sequence in string literal".to_string());
                }
            },
            other => string_val.push(other),
        }
    }

    Err("Unterminated string literal".to_string())
}
