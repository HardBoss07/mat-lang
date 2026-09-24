use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace0;
use winnow::combinator::{alt, opt};
use winnow::token::literal;

use crate::ast::{Span, Statement, Type};
use crate::parser::expression::{parse_expression, parse_identifier_str};

pub fn parse_type(input: &mut &str) -> ModalResult<Type> {
    let _ = multispace0.parse_next(input)?;
    let type_str = parse_identifier_str.parse_next(input)?;
    match type_str {
        "int" => Ok(Type::Int),
        "i32" => Ok(Type::I32),
        "i16" => Ok(Type::I16),
        "i8" => Ok(Type::I8),
        "f64" => Ok(Type::F64),
        "bool" => Ok(Type::Bool),
        "String" => Ok(Type::String),
        other => Ok(Type::Custom(other.to_string())),
    }
}

pub fn parse_let_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = multispace0.parse_next(input)?;
    let _ = literal("let").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let is_mutable = opt(literal("mut")).parse_next(input)?.is_some();
    let _ = multispace0.parse_next(input)?;

    let name = parse_identifier_str.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let _ = literal(':').parse_next(input)?;
    let ty = parse_type.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let _ = literal('=').parse_next(input)?;
    let value = parse_expression.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Let {
        name: name.to_string(),
        is_mutable,
        ty,
        value,
        span: Span::new(0, 0),
    })
}

pub fn parse_assignment_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = multispace0.parse_next(input)?;
    let target = parse_identifier_str.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal('=').parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let value = parse_expression.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Assignment {
        target: target.to_string(),
        value,
        span: Span::new(0, 0),
    })
}

pub fn parse_increment_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = multispace0.parse_next(input)?;
    let target = parse_identifier_str.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal("++").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Increment {
        target: target.to_string(),
        span: Span::new(0, 0),
    })
}

pub fn parse_decrement_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = multispace0.parse_next(input)?;
    let target = parse_identifier_str.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal("--").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Decrement {
        target: target.to_string(),
        span: Span::new(0, 0),
    })
}

pub fn parse_expression_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = multispace0.parse_next(input)?;
    let expr = parse_expression.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Expression(expr))
}

pub fn parse_statement(input: &mut &str) -> ModalResult<Statement> {
    alt((
        parse_let_statement,
        parse_increment_statement,
        parse_decrement_statement,
        parse_assignment_statement,
        parse_expression_statement,
    ))
    .parse_next(input)
}
