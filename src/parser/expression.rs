use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::{alphanumeric1, multispace0};
use winnow::combinator::{alt, delimited, separated};
use winnow::token::take_until;

use crate::ast::{Expression, Span};

pub fn parse_string_literal(input: &mut &str) -> ModalResult<Expression> {
    let start = 0;
    let content: &str = delimited('"', take_until(0.., '"'), '"').parse_next(input)?;
    let end = content.len() + 2;

    Ok(Expression::StringLiteral(
        content.to_string(),
        Span::new(start, end),
    ))
}

pub fn parse_call_expression(input: &mut &str) -> ModalResult<Expression> {
    let callee: &str = alphanumeric1.parse_next(input)?;
    let args: Vec<Expression> =
        delimited('(', separated(0.., parse_expression, ','), ')').parse_next(input)?;

    Ok(Expression::Call {
        callee: callee.to_string(),
        arguments: args,
        span: Span::new(0, 0),
    })
}

pub fn parse_expression(input: &mut &str) -> ModalResult<Expression> {
    let _ = multispace0.parse_next(input)?;
    alt((parse_string_literal, parse_call_expression)).parse_next(input)
}
