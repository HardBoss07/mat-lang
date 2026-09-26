use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::{alpha1, digit1, multispace0};
use winnow::combinator::{alt, delimited, separated};
use winnow::token::take_while;

use super::literals::{
    parse_bool_literal, parse_float_literal, parse_int_literal, parse_string_or_interpolated,
};
use crate::ast::{Expression, Span};

pub fn parse_identifier_str<'a>(input: &mut &'a str) -> ModalResult<&'a str> {
    let _ = multispace0.parse_next(input)?;
    (
        alpha1,
        take_while(0.., |c: char| c.is_alphanumeric() || c == '_'),
    )
        .take()
        .parse_next(input)
}

pub fn parse_identifier(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    match parse_identifier_str.parse_next(input) {
        Ok(name) => Ok(Expression::Identifier(
            name.to_string(),
            Span::new(0, name.len()),
        )),
        Err(e) => {
            *input = checkpoint;
            Err(e)
        }
    }
}

pub fn parse_tuple_or_parenthesized(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;
    if !input.starts_with('(') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = multispace0.parse_next(input)?;

    if input.starts_with(')') {
        *input = &input[1..];
        return Ok(Expression::TupleLiteral(vec![], Span::new(0, 0)));
    }

    let first = match parse_expression.parse_next(input) {
        Ok(expr) => expr,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    if input.starts_with(',') {
        let mut elements = vec![first];
        while input.starts_with(',') {
            *input = &input[1..];
            let _ = multispace0.parse_next(input)?;
            if input.starts_with(')') {
                break;
            }
            match parse_expression.parse_next(input) {
                Ok(elem) => elements.push(elem),
                Err(e) => {
                    *input = checkpoint;
                    return Err(e);
                }
            }
            let _ = multispace0.parse_next(input)?;
        }
        if input.starts_with(')') {
            *input = &input[1..];
            Ok(Expression::TupleLiteral(elements, Span::new(0, 0)))
        } else {
            *input = checkpoint;
            Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ))
        }
    } else {
        if input.starts_with(')') {
            *input = &input[1..];
            Ok(first)
        } else {
            *input = checkpoint;
            Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ))
        }
    }
}

pub fn parse_array_literal(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;
    if !input.starts_with('[') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    let arr_res: ModalResult<Vec<Expression>> =
        delimited('[', separated(0.., parse_expression, ','), ']').parse_next(input);
    if let Ok(elements) = arr_res {
        return Ok(Expression::ArrayLiteral(elements, Span::new(0, 0)));
    }

    *input = checkpoint;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}

pub fn parse_primary_expression(input: &mut &str) -> ModalResult<Expression> {
    let _ = multispace0.parse_next(input)?;
    alt((
        parse_string_or_interpolated,
        parse_bool_literal,
        parse_float_literal,
        parse_int_literal,
        parse_tuple_or_parenthesized,
        parse_array_literal,
        parse_identifier,
    ))
    .parse_next(input)
}

pub fn parse_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut expr = parse_primary_expression.parse_next(input)?;

    loop {
        let _ = multispace0.parse_next(input)?;
        if input.starts_with('(') {
            let callee_name = match &expr {
                Expression::Identifier(name, _) => name.clone(),
                _ => break,
            };
            let args_res: ModalResult<Vec<Expression>> =
                delimited('(', separated(0.., parse_expression, ','), ')').parse_next(input);
            if let Ok(args) = args_res {
                expr = Expression::Call {
                    callee: callee_name,
                    arguments: args,
                    span: Span::new(0, 0),
                };
                continue;
            }
            break;
        } else if input.starts_with('.') {
            let mut checkpoint = *input;
            checkpoint = &checkpoint[1..];
            let idx_res: ModalResult<&str> = digit1.parse_next(&mut checkpoint);
            if let Ok(idx_str) = idx_res {
                *input = checkpoint;
                let index: usize = idx_str.parse().unwrap();
                expr = Expression::TupleAccess {
                    expr: Box::new(expr),
                    index,
                    span: Span::new(0, 0),
                };
                continue;
            } else {
                break;
            }
        } else if input.starts_with('[') {
            let mut checkpoint = *input;
            checkpoint = &checkpoint[1..];
            if let Ok(index) = parse_expression.parse_next(&mut checkpoint) {
                let _ = multispace0.parse_next(&mut checkpoint)?;
                if checkpoint.starts_with(']') {
                    checkpoint = &checkpoint[1..];
                    *input = checkpoint;
                    expr = Expression::ArrayAccess {
                        expr: Box::new(expr),
                        index: Box::new(index),
                        span: Span::new(0, 0),
                    };
                    continue;
                }
            }
            break;
        }
        break;
    }

    Ok(expr)
}
