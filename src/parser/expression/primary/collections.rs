use winnow::ModalResult;
use winnow::Parser;
use winnow::combinator::{delimited, separated};

use super::super::parse_expression;
use crate::ast::{Expression, Span};
use crate::parser::utils::skip_ws_and_comments;

pub fn parse_tuple_or_parenthesized(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with('(') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

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
    let _ = skip_ws_and_comments(input)?;

    if input.starts_with(',') {
        let mut elements = vec![first];
        while input.starts_with(',') {
            *input = &input[1..];
            let _ = skip_ws_and_comments(input)?;
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
            let _ = skip_ws_and_comments(input)?;
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
    } else if input.starts_with(')') {
        *input = &input[1..];
        Ok(first)
    } else {
        *input = checkpoint;
        Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ))
    }
}

pub fn parse_array_literal(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
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
