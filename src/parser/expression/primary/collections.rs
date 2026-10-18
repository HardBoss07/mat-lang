use winnow::ModalResult;
use winnow::Parser;

use super::super::parse_expression;
use crate::ast::Expression;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

pub fn parse_tuple_or_parenthesized(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;
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
        return Ok(Expression::TupleLiteral(
            vec![],
            get_span_between(tok_start, *input),
        ));
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
            Ok(Expression::TupleLiteral(
                elements,
                get_span_between(tok_start, *input),
            ))
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
    let tok_start = *input;
    if !input.starts_with('[') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];

    let mut elements = Vec::new();
    loop {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with(']') {
            *input = &input[1..];
            return Ok(Expression::ArrayLiteral(
                elements,
                get_span_between(tok_start, *input),
            ));
        }

        let expr = match parse_expression.parse_next(input) {
            Ok(e) => e,
            Err(e) => {
                *input = checkpoint;
                return Err(e);
            }
        };
        elements.push(expr);

        let _ = skip_ws_and_comments(input)?;
        if input.starts_with(',') {
            *input = &input[1..];
            let _ = skip_ws_and_comments(input)?;
            if input.starts_with(']') {
                *input = &input[1..];
                return Ok(Expression::ArrayLiteral(
                    elements,
                    get_span_between(tok_start, *input),
                ));
            }
        } else if input.starts_with(']') {
            *input = &input[1..];
            return Ok(Expression::ArrayLiteral(
                elements,
                get_span_between(tok_start, *input),
            ));
        } else {
            *input = checkpoint;
            return Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ));
        }
    }
}

pub fn parse_vector_literal(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;
    if !input.starts_with('{') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];

    let mut elements = Vec::new();
    loop {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with('}') {
            *input = &input[1..];
            return Ok(Expression::VectorLiteral(
                elements,
                get_span_between(tok_start, *input),
            ));
        }

        let expr = match parse_expression.parse_next(input) {
            Ok(e) => e,
            Err(e) => {
                *input = checkpoint;
                return Err(e);
            }
        };
        elements.push(expr);

        let _ = skip_ws_and_comments(input)?;
        if input.starts_with(',') {
            *input = &input[1..];
            let _ = skip_ws_and_comments(input)?;
            if input.starts_with('}') {
                *input = &input[1..];
                return Ok(Expression::VectorLiteral(
                    elements,
                    get_span_between(tok_start, *input),
                ));
            }
        } else if input.starts_with('}') {
            *input = &input[1..];
            return Ok(Expression::VectorLiteral(
                elements,
                get_span_between(tok_start, *input),
            ));
        } else {
            *input = checkpoint;
            return Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ));
        }
    }
}
