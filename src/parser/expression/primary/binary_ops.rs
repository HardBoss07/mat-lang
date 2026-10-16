//! Binary operator expression hierarchy and precedence climbing.
//!
//! This module provides parsers for multiplicative, additive, shift, relational,
//! equality, and logical expressions.

use winnow::ModalResult;
use winnow::Parser;

use super::postfix::parse_postfix_expression;
use crate::ast::{BinaryOp, Expression, Span};
use crate::parser::utils::skip_ws_and_comments;

/// Parses multiplicative expressions (`*`, `/`, `%`).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression`] tree.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_multiplicative_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_postfix_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with("*=") || input.starts_with("/=") || input.starts_with("%=") {
            break;
        }

        let op = if input.starts_with('*') {
            BinaryOp::Mul
        } else if input.starts_with('/') {
            BinaryOp::Div
        } else if input.starts_with('%') {
            BinaryOp::Mod
        } else {
            break;
        };

        *input = &input[1..];

        let right = parse_postfix_expression.parse_next(input)?;
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

/// Parses additive expressions (`+`, `-`).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression`] tree.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_additive_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_multiplicative_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with("+=")
            || input.starts_with("-=")
            || input.starts_with("++")
            || input.starts_with("--")
        {
            break;
        }

        let op = if input.starts_with('+') {
            BinaryOp::Add
        } else if input.starts_with('-') {
            BinaryOp::Sub
        } else {
            break;
        };

        *input = &input[1..];

        let right = parse_multiplicative_expression.parse_next(input)?;
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

/// Parses bitwise shift expressions (`<<`, `>>`).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression`] tree.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_shift_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_additive_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with("<<=") || input.starts_with(">>=") {
            break;
        }

        let op = if input.starts_with("<<") {
            BinaryOp::Shl
        } else if input.starts_with(">>") {
            BinaryOp::Shr
        } else {
            break;
        };

        *input = &input[2..];

        let right = parse_additive_expression.parse_next(input)?;
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

/// Parses relational comparison expressions (`<`, `>`, `<=`, `>=`).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression`] tree.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_relational_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_shift_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with("<<") || input.starts_with(">>") {
            break;
        }

        let op = if input.starts_with("<=") {
            BinaryOp::Lte
        } else if input.starts_with(">=") {
            BinaryOp::Gte
        } else if input.starts_with('<') {
            BinaryOp::Lt
        } else if input.starts_with('>') {
            BinaryOp::Gt
        } else {
            break;
        };

        match op {
            BinaryOp::Lte | BinaryOp::Gte => *input = &input[2..],
            _ => *input = &input[1..],
        }

        let right = parse_shift_expression.parse_next(input)?;
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

/// Parses equality expressions (`==`, `!=`).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression`] tree.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_equality_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_relational_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;

        let op = if input.starts_with("==") {
            BinaryOp::Eq
        } else if input.starts_with("!=") {
            BinaryOp::Neq
        } else {
            break;
        };

        *input = &input[2..];

        let right = parse_relational_expression.parse_next(input)?;
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

/// Parses logical AND expressions (`&&`).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression`] tree.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_logical_and_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_equality_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;

        if input.starts_with("&&") {
            *input = &input[2..];
            let right = parse_equality_expression.parse_next(input)?;
            let start = left.span().start;
            let end = right.span().end;
            left = Expression::Binary {
                op: BinaryOp::And,
                left: Box::new(left),
                right: Box::new(right),
                span: Span::new(start, end),
            };
        } else {
            break;
        }
    }

    Ok(left)
}

/// Parses logical OR expressions (`||`).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression`] tree.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_logical_or_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_logical_and_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;

        if input.starts_with("||") {
            *input = &input[2..];
            let right = parse_logical_and_expression.parse_next(input)?;
            let start = left.span().start;
            let end = right.span().end;
            left = Expression::Binary {
                op: BinaryOp::Or,
                left: Box::new(left),
                right: Box::new(right),
                span: Span::new(start, end),
            };
        } else {
            break;
        }
    }

    Ok(left)
}
