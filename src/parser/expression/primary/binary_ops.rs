//! Binary operator expression hierarchy and precedence climbing.
//!
//! This module provides parsers for multiplicative, additive, shift, relational,
//! equality, and logical expressions using a precedence climbing structure.

use winnow::ModalResult;

use super::postfix::parse_postfix_expression;
use crate::ast::{BinaryOp, Expression, Span};
use crate::parser::utils::skip_ws_and_comments;

/// Combines a left expression and a right expression into a [`Expression::Binary`] with combined span.
#[inline]
fn make_binary_expr(op: BinaryOp, left: Expression, right: Expression) -> Expression {
    let start = left.span().start;
    let end = right.span().end;
    Expression::Binary {
        op,
        left: Box::new(left),
        right: Box::new(right),
        span: Span::new(start, end),
    }
}

/// Helper that repeats parsing binary operators and right-hand expressions for a given precedence level.
fn parse_binary_climbing<'a, F, G>(
    input: &mut &'a str,
    mut parse_operand: F,
    mut match_op: G,
) -> ModalResult<Expression>
where
    F: FnMut(&mut &'a str) -> ModalResult<Expression>,
    G: FnMut(&mut &'a str) -> ModalResult<Option<BinaryOp>>,
{
    let mut left = parse_operand(input)?;

    loop {
        skip_ws_and_comments(input)?;
        match match_op(input)? {
            Some(op) => {
                let right = parse_operand(input)?;
                left = make_binary_expr(op, left, right);
            }
            None => break,
        }
    }

    Ok(left)
}

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
    parse_binary_climbing(input, parse_postfix_expression, |input| {
        if input.starts_with("*=") || input.starts_with("/=") || input.starts_with("%=") {
            return Ok(None);
        }

        let op = if input.starts_with('*') {
            BinaryOp::Mul
        } else if input.starts_with('/') {
            BinaryOp::Div
        } else if input.starts_with('%') {
            BinaryOp::Mod
        } else {
            return Ok(None);
        };

        *input = &input[1..];
        Ok(Some(op))
    })
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
    parse_binary_climbing(input, parse_multiplicative_expression, |input| {
        if input.starts_with("+=")
            || input.starts_with("-=")
            || input.starts_with("++")
            || input.starts_with("--")
        {
            return Ok(None);
        }

        let op = if input.starts_with('+') {
            BinaryOp::Add
        } else if input.starts_with('-') {
            BinaryOp::Sub
        } else {
            return Ok(None);
        };

        *input = &input[1..];
        Ok(Some(op))
    })
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
    parse_binary_climbing(input, parse_additive_expression, |input| {
        if input.starts_with("<<=") || input.starts_with(">>=") {
            return Ok(None);
        }

        let op = if input.starts_with("<<") {
            BinaryOp::Shl
        } else if input.starts_with(">>") {
            BinaryOp::Shr
        } else {
            return Ok(None);
        };

        *input = &input[2..];
        Ok(Some(op))
    })
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
    parse_binary_climbing(input, parse_shift_expression, |input| {
        if input.starts_with("<<") || input.starts_with(">>") {
            return Ok(None);
        }

        let (op, advance) = if input.starts_with("<=") {
            (BinaryOp::Lte, 2)
        } else if input.starts_with(">=") {
            (BinaryOp::Gte, 2)
        } else if input.starts_with('<') {
            (BinaryOp::Lt, 1)
        } else if input.starts_with('>') {
            (BinaryOp::Gt, 1)
        } else {
            return Ok(None);
        };

        *input = &input[advance..];
        Ok(Some(op))
    })
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
    parse_binary_climbing(input, parse_relational_expression, |input| {
        let op = if input.starts_with("==") {
            BinaryOp::Eq
        } else if input.starts_with("!=") {
            BinaryOp::Neq
        } else {
            return Ok(None);
        };

        *input = &input[2..];
        Ok(Some(op))
    })
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
    parse_binary_climbing(input, parse_equality_expression, |input| {
        if input.starts_with("&&") {
            *input = &input[2..];
            Ok(Some(BinaryOp::And))
        } else {
            Ok(None)
        }
    })
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
    parse_binary_climbing(input, parse_logical_and_expression, |input| {
        if input.starts_with("||") {
            *input = &input[2..];
            Ok(Some(BinaryOp::Or))
        } else {
            Ok(None)
        }
    })
}
