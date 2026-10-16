//! Constructor and complex primary expression parsing.
//!
//! This module provides parsers for `Ok(...)` / `Err(...)` result constructors
//! and struct initialization expressions.

use winnow::ModalResult;
use winnow::Parser;
use winnow::token::literal;

use super::identifier::{parse_identifier_str, parse_single_identifier_str};
use crate::ast::Expression;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Parses an `Ok(...)` or `Err(...)` expression constructor.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression::Ok`] or [`Expression::Err`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed or not matched.
pub fn parse_ok_or_err_expression(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;

    let is_ok = match check_result_constructor_tag(input, checkpoint)? {
        Some(ok_flag) => ok_flag,
        None => {
            *input = checkpoint;
            return Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ));
        }
    };

    if is_ok {
        let _ = literal("Ok").parse_next(input)?;
    } else {
        let _ = literal("Err").parse_next(input)?;
    }

    let inner_expr = parse_enclosed_result_argument(input, checkpoint)?;
    let tok_end = *input;

    if is_ok {
        Ok(Expression::Ok(
            Box::new(inner_expr),
            get_span_between(tok_start, tok_end),
        ))
    } else {
        Ok(Expression::Err(
            Box::new(inner_expr),
            get_span_between(tok_start, tok_end),
        ))
    }
}

/// Checks whether the prefix matches `Ok` or `Err` with valid identifier boundaries.
fn check_result_constructor_tag(input: &str, _checkpoint: &str) -> ModalResult<Option<bool>> {
    if input.starts_with("Ok") {
        let after = &input[2..];
        if after.chars().next().map_or(false, |character| {
            character.is_alphanumeric() || character == '_'
        }) {
            return Ok(None);
        }
        Ok(Some(true))
    } else if input.starts_with("Err") {
        let after = &input[3..];
        if after.chars().next().map_or(false, |character| {
            character.is_alphanumeric() || character == '_'
        }) {
            return Ok(None);
        }
        Ok(Some(false))
    } else {
        Ok(None)
    }
}

/// Parses the parenthesized inner argument of `Ok(...)` or `Err(...)`.
fn parse_enclosed_result_argument<'a>(
    input: &mut &'a str,
    checkpoint: &'a str,
) -> ModalResult<Expression> {
    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with('(') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let inner_expr = match crate::parser::expression::parse_expression.parse_next(input) {
        Ok(expr) => expr,
        Err(err) => {
            *input = checkpoint;
            return Err(err);
        }
    };
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with(')') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    Ok(inner_expr)
}

/// Parses a struct initialization expression (e.g. `Point { x: 1, y: 2 }`).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression::StructInit`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed or not a struct init.
pub fn parse_struct_init(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;

    let struct_name = match parse_identifier_str.parse_next(input) {
        Ok(name) => name.to_string(),
        Err(err) => {
            *input = checkpoint;
            return Err(err);
        }
    };

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with('{') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let fields = parse_struct_init_fields(input, checkpoint)?;
    let tok_end = *input;

    Ok(Expression::StructInit {
        name: struct_name,
        fields,
        span: get_span_between(tok_start, tok_end),
    })
}

/// Parses comma-separated field bindings inside struct initialization braces.
fn parse_struct_init_fields<'a>(
    input: &mut &'a str,
    checkpoint: &'a str,
) -> ModalResult<Vec<(String, Expression)>> {
    let mut fields = Vec::new();
    while !input.is_empty() && !input.starts_with('}') {
        let field_name = match parse_single_identifier_str.parse_next(input) {
            Ok(name) => name.to_string(),
            Err(err) => {
                *input = checkpoint;
                return Err(err);
            }
        };

        let _ = skip_ws_and_comments(input)?;
        if !input.starts_with(':') {
            *input = checkpoint;
            return Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ));
        }
        *input = &input[1..];
        let _ = skip_ws_and_comments(input)?;

        let val_expr = match crate::parser::expression::parse_expression.parse_next(input) {
            Ok(expr) => expr,
            Err(err) => {
                *input = checkpoint;
                return Err(err);
            }
        };

        fields.push((field_name, val_expr));

        let _ = skip_ws_and_comments(input)?;
        if input.starts_with(',') {
            *input = &input[1..];
            let _ = skip_ws_and_comments(input)?;
        } else {
            break;
        }
    }

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with('}') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    Ok(fields)
}
