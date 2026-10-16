//! Identifier parsing utilities for expressions and syntax tokens.
//!
//! This module provides parsers for single identifiers and qualified path identifiers
//! separated by `::`.

use winnow::ModalResult;
use winnow::Parser;
use winnow::token::take_while;

use crate::ast::Expression;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Parses a single lexical identifier string (alphanumeric or underscore, starting with alpha/underscore).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The slice corresponding to the parsed identifier.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if no valid identifier begins at the current position.
pub fn parse_single_identifier_str<'a>(input: &mut &'a str) -> ModalResult<&'a str> {
    let _ = skip_ws_and_comments(input)?;
    let checkpoint = *input;

    if input.is_empty() {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    let first_char = input.chars().next().unwrap();
    if !first_char.is_alphabetic() && first_char != '_' {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    take_while(1.., |c: char| c.is_alphanumeric() || c == '_').parse_next(input)
}

/// Parses an identifier string possibly qualified by double colons (`::`).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The slice containing the entire qualified path string.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if no valid initial identifier is found.
pub fn parse_identifier_str<'a>(input: &mut &'a str) -> ModalResult<&'a str> {
    let _ = skip_ws_and_comments(input)?;
    let start = *input;
    let _ = parse_single_identifier_str(input)?;

    while input.starts_with("::") {
        let checkpoint = *input;
        *input = &input[2..];
        if parse_single_identifier_str(input).is_err() {
            *input = checkpoint;
            break;
        }
    }

    let len = start.len() - input.len();
    Ok(&start[..len])
}

/// Parses an identifier expression node.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression::Identifier`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if identifier parsing fails.
pub fn parse_identifier(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;
    match parse_identifier_str.parse_next(input) {
        Ok(name) => Ok(Expression::Identifier(
            name.to_string(),
            get_span_between(tok_start, *input),
        )),
        Err(err) => {
            *input = checkpoint;
            Err(err)
        }
    }
}
