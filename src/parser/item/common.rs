//! Shared parsing utilities for items and item components.
//!
//! This module provides common helper routines such as visibility keyword parsing.

use winnow::ModalResult;
use winnow::Parser;
use winnow::token::literal;

use crate::parser::utils::skip_ws_and_comments;

/// Parses an optional `pub` keyword at the beginning of an item or field.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// `Ok(true)` if a valid `pub` modifier was present, `Ok(false)` otherwise.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if parsing fails unexpectedly.
pub fn parse_optional_pub(input: &mut &str) -> ModalResult<bool> {
    if !input.starts_with("pub") {
        return Ok(false);
    }
    let after_pub = &input[3..];
    let is_valid_pub_boundary = after_pub.chars().next().map_or(true, |character| {
        character.is_whitespace() || character == '/' || character == '{'
    });

    if is_valid_pub_boundary {
        let _ = literal("pub").parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        Ok(true)
    } else {
        Ok(false)
    }
}
