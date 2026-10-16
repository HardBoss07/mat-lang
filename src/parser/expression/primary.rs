//! Primary expression parsing and operator precedence coordination.
//!
//! This module coordinates parsing of literals, constructors, identifiers,
//! and binary operator precedence climbing for expressions in MAT.

pub mod binary_ops;
pub mod collections;
pub mod constructors;
pub mod identifier;
pub mod postfix;

pub use binary_ops::*;
pub use collections::{parse_array_literal, parse_tuple_or_parenthesized, parse_vector_literal};
pub use constructors::*;
pub use identifier::*;
pub use postfix::parse_postfix_expression;

use winnow::ModalResult;
use winnow::Parser;
use winnow::combinator::alt;

use super::literals::{
    parse_bool_literal, parse_float_literal, parse_int_literal, parse_string_or_interpolated,
};
use crate::ast::Expression;
use crate::parser::utils::skip_ws_and_comments;

/// Parses a primary atomic expression (literals, identifiers, constructors, collections, parenthesized).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if no primary expression is matched.
pub fn parse_primary_expression(input: &mut &str) -> ModalResult<Expression> {
    let _ = skip_ws_and_comments(input)?;
    alt((
        parse_string_or_interpolated,
        parse_bool_literal,
        parse_float_literal,
        parse_int_literal,
        parse_ok_or_err_expression,
        parse_tuple_or_parenthesized,
        parse_array_literal,
        parse_struct_init,
        parse_vector_literal,
        parse_identifier,
    ))
    .parse_next(input)
}

/// Parses a full expression using operator precedence climbing starting at logical OR.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Expression`] root node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_expression(input: &mut &str) -> ModalResult<Expression> {
    parse_logical_or_expression(input)
}
