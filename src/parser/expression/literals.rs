//! Literal expression parsing.
//!
//! Parses numeric literals (integers with `0b`/`0x` support, floats), boolean constants (`tru`, `fal`),
//! and delegates string literal and interpolation parsing to the [`string_interpolation`] submodule.

pub mod string_interpolation;

#[allow(unused_imports)]
pub use string_interpolation::*;

use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::digit1;
use winnow::combinator::alt;
use winnow::token::literal;

use crate::ast::Expression;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Checks whether the input begins with a negative sign prefix and advances the input if so.
fn check_and_consume_negative_sign(input: &mut &str) -> bool {
    if input.starts_with('-') && !input.starts_with("--") && !input.starts_with("-=") {
        let rest = &input[1..];
        if rest.chars().next().map_or(false, |c| c.is_ascii_digit()) {
            *input = rest;
            return true;
        }
    }
    false
}

/// Parses an explicit radix integer literal (`0b...` or `0x...`).
fn parse_radix_int(
    input: &mut &str,
    prefix: &str,
    radix: u32,
    is_digit: fn(char) -> bool,
) -> Option<i64> {
    if input.len() < prefix.len() || !input[..prefix.len()].eq_ignore_ascii_case(prefix) {
        return None;
    }
    let mut digits = String::new();
    let mut rest = &input[prefix.len()..];
    while let Some(c) = rest.chars().next() {
        if is_digit(c) {
            digits.push(c);
            rest = &rest[c.len_utf8()..];
        } else if c == '_' {
            rest = &rest[1..];
        } else {
            break;
        }
    }
    if digits.is_empty() {
        return None;
    }
    let value = i64::from_str_radix(&digits, radix).ok()?;
    *input = rest;
    Some(value)
}

/// Parses an integer literal in decimal, hexadecimal (`0x`), or binary (`0b`) format.
///
/// # Arguments
///
/// * `input` - Input stream slice.
///
/// # Errors
///
/// Returns [`winnow::error::ErrMode::Backtrack`] if not an integer literal.
pub fn parse_int_literal(input: &mut &str) -> ModalResult<Expression> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;

    let is_neg = check_and_consume_negative_sign(input);

    if let Some(val) = parse_radix_int(input, "0b", 2, |c| c == '0' || c == '1') {
        let final_val = if is_neg { -val } else { val };
        return Ok(Expression::IntLiteral(
            final_val,
            get_span_between(tok_start, *input),
        ));
    }

    if let Some(val) = parse_radix_int(input, "0x", 16, |c| c.is_ascii_hexdigit()) {
        let final_val = if is_neg { -val } else { val };
        return Ok(Expression::IntLiteral(
            final_val,
            get_span_between(tok_start, *input),
        ));
    }

    let mut dec_str = String::new();
    let mut count = 0;
    while let Some(c) = input.chars().next() {
        if c.is_ascii_digit() {
            dec_str.push(c);
            *input = &input[1..];
            count += 1;
        } else if c == '_' && count > 0 {
            *input = &input[1..];
        } else {
            break;
        }
    }

    if !dec_str.is_empty() {
        if let Ok(mut val) = dec_str.parse::<i64>() {
            if is_neg {
                val = -val;
            }
            return Ok(Expression::IntLiteral(
                val,
                get_span_between(tok_start, *input),
            ));
        }
    }

    *input = start_input;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}

/// Parses a 64-bit floating-point literal (e.g. `3.14159`).
///
/// # Arguments
///
/// * `input` - Input stream slice.
///
/// # Errors
///
/// Returns [`winnow::error::ErrMode::Backtrack`] if not a float literal.
pub fn parse_float_literal(input: &mut &str) -> ModalResult<Expression> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;

    let is_neg = check_and_consume_negative_sign(input);

    let float_res: ModalResult<&str> = (digit1, '.', digit1).take().parse_next(input);
    if let Ok(float_str) = float_res {
        if let Ok(mut val) = float_str.parse::<f64>() {
            if is_neg {
                val = -val;
            }
            return Ok(Expression::FloatLiteral(
                val,
                get_span_between(tok_start, *input),
            ));
        }
    }
    *input = start_input;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}

/// Parses a boolean literal (`tru` or `fal`).
///
/// # Arguments
///
/// * `input` - Input stream slice.
///
/// # Errors
///
/// Returns [`winnow::error::ErrMode::Backtrack`] if not a bool literal.
pub fn parse_bool_literal(input: &mut &str) -> ModalResult<Expression> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;
    let bool_res: ModalResult<bool> =
        alt((literal("tru").map(|_| true), literal("fal").map(|_| false))).parse_next(input);
    if let Ok(val) = bool_res {
        return Ok(Expression::BoolLiteral(
            val,
            get_span_between(tok_start, *input),
        ));
    }
    *input = start_input;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}
