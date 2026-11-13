//! String interpolation and escape sequence parser.
//!
//! Parses double-quoted strings containing literal characters, escape codes (`\n`, `\t`, `\"`),
//! and expression holes (`{expr:format}`).

use winnow::ModalResult;
use winnow::Parser;

use super::super::primary::parse_expression;
use crate::ast::{Expression, FormatSpecifier};
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Parses a string format specifier suffix (e.g. `:bin`, `:b`, `:hex`, `:x`).
fn parse_format_specifier(trimmed: &str) -> (&str, FormatSpecifier) {
    if let Some(stripped) = trimmed.strip_suffix(":bin") {
        (stripped, FormatSpecifier::Bin)
    } else if let Some(stripped) = trimmed.strip_suffix(":b") {
        (stripped, FormatSpecifier::Bin)
    } else if let Some(stripped) = trimmed.strip_suffix(":hex") {
        (stripped, FormatSpecifier::Hex)
    } else if let Some(stripped) = trimmed.strip_suffix(":x") {
        (stripped, FormatSpecifier::Hex)
    } else {
        (trimmed, FormatSpecifier::None)
    }
}

/// Parses an escape sequence following a backslash character.
fn append_escaped_char(current_text: &mut String, c: char) {
    match c {
        'n' => current_text.push('\n'),
        't' => current_text.push('\t'),
        '\\' => current_text.push('\\'),
        '"' => current_text.push('"'),
        '\'' => current_text.push('\''),
        other => {
            current_text.push('\\');
            current_text.push(other);
        }
    }
}

/// Parses the inner expression and format specifier inside an interpolation hole `{...}`.
fn parse_interpolation_hole(input: &mut &str) -> Option<(Expression, FormatSpecifier)> {
    let mut expr_str = String::new();
    let mut in_brace_escaped = false;

    while let Some(inner_c) = input.chars().next() {
        *input = &input[inner_c.len_utf8()..];

        if in_brace_escaped {
            expr_str.push(inner_c);
            in_brace_escaped = false;
        } else if inner_c == '\\' {
            in_brace_escaped = true;
            expr_str.push(inner_c);
        } else if inner_c == '}' {
            break;
        } else {
            expr_str.push(inner_c);
        }
    }

    let (expr_part, specifier) = parse_format_specifier(expr_str.trim());
    let mut expr_slice = expr_part.trim();
    parse_expression
        .parse_next(&mut expr_slice)
        .ok()
        .map(|expr| (expr, specifier))
}

/// Parses an interpolated or regular string literal enclosed in double quotes.
///
/// # Arguments
///
/// * `input` - Input stream slice.
///
/// # Errors
///
/// Returns [`winnow::error::ErrMode::Backtrack`] if not a string literal.
pub fn parse_string_or_interpolated(input: &mut &str) -> ModalResult<Expression> {
    let start_input = *input;
    skip_ws_and_comments(input)?;
    let tok_start = *input;

    if !input.starts_with('"') {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    *input = &input[1..];

    let mut parts: Vec<(Expression, FormatSpecifier)> = Vec::new();
    let mut current_text = String::new();
    let mut escaped = false;

    while let Some(c) = input.chars().next() {
        *input = &input[c.len_utf8()..];

        if escaped {
            append_escaped_char(&mut current_text, c);
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else if c == '"' {
            break;
        } else if c == '{' {
            if !current_text.is_empty() {
                parts.push((
                    Expression::StringLiteral(
                        current_text.clone(),
                        get_span_between(tok_start, input),
                    ),
                    FormatSpecifier::None,
                ));
                current_text.clear();
            }

            if let Some(hole) = parse_interpolation_hole(input) {
                parts.push(hole);
            }
        } else {
            current_text.push(c);
        }
    }

    let tok_end = *input;

    if !current_text.is_empty() {
        parts.push((
            Expression::StringLiteral(current_text, get_span_between(tok_start, tok_end)),
            FormatSpecifier::None,
        ));
    }

    let full_span = get_span_between(tok_start, tok_end);

    if parts.len() == 1
        && let (Expression::StringLiteral(s, _), FormatSpecifier::None) = &parts[0]
    {
        return Ok(Expression::StringLiteral(s.clone(), full_span));
    }

    Ok(Expression::InterpolatedString(parts, full_span))
}
