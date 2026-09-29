use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::digit1;
use winnow::combinator::alt;
use winnow::token::literal;

use super::primary::parse_expression;
use crate::ast::{Expression, FormatSpecifier, Span};
use crate::parser::utils::skip_ws_and_comments;

pub fn parse_int_literal(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;

    let remaining = *input;
    if remaining.starts_with("0b") || remaining.starts_with("0B") {
        *input = &remaining[2..];
        let mut bin_str = String::new();
        while !input.is_empty() {
            let c = input.chars().next().unwrap();
            if c == '0' || c == '1' {
                bin_str.push(c);
                *input = &input[1..];
            } else if c == '_' {
                *input = &input[1..];
            } else {
                break;
            }
        }
        if !bin_str.is_empty() {
            if let Ok(value) = i64::from_str_radix(&bin_str, 2) {
                return Ok(Expression::IntLiteral(
                    value,
                    Span::new(0, remaining.len() - input.len()),
                ));
            }
        }

        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    if remaining.starts_with("0x") || remaining.starts_with("0X") {
        *input = &remaining[2..];
        let mut hex_str = String::new();
        while !input.is_empty() {
            let c = input.chars().next().unwrap();
            if c.is_ascii_hexdigit() {
                hex_str.push(c);
                *input = &input[1..];
            } else if c == '_' {
                *input = &input[1..];
            } else {
                break;
            }
        }
        if !hex_str.is_empty() {
            if let Ok(value) = i64::from_str_radix(&hex_str, 16) {
                return Ok(Expression::IntLiteral(
                    value,
                    Span::new(0, remaining.len() - input.len()),
                ));
            }
        }

        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    let start_len = input.len();
    let mut dec_str = String::new();
    let mut count = 0;
    while !input.is_empty() {
        let c = input.chars().next().unwrap();
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
        if let Ok(val) = dec_str.parse::<i64>() {
            return Ok(Expression::IntLiteral(
                val,
                Span::new(0, start_len - input.len()),
            ));
        }
    }

    *input = checkpoint;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}

pub fn parse_float_literal(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    let float_res: ModalResult<&str> = (digit1, '.', digit1).take().parse_next(input);
    if let Ok(float_str) = float_res {
        if let Ok(val) = float_str.parse::<f64>() {
            return Ok(Expression::FloatLiteral(val, Span::new(0, float_str.len())));
        }
    }
    *input = checkpoint;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}

pub fn parse_bool_literal(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    let bool_res: ModalResult<bool> =
        alt((literal("tru").map(|_| true), literal("fal").map(|_| false))).parse_next(input);
    if let Ok(val) = bool_res {
        return Ok(Expression::BoolLiteral(val, Span::new(0, 3)));
    }
    *input = checkpoint;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}

pub fn parse_string_or_interpolated(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with('"') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    *input = &input[1..];

    let mut parts: Vec<(Expression, FormatSpecifier)> = Vec::new();
    let mut current_text = String::new();
    let mut escaped = false;

    while !input.is_empty() {
        let c = input.chars().next().unwrap();
        *input = &input[c.len_utf8()..];

        if escaped {
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
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else if c == '"' {
            break;
        } else if c == '{' {
            if !current_text.is_empty() {
                parts.push((
                    Expression::StringLiteral(current_text.clone(), Span::new(0, 0)),
                    FormatSpecifier::None,
                ));
                current_text.clear();
            }

            let mut expr_str = String::new();
            let mut in_brace_escaped = false;

            while !input.is_empty() {
                let inner_c = input.chars().next().unwrap();
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

            let trimmed = expr_str.trim();
            let (expr_part, specifier) = if trimmed.ends_with(":bin") {
                (&trimmed[..trimmed.len() - 4], FormatSpecifier::Bin)
            } else if trimmed.ends_with(":b") {
                (&trimmed[..trimmed.len() - 2], FormatSpecifier::Bin)
            } else if trimmed.ends_with(":hex") {
                (&trimmed[..trimmed.len() - 4], FormatSpecifier::Hex)
            } else if trimmed.ends_with(":x") {
                (&trimmed[..trimmed.len() - 2], FormatSpecifier::Hex)
            } else {
                (trimmed, FormatSpecifier::None)
            };

            let mut expr_slice = expr_part.trim();
            if let Ok(parsed_expr) = parse_expression.parse_next(&mut expr_slice) {
                parts.push((parsed_expr, specifier));
            }
        } else {
            current_text.push(c);
        }
    }

    if !current_text.is_empty() {
        parts.push((
            Expression::StringLiteral(current_text, Span::new(0, 0)),
            FormatSpecifier::None,
        ));
    }

    if parts.len() == 1 {
        if let (Expression::StringLiteral(s, span), FormatSpecifier::None) = &parts[0] {
            return Ok(Expression::StringLiteral(s.clone(), *span));
        }
    }

    Ok(Expression::InterpolatedString(parts, Span::new(0, 0)))
}
