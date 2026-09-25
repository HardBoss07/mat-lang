use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::{alpha1, digit1, multispace0};
use winnow::combinator::{alt, delimited, separated};
use winnow::token::{literal, take_while};

use crate::ast::{Expression, Span};

pub fn parse_identifier_str<'a>(input: &mut &'a str) -> ModalResult<&'a str> {
    let _ = multispace0.parse_next(input)?;
    (
        alpha1,
        take_while(0.., |c: char| c.is_alphanumeric() || c == '_'),
    )
        .take()
        .parse_next(input)
}

pub fn parse_identifier(input: &mut &str) -> ModalResult<Expression> {
    let name = parse_identifier_str.parse_next(input)?;
    Ok(Expression::Identifier(
        name.to_string(),
        Span::new(0, name.len()),
    ))
}

pub fn parse_int_literal(input: &mut &str) -> ModalResult<Expression> {
    let _ = multispace0.parse_next(input)?;
    let digits = digit1.parse_next(input)?;
    let val = digits.parse::<i64>().unwrap();
    Ok(Expression::IntLiteral(val, Span::new(0, digits.len())))
}

pub fn parse_float_literal(input: &mut &str) -> ModalResult<Expression> {
    let _ = multispace0.parse_next(input)?;
    let float_str: &str = (digit1, '.', digit1).take().parse_next(input)?;
    let val: f64 = float_str.parse().unwrap();
    Ok(Expression::FloatLiteral(val, Span::new(0, float_str.len())))
}

pub fn parse_bool_literal(input: &mut &str) -> ModalResult<Expression> {
    let _ = multispace0.parse_next(input)?;
    let val =
        alt((literal("tru").map(|_| true), literal("fal").map(|_| false))).parse_next(input)?;

    Ok(Expression::BoolLiteral(val, Span::new(0, 3)))
}

pub fn parse_string_or_interpolated(input: &mut &str) -> ModalResult<Expression> {
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with('"') {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    // Advance past opening quote
    *input = &input[1..];

    let mut parts = Vec::new();
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
                parts.push(Expression::StringLiteral(
                    current_text.clone(),
                    Span::new(0, 0),
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

            let mut expr_slice = expr_str.trim();
            if let Ok(parsed_expr) = parse_expression.parse_next(&mut expr_slice) {
                parts.push(parsed_expr);
            }
        } else {
            current_text.push(c);
        }
    }

    if !current_text.is_empty() {
        parts.push(Expression::StringLiteral(current_text, Span::new(0, 0)));
    }

    if parts.len() == 1 {
        if let Some(Expression::StringLiteral(s, span)) = parts.pop() {
            return Ok(Expression::StringLiteral(s, span));
        }
    }

    Ok(Expression::InterpolatedString(parts, Span::new(0, 0)))
}

pub fn parse_string_literal(input: &mut &str) -> ModalResult<Expression> {
    parse_string_or_interpolated(input)
}

pub fn parse_call_expression(input: &mut &str) -> ModalResult<Expression> {
    let callee = parse_identifier_str.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let args: Vec<Expression> =
        delimited('(', separated(0.., parse_expression, ','), ')').parse_next(input)?;

    Ok(Expression::Call {
        callee: callee.to_string(),
        arguments: args,
        span: Span::new(0, 0),
    })
}

pub fn parse_expression(input: &mut &str) -> ModalResult<Expression> {
    let _ = multispace0.parse_next(input)?;
    alt((
        parse_call_expression,
        parse_string_or_interpolated,
        parse_bool_literal,
        parse_float_literal,
        parse_int_literal,
        parse_identifier,
    ))
    .parse_next(input)
}
