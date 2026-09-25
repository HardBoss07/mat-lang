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
    let checkpoint = *input;
    match parse_identifier_str.parse_next(input) {
        Ok(name) => Ok(Expression::Identifier(
            name.to_string(),
            Span::new(0, name.len()),
        )),
        Err(e) => {
            *input = checkpoint;
            Err(e)
        }
    }
}

pub fn parse_int_literal(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;
    let digits_res: ModalResult<&str> = digit1.parse_next(input);
    if let Ok(digits) = digits_res {
        if let Ok(val) = digits.parse::<i64>() {
            return Ok(Expression::IntLiteral(val, Span::new(0, digits.len())));
        }
    }
    *input = checkpoint;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}

pub fn parse_float_literal(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;
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
    let _ = multispace0.parse_next(input)?;
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
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with('"') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

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

pub fn parse_tuple_or_parenthesized(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;
    if !input.starts_with('(') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = multispace0.parse_next(input)?;

    if input.starts_with(')') {
        *input = &input[1..];
        return Ok(Expression::TupleLiteral(vec![], Span::new(0, 0)));
    }

    let first = match parse_expression.parse_next(input) {
        Ok(expr) => expr,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    if input.starts_with(',') {
        let mut elements = vec![first];
        while input.starts_with(',') {
            *input = &input[1..];
            let _ = multispace0.parse_next(input)?;
            if input.starts_with(')') {
                break;
            }
            match parse_expression.parse_next(input) {
                Ok(elem) => elements.push(elem),
                Err(e) => {
                    *input = checkpoint;
                    return Err(e);
                }
            }
            let _ = multispace0.parse_next(input)?;
        }
        if input.starts_with(')') {
            *input = &input[1..];
            Ok(Expression::TupleLiteral(elements, Span::new(0, 0)))
        } else {
            *input = checkpoint;
            Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ))
        }
    } else {
        if input.starts_with(')') {
            *input = &input[1..];
            Ok(first)
        } else {
            *input = checkpoint;
            Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ))
        }
    }
}

pub fn parse_array_literal(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;
    if !input.starts_with('[') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    let arr_res: ModalResult<Vec<Expression>> =
        delimited('[', separated(0.., parse_expression, ','), ']').parse_next(input);
    if let Ok(elements) = arr_res {
        return Ok(Expression::ArrayLiteral(elements, Span::new(0, 0)));
    }

    *input = checkpoint;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}

pub fn parse_primary_expression(input: &mut &str) -> ModalResult<Expression> {
    let _ = multispace0.parse_next(input)?;
    alt((
        parse_string_or_interpolated,
        parse_bool_literal,
        parse_float_literal,
        parse_int_literal,
        parse_tuple_or_parenthesized,
        parse_array_literal,
        parse_identifier,
    ))
    .parse_next(input)
}

pub fn parse_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut expr = parse_primary_expression.parse_next(input)?;

    loop {
        let _ = multispace0.parse_next(input)?;
        if input.starts_with('(') {
            let callee_name = match &expr {
                Expression::Identifier(name, _) => name.clone(),
                _ => break,
            };
            let args_res: ModalResult<Vec<Expression>> =
                delimited('(', separated(0.., parse_expression, ','), ')').parse_next(input);
            if let Ok(args) = args_res {
                expr = Expression::Call {
                    callee: callee_name,
                    arguments: args,
                    span: Span::new(0, 0),
                };
                continue;
            }
            break;
        } else if input.starts_with('.') {
            let mut checkpoint = *input;
            checkpoint = &checkpoint[1..];
            let idx_res: ModalResult<&str> = digit1.parse_next(&mut checkpoint);
            if let Ok(idx_str) = idx_res {
                *input = checkpoint;
                let index: usize = idx_str.parse().unwrap();
                expr = Expression::TupleAccess {
                    expr: Box::new(expr),
                    index,
                    span: Span::new(0, 0),
                };
                continue;
            } else {
                break;
            }
        } else if input.starts_with('[') {
            let mut checkpoint = *input;
            checkpoint = &checkpoint[1..];
            if let Ok(index) = parse_expression.parse_next(&mut checkpoint) {
                let _ = multispace0.parse_next(&mut checkpoint)?;
                if checkpoint.starts_with(']') {
                    checkpoint = &checkpoint[1..];
                    *input = checkpoint;
                    expr = Expression::ArrayAccess {
                        expr: Box::new(expr),
                        index: Box::new(index),
                        span: Span::new(0, 0),
                    };
                    continue;
                }
            }
            break;
        }
        break;
    }

    Ok(expr)
}
