use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::{alpha1, digit1, multispace0};
use winnow::combinator::{alt, delimited, separated};
use winnow::token::{literal, take_while};

use super::literals::{
    parse_bool_literal, parse_float_literal, parse_int_literal, parse_string_or_interpolated,
};
use crate::ast::{BinaryOp, Expression, Span};

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
    } else if input.starts_with(')') {
        *input = &input[1..];
        Ok(first)
    } else {
        *input = checkpoint;
        Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ))
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

pub fn parse_ok_or_err_expression(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    let is_ok = if input.starts_with("Ok") {
        let after = &input[2..];
        if after
            .chars()
            .next()
            .map_or(false, |c| c.is_alphanumeric() || c == '_')
        {
            *input = checkpoint;
            return Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ));
        }
        true
    } else if input.starts_with("Err") {
        let after = &input[3..];
        if after
            .chars()
            .next()
            .map_or(false, |c| c.is_alphanumeric() || c == '_')
        {
            *input = checkpoint;
            return Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ));
        }
        false
    } else {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    };

    if is_ok {
        let _ = literal("Ok").parse_next(input)?;
    } else {
        let _ = literal("Err").parse_next(input)?;
    }

    let _ = multispace0.parse_next(input)?;
    if !input.starts_with('(') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = multispace0.parse_next(input)?;

    let inner_expr = match parse_expression.parse_next(input) {
        Ok(e) => e,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(')') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];

    if is_ok {
        Ok(Expression::Ok(Box::new(inner_expr), Span::new(0, 0)))
    } else {
        Ok(Expression::Err(Box::new(inner_expr), Span::new(0, 0)))
    }
}

pub fn parse_primary_expression(input: &mut &str) -> ModalResult<Expression> {
    let _ = multispace0.parse_next(input)?;
    alt((
        parse_string_or_interpolated,
        parse_bool_literal,
        parse_float_literal,
        parse_int_literal,
        parse_ok_or_err_expression,
        parse_tuple_or_parenthesized,
        parse_array_literal,
        parse_identifier,
    ))
    .parse_next(input)
}

pub fn parse_postfix_expression(input: &mut &str) -> ModalResult<Expression> {
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
            }
            break;
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

pub fn parse_multiplicative_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_postfix_expression.parse_next(input)?;

    loop {
        let _ = multispace0.parse_next(input)?;
        if input.starts_with("*=") || input.starts_with("/=") || input.starts_with("%=") {
            break;
        }

        let op = if input.starts_with('*') {
            BinaryOp::Mul
        } else if input.starts_with('/') {
            BinaryOp::Div
        } else if input.starts_with('%') {
            BinaryOp::Mod
        } else {
            break;
        };

        *input = &input[1..];

        let right = parse_postfix_expression.parse_next(input)?;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(0, 0),
        };
    }

    Ok(left)
}

pub fn parse_additive_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_multiplicative_expression.parse_next(input)?;

    loop {
        let _ = multispace0.parse_next(input)?;
        if input.starts_with("+=")
            || input.starts_with("-=")
            || input.starts_with("++")
            || input.starts_with("--")
        {
            break;
        }

        let op = if input.starts_with('+') {
            BinaryOp::Add
        } else if input.starts_with('-') {
            BinaryOp::Sub
        } else {
            break;
        };

        *input = &input[1..];

        let right = parse_multiplicative_expression.parse_next(input)?;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(0, 0),
        };
    }

    Ok(left)
}

pub fn parse_shift_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_additive_expression.parse_next(input)?;

    loop {
        let _ = multispace0.parse_next(input)?;
        if input.starts_with("<<=") || input.starts_with(">>=") {
            break;
        }

        let op = if input.starts_with("<<") {
            BinaryOp::Shl
        } else if input.starts_with(">>") {
            BinaryOp::Shr
        } else {
            break;
        };

        *input = &input[2..];

        let right = parse_additive_expression.parse_next(input)?;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(0, 0),
        };
    }

    Ok(left)
}

pub fn parse_relational_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_shift_expression.parse_next(input)?;

    loop {
        let _ = multispace0.parse_next(input)?;
        if input.starts_with("<<") || input.starts_with(">>") {
            break;
        }

        let op = if input.starts_with("<=") {
            BinaryOp::Lte
        } else if input.starts_with(">=") {
            BinaryOp::Gte
        } else if input.starts_with('<') {
            BinaryOp::Lt
        } else if input.starts_with('>') {
            BinaryOp::Gt
        } else {
            break;
        };

        match op {
            BinaryOp::Lte | BinaryOp::Gte => *input = &input[2..],
            _ => *input = &input[1..],
        }

        let right = parse_shift_expression.parse_next(input)?;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(0, 0),
        };
    }

    Ok(left)
}

pub fn parse_equality_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_relational_expression.parse_next(input)?;

    loop {
        let _ = multispace0.parse_next(input)?;

        let op = if input.starts_with("==") {
            BinaryOp::Eq
        } else if input.starts_with("!=") {
            BinaryOp::Neq
        } else {
            break;
        };

        *input = &input[2..];

        let right = parse_relational_expression.parse_next(input)?;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(0, 0),
        };
    }

    Ok(left)
}

pub fn parse_logical_and_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_equality_expression.parse_next(input)?;

    loop {
        let _ = multispace0.parse_next(input)?;

        if input.starts_with("&&") {
            *input = &input[2..];
            let right = parse_equality_expression.parse_next(input)?;
            left = Expression::Binary {
                op: BinaryOp::And,
                left: Box::new(left),
                right: Box::new(right),
                span: Span::new(0, 0),
            };
        } else {
            break;
        }
    }

    Ok(left)
}

pub fn parse_logical_or_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_logical_and_expression.parse_next(input)?;

    loop {
        let _ = multispace0.parse_next(input)?;

        if input.starts_with("||") {
            *input = &input[2..];
            let right = parse_logical_and_expression.parse_next(input)?;
            left = Expression::Binary {
                op: BinaryOp::Or,
                left: Box::new(left),
                right: Box::new(right),
                span: Span::new(0, 0),
            };
        } else {
            break;
        }
    }

    Ok(left)
}

pub fn parse_expression(input: &mut &str) -> ModalResult<Expression> {
    parse_logical_or_expression(input)
}
