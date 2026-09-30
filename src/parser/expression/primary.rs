pub mod collections;
pub mod postfix;

pub use collections::{parse_array_literal, parse_tuple_or_parenthesized};
pub use postfix::parse_postfix_expression;

use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::alpha1;
use winnow::combinator::alt;
use winnow::token::{literal, take_while};

use super::literals::{
    parse_bool_literal, parse_float_literal, parse_int_literal, parse_string_or_interpolated,
};
use crate::ast::{BinaryOp, Expression, Span};
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

pub fn parse_identifier_str<'a>(input: &mut &'a str) -> ModalResult<&'a str> {
    let _ = skip_ws_and_comments(input)?;
    (
        alpha1,
        take_while(0.., |c: char| c.is_alphanumeric() || c == '_'),
    )
        .take()
        .parse_next(input)
}

pub fn parse_identifier(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;
    match parse_identifier_str.parse_next(input) {
        Ok(name) => Ok(Expression::Identifier(
            name.to_string(),
            get_span_between(tok_start, *input),
        )),
        Err(e) => {
            *input = checkpoint;
            Err(e)
        }
    }
}

pub fn parse_ok_or_err_expression(input: &mut &str) -> ModalResult<Expression> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;
    let tok_start = *input;

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

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with('(') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let inner_expr = match parse_expression.parse_next(input) {
        Ok(e) => e,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
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
        parse_identifier,
    ))
    .parse_next(input)
}

pub fn parse_multiplicative_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_postfix_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;
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
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

pub fn parse_additive_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_multiplicative_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;
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
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

pub fn parse_shift_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_additive_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;
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
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

pub fn parse_relational_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_shift_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;
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
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

pub fn parse_equality_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_relational_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;

        let op = if input.starts_with("==") {
            BinaryOp::Eq
        } else if input.starts_with("!=") {
            BinaryOp::Neq
        } else {
            break;
        };

        *input = &input[2..];

        let right = parse_relational_expression.parse_next(input)?;
        let start = left.span().start;
        let end = right.span().end;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: Span::new(start, end),
        };
    }

    Ok(left)
}

pub fn parse_logical_and_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut left = parse_equality_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;

        if input.starts_with("&&") {
            *input = &input[2..];
            let right = parse_equality_expression.parse_next(input)?;
            let start = left.span().start;
            let end = right.span().end;
            left = Expression::Binary {
                op: BinaryOp::And,
                left: Box::new(left),
                right: Box::new(right),
                span: Span::new(start, end),
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
        let _ = skip_ws_and_comments(input)?;

        if input.starts_with("||") {
            *input = &input[2..];
            let right = parse_logical_and_expression.parse_next(input)?;
            let start = left.span().start;
            let end = right.span().end;
            left = Expression::Binary {
                op: BinaryOp::Or,
                left: Box::new(left),
                right: Box::new(right),
                span: Span::new(start, end),
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
