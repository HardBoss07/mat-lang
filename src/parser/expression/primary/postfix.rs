use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::digit1;
use winnow::combinator::{delimited, separated};

use super::super::parse_expression;
use super::parse_primary_expression;
use crate::ast::Expression;
use crate::parser::utils::{get_span_between_ptrs, skip_ws_and_comments};

pub fn parse_postfix_expression(input: &mut &str) -> ModalResult<Expression> {
    let mut expr = parse_primary_expression.parse_next(input)?;

    loop {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with('(') {
            let callee_name = match &expr {
                Expression::Identifier(name, _) => name.clone(),
                _ => break,
            };
            let start_offset = expr.span().start;
            let args_res: ModalResult<Vec<Expression>> =
                delimited('(', separated(0.., parse_expression, ','), ')').parse_next(input);
            if let Ok(args) = args_res {
                expr = Expression::Call {
                    callee: callee_name,
                    arguments: args,
                    span: get_span_between_ptrs(start_offset, *input),
                };
                continue;
            }
            break;
        } else if input.starts_with('.') {
            let start_offset = expr.span().start;
            let mut checkpoint = *input;
            checkpoint = &checkpoint[1..];
            let idx_res: ModalResult<&str> = digit1.parse_next(&mut checkpoint);
            if let Ok(idx_str) = idx_res {
                *input = checkpoint;
                let index: usize = idx_str.parse().unwrap();
                expr = Expression::TupleAccess {
                    expr: Box::new(expr),
                    index,
                    span: get_span_between_ptrs(start_offset, *input),
                };
                continue;
            }
            break;
        } else if input.starts_with('[') {
            let start_offset = expr.span().start;
            let mut checkpoint = *input;
            checkpoint = &checkpoint[1..];
            if let Ok(index) = parse_expression.parse_next(&mut checkpoint) {
                let _ = skip_ws_and_comments(&mut checkpoint)?;
                if checkpoint.starts_with(']') {
                    checkpoint = &checkpoint[1..];
                    *input = checkpoint;
                    expr = Expression::ArrayAccess {
                        expr: Box::new(expr),
                        index: Box::new(index),
                        span: get_span_between_ptrs(start_offset, *input),
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
