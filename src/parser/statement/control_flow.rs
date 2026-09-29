use winnow::ModalResult;
use winnow::Parser;
use winnow::token::literal;

use crate::ast::{MatchArm, MatchPattern, Span, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::statement::parse_block;
use crate::parser::statement::parse_statement;
use crate::parser::utils::{keyword, skip_ws_and_comments, symbol};

pub fn parse_match_arm(input: &mut &str) -> ModalResult<MatchArm> {
    let _ = skip_ws_and_comments(input)?;

    let pattern = if input.starts_with("Ok") {
        let _ = literal("Ok").parse_next(input)?;
        let _ = symbol("(").parse_next(input)?;
        let var_name = parse_identifier_str.parse_next(input)?.to_string();
        let _ = symbol(")").parse_next(input)?;
        MatchPattern::Ok(var_name)
    } else if input.starts_with("Err") {
        let _ = literal("Err").parse_next(input)?;
        let _ = symbol("(").parse_next(input)?;
        let var_name = parse_identifier_str.parse_next(input)?.to_string();
        let _ = symbol(")").parse_next(input)?;
        MatchPattern::Err(var_name)
    } else if input.starts_with('_') {
        *input = &input[1..];
        MatchPattern::Wildcard
    } else {
        let expr = parse_expression.parse_next(input)?;
        MatchPattern::Literal(expr)
    };

    let _ = symbol("=>").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let body = if input.starts_with('{') {
        parse_block(input)?
    } else {
        let stmt = parse_statement.parse_next(input)?;
        vec![stmt]
    };

    Ok(MatchArm { pattern, body })
}

pub fn parse_match_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = keyword("match").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let expr = parse_expression.parse_next(input)?;
    let _ = symbol("{").parse_next(input)?;

    let mut arms = Vec::new();
    let _ = skip_ws_and_comments(input)?;
    while !input.starts_with('}') && !input.is_empty() {
        let arm = parse_match_arm.parse_next(input)?;
        arms.push(arm);
        let _ = skip_ws_and_comments(input)?;
    }

    let _ = symbol("}").parse_next(input)?;

    Ok(Statement::Match {
        expr,
        arms,
        span: Span::new(0, 0),
    })
}

pub fn parse_if_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = keyword("if").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let condition = parse_expression.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let then_branch = parse_block(input)?;
    let _ = skip_ws_and_comments(input)?;

    let else_branch = if input.starts_with("else") {
        let after_else = &input[4..];
        if !after_else
            .chars()
            .next()
            .map_or(false, |c| c.is_alphanumeric() || c == '_')
        {
            let _ = literal("else").parse_next(input)?;
            let _ = skip_ws_and_comments(input)?;
            if input.starts_with("if") {
                let stmt = parse_if_statement(input)?;
                Some(vec![stmt])
            } else {
                let b = parse_block(input)?;
                Some(b)
            }
        } else {
            None
        }
    } else {
        None
    };

    Ok(Statement::If {
        condition,
        then_branch,
        else_branch,
        span: Span::new(0, 0),
    })
}
