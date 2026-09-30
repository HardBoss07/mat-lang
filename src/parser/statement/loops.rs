use winnow::ModalResult;
use winnow::Parser;
use winnow::combinator::alt;

use crate::ast::Statement;
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::statement::assignment::{
    parse_assignment_statement, parse_compound_assignment_statement,
};
use crate::parser::statement::declaration::parse_let_statement;
use crate::parser::statement::parse_block;
use crate::parser::utils::{get_span_between, keyword, skip_ws_and_comments, symbol};

fn parse_step_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = skip_ws_and_comments(input)?;

    let target = parse_identifier_str.parse_next(input)?.to_string();
    let _ = skip_ws_and_comments(input)?;

    if input.starts_with("++") {
        *input = &input[2..];
        let end_input = *input;
        Ok(Statement::Increment {
            target,
            span: get_span_between(checkpoint, end_input),
        })
    } else if input.starts_with("--") {
        *input = &input[2..];
        let end_input = *input;
        Ok(Statement::Decrement {
            target,
            span: get_span_between(checkpoint, end_input),
        })
    } else if input.starts_with("+=")
        || input.starts_with("-=")
        || input.starts_with("*=")
        || input.starts_with("/=")
        || input.starts_with("%=")
    {
        *input = checkpoint;
        parse_compound_assignment_statement(input)
    } else if input.starts_with('=') {
        *input = checkpoint;
        parse_assignment_statement(input)
    } else {
        *input = checkpoint;
        Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ))
    }
}

pub fn parse_loop_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("loop").parse_next(input)?;
    let body = parse_block(input)?;
    let end_input = *input;

    Ok(Statement::Loop {
        body,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_while_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("while").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let condition = parse_expression.parse_next(input)?;
    let body = parse_block(input)?;
    let end_input = *input;

    Ok(Statement::While {
        condition,
        body,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_fori_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("fori").parse_next(input)?;
    let _ = symbol("(").parse_next(input)?;

    let init = alt((
        parse_let_statement,
        parse_compound_assignment_statement,
        parse_assignment_statement,
    ))
    .parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let condition = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;

    let step = parse_step_statement(input)?;
    let _ = symbol(")").parse_next(input)?;

    let body = parse_block(input)?;
    let end_input = *input;

    Ok(Statement::ForI {
        init: Box::new(init),
        condition,
        step: Box::new(step),
        body,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_for_in_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with("for") || input.starts_with("fori") {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = keyword("for").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let var_name = parse_identifier_str.parse_next(input)?.to_string();
    let _ = keyword("in").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let iterable = parse_expression.parse_next(input)?;
    let body = parse_block(input)?;
    let end_input = *input;

    Ok(Statement::ForIn {
        var_name,
        iterable,
        body,
        span: get_span_between(start_input, end_input),
    })
}
