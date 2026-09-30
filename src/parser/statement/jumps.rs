use winnow::ModalResult;
use winnow::Parser;

use crate::ast::Statement;
use crate::parser::expression::parse_expression;
use crate::parser::utils::{get_span_between, keyword, skip_ws_and_comments, symbol};

pub fn parse_return_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("return").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    if input.starts_with(';') {
        let _ = symbol(";").parse_next(input)?;
        let end_input = *input;
        return Ok(Statement::Return(
            None,
            get_span_between(start_input, end_input),
        ));
    }

    let expr = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::Return(
        Some(expr),
        get_span_between(start_input, end_input),
    ))
}

pub fn parse_break_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("break").parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::Break(get_span_between(start_input, end_input)))
}

pub fn parse_continue_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("continue").parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::Continue(get_span_between(
        start_input,
        end_input,
    )))
}

pub fn parse_expression_statement(input: &mut &str) -> ModalResult<Statement> {
    let expr = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;

    Ok(Statement::Expression(expr))
}
