use winnow::ModalResult;
use winnow::Parser;

use crate::ast::{Span, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::utils::{keyword, skip_ws_and_comments, symbol};

pub fn parse_return_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = keyword("return").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    if input.starts_with(';') {
        let _ = symbol(";").parse_next(input)?;
        return Ok(Statement::Return(None, Span::new(0, 0)));
    }

    let expr = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;

    Ok(Statement::Return(Some(expr), Span::new(0, 0)))
}

pub fn parse_break_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = keyword("break").parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;

    Ok(Statement::Break(Span::new(0, 0)))
}

pub fn parse_continue_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = keyword("continue").parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;

    Ok(Statement::Continue(Span::new(0, 0)))
}

pub fn parse_expression_statement(input: &mut &str) -> ModalResult<Statement> {
    let expr = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;

    Ok(Statement::Expression(expr))
}
