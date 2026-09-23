use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace0;
use winnow::token::literal;

use crate::ast::Statement;
use crate::parser::expression::parse_expression;

pub fn parse_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = multispace0.parse_next(input)?;
    let expr = parse_expression.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Expression(expr))
}
