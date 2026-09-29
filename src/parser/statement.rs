pub mod assignment;
pub mod control_flow;
pub mod declaration;
pub mod jumps;
pub mod loops;

pub use assignment::{
    parse_assignment_statement, parse_compound_assignment_statement, parse_decrement_statement,
    parse_increment_statement,
};
pub use control_flow::{parse_if_statement, parse_match_arm, parse_match_statement};
pub use declaration::parse_let_statement;
pub use jumps::{
    parse_break_statement, parse_continue_statement, parse_expression_statement,
    parse_return_statement,
};
pub use loops::{
    parse_for_in_statement, parse_fori_statement, parse_loop_statement, parse_while_statement,
};

use winnow::ModalResult;
use winnow::Parser;
use winnow::combinator::alt;

use crate::ast::Statement;
use crate::parser::utils::{skip_ws_and_comments, symbol};

pub fn parse_block(input: &mut &str) -> ModalResult<Vec<Statement>> {
    let _ = symbol("{").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let mut statements = Vec::new();
    while !input.starts_with('}') && !input.is_empty() {
        let stmt = parse_statement.parse_next(input)?;
        statements.push(stmt);
        let _ = skip_ws_and_comments(input)?;
    }

    let _ = symbol("}").parse_next(input)?;
    Ok(statements)
}

pub fn parse_statement(input: &mut &str) -> ModalResult<Statement> {
    alt((
        parse_let_statement,
        parse_return_statement,
        parse_if_statement,
        parse_match_statement,
        parse_loop_statement,
        parse_while_statement,
        parse_fori_statement,
        parse_for_in_statement,
        parse_break_statement,
        parse_continue_statement,
        parse_increment_statement,
        parse_decrement_statement,
        parse_compound_assignment_statement,
        parse_assignment_statement,
        parse_expression_statement,
    ))
    .parse_next(input)
}
