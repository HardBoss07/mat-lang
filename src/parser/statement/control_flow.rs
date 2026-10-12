use winnow::ModalResult;
use winnow::Parser;

use crate::ast::{MatchArm, MatchPattern, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::statement::parse_block;
use crate::parser::utils::{get_span_between, keyword, skip_ws_and_comments, symbol};

pub fn parse_match_pattern(input: &mut &str) -> ModalResult<MatchPattern> {
    let _ = skip_ws_and_comments(input)?;

    if symbol("_").parse_next(input).is_ok() {
        return Ok(MatchPattern::Wildcard);
    }

    let checkpoint = *input;

    if keyword("Ok").parse_next(input).is_ok() {
        let _ = skip_ws_and_comments(input)?;
        if symbol("(").parse_next(input).is_ok() {
            let _ = skip_ws_and_comments(input)?;
            let var = parse_identifier_str.parse_next(input)?.to_string();
            let _ = skip_ws_and_comments(input)?;
            let _ = symbol(")").parse_next(input)?;
            return Ok(MatchPattern::Ok(var));
        }
    }
    *input = checkpoint;

    if keyword("Err").parse_next(input).is_ok() {
        let _ = skip_ws_and_comments(input)?;
        if symbol("(").parse_next(input).is_ok() {
            let _ = skip_ws_and_comments(input)?;
            let var = parse_identifier_str.parse_next(input)?.to_string();
            let _ = skip_ws_and_comments(input)?;
            let _ = symbol(")").parse_next(input)?;
            return Ok(MatchPattern::Err(var));
        }
    }
    *input = checkpoint;

    if let Ok(first_id) = parse_identifier_str.parse_next(input) {
        let _ = skip_ws_and_comments(input)?;
        let (enum_name, variant) = if symbol("::").parse_next(input).is_ok() {
            let _ = skip_ws_and_comments(input)?;
            let var_id = parse_identifier_str.parse_next(input)?;
            (Some(first_id.to_string()), var_id.to_string())
        } else {
            (None, first_id.to_string())
        };

        let _ = skip_ws_and_comments(input)?;

        if symbol("(").parse_next(input).is_ok() {
            let mut vars = Vec::new();
            let _ = skip_ws_and_comments(input)?;
            if !input.starts_with(')') {
                loop {
                    let v = parse_identifier_str.parse_next(input)?.to_string();
                    vars.push(v);
                    let _ = skip_ws_and_comments(input)?;
                    if symbol(",").parse_next(input).is_err() {
                        break;
                    }
                    let _ = skip_ws_and_comments(input)?;
                }
            }
            let _ = symbol(")").parse_next(input)?;
            return Ok(MatchPattern::EnumTuple {
                enum_name,
                variant,
                vars,
            });
        }

        if symbol("{").parse_next(input).is_ok() {
            let mut fields = Vec::new();
            let _ = skip_ws_and_comments(input)?;
            if !input.starts_with('}') {
                loop {
                    let field = parse_identifier_str.parse_next(input)?.to_string();
                    let _ = skip_ws_and_comments(input)?;
                    let binding = if symbol(":").parse_next(input).is_ok() {
                        let _ = skip_ws_and_comments(input)?;
                        parse_identifier_str.parse_next(input)?.to_string()
                    } else {
                        field.clone()
                    };
                    fields.push((field, binding));
                    let _ = skip_ws_and_comments(input)?;
                    if symbol(",").parse_next(input).is_err() {
                        break;
                    }
                    let _ = skip_ws_and_comments(input)?;
                }
            }
            let _ = symbol("}").parse_next(input)?;
            return Ok(MatchPattern::EnumStruct {
                enum_name,
                variant,
                fields,
            });
        }

        if enum_name.is_some() || variant.chars().next().map_or(false, |c| c.is_uppercase()) {
            return Ok(MatchPattern::EnumUnit { enum_name, variant });
        }

        *input = checkpoint;
    } else {
        *input = checkpoint;
    }

    let expr = parse_expression(input)?;
    Ok(MatchPattern::Literal(expr))
}

pub fn parse_match_arm(input: &mut &str) -> ModalResult<MatchArm> {
    let _ = skip_ws_and_comments(input)?;
    let pattern = parse_match_pattern(input)?;
    let _ = skip_ws_and_comments(input)?;
    let _ = symbol("=>").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let body = if input.starts_with('{') {
        parse_block(input)?
    } else {
        let stmt = crate::parser::statement::parse_statement(input)?;
        vec![stmt]
    };

    Ok(MatchArm { pattern, body })
}

pub fn parse_if_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("if").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let condition = parse_expression.parse_next(input)?;
    let then_branch = parse_block(input)?;
    let _ = skip_ws_and_comments(input)?;

    let else_branch = if keyword("else").parse_next(input).is_ok() {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with("if") {
            let nested_if = parse_if_statement(input)?;
            Some(vec![nested_if])
        } else {
            Some(parse_block(input)?)
        }
    } else {
        None
    };

    let end_input = *input;
    Ok(Statement::If {
        condition,
        then_branch,
        else_branch,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_match_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("match").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let expr = parse_expression.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;
    let _ = symbol("{").parse_next(input)?;

    let mut arms = Vec::new();
    loop {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with('}') {
            break;
        }
        let arm = parse_match_arm(input)?;
        arms.push(arm);
    }

    let _ = symbol("}").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::Match {
        expr,
        arms,
        span: get_span_between(start_input, end_input),
    })
}
