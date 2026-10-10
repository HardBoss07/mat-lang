use winnow::ModalResult;
use winnow::Parser;
use winnow::token::literal;

use crate::ast::{MatchArm, MatchPattern, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::statement::parse_block;
use crate::parser::statement::parse_statement;
use crate::parser::utils::{get_span_between, keyword, skip_ws_and_comments, symbol};

pub fn parse_match_arm(input: &mut &str) -> ModalResult<MatchArm> {
    let _ = skip_ws_and_comments(input)?;

    let pattern = if input.starts_with("Ok(") || input.starts_with("Ok (") {
        let _ = literal("Ok").parse_next(input)?;
        let _ = symbol("(").parse_next(input)?;
        let var_name = parse_identifier_str.parse_next(input)?.to_string();
        let _ = symbol(")").parse_next(input)?;
        MatchPattern::Ok(var_name)
    } else if input.starts_with("Err(") || input.starts_with("Err (") {
        let _ = literal("Err").parse_next(input)?;
        let _ = symbol("(").parse_next(input)?;
        let var_name = parse_identifier_str.parse_next(input)?.to_string();
        let _ = symbol(")").parse_next(input)?;
        MatchPattern::Err(var_name)
    } else if input.starts_with('_')
        && !input
            .chars()
            .nth(1)
            .map_or(false, |c| c.is_alphanumeric() || c == '_')
    {
        *input = &input[1..];
        MatchPattern::Wildcard
    } else {
        let checkpoint = *input;
        if let Ok(path) = parse_identifier_str.parse_next(input) {
            let _ = skip_ws_and_comments(input)?;
            if path.contains("::") || input.starts_with('(') || input.starts_with('{') {
                let parts: Vec<&str> = path.split("::").collect();
                let (enum_name, variant) = if parts.len() > 1 {
                    (
                        Some(parts[..parts.len() - 1].join("::")),
                        parts.last().unwrap().to_string(),
                    )
                } else {
                    (None, parts[0].to_string())
                };

                if input.starts_with('(') {
                    let _ = symbol("(").parse_next(input)?;
                    let mut vars = Vec::new();
                    if !input.starts_with(')') {
                        while !input.is_empty() {
                            let v = parse_identifier_str.parse_next(input)?.to_string();
                            vars.push(v);
                            let _ = skip_ws_and_comments(input)?;
                            if input.starts_with(',') {
                                *input = &input[1..];
                                let _ = skip_ws_and_comments(input)?;
                                if input.starts_with(')') {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                    }
                    let _ = symbol(")").parse_next(input)?;
                    MatchPattern::EnumTuple {
                        enum_name,
                        variant,
                        vars,
                    }
                } else if input.starts_with('{') {
                    let _ = symbol("{").parse_next(input)?;
                    let mut fields = Vec::new();
                    if !input.starts_with('}') {
                        while !input.is_empty() {
                            let field_name = parse_identifier_str.parse_next(input)?.to_string();
                            let _ = skip_ws_and_comments(input)?;
                            let binding_name = if input.starts_with(':') {
                                *input = &input[1..];
                                let _ = skip_ws_and_comments(input)?;
                                parse_identifier_str.parse_next(input)?.to_string()
                            } else {
                                field_name.clone()
                            };
                            fields.push((field_name, binding_name));
                            let _ = skip_ws_and_comments(input)?;
                            if input.starts_with(',') {
                                *input = &input[1..];
                                let _ = skip_ws_and_comments(input)?;
                                if input.starts_with('}') {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                    }
                    let _ = symbol("}").parse_next(input)?;
                    MatchPattern::EnumStruct {
                        enum_name,
                        variant,
                        fields,
                    }
                } else if path.contains("::") {
                    MatchPattern::EnumUnit { enum_name, variant }
                } else {
                    *input = checkpoint;
                    let expr = parse_expression.parse_next(input)?;
                    MatchPattern::Literal(expr)
                }
            } else {
                *input = checkpoint;
                let expr = parse_expression.parse_next(input)?;
                MatchPattern::Literal(expr)
            }
        } else {
            *input = checkpoint;
            let expr = parse_expression.parse_next(input)?;
            MatchPattern::Literal(expr)
        }
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
    let start_input = *input;
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
    let end_input = *input;

    Ok(Statement::Match {
        expr,
        arms,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_if_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
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
    let end_input = *input;

    Ok(Statement::If {
        condition,
        then_branch,
        else_branch,
        span: get_span_between(start_input, end_input),
    })
}
