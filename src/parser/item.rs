use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace1;
use winnow::token::literal;

use crate::ast::{
    EnumDeclaration, EnumVariant, EnumVariantPayload, FunctionDeclaration, GlobalLetDeclaration,
    ImportDeclaration, Item, Param, StructDeclaration, StructField, Type,
};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_single_identifier_str;
use crate::parser::statement::parse_block;
use crate::parser::types::parse_type;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

pub fn parse_struct_field(input: &mut &str) -> ModalResult<StructField> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = if input.starts_with("pub") {
        let after_pub = &input[3..];
        if after_pub
            .chars()
            .next()
            .map_or(true, |c| c.is_whitespace() || c == '/' || c == '{')
        {
            let _ = literal("pub").parse_next(input)?;
            let _ = skip_ws_and_comments(input)?;
            true
        } else {
            false
        }
    } else {
        false
    };

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with(':') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let ty = parse_type.parse_next(input)?;
    let end_input = *input;

    Ok(StructField {
        is_pub,
        name,
        ty,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_struct(input: &mut &str) -> ModalResult<StructDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        true
    } else {
        false
    };

    parse_struct_with_pub(input, is_pub, start_input)
}

fn parse_struct_with_pub(
    input: &mut &str,
    is_pub: bool,
    start_input: &str,
) -> ModalResult<StructDeclaration> {
    if !input.starts_with("struct") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("struct").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with('{') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let mut fields = Vec::new();
    while !input.is_empty() && !input.starts_with('}') {
        let field = parse_struct_field.parse_next(input)?;
        fields.push(field);
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with(',') {
            *input = &input[1..];
            let _ = skip_ws_and_comments(input)?;
        } else {
            break;
        }
    }

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with('}') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(StructDeclaration {
        is_pub,
        name,
        fields,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_enum(input: &mut &str) -> ModalResult<EnumDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        true
    } else {
        false
    };

    parse_enum_with_pub(input, is_pub, start_input)
}

fn parse_enum_with_pub(
    input: &mut &str,
    is_pub: bool,
    start_input: &str,
) -> ModalResult<EnumDeclaration> {
    if !input.starts_with("enum") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("enum").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with('{') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let mut variants = Vec::new();
    while !input.is_empty() && !input.starts_with('}') {
        let var_start = *input;
        let var_name = parse_single_identifier_str
            .parse_next(input)
            .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
            .to_string();
        let _ = skip_ws_and_comments(input)?;

        let payload = if input.starts_with('(') {
            *input = &input[1..];
            let _ = skip_ws_and_comments(input)?;
            let mut tuple_types = Vec::new();
            if !input.starts_with(')') {
                while !input.is_empty() {
                    let ty = parse_type.parse_next(input)?;
                    tuple_types.push(ty);
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
            if !input.starts_with(')') {
                return Err(winnow::error::ErrMode::Cut(
                    winnow::error::ContextError::default(),
                ));
            }
            *input = &input[1..];
            EnumVariantPayload::Tuple(tuple_types)
        } else if input.starts_with('{') {
            *input = &input[1..];
            let _ = skip_ws_and_comments(input)?;
            let mut fields = Vec::new();
            if !input.starts_with('}') {
                while !input.is_empty() {
                    let field = parse_struct_field.parse_next(input)?;
                    fields.push(field);
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
            if !input.starts_with('}') {
                return Err(winnow::error::ErrMode::Cut(
                    winnow::error::ContextError::default(),
                ));
            }
            *input = &input[1..];
            EnumVariantPayload::Struct(fields)
        } else {
            EnumVariantPayload::Unit
        };

        let var_end = *input;
        variants.push(EnumVariant {
            name: var_name,
            payload,
            span: get_span_between(var_start, var_end),
        });

        let _ = skip_ws_and_comments(input)?;
        if input.starts_with(',') {
            *input = &input[1..];
            let _ = skip_ws_and_comments(input)?;
        } else {
            break;
        }
    }

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with('}') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(EnumDeclaration {
        is_pub,
        name,
        variants,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_global_let(input: &mut &str) -> ModalResult<GlobalLetDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        true
    } else {
        false
    };

    parse_global_let_with_pub(input, is_pub, start_input)
}

fn parse_global_let_with_pub(
    input: &mut &str,
    is_pub: bool,
    start_input: &str,
) -> ModalResult<GlobalLetDeclaration> {
    if !input.starts_with("let") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("let").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let is_mutable = if input.starts_with("mut") {
        let _ = literal("mut").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        true
    } else {
        false
    };

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    let ty = if input.starts_with(':') {
        *input = &input[1..];
        let _ = skip_ws_and_comments(input)?;
        Some(parse_type.parse_next(input)?)
    } else {
        None
    };

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with('=') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let value = parse_expression(input)?;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with(';') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(GlobalLetDeclaration {
        is_pub,
        name,
        is_mutable,
        ty,
        value,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_param(input: &mut &str) -> ModalResult<Param> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_mutable = if input.starts_with("mut") {
        let after_mut = &input[3..];
        if after_mut
            .chars()
            .next()
            .map_or(false, |c| c.is_whitespace() || c == '_')
        {
            let _ = literal("mut").parse_next(input)?;
            let _ = skip_ws_and_comments(input)?;
            true
        } else {
            false
        }
    } else {
        false
    };

    let name = match parse_single_identifier_str.parse_next(input) {
        Ok(n) => n.to_string(),
        Err(e) => {
            *input = start_input;
            return Err(e);
        }
    };
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with(':') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let ty = parse_type.parse_next(input)?;
    let end_input = *input;

    Ok(Param {
        is_mutable,
        name,
        ty,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_import(input: &mut &str) -> ModalResult<ImportDeclaration> {
    let start_input = *input;
    parse_import_with_start(input, start_input)
}

fn parse_import_with_start(input: &mut &str, start_input: &str) -> ModalResult<ImportDeclaration> {
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with("import") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("import").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let is_std = if input.starts_with("std::") {
        *input = &input[5..];
        true
    } else {
        false
    };

    let mut path = Vec::new();
    let first_seg = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    path.push(first_seg);

    while input.starts_with("::") {
        *input = &input[2..];
        let _ = skip_ws_and_comments(input)?;
        let seg = parse_single_identifier_str
            .parse_next(input)
            .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
            .to_string();
        path.push(seg);
    }

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with(';') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];

    let end_input = *input;

    Ok(ImportDeclaration {
        is_std,
        path,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_function(input: &mut &str) -> ModalResult<FunctionDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        true
    } else {
        false
    };

    parse_function_with_pub(input, is_pub, start_input)
}

fn parse_function_with_pub(
    input: &mut &str,
    is_pub: bool,
    start_input: &str,
) -> ModalResult<FunctionDeclaration> {
    if !input.starts_with("fn") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("fn").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with('(') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let mut params = Vec::new();
    if !input.starts_with(')') {
        while !input.is_empty() {
            let param = parse_param.parse_next(input)?;
            params.push(param);
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

    if !input.starts_with(')') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let return_type = if input.starts_with("->") {
        *input = &input[2..];
        let _ = skip_ws_and_comments(input)?;
        parse_type.parse_next(input)?
    } else {
        Type::Void
    };

    let _ = skip_ws_and_comments(input)?;
    let body = parse_block(input)?;
    let end_input = *input;

    Ok(FunctionDeclaration {
        is_pub,
        name,
        params,
        return_type,
        body,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_item(input: &mut &str) -> ModalResult<Item> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    if input.is_empty() {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    let is_pub = if input.starts_with("pub") {
        let after_pub = &input[3..];
        if after_pub
            .chars()
            .next()
            .map_or(true, |c| c.is_whitespace() || c == '/' || c == '{')
        {
            let _ = literal("pub").parse_next(input)?;
            let _ = skip_ws_and_comments(input)?;
            true
        } else {
            false
        }
    } else {
        false
    };

    if input.starts_with("import") {
        if is_pub {
            return Err(winnow::error::ErrMode::Cut(
                winnow::error::ContextError::default(),
            ));
        }
        let decl = parse_import_with_start(input, start_input)?;
        return Ok(Item::Import(decl));
    } else if input.starts_with("struct") {
        let decl = parse_struct_with_pub(input, is_pub, start_input)?;
        return Ok(Item::Struct(decl));
    } else if input.starts_with("enum") {
        let decl = parse_enum_with_pub(input, is_pub, start_input)?;
        return Ok(Item::Enum(decl));
    } else if input.starts_with("fn") {
        let decl = parse_function_with_pub(input, is_pub, start_input)?;
        return Ok(Item::Function(decl));
    } else if input.starts_with("let") {
        let decl = parse_global_let_with_pub(input, is_pub, start_input)?;
        return Ok(Item::GlobalLet(decl));
    }

    Err(winnow::error::ErrMode::Cut(
        winnow::error::ContextError::default(),
    ))
}
