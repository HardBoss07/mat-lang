use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::{digit1, multispace0};
use winnow::combinator::{delimited, separated};

use crate::ast::Type;
use crate::parser::expression::primary::parse_identifier_str;

pub fn parse_type(input: &mut &str) -> ModalResult<Type> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if input.starts_with('(') {
        let tuple_res: ModalResult<Vec<Type>> =
            delimited('(', separated(0.., parse_type, ','), ')').parse_next(input);
        if let Ok(types) = tuple_res {
            return Ok(Type::Tuple(types));
        }
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    if input.starts_with('[') {
        let mut inner_cp = *input;
        inner_cp = &inner_cp[1..];
        if let Ok(elem_type) = parse_type.parse_next(&mut inner_cp) {
            let _ = multispace0.parse_next(&mut inner_cp)?;
            if inner_cp.starts_with(';') {
                inner_cp = &inner_cp[1..];
                let _ = multispace0.parse_next(&mut inner_cp)?;
                let len_res: ModalResult<&str> = digit1.parse_next(&mut inner_cp);
                if let Ok(len_str) = len_res {
                    if let Ok(len) = len_str.parse::<usize>() {
                        let _ = multispace0.parse_next(&mut inner_cp)?;
                        if inner_cp.starts_with(']') {
                            inner_cp = &inner_cp[1..];
                            *input = inner_cp;
                            return Ok(Type::Array(Box::new(elem_type), len));
                        }
                    }
                }
            }
        }
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    if let Ok(type_str) = parse_identifier_str.parse_next(input) {
        match type_str {
            "int" => return Ok(Type::Int),
            "i32" => return Ok(Type::I32),
            "i16" => return Ok(Type::I16),
            "i8" => return Ok(Type::I8),
            "f64" => return Ok(Type::F64),
            "f32" => return Ok(Type::F32),
            "bool" => return Ok(Type::Bool),
            "String" => return Ok(Type::String),
            other => return Ok(Type::Custom(other.to_string())),
        }
    }

    *input = checkpoint;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}
