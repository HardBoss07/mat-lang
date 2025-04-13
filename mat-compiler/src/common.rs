use crate::enums::{StringPart, Token};

pub fn s_lit_to_s_parts(s_lit: String) -> Option<Vec<StringPart>> {
    let mut parts = Vec::new();

    let mut current_literal = String::new();
    let mut in_interpolation = false;
    let mut variable_name = String::new();

    for c in s_lit.chars() {
        match c {
            '{' => {
                if in_interpolation {
                    return None;
                }
                if !current_literal.is_empty() {
                    parts.push(StringPart::Literal(current_literal.clone()));
                    current_literal.clear();
                }
                in_interpolation = true;
            }
            '}' => {
                if !in_interpolation {
                    return None;
                }
                if !variable_name.is_empty() {
                    parts.push(StringPart::Variable(variable_name.clone()));
                    variable_name.clear();
                }
                in_interpolation = false;
            }
            _ => {
                if in_interpolation {
                    variable_name.push(c);
                } else {
                    current_literal.push(c);
                }
            }
        }
    }

    if !current_literal.is_empty() {
        parts.push(StringPart::Literal(current_literal));
    }

    Some(parts)
}

pub fn s_parts_to_string(s_parts: Vec<StringPart>) -> String {
    let mut string = String::new();
    let mut variables = Vec::new();

    for part in s_parts {
        match part {
            StringPart::Literal(lit) => string.push_str(&lit),
            StringPart::Variable(var) => {
                string.push_str("{}");
                variables.push(var);
            }
        }
    }

    if variables.is_empty() {
        format!("\"{}\"", string)
    } else {
        format!("\"{}\", {}", string, variables.join(", "))
    }
}