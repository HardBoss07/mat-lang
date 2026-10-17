//! Type compatibility checking for the MAT language type system.
//!
//! This module provides functions to verify whether two types are structurally
//! and semantically compatible during type synthesis and checking.

use crate::ast::Type;

/// Determines whether two types are compatible with each other.
///
/// This checks for exact equality, `Type::Infer` wildcards, structurally compatible
/// custom/struct types (matching short names), compatible result types, vector element
/// types, array element types with identical lengths, and pairwise-compatible tuple elements.
///
/// # Arguments
///
/// * `first` - The first type to compare.
/// * `second` - The second type to compare against.
///
/// # Returns
///
/// `true` if the types are considered compatible, `false` otherwise.
pub fn types_compatible(first: &Type, second: &Type) -> bool {
    if first == second {
        return true;
    }
    if *first == Type::Infer || *second == Type::Infer {
        return true;
    }
    match (first, second) {
        (
            Type::Custom(name_a) | Type::Struct(name_a),
            Type::Custom(name_b) | Type::Struct(name_b),
        ) => {
            let short_a = name_a.split("::").last().unwrap_or(name_a);
            let short_b = name_b.split("::").last().unwrap_or(name_b);
            short_a == short_b
        }
        (Type::Result(ok_a, err_a), Type::Result(ok_b, err_b)) => {
            types_compatible(ok_a, ok_b) && types_compatible(err_a, err_b)
        }
        (Type::Vector(elem_a), Type::Vector(elem_b)) => types_compatible(elem_a, elem_b),
        (Type::Array(elem_a, len_a), Type::Array(elem_b, len_b)) => {
            len_a == len_b && types_compatible(elem_a, elem_b)
        }
        (Type::Tuple(elements_a), Type::Tuple(elements_b)) => {
            elements_a.len() == elements_b.len()
                && elements_a
                    .iter()
                    .zip(elements_b.iter())
                    .all(|(element_a, element_b)| types_compatible(element_a, element_b))
        }
        _ => false,
    }
}
