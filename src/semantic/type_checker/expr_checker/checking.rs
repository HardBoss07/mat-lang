//! Expression type verification against expected target types.

pub mod collections;
pub mod literals;
pub mod nominal;

use super::super::super::symbol_table::SymbolTable;
use super::super::TypeChecker;
use crate::ast::{Expression, Type};
use crate::error::Result;

impl TypeChecker {
    /// Checks that an expression conforms to an expected target type.
    pub fn check_expr(
        &self,
        expr: &Expression,
        expected_ty: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        match (expr, expected_ty) {
            // Integer & Float Literals
            (Expression::IntLiteral(val, span), target_ty) => {
                self.check_int_literal(*val, *span, target_ty)
            }
            (Expression::FloatLiteral(val, span), target_ty) => {
                self.check_float_literal(*val, *span, target_ty)
            }

            // Collections & Containers
            (Expression::TupleLiteral(elements, span), Type::Tuple(target_types)) => {
                self.check_tuple_literal(elements, target_types, *span, symbols)
            }
            (
                Expression::ArrayLiteral(elements, span),
                Type::Array(target_elem_ty, expected_len),
            ) => self.check_array_literal(elements, target_elem_ty, *expected_len, *span, symbols),
            (Expression::VectorLiteral(elements, _), Type::Vector(target_elem_ty)) => {
                self.check_vector_literal(elements, target_elem_ty, symbols)
            }
            (
                Expression::Call {
                    callee,
                    arguments,
                    span,
                },
                Type::Vector(target_elem_ty),
            ) if callee == "Vec::new" => self.check_vec_new_call(arguments, target_elem_ty, *span),

            // Result Constructors
            (Expression::Ok(val, _), Type::Result(ok_ty, _)) => {
                self.check_ok_constructor(val, ok_ty, expected_ty, symbols)
            }
            (Expression::Err(val, _), Type::Result(_, err_ty)) => {
                self.check_err_constructor(val, err_ty, expected_ty, symbols)
            }

            // Nominal Types (Structs, Enums, Nominal Calls)
            (Expression::StructInit { name, .. }, Type::Struct(_) | Type::Custom(_)) => {
                self.check_struct_init_target(expr, name, expected_ty, symbols)
            }
            (
                Expression::EnumInit {
                    enum_name,
                    payload,
                    span,
                    ..
                },
                Type::Struct(_) | Type::Custom(_),
            ) => self.check_enum_init_target(
                enum_name.as_deref(),
                payload,
                *span,
                expected_ty,
                symbols,
            ),
            (Expression::Call { callee, .. }, Type::Struct(_) | Type::Custom(_))
                if callee != "Vec::new" =>
            {
                self.check_nominal_call(expr, callee, expected_ty, symbols)
            }

            // Binary Operations against expected numeric target types
            (
                Expression::Binary {
                    op, left, right, ..
                },
                target_ty,
            ) if matches!(
                target_ty,
                Type::Int | Type::I32 | Type::I16 | Type::I8 | Type::F64 | Type::F32
            ) =>
            {
                self.check_binary_expr(expr, op, left, right, target_ty, symbols)
            }

            // Fallback: Delegated to check_fallback_expr
            _ => self.check_fallback_expr(expr, expected_ty, symbols),
        }
    }
}
