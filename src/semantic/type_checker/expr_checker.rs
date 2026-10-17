//! Expression type synthesis and checking for the MAT language compiler.
//!
//! This module coordinates expression type inference, checking against target types,
//! method call resolution, and literal type compatibility.

pub mod calls;
pub mod checking;
pub mod collections;
pub mod compatibility;
pub mod methods;
pub mod struct_enum;

#[allow(unused_imports)]
pub(crate) use calls::*;
#[allow(unused_imports)]
pub(crate) use checking::*;
#[allow(unused_imports)]
pub(crate) use collections::*;
#[allow(unused_imports)]
pub use compatibility::types_compatible;
#[allow(unused_imports)]
pub(crate) use methods::*;
#[allow(unused_imports)]
pub(crate) use struct_enum::*;

use crate::ast::{Expression, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Synthesizes the type of an expression in a bottom-up direction.
    pub fn synthesize_expr(&self, expr: &Expression, symbols: &SymbolTable) -> Result<Type> {
        match expr {
            Expression::Identifier(name, span) => {
                self.synthesize_identifier_expr(name, *span, symbols)
            }
            Expression::IntLiteral(_, _) => Ok(Type::Int),
            Expression::FloatLiteral(_, _) => Ok(Type::F64),
            Expression::BoolLiteral(_, _) => Ok(Type::Bool),
            Expression::StringLiteral(_, _) => Ok(Type::String),
            Expression::InterpolatedString(parts, _) => {
                self.synthesize_interpolated_string(parts, symbols)
            }
            Expression::Binary {
                op, left, right, ..
            } => self.synthesize_binary_expr(op, left, right, symbols),
            Expression::TupleLiteral(elements, _) => {
                self.synthesize_tuple_literal(elements, symbols)
            }
            Expression::ArrayLiteral(elements, span) => {
                self.synthesize_array_literal(elements, *span, symbols)
            }
            Expression::TupleAccess { expr, index, span } => {
                self.synthesize_tuple_access(expr, *index, *span, symbols)
            }
            Expression::VectorLiteral(elements, span) => {
                self.synthesize_vector_literal(elements, *span, symbols)
            }
            Expression::StructInit { name, fields, span } => {
                self.synthesize_struct_init_expr(name, fields, *span, symbols)
            }
            Expression::EnumInit {
                enum_name,
                variant_name,
                payload,
                span,
            } => self.synthesize_enum_init_expr(
                enum_name.as_deref(),
                variant_name,
                payload,
                *span,
                symbols,
            ),
            Expression::FieldAccess { expr, field, span } => {
                self.synthesize_field_access(expr, field, *span, symbols)
            }
            Expression::MethodCall {
                receiver,
                method,
                args,
                span,
            } => {
                let receiver_ty = self.synthesize_expr(receiver, symbols)?;
                self.check_method_call(receiver, &receiver_ty, method, args, *span, symbols)
            }
            Expression::ArrayAccess { expr, index, span } => {
                self.synthesize_array_access(expr, index, *span, symbols)
            }
            Expression::Call {
                callee,
                arguments,
                span,
            } => self.synthesize_call_expr(callee, arguments, *span, symbols),
            Expression::Ok(_, span) | Expression::Err(_, span) => Err(self.type_error(
                "Ok(...) and Err(...) constructors require type context",
                *span,
            )),
        }
    }
}
