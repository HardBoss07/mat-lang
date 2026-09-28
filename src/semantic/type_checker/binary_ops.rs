use super::super::symbol_table::SymbolTable;
use super::TypeChecker;
use crate::ast::{BinaryOp, Expression, Type};
use crate::error::{MatcError, Result};

impl TypeChecker {
    pub(crate) fn synthesize_binary_expr(
        &self,
        op: &BinaryOp,
        left: &Expression,
        right: &Expression,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let left_ty = self.synthesize_expr(left, symbols)?;
        let right_ty = self.synthesize_expr(right, symbols)?;
        if left_ty != right_ty {
            return Err(MatcError::TypeError {
                message: format!(
                    "Binary operator type mismatch: expected {:?}, got {:?}",
                    left_ty, right_ty
                ),
            });
        }
        match op {
            BinaryOp::Eq
            | BinaryOp::Neq
            | BinaryOp::Lt
            | BinaryOp::Lte
            | BinaryOp::Gt
            | BinaryOp::Gte
            | BinaryOp::And
            | BinaryOp::Or => Ok(Type::Bool),
            _ => Ok(left_ty),
        }
    }
}
