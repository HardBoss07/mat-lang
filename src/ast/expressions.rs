//! Abstract Syntax Tree expression and operator definitions.
//!
//! This module defines the [`Expression`] enum, binary operators ([`BinaryOp`]),
//! format specifiers, and span-retrieval methods.

use serde::Serialize;

use super::span::Span;

/// Binary operators supported in MAT expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

/// Format specifier applied within interpolated string expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum FormatSpecifier {
    None,
    Bin,
    Hex,
}

/// Abstract syntax tree representation of an expression.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expression {
    Identifier(String, #[serde(skip)] Span),
    IntLiteral(i64, #[serde(skip)] Span),
    FloatLiteral(f64, #[serde(skip)] Span),
    BoolLiteral(bool, #[serde(skip)] Span),
    StringLiteral(String, #[serde(skip)] Span),
    InterpolatedString(Vec<(Expression, FormatSpecifier)>, #[serde(skip)] Span),
    TupleLiteral(Vec<Expression>, #[serde(skip)] Span),
    ArrayLiteral(Vec<Expression>, #[serde(skip)] Span),
    VectorLiteral(Vec<Expression>, #[serde(skip)] Span),
    StructInit {
        name: String,
        fields: Vec<(String, Expression)>,
        #[serde(skip)]
        span: Span,
    },
    EnumInit {
        enum_name: Option<String>,
        variant_name: String,
        payload: Option<Box<Expression>>,
        #[serde(skip)]
        span: Span,
    },
    FieldAccess {
        expr: Box<Expression>,
        field: String,
        #[serde(skip)]
        span: Span,
    },
    MethodCall {
        receiver: Box<Expression>,
        method: String,
        args: Vec<Expression>,
        #[serde(skip)]
        span: Span,
    },
    Ok(Box<Expression>, #[serde(skip)] Span),
    Err(Box<Expression>, #[serde(skip)] Span),
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
        #[serde(skip)]
        span: Span,
    },
    TupleAccess {
        expr: Box<Expression>,
        index: usize,
        #[serde(skip)]
        span: Span,
    },
    ArrayAccess {
        expr: Box<Expression>,
        index: Box<Expression>,
        #[serde(skip)]
        span: Span,
    },
    Call {
        callee: String,
        arguments: Vec<Expression>,
        #[serde(skip)]
        span: Span,
    },
}

impl Expression {
    /// Returns the source code span associated with this expression node.
    pub fn span(&self) -> Span {
        match self {
            Expression::Identifier(_, span)
            | Expression::IntLiteral(_, span)
            | Expression::FloatLiteral(_, span)
            | Expression::BoolLiteral(_, span)
            | Expression::StringLiteral(_, span)
            | Expression::InterpolatedString(_, span)
            | Expression::TupleLiteral(_, span)
            | Expression::ArrayLiteral(_, span)
            | Expression::VectorLiteral(_, span)
            | Expression::StructInit { span, .. }
            | Expression::EnumInit { span, .. }
            | Expression::FieldAccess { span, .. }
            | Expression::MethodCall { span, .. }
            | Expression::Ok(_, span)
            | Expression::Err(_, span)
            | Expression::Binary { span, .. }
            | Expression::TupleAccess { span, .. }
            | Expression::ArrayAccess { span, .. }
            | Expression::Call { span, .. } => *span,
        }
    }
}
