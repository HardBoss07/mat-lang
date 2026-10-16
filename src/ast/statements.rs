//! Abstract Syntax Tree statement and pattern matching definitions.
//!
//! This module defines the [`Statement`] enum, [`MatchArm`], and [`MatchPattern`] variants.

use serde::Serialize;

use super::expressions::{BinaryOp, Expression};
use super::span::Span;
use super::types::Type;

/// Pattern to match against in a `match` construct arm.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum MatchPattern {
    Ok(String),
    Err(String),
    EnumUnit {
        enum_name: Option<String>,
        variant: String,
    },
    EnumTuple {
        enum_name: Option<String>,
        variant: String,
        vars: Vec<String>,
    },
    EnumStruct {
        enum_name: Option<String>,
        variant: String,
        fields: Vec<(String, String)>,
    },
    Literal(Expression),
    Wildcard,
}

/// An individual arm of a `match` expression containing a pattern and body statements.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: Vec<Statement>,
}

/// Abstract syntax tree representation of a statement.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Statement {
    Let {
        name: String,
        is_mutable: bool,
        ty: Option<Type>,
        value: Expression,
        #[serde(skip)]
        span: Span,
    },
    Assignment {
        target: String,
        value: Expression,
        #[serde(skip)]
        span: Span,
    },
    CompoundAssignment {
        target: String,
        op: BinaryOp,
        value: Expression,
        #[serde(skip)]
        span: Span,
    },
    Increment {
        target: String,
        #[serde(skip)]
        span: Span,
    },
    Decrement {
        target: String,
        #[serde(skip)]
        span: Span,
    },
    Loop {
        body: Vec<Statement>,
        #[serde(skip)]
        span: Span,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
        #[serde(skip)]
        span: Span,
    },
    ForI {
        init: Box<Statement>,
        condition: Expression,
        step: Box<Statement>,
        body: Vec<Statement>,
        #[serde(skip)]
        span: Span,
    },
    ForIn {
        var_name: String,
        iterable: Expression,
        body: Vec<Statement>,
        #[serde(skip)]
        span: Span,
    },
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
        #[serde(skip)]
        span: Span,
    },
    Match {
        expr: Expression,
        arms: Vec<MatchArm>,
        #[serde(skip)]
        span: Span,
    },
    Return(Option<Expression>, #[serde(skip)] Span),
    Break(#[serde(skip)] Span),
    Continue(#[serde(skip)] Span),
    Expression(Expression),
}

impl Statement {
    /// Returns the source code span associated with this statement node.
    pub fn span(&self) -> Span {
        match self {
            Statement::Let { span, .. }
            | Statement::Assignment { span, .. }
            | Statement::CompoundAssignment { span, .. }
            | Statement::Increment { span, .. }
            | Statement::Decrement { span, .. }
            | Statement::Loop { span, .. }
            | Statement::While { span, .. }
            | Statement::ForI { span, .. }
            | Statement::ForIn { span, .. }
            | Statement::If { span, .. }
            | Statement::Match { span, .. }
            | Statement::Return(_, span)
            | Statement::Break(span)
            | Statement::Continue(span) => *span,
            Statement::Expression(expr) => expr.span(),
        }
    }
}
