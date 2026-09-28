pub mod span;
pub mod types;

pub use span::Span;
pub use types::Type;

use serde::Serialize;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum FormatSpecifier {
    None,
    Bin,
    Hex,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    #[serde(skip)]
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum MatchPattern {
    Ok(String),
    Err(String),
    Literal(Expression),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: Vec<Statement>,
}

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
            | Expression::Ok(_, span)
            | Expression::Err(_, span)
            | Expression::Binary { span, .. }
            | Expression::TupleAccess { span, .. }
            | Expression::ArrayAccess { span, .. }
            | Expression::Call { span, .. } => *span,
        }
    }
}

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

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Vec<Statement>,
    #[serde(skip)]
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Item {
    Function(FunctionDeclaration),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Program {
    pub items: Vec<Item>,
}
