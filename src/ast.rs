pub mod span;
pub mod types;

pub use span::Span;
pub use types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatSpecifier {
    None,
    Bin,
    Hex,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(String, Span),
    IntLiteral(i64, Span),
    FloatLiteral(f64, Span),
    BoolLiteral(bool, Span),
    StringLiteral(String, Span),
    InterpolatedString(Vec<(Expression, FormatSpecifier)>, Span),
    TupleLiteral(Vec<Expression>, Span),
    ArrayLiteral(Vec<Expression>, Span),
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
        span: Span,
    },
    TupleAccess {
        expr: Box<Expression>,
        index: usize,
        span: Span,
    },
    ArrayAccess {
        expr: Box<Expression>,
        index: Box<Expression>,
        span: Span,
    },
    Call {
        callee: String,
        arguments: Vec<Expression>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        name: String,
        is_mutable: bool,
        ty: Option<Type>,
        value: Expression,
        span: Span,
    },
    Assignment {
        target: String,
        value: Expression,
        span: Span,
    },
    CompoundAssignment {
        target: String,
        op: BinaryOp,
        value: Expression,
        span: Span,
    },
    Increment {
        target: String,
        span: Span,
    },
    Decrement {
        target: String,
        span: Span,
    },
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub return_type: Type,
    pub body: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(FunctionDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}
