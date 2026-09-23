pub mod span;
pub mod types;

pub use span::Span;
pub use types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        name: Identifier,
        type_annotation: Option<Type>,
        initializer: Expression,
        is_mutable: bool,
        span: Span,
    },
    Return {
        value: Option<Expression>,
        span: Span,
    },
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Integer(i64, Span),
    Float(f64, Span),
    Boolean(bool, Span),
    StringLiteral(String, Span),
    Identifier(Identifier),
}
