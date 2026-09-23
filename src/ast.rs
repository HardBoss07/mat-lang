pub mod span;
pub mod types;

pub use span::Span;
pub use types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    StringLiteral(String, Span),
    Call {
        callee: String,
        arguments: Vec<Expression>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
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
