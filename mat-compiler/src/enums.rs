#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(String),
    StringLiteral(String),
    Symbol(char),
    Identifier(String),
    Integer(i32),
    Operator(char),
    Character(char),
    Float(f64),
    Bool(bool),
    Condition(String),
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Function(String, Vec<ASTNode>),
    FunctionCall(String),
    Print(Vec<StringPart>),
    VariableDeclaration(String, PrimitiveVariable),
    VariableChangeValue(String, PrimitiveVariable),
    Operation(char, String, PrimitiveVariable),
    IfStatement(String, Vec<ASTNode>),
    ElseStatement(Vec<ASTNode>),
    WhileLoop(String, Vec<ASTNode>),
}

#[derive(Debug, Clone)]
pub enum ComplexVariable {
    Array(String, Vec<PrimitiveVariable>),
}

#[derive(Debug, Clone)]
pub enum PrimitiveVariable {
    Integer(i32),
    Character(char),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum StringPart {
    Literal(String),
    Variable(String),
}
