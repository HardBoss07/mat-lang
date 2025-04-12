#[derive(Debug, Clone)]
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
    MainFunction(Vec<ASTNode>),
    Print(Vec<PrintPart>),
    VariableDeclaration(String, VariableType),
    VariableChangeValue(String, VariableType),
    Operation(char, String, VariableType),
    IfStatement(String, Vec<ASTNode>),
    ElseStatement(Vec<ASTNode>),
    WhileLoop(String, Vec<ASTNode>),
}

#[derive(Debug, Clone)]
pub enum VariableType {
    Integer(i32),
    Character(char),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum PrintPart {
    Literal(String),
    Variable(String),
}
