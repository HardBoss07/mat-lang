#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int, // 64-bit signed integer (default)
    I32,
    I16,
    I8,
    F64,
    Bool,
    Char,
    String,
    Custom(String),
    Result(Box<Type>, Box<Type>),
}
