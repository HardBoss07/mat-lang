use miette::Diagnostic;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, MatcError>;

#[derive(Error, Diagnostic, Debug)]
pub enum MatcError {
    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Syntax Error: {message}")]
    #[diagnostic(code(matc::syntax_error))]
    SyntaxError {
        message: String,
        #[label("here")]
        span: (usize, usize),
    },

    #[error("Type Error: {message}")]
    #[diagnostic(code(matc::type_error))]
    TypeError { message: String },

    #[error("LLVM Codegen Error: {0}")]
    CodegenError(String),
}
