use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

pub type Result<T> = std::result::Result<T, MatcError>;

#[derive(Error, Diagnostic, Debug)]
pub enum MatcError {
    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Syntax Error: {message}")]
    #[diagnostic(code(matc::syntax_error), help("check syntax around this token"))]
    SyntaxError {
        #[source_code]
        src: NamedSource<String>,

        message: String,

        #[label("{}", message)]
        span: SourceSpan,
    },

    #[error("Type Error: {message}")]
    #[diagnostic(
        code(matc::type_error),
        help("ensure variable types match the expected function or assignment signature")
    )]
    TypeError {
        #[source_code]
        src: NamedSource<String>,

        message: String,

        #[label("type mismatch here")]
        span: SourceSpan,
    },

    #[error("Type Error: Undefined variable: {name}")]
    #[diagnostic(
        code(matc::undefined_variable),
        help("ensure the variable is declared in the current scope before using it")
    )]
    UndefinedVariable {
        #[source_code]
        src: NamedSource<String>,

        name: String,

        #[label("undefined variable here")]
        span: SourceSpan,
    },

    #[error("Type Error: Undefined function: {name}")]
    #[diagnostic(
        code(matc::undefined_function),
        help("ensure the function is defined or imported before calling it")
    )]
    UndefinedFunction {
        #[source_code]
        src: NamedSource<String>,

        name: String,

        #[label("undefined function here")]
        span: SourceSpan,
    },

    #[error("LLVM Codegen Error: {0}")]
    CodegenError(String),
}

impl MatcError {
    pub fn syntax_error(
        file_name: impl Into<String>,
        source: impl Into<String>,
        message: impl Into<String>,
        span: impl Into<SourceSpan>,
    ) -> Self {
        MatcError::SyntaxError {
            src: NamedSource::new(file_name.into(), source.into()),
            message: message.into(),
            span: span.into(),
        }
    }

    pub fn type_error(
        file_name: impl Into<String>,
        source: impl Into<String>,
        message: impl Into<String>,
        span: impl Into<SourceSpan>,
    ) -> Self {
        MatcError::TypeError {
            src: NamedSource::new(file_name.into(), source.into()),
            message: message.into(),
            span: span.into(),
        }
    }

    pub fn undefined_variable(
        file_name: impl Into<String>,
        source: impl Into<String>,
        name: impl Into<String>,
        span: impl Into<SourceSpan>,
    ) -> Self {
        MatcError::UndefinedVariable {
            src: NamedSource::new(file_name.into(), source.into()),
            name: name.into(),
            span: span.into(),
        }
    }

    pub fn undefined_function(
        file_name: impl Into<String>,
        source: impl Into<String>,
        name: impl Into<String>,
        span: impl Into<SourceSpan>,
    ) -> Self {
        MatcError::UndefinedFunction {
            src: NamedSource::new(file_name.into(), source.into()),
            name: name.into(),
            span: span.into(),
        }
    }
}
