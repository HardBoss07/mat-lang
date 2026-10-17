//! Collection and container expression code generation.
//!
//! This module coordinates compiling tuple literals, array literals, vector literals,
//! struct initializations, and container method invocations into LLVM IR.

pub mod literals;
pub mod methods;
pub mod struct_init;

#[allow(unused_imports)]
pub(crate) use literals::*;
#[allow(unused_imports)]
pub(crate) use methods::*;
#[allow(unused_imports)]
pub(crate) use struct_init::*;
