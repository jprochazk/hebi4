#![allow(dead_code, unused_variables)] // TEMP
#![allow(private_bounds)] // intentional

// NOTE: spaces between `mod` ensure ordering

pub mod span;

pub mod error;

#[macro_use]
mod intern;

#[macro_use]
mod token;

mod ast;
mod parser;

#[macro_use]
mod codegen;

mod vm;

pub use error::Result;
pub use vm::{Chunk, Vm, value::Value};

pub fn eval(code: &str) -> Result<Value> {
    let tokens = token::tokenize(code);
    let ast = parser::parse(&tokens)?;
    let chunk = codegen::emit(&ast)?;

    todo!()
}

#[doc(hidden)]
pub mod __macro {
    pub use crate::vm::gc::{Root, StackRoot};
}
