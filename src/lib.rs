#![allow(dead_code, unused_variables)] // TEMP

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

pub use vm::{Chunk, Value, Vm};

pub fn eval(code: &str) -> Result<vm::Value> {
    let tokens = token::tokenize(code);
    let ast = parser::parse(&tokens)?;
    let chunk = codegen::emit(&ast)?;

    todo!()
}
