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
pub use vm::Value;

pub fn eval(code: &str) -> Result<vm::Value> {
    let tokens = token::tokenize(code);
    let ast = parser::parse(&tokens)?;
    let module = codegen::emit(&ast)?;

    todo!()
}
