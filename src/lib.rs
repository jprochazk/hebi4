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

pub use codegen::emit;
pub use error::Result;
pub use parser::parse;
pub use token::tokenize;
pub use vm::{Chunk, Vm, value::ValueRaw};

// pub fn eval(code: &str) -> Result<ValueRaw> {
//     let tokens = token::tokenize(code);
//     let ast = parser::parse(&tokens)?;
//     let chunk = codegen::emit(&ast)?;

//     let mut vm = Vm::new();

//     vm.with(|vm| {
//         match vm.run(chunk) {
//             Ok(result) => vm.fmt(result).to_string(),
//             Err(err) => err.render(code).to_string(),
//         }
//     });

//     todo!()
// }

#[doc(hidden)]
pub mod __macro {
    pub use crate::vm::gc::{Gc, Root, StackRoot, ValueRoot};
    pub use crate::vm::value::{List, ValueRaw};
}
