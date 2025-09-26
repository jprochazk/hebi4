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
pub use vm::{Module, Vm, value::ValueRaw};

#[doc(hidden)]
pub mod __macro {
    pub use crate::vm::gc::{Gc, Root, StackRoot, ValueRoot};
    pub use crate::vm::value::{List, ValueRaw};
}
