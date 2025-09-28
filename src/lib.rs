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
pub use vm::{Module, Vm as Hebi, value::ValueRaw};

impl Module {
    /// Compile Hebi source code into a [`Module`].
    ///
    /// The resulting error can be rendered using [`Error::render`][error::Error::render].
    pub fn compile(s: &str) -> Result<Self> {
        let tokens = tokenize(&s);
        let ast = parse(&tokens)?;
        emit(&ast)
    }
}

impl std::str::FromStr for Module {
    type Err = error::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Self::compile(s)
    }
}

#[doc(hidden)]
pub mod __macro {
    pub use crate::vm::gc::{Gc, Root, StackRoot, ValueRoot};
    pub use crate::vm::value::{List, ValueRaw};
}

const _: () = assert!(
    cfg!(target_endian = "little"),
    "only little-endian architectures are supported"
);
