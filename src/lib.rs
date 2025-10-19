#![allow(dead_code, unused_variables)] // TEMP
#![allow(private_bounds)] // intentional
#![allow(unsafe_op_in_unsafe_fn)] // this largely doesn't help

// NOTE: spaces between `mod` ensure ordering

pub mod span;

pub mod error;
pub use error::{Error, Result};

#[macro_use]
mod intern;

mod module;

mod thin;

#[macro_use]
mod token;

mod ast;

mod parser;

#[macro_use]
mod codegen;

mod vm;

mod disasm;

pub use module::Module;
pub use vm::{Hebi, value::ValueRaw};

pub fn parse(code: &str) -> Result<ast::Ast> {
    let tokens = token::tokenize(code);
    parser::parse(&tokens)
}

const _: () = assert!(
    cfg!(target_endian = "little"),
    "only little-endian architectures are supported"
);
