#![allow(dead_code, unused_variables)] // TEMP
#![allow(private_bounds)] // intentional
#![allow(unsafe_op_in_unsafe_fn)] // this largely doesn't help

// NOTE: spaces between `mod` ensure ordering

pub mod span;

pub mod error;
pub use error::{Error, Result};

#[macro_use]
mod intern;

pub mod module;

mod tag;
mod thin;

#[macro_use]
pub mod token;

pub mod ast;

pub mod parser;

#[macro_use]
pub mod codegen;

mod vm;
pub use vm::{gc, value};

pub mod disasm;

pub mod core;

pub mod prelude {
    pub use crate::{
        codegen::EmitOptions,
        error::{Result as HebiResult, error},
        module::{
            Extern, Module, NativeFunction, NativeModule, Param, Ret, TryFromHebiValueRaw,
            TryIntoHebiValueRaw, Value, f,
        },
        vm::{
            Hebi, Stdio, StdioWrite,
            gc::{
                self, GcAnyRef, GcAnyRoot, GcRef, GcRoot, GcUninitRoot, Heap, ValueRef, ValueRoot,
                let_root, let_root_unchecked, reroot,
            },
            value::{self, ExternAny, List, Str, Table, extern_data, host_function::Context},
        },
    };
}

pub fn parse(code: &str) -> Result<ast::Ast> {
    let tokens = token::tokenize(code);
    parser::parse(&tokens)
}

const _: () = assert!(
    cfg!(target_endian = "little"),
    "only little-endian architectures are supported"
);
