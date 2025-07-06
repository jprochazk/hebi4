pub mod asm;

use std::borrow::Cow;

use bumpalo::{Bump, collections::Vec, vec};

use crate::{
    ast::{Ast, Ident, Stmt},
    error::{Error, Result, error},
};

pub fn emit(ast: &Ast) -> Result<Module> {
    todo!()
}

pub struct Module {}

struct ModuleState {}

pub struct Function {}

struct FunctionState<'func, 'ast, 'bump> {
    parent: Option<&'func mut FunctionState<'func, 'ast, 'bump>>,

    name: Cow<'ast, str>,

    // todo: params
    scopes: Vec<'bump, Vec<'bump, Var<'ast>>>,
}

struct Var<'ast> {
    name: Cow<'ast, str>,
    register: u8,
}

impl<'func, 'ast, 'bump> FunctionState<'func, 'ast, 'bump> {
    fn new(
        parent: Option<&'func mut FunctionState<'func, 'ast, 'bump>>,
        name: Cow<'ast, str>,
        buf: &'bump Bump,
    ) -> Self {
        Self {
            parent,
            name,
            scopes: vec![in buf; vec![in buf]],
        }
    }
}

fn emit_func<'func, 'ast, 'bump>(
    m: &mut ModuleState,
    f: Option<&'func mut FunctionState<'func, 'ast, 'bump>>,
    buf: &'bump Bump,
    ast: &'ast Ast,
    name: Cow<'ast, str>,
    params: &'ast [Ident],
    body: &'ast [Stmt],
) -> Result<Function> {
    let mut f = FunctionState::new(f, name, buf);

    todo!()
}
