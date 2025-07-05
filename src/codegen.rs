use crate::{
    ast::Ast,
    error::{Error, Result, error},
};

pub fn emit(ast: &Ast) -> Result<Module> {
    todo!()
}

pub struct Module {}

struct State {
    module: Module,
}

impl State {
    #[inline]
    fn new() -> Self {
        Self { module: Module {} }
    }
}

fn emit_root(mut p: State, ast: &Ast) -> Result<Module> {
    for stmt in ast.root().body() {
        match stmt.kind() {
            crate::ast::StmtKind::Var(node) => todo!(),
            crate::ast::StmtKind::Loop(node) => todo!(),
            crate::ast::StmtKind::StmtExpr(node) => todo!(),
            crate::ast::StmtKind::FuncDecl(node) => todo!(),
        }
    }

    todo!()
}
