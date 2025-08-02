#[macro_use]
pub mod opcodes;

use crate::{
    ast::{self, Expr, Node, NodeList},
    span::Span,
    vm::{Context, FuncInfo, Module, Value},
};
use bumpalo::{Bump, collections::Vec, vec};
use opcodes::{Instruction, Reg, asm};
use std::borrow::Cow;

use crate::{
    ast::{Ast, Ident, Stmt},
    error::{Error, Result, error},
};

pub fn emit(ast: &Ast) -> Result<Module> {
    let buf = &Bump::new();
    let m = &mut ModuleState {};

    let main = emit_func(
        m,
        None,
        buf,
        "@main".into(),
        NodeList::empty(ast),
        &[],
        ast.root().body(),
    )?;

    todo!()
}

struct ModuleState {}

struct FunctionState<'func, 'ast, 'bump> {
    parent: Option<&'func mut FunctionState<'func, 'ast, 'bump>>,
    name: Cow<'ast, str>,
    scopes: Vec<'bump, Vec<'bump, Local<'ast>>>,
    ra: RegAlloc,
    code: Vec<'bump, Instruction>,
}

struct RegAlloc {
    current: u8,
    num: u8,
}

impl RegAlloc {
    fn new() -> Self {
        Self { current: 0, num: 0 }
    }

    fn alloc(&mut self) -> Reg {
        let r = unsafe { Reg::new_unchecked(self.current) };

        self.current += 1;
        if self.current > self.num {
            self.num = self.current;
        }

        r
    }

    fn free(&mut self, r: Reg) {
        if self.current != r.get() + 1 {
            panic!("registers freed out of order");
        }
        self.current = r.get();
    }

    fn reset_to(&mut self, r: Reg) {
        self.current = r.get();
    }
}

struct Local<'ast> {
    name: Cow<'ast, str>,
    span: Span,
    reg: Reg,
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
            scopes: vec![in buf],
            ra: RegAlloc::new(),
            code: vec![in buf],
        }
    }

    fn begin_scope(&mut self, buf: &'bump Bump) {
        self.scopes.push(vec![in buf]);
    }

    fn end_scope(&mut self) {
        let scope = self
            .scopes
            .pop()
            .expect("`end_scope` called without any scopes");
        if let Some(var) = scope.first() {
            self.ra.reset_to(var.reg);
        }
    }

    fn reg(&mut self) -> Reg {
        self.ra.alloc()
    }

    fn rfree(&mut self, reg: Reg) {
        self.ra.free(reg);
    }

    fn decl_local(&mut self, name: &'ast str, span: Span) {
        let reg = self.reg();
        let scope = self
            .scopes
            .last_mut()
            .expect("must always have at least one scope");
        scope.push(Local {
            name: name.into(),
            span,
            reg,
        })
    }

    fn emit(&mut self, inst: Instruction, span: Span) {
        self.code.push(inst);
        // TODO: spans
        // TODO: peep-opt
    }
}

fn emit_func<'func, 'ast, 'bump>(
    m: &mut ModuleState,
    parent: Option<&'func mut FunctionState<'func, 'ast, 'bump>>,
    buf: &'bump Bump,
    name: Cow<'ast, str>,
    params: NodeList<'ast, Ident>,
    param_spans: &'ast [Span],
    body: NodeList<'ast, Stmt>,
) -> Result<FuncInfo> {
    let f = &mut FunctionState::new(parent, name, buf);

    f.begin_scope(buf);
    // NOTE: don't free `ret_reg` as locals are placed above it,
    //       will be freed in `end_scope`.
    let ret_reg = f.reg();
    for (param, &span) in params.iter().zip(param_spans.iter()) {
        f.decl_local(param.get(), span);
    }

    emit_stmt_list_with_tail(f, body, Some(ret_reg))?;

    f.end_scope();

    todo!()
}

fn emit_stmt_list_with_tail(
    f: &mut FunctionState,
    list: NodeList<Stmt>,
    dst: Option<Reg>,
) -> Result<()> {
    let (stmt_list, tail) = match list.last().map(|node| node.kind()) {
        Some(ast::StmtKind::StmtExpr(tail)) => (list.slice(0..list.len() - 1).unwrap(), Some(tail)),
        _ => (list, None),
    };

    for stmt in stmt_list {
        emit_stmt(f, stmt)?;
    }

    use asm::*;
    match (tail, dst) {
        (None, None) => {}
        (None, Some(dst)) => {
            f.emit(lnil(dst), Span::empty());
        }
        (Some(tail), None) => {
            let _ = emit_expr(f, tail.inner(), tail.inner_span(), None)?;
        }
        (Some(tail), Some(dst)) => {
            emit_expr_into(f, tail.inner(), tail.inner_span(), dst)?;
        }
    }

    Ok(())
}

fn emit_stmt(f: &mut FunctionState, stmt: Node<Stmt>) -> Result<()> {
    match stmt.kind() {
        ast::StmtKind::Var(node) => todo!(),
        ast::StmtKind::Loop(node) => todo!(),
        ast::StmtKind::StmtExpr(node) => {
            let _ = emit_expr(f, node.inner(), node.inner_span(), None)?;
        }
        ast::StmtKind::FuncDecl(node) => todo!(),
    }

    Ok(())
}

fn emit_expr(
    f: &mut FunctionState,
    expr: Node<Expr>,
    span: Span,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    match expr.kind() {
        ast::ExprKind::Return(node) => todo!(),
        ast::ExprKind::Break(node) => todo!(),
        ast::ExprKind::Continue(node) => todo!(),
        ast::ExprKind::IfSimple(node) => todo!(),
        ast::ExprKind::IfMulti(node) => todo!(),
        ast::ExprKind::Block(node) => todo!(),
        ast::ExprKind::FuncAnon(node) => todo!(),
        ast::ExprKind::GetVar(node) => todo!(),
        ast::ExprKind::SetVar(node) => todo!(),
        ast::ExprKind::GetField(node) => todo!(),
        ast::ExprKind::SetField(node) => todo!(),
        ast::ExprKind::GetIndex(node) => todo!(),
        ast::ExprKind::SetIndex(node) => todo!(),
        ast::ExprKind::Call(node) => todo!(),
        ast::ExprKind::CallObject(node) => todo!(),
        ast::ExprKind::Infix(node) => todo!(),
        ast::ExprKind::Prefix(node) => todo!(),
        ast::ExprKind::Array(node) => todo!(),
        ast::ExprKind::Object(node) => todo!(),
        ast::ExprKind::Int32(node) => todo!(),
        ast::ExprKind::Int64(node) => todo!(),
        ast::ExprKind::Float32(node) => todo!(),
        ast::ExprKind::Float64(node) => todo!(),
        ast::ExprKind::Bool(node) => todo!(),
        ast::ExprKind::Str(node) => todo!(),
        ast::ExprKind::Nil(node) => todo!(),
    }
}

fn emit_expr_into(f: &mut FunctionState, expr: Node<Expr>, span: Span, dst: Reg) -> Result<()> {
    if let Some(src) = emit_expr(f, expr, span, Some(dst))? {
        if src.get() != dst.get() {
            use asm::*;
            f.emit(mov(dst, src), span);
        }
    }

    Ok(())
}
