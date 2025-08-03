#[macro_use]
pub mod opcodes;

use crate::{
    ast::{self, Expr, Node, NodeList},
    span::Span,
    vm::{self, Context, FuncInfo, Literal, Module, Value},
};
use bumpalo::{Bump, collections::Vec, vec};
use hashbrown::HashMap;
use opcodes::{Imm16, Instruction, Lit, Reg, asm};
use std::borrow::Cow;

use crate::{
    ast::{Ast, Ident, Stmt},
    error::{Result, error},
};

macro_rules! f {
    ($m:expr) => {
        unsafe {
            debug_assert!(!($m).funcs.is_empty());
            ($m).funcs.last_mut().unwrap_unchecked()
        }
    };
}

macro_rules! asm {
    (in $m:ident; $op:ident $($args:expr),*) => {
        $m.emit(asm::$op($($args),*), Span::empty())
    };
    (in $m:ident at $span:expr; $op:ident $($args:expr),*) => {
        $m.emit(asm::$op($($args),*), $span)
    };
}

pub fn emit(ast: &Ast) -> Result<Module> {
    let buf = &Bump::new();
    let mut m = ModuleState {
        buf,
        funcs: vec![in buf],
        literals: Literals::new(buf),
    };
    let main = emit_func(
        &mut m,
        "@main".into(),
        Span::empty(),
        NodeList::empty(ast),
        &[],
        ast.root().body(),
    )?;

    Ok(Module {
        main,
        literals: m.literals.flat.into_iter().collect(),
    })
}

struct ModuleState<'ast, 'bump> {
    buf: &'bump Bump,
    funcs: Vec<'bump, FunctionState<'ast, 'bump>>,
    literals: Literals<'bump>,
}

struct FunctionState<'ast, 'bump> {
    name: Cow<'ast, str>,
    scopes: Vec<'bump, Vec<'bump, Local<'ast>>>,
    ra: RegAlloc,
    code: Vec<'bump, Instruction>,
    dbg: FunctionDebug<'bump>,
}

struct FunctionDebug<'bump> {
    spans: Vec<'bump, Span>,
    locals: Vec<'bump, vm::dbg::Local>,
}

struct RegAlloc {
    current: u8,
    num: u8,
}

struct Literals<'bump> {
    flat: Vec<'bump, Literal>,
    ints: HashMap<i64, Lit, rustc_hash::FxBuildHasher, &'bump Bump>,
}

impl<'bump> Literals<'bump> {
    fn new(buf: &'bump Bump) -> Self {
        Self {
            flat: vec![in buf],
            ints: HashMap::with_capacity_and_hasher_in(
                16,
                rustc_hash::FxBuildHasher::default(),
                buf,
            ),
        }
    }

    fn next_id(flat: &mut Vec<'bump, Literal>, span: Span) -> Result<Lit> {
        let id = flat.len();
        if id > u16::MAX as usize {
            return error("too many literals", span).into();
        }
        Ok(unsafe { Lit::new_unchecked(id as u16) })
    }

    fn i64(&mut self, v: i64, span: Span) -> Result<Lit> {
        match self.ints.entry(v) {
            hashbrown::hash_map::Entry::Occupied(entry) => Ok(*entry.get()),
            hashbrown::hash_map::Entry::Vacant(entry) => {
                let id = Self::next_id(&mut self.flat, span)?;
                self.flat.push(Literal::Int(v));
                entry.insert(id);

                Ok(id)
            }
        }
    }

    fn func(&mut self, v: FuncInfo, span: Span) -> Result<Lit> {
        let id = Self::next_id(&mut self.flat, span)?;
        self.flat.push(Literal::Func(Box::new(v)));

        Ok(id)
    }
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

impl<'ast, 'bump> FunctionState<'ast, 'bump> {
    fn new(name: Cow<'ast, str>, buf: &'bump Bump) -> Self {
        Self {
            name,
            scopes: vec![in buf],
            ra: RegAlloc::new(),
            code: vec![in buf],

            dbg: FunctionDebug {
                spans: vec![in buf],
                locals: vec![in buf],
            },
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
        });
        self.dbg.locals.push(vm::dbg::Local { span, reg })
    }

    fn emit(&mut self, inst: Instruction, span: Span) {
        self.code.push(inst);
        self.dbg.spans.push(span);
        // TODO: peep-opt
    }
}

impl<'ast, 'bump> ModuleState<'ast, 'bump> {
    fn begin_scope(&mut self, buf: &'bump Bump) {
        f!(self).begin_scope(buf);
    }

    fn end_scope(&mut self) {
        f!(self).end_scope()
    }

    fn reg(&mut self) -> Reg {
        f!(self).ra.alloc()
    }

    fn rfree(&mut self, reg: Reg) {
        f!(self).ra.free(reg);
    }

    fn decl_local(&mut self, name: &'ast str, span: Span) {
        f!(self).decl_local(name, span)
    }

    fn emit(&mut self, inst: Instruction, span: Span) {
        f!(self).emit(inst, span)
    }
}

fn emit_func<'ast, 'bump>(
    m: &mut ModuleState<'ast, 'bump>,
    name: Cow<'ast, str>,
    span: Span,
    params: NodeList<'ast, Ident>,
    param_spans: &'ast [Span],
    body: NodeList<'ast, Stmt>,
) -> Result<Lit> {
    if params.len() > u8::MAX as usize {
        return error(
            "too many parameters, maximum is 256",
            *param_spans.last().unwrap(),
        )
        .into();
    }

    let f = FunctionState::new(name, m.buf);
    m.funcs.push(f);

    {
        m.begin_scope(m.buf);
        // NOTE: don't free `ret_reg` as locals are placed above it,
        //       will be freed in `end_scope`.
        let ret_reg = m.reg();
        for (param, &span) in params.iter().zip(param_spans.iter()) {
            m.decl_local(param.get(), span);
        }

        emit_stmt_list_with_tail(m, body, Some(ret_reg))?;

        m.end_scope();
    }
    let f = m.funcs.pop().expect("function stack is empty");

    let f = FuncInfo {
        name: f.name.into(),
        nparams: params.len() as u8,
        nstack: f.ra.num,
        code: f.code.into_iter().collect(),
        dbg: Box::new(crate::vm::dbg::FuncDebugInfo {
            spans: f.dbg.spans.into_iter().collect(),
            locals: f.dbg.locals.into_iter().collect(),
        }),
    };

    let id = m.literals.func(f, span)?;

    Ok(id)
}

fn emit_stmt_list_with_tail(
    m: &mut ModuleState,
    list: NodeList<Stmt>,
    dst: Option<Reg>,
) -> Result<()> {
    let (stmt_list, tail) = match list.last().map(|node| node.kind()) {
        Some(ast::StmtKind::StmtExpr(tail)) => (list.slice(0..list.len() - 1).unwrap(), Some(tail)),
        _ => (list, None),
    };

    for stmt in stmt_list {
        emit_stmt(m, stmt)?;
    }

    match (tail, dst) {
        (None, None) => {}
        (None, Some(dst)) => {
            asm! {
                in m;
                lnil dst
            };
        }
        (Some(tail), None) => {
            let _ = emit_expr(m, tail.inner(), tail.inner_span(), None)?;
        }
        (Some(tail), Some(dst)) => {
            emit_expr_into(m, tail.inner(), tail.inner_span(), dst)?;
        }
    }

    Ok(())
}

fn emit_stmt(m: &mut ModuleState, stmt: Node<Stmt>) -> Result<()> {
    match stmt.kind() {
        ast::StmtKind::Var(node) => todo!(),
        ast::StmtKind::Loop(node) => todo!(),
        ast::StmtKind::StmtExpr(node) => {
            let _ = emit_expr(m, node.inner(), node.inner_span(), None)?;
        }
        ast::StmtKind::FuncDecl(node) => todo!(),
    }

    Ok(())
}

fn emit_expr(
    m: &mut ModuleState,
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
        ast::ExprKind::Int32(node) => emit_expr_int(m, IntExpr::I32(node), span, dst),
        ast::ExprKind::Int64(node) => emit_expr_int(m, IntExpr::I64(node), span, dst),
        ast::ExprKind::Float32(node) => todo!(),
        ast::ExprKind::Float64(node) => todo!(),
        ast::ExprKind::Bool(node) => todo!(),
        ast::ExprKind::Str(node) => todo!(),
        ast::ExprKind::Nil(node) => todo!(),
    }
}

fn emit_expr_into(m: &mut ModuleState, expr: Node<Expr>, span: Span, dst: Reg) -> Result<()> {
    if let Some(src) = emit_expr(m, expr, span, Some(dst))? {
        if src.get() != dst.get() {
            asm! {
                in m at span;
                mov dst, src
            };
        }
    }

    Ok(())
}

enum IntExpr<'a> {
    I32(Node<'a, ast::Int32>),
    I64(Node<'a, ast::Int64>),
}

impl<'a> IntExpr<'a> {
    fn value(&self) -> i64 {
        match self {
            IntExpr::I32(node) => (*node.value()) as i64,
            IntExpr::I64(node) => *node.get(),
        }
    }
}

fn emit_expr_int(
    m: &mut ModuleState,
    expr: IntExpr,
    span: Span,
    dst: Option<Reg>,
) -> Result<Option<Reg>> {
    let f = f!(m);
    let v: i64 = expr.value();
    if let Some(dst) = dst {
        if v <= i16::MAX as i64 {
            let v = unsafe { Imm16::new_unchecked(v as i16) };
            asm! {
                in f at span;
                lsmi dst, v
            };
        } else {
            let id = m.literals.i64(v as i64, span)?;
            asm! {
                in f at span;
                lint dst, id
            }
        }
    }

    Ok(None)
}
