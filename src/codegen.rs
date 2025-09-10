#[macro_use]
pub mod opcodes;

use crate::{
    ast::{self, Expr, Node, NodeList, f64n},
    span::Span,
    vm::{
        self, Chunk, Context, Control, FuncInfo,
        value::{Literal, ValueRaw},
    },
};
use bumpalo::{Bump, collections::Vec, vec};
use hashbrown::HashMap;
use opcodes::{FnId, Imm16, Imm24s, Instruction, Lit, Reg, asm, i24};
use std::borrow::Cow;

use crate::{
    ast::{Ast, Ident, Stmt},
    error::{Result, error},
};

// TODO: basic optimizations (configurable)
// - peephole:
//   - const eval
// - jump chaining
// - dead code elimination
//   - mark bb exit (break, return, continue, if),
//     any emitted code is discarded until the exit label is bound

macro_rules! f {
    (&$m:expr) => {
        unsafe {
            debug_assert!(!($m).func_stack.is_empty());
            ($m).func_stack.last().unwrap_unchecked()
        }
    };

    ($m:expr) => {
        unsafe {
            debug_assert!(!($m).func_stack.is_empty());
            ($m).func_stack.last_mut().unwrap_unchecked()
        }
    };
}

const NO_SPAN: Span = Span::empty();

pub fn emit(ast: &Ast) -> Result<Chunk> {
    let buf = &Bump::new();
    let mut m = State {
        buf,
        func_stack: vec![in buf],
        func_table: vec![in buf],
    };
    let main = emit_func(
        &mut m,
        "@main".into(),
        Span::empty(),
        NodeList::empty(ast),
        &[],
        ast.root().body(),
    )?;

    Ok(Chunk::new(main, m.func_table.into_iter().collect()))
}

struct State<'a> {
    buf: &'a Bump,
    func_stack: Vec<'a, FunctionState<'a>>,
    func_table: Vec<'a, FuncInfo>,
}

struct FunctionState<'a> {
    name: Cow<'a, str>,
    scopes: Vec<'a, Vec<'a, Local<'a>>>,
    ra: RegAlloc,
    code: Vec<'a, Instruction>,
    literals: Literals<'a>,
    loop_: Option<Loop<'a>>,

    dbg: FunctionDebug<'a>,
}

struct FunctionDebug<'a> {
    spans: Vec<'a, Span>,
    locals: Vec<'a, vm::dbg::Local>,
}

struct RegAlloc {
    current: u8,
    num: u8,
}

struct Literals<'a> {
    flat: Vec<'a, Literal>,
    ints: HashMap<i64, Lit, rustc_hash::FxBuildHasher, &'a Bump>,
    floats: HashMap<f64n, Lit, rustc_hash::FxBuildHasher, &'a Bump>,
    strings: HashMap<&'a str, Lit, rustc_hash::FxBuildHasher, &'a Bump>,
}

impl<'a> Literals<'a> {
    fn new(buf: &'a Bump) -> Self {
        Self {
            flat: vec![in buf],
            ints: HashMap::with_capacity_and_hasher_in(
                16,
                rustc_hash::FxBuildHasher::default(),
                buf,
            ),
            floats: HashMap::with_capacity_and_hasher_in(
                16,
                rustc_hash::FxBuildHasher::default(),
                buf,
            ),
            strings: HashMap::with_capacity_and_hasher_in(
                16,
                rustc_hash::FxBuildHasher::default(),
                buf,
            ),
        }
    }

    fn next_id(flat: &mut Vec<'a, Literal>, span: Span) -> Result<Lit> {
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

    fn f64(&mut self, v: f64n, span: Span) -> Result<Lit> {
        match self.floats.entry(v) {
            hashbrown::hash_map::Entry::Occupied(entry) => Ok(*entry.get()),
            hashbrown::hash_map::Entry::Vacant(entry) => {
                let id = Self::next_id(&mut self.flat, span)?;
                self.flat.push(Literal::Float(v.get()));
                entry.insert(id);

                Ok(id)
            }
        }
    }

    fn str(&mut self, v: &str, span: Span) -> Result<Lit> {
        let s = self.strings.allocator().alloc_str(v);
        match self.strings.entry(s) {
            hashbrown::hash_map::Entry::Occupied(entry) => Ok(*entry.get()),
            hashbrown::hash_map::Entry::Vacant(entry) => {
                let id = Self::next_id(&mut self.flat, span)?;
                self.flat.push(Literal::String(v.to_owned()));
                entry.insert(id);

                Ok(id)
            }
        }
    }
}

impl RegAlloc {
    fn new() -> Self {
        Self { current: 0, num: 0 }
    }

    fn alloc(&mut self, span: Span) -> Result<Reg> {
        if self.current == u8::MAX {
            return error("too many registers", span).into();
        }

        let r = unsafe { Reg::new_unchecked(self.current) };

        self.current += 1;
        if self.current > self.num {
            self.num = self.current;
        }

        Ok(r)
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

struct Local<'a> {
    name: Cow<'a, str>,
    span: Span,
    reg: Reg,
}

struct Loop<'a> {
    /// Points to first instruction in the loop body.
    entry: BackwardLabel,

    /// Points to after the last instruction in the loop body.
    exit: ForwardLabel<'a>,
}

struct ForwardLabel<'a> {
    /// Position of targets to patch
    patch_targets: Vec<'a, usize>,
}

impl<'a> ForwardLabel<'a> {
    fn add_target(&mut self, pos: usize) {
        self.patch_targets.push(pos);
    }
}

struct BackwardLabel {
    /// Instruction position
    pos: usize,
}

impl BackwardLabel {
    fn offset(&self, jmp_pos: usize, span: Span) -> Result<Imm24s> {
        let offset = i24::try_from((self.pos as isize) - (jmp_pos as isize))
            .map_err(|_| error("jump offset exceeds u24::MAX", span))?;
        Ok(unsafe { Imm24s::new_unchecked(offset) })
    }
}

impl<'a> FunctionState<'a> {
    fn new(name: Cow<'a, str>, buf: &'a Bump) -> Self {
        Self {
            name,
            scopes: vec![in buf],
            ra: RegAlloc::new(),
            code: vec![in buf],
            literals: Literals::new(buf),
            loop_: None,

            dbg: FunctionDebug {
                spans: vec![in buf],
                locals: vec![in buf],
            },
        }
    }

    fn begin_scope(&mut self, buf: &'a Bump) {
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

    fn begin_loop(&mut self, buf: &'a Bump) -> Option<Loop<'a>> {
        let entry = self.backward_label();
        let exit = self.forward_label(buf);
        self.loop_.replace(Loop { entry, exit })
    }

    fn end_loop(&mut self, prev: Option<Loop<'a>>) -> Result<()> {
        let loop_ = std::mem::replace(&mut self.loop_, prev).expect("some loop");

        self.bind_forward_label(loop_.exit)
    }

    fn reg(&mut self, span: Span) -> Result<Reg> {
        self.ra.alloc(span)
    }

    fn rfree(&mut self, reg: Reg) {
        self.ra.free(reg);
    }

    fn decl_local(&mut self, name: &'a str, reg: Reg, span: Span) -> Result<()> {
        let scope = self
            .scopes
            .last_mut()
            .expect("must always have at least one scope");
        scope.push(Local {
            name: name.into(),
            span,
            reg,
        });
        self.dbg.locals.push(vm::dbg::Local { span, reg });
        Ok(())
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            for local in scope {
                if local.name == name {
                    return Some(Symbol::Local(local.reg));
                }
            }
        }

        None
    }

    fn resolve_in_scope(&self, name: &str) -> Option<Symbol> {
        for local in self.scopes.last()? {
            if local.name == name {
                return Some(Symbol::Local(local.reg));
            }
        }

        None
    }

    fn emit(&mut self, inst: Instruction, span: Span) {
        self.code.push(inst);
        self.dbg.spans.push(span);
        // TODO: peep-opt
    }

    fn forward_label(&self, buf: &'a Bump) -> ForwardLabel<'a> {
        ForwardLabel {
            patch_targets: vec![in buf],
        }
    }

    fn backward_label(&self) -> BackwardLabel {
        BackwardLabel {
            pos: self.code.len(),
        }
    }

    fn bind_forward_label(&mut self, label: ForwardLabel) -> Result<()> {
        let pos = self.code.len();
        let span = *self.dbg.spans.last().unwrap();
        for target in label.patch_targets {
            let offset = i24::try_from((pos - target) as isize)
                .map_err(|_| error("jump offset out of bounds for i24", span))?;

            let span = self.dbg.spans[target];
            let ins = &mut self.code[target];
            let Instruction::Jmp { rel } = ins else {
                return error("invalid label referree", span).into();
            };
            *rel = unsafe { Imm24s::new_unchecked(offset) }
        }

        Ok(())
    }
}

impl<'a> State<'a> {
    fn begin_scope(&mut self) {
        f!(self).begin_scope(self.buf);
    }

    fn end_scope(&mut self) {
        f!(self).end_scope()
    }

    fn begin_loop(&mut self) -> Option<Loop<'a>> {
        f!(self).begin_loop(self.buf)
    }

    fn end_loop(&mut self, prev: Option<Loop<'a>>) -> Result<()> {
        f!(self).end_loop(prev)
    }

    fn reg(&mut self, span: Span) -> Result<Reg> {
        f!(self).reg(span)
    }

    fn rfree(&mut self, reg: Reg) {
        f!(self).rfree(reg);
    }

    fn decl_local(&mut self, name: &'a str, reg: Reg, span: Span) -> Result<()> {
        f!(self).decl_local(name, reg, span)
    }

    fn resolve(&self, name: &str, span: Span) -> Result<Symbol> {
        if let Some(symbol) = f!(&self).resolve(name) {
            return Ok(symbol);
        }

        error("could not resolve name", span).into()
    }

    fn resolve_in_scope(&self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = f!(&self).resolve_in_scope(name) {
            return Some(symbol);
        }

        None
    }

    fn emit(&mut self, inst: Instruction, span: Span) {
        f!(self).emit(inst, span)
    }

    fn finish_function(&mut self, f: FuncInfo, span: Span) -> Result<FnId> {
        let id = self.func_table.len();
        if id > u16::MAX as usize {
            return error("too many functions", span).into();
        }
        let id = unsafe { FnId::new_unchecked(id as u16) };

        self.func_table.push(f);

        Ok(id)
    }
}

enum Symbol {
    Local(Reg),
}

fn emit_func<'a>(
    m: &mut State<'a>,
    name: Cow<'a, str>,
    span: Span,
    params: NodeList<'a, Ident>,
    param_spans: &'a [Span],
    body: NodeList<'a, Stmt>,
) -> Result<FnId> {
    if params.len() > u8::MAX as usize {
        return error(
            "too many parameters, maximum is 256",
            *param_spans.last().unwrap(),
        )
        .into();
    }

    let f = FunctionState::new(name, m.buf);
    m.func_stack.push(f);

    {
        m.begin_scope();
        // NOTE: don't free `ret_reg` as locals are placed above it,
        //       will be freed in `end_scope`.
        let ret_reg = m.reg(span)?;
        for (param, &span) in params.iter().zip(param_spans.iter()) {
            let dst = m.reg(span)?;
            m.decl_local(param.get(), dst, span)?;
        }

        let _ = emit_stmt_list_with_tail(m, body, Some(ret_reg).into())?;

        m.emit(asm::ret(), NO_SPAN);
        m.end_scope();
    }
    let f = m.func_stack.pop().expect("function stack is empty");

    let f = FuncInfo::new(
        f.name.into_owned(),
        params.len() as u8,
        f.ra.num,
        f.code.into_iter().collect(),
        f.literals.flat.into_iter().collect(),
        crate::vm::dbg::FuncDebugInfo {
            spans: f.dbg.spans.into_iter().collect(),
            locals: f.dbg.locals.into_iter().collect(),
        },
    );

    let id = m.finish_function(f, span)?;

    Ok(id)
}

#[derive(Clone, Copy)]
#[must_use = "unused register"]
struct MaybeReg(Option<Reg>);

const NO_REG: MaybeReg = MaybeReg(None);

impl MaybeReg {
    fn inner(self) -> Option<Reg> {
        self.into()
    }
}

impl From<Option<Reg>> for MaybeReg {
    fn from(value: Option<Reg>) -> Self {
        Self(value)
    }
}

impl From<MaybeReg> for Option<Reg> {
    fn from(value: MaybeReg) -> Self {
        value.0
    }
}

fn emit_stmt_list_with_tail<'a>(
    m: &mut State<'a>,
    list: NodeList<'a, Stmt>,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    let (stmt_list, tail) = match list.last().map(|node| node.kind()) {
        Some(ast::StmtKind::StmtExpr(tail)) => (list.slice(0..list.len() - 1).unwrap(), Some(tail)),
        _ => (list, None),
    };

    for stmt in stmt_list {
        emit_stmt(m, stmt)?;
    }

    match (tail, dst.inner()) {
        (None, None) => Ok(NO_REG),
        (None, Some(dst)) => {
            m.emit(asm::lnil(dst), NO_SPAN);

            Ok(MaybeReg(Some(dst)))
        }
        (Some(tail), None) => {
            let dst = emit_expr(m, tail.inner(), tail.inner_span(), NO_REG)?;
            Ok(dst)
        }
        (Some(tail), Some(dst)) => {
            emit_expr_into(m, tail.inner(), tail.inner_span(), dst)?;
            Ok(MaybeReg(Some(dst)))
        }
    }
}

fn emit_stmt<'a>(m: &mut State<'a>, stmt: Node<'a, Stmt>) -> Result<()> {
    match stmt.kind() {
        ast::StmtKind::Var(node) => emit_stmt_var(m, node)?,
        ast::StmtKind::Loop(node) => emit_stmt_loop(m, node)?,
        ast::StmtKind::StmtExpr(node) => {
            let _ = emit_expr(m, node.inner(), node.inner_span(), NO_REG)?;
        }
        ast::StmtKind::FuncDecl(node) => todo!(),
    }

    Ok(())
}

fn emit_stmt_var<'a>(m: &mut State<'a>, var: Node<'a, ast::Var>) -> Result<()> {
    let (redeclaration, dst) = match m.resolve_in_scope(var.name().get()) {
        Some(Symbol::Local(reg)) => (true, reg),
        _ => (false, m.reg(var.name_span())?),
    };
    emit_expr_into(m, var.value(), var.value_span(), dst)?;
    if !redeclaration {
        m.decl_local(var.name().get(), dst, var.name_span())?;
    }

    Ok(())
}

fn emit_stmt_loop<'a>(m: &mut State<'a>, loop_: Node<'a, ast::Loop>) -> Result<()> {
    let prev_loop = m.begin_loop();
    m.begin_scope();

    for stmt in loop_.body() {
        emit_stmt(m, stmt)?;
    }

    m.end_scope();

    // unconditional jump back to start
    let span = loop_.body_spans().last().copied().unwrap_or_default();
    let pos = f!(&m).code.len();
    let rel = f!(&m)
        .loop_
        .as_ref()
        .expect("some loop")
        .entry
        .offset(pos, span)?;
    m.emit(asm::jmp(rel), span);

    m.end_loop(prev_loop)?;

    Ok(())
}

fn emit_expr<'a>(
    m: &mut State<'a>,
    expr: Node<'a, Expr>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    match expr.kind() {
        ast::ExprKind::Return(node) => todo!(),
        ast::ExprKind::Break(node) => emit_expr_break(m, node, span, dst),
        ast::ExprKind::Continue(node) => emit_expr_continue(m, node, span, dst),
        ast::ExprKind::IfSimple(node) => todo!(),
        ast::ExprKind::IfMulti(node) => todo!(),
        ast::ExprKind::Block(node) => emit_expr_block(m, node, span, dst),
        ast::ExprKind::FuncAnon(node) => todo!(),
        ast::ExprKind::GetVar(node) => emit_expr_get_var(m, node, span, dst),
        ast::ExprKind::SetVar(node) => emit_expr_set_var(m, node, span, dst),
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
        ast::ExprKind::Float32(node) => emit_expr_float(m, FloatExpr::F32(node), span, dst),
        ast::ExprKind::Float64(node) => emit_expr_float(m, FloatExpr::F64(node), span, dst),
        ast::ExprKind::Bool(node) => emit_expr_bool(m, node, span, dst),
        ast::ExprKind::Str(node) => emit_expr_str(m, node, span, dst),
        ast::ExprKind::Nil(node) => emit_expr_nil(m, node, span, dst),
    }
}

fn emit_expr_into<'a>(m: &mut State<'a>, expr: Node<'a, Expr>, span: Span, dst: Reg) -> Result<()> {
    if let Some(src) = emit_expr(m, expr, span, Some(dst).into())?.inner() {
        if src.get() != dst.get() {
            m.emit(asm::mov(dst, src), span);
        }
    }

    Ok(())
}

fn emit_expr_break<'a>(
    m: &mut State<'a>,
    brk: Node<'a, ast::Break>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    let f = f!(m);
    let Some(mut loop_) = f.loop_.take() else {
        return error("cannot use `break` outside of loop", span).into();
    };

    // break = unconditional jump to loop exit
    let pos = f.code.len();
    let rel = unsafe { Imm24s::new_unchecked(i24::ZERO) };

    f.emit(asm::jmp(rel), span);
    loop_.exit.add_target(pos);

    f.loop_ = Some(loop_);

    Ok(NO_REG)
}

fn emit_expr_continue<'a>(
    m: &mut State<'a>,
    brk: Node<'a, ast::Continue>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    let Some(loop_) = f!(m).loop_.take() else {
        return error("cannot use `continue` outside of loop", span).into();
    };

    // continue = unconditional jump back to loop entry
    let pos = f!(m).code.len();
    let rel = loop_.entry.offset(pos, span)?;

    m.emit(asm::jmp(rel), span);

    f!(m).loop_ = Some(loop_);

    Ok(NO_REG)
}

fn emit_expr_block<'a>(
    m: &mut State<'a>,
    block: Node<'a, ast::Block>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    m.begin_scope();
    let dst = emit_stmt_list_with_tail(m, block.body(), dst)?;
    m.end_scope();
    Ok(dst)
}

fn emit_expr_get_var<'a>(
    m: &mut State<'a>,
    get: Node<'a, ast::GetVar>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    // variable must exist at this point
    let reg = match m.resolve(get.name().get(), get.name_span())? {
        Symbol::Local(reg) => reg,
    };

    Ok(MaybeReg(Some(reg)))
}

fn emit_expr_set_var<'a>(
    m: &mut State<'a>,
    set: Node<'a, ast::SetVar>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    // variable must exist at this point
    match m.resolve(set.base().name().get(), set.base().name_span())? {
        // when adding other symbols, remember to error out here,
        // only local variables may be assigned to
        Symbol::Local(dst) => {
            emit_expr_into(m, set.value(), span, dst)?;
        }
    };

    Ok(NO_REG)
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

fn emit_expr_int(m: &mut State, int: IntExpr, span: Span, dst: MaybeReg) -> Result<MaybeReg> {
    let f = f!(m);
    let v: i64 = int.value();
    if let Some(dst) = dst.inner() {
        if v <= i16::MAX as i64 {
            let v = unsafe { Imm16::new_unchecked(v as i16) };
            f.emit(asm::lsmi(dst, v), span);
        } else {
            let id = f.literals.i64(v as i64, span)?;
            f.emit(asm::lint(dst, id), span);
        }
    }

    Ok(NO_REG)
}

enum FloatExpr<'a> {
    F32(Node<'a, ast::Float32>),
    F64(Node<'a, ast::Float64>),
}

impl<'a> FloatExpr<'a> {
    fn value(&self) -> f64n {
        match self {
            FloatExpr::F32(node) => f64n::new(*node.value() as f64),
            FloatExpr::F64(node) => *node.get(),
        }
    }
}

fn emit_expr_float(m: &mut State, float: FloatExpr, span: Span, dst: MaybeReg) -> Result<MaybeReg> {
    let f = f!(m);
    let v: f64n = float.value();
    if let Some(dst) = dst.inner() {
        let id = f.literals.f64(v, span)?;
        f.emit(asm::lnum(dst, id), span);
    }

    Ok(NO_REG)
}

fn emit_expr_bool(
    m: &mut State,
    bool: Node<ast::Bool>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    let f = f!(m);
    let v: bool = *bool.value();
    if let Some(dst) = dst.inner() {
        match v {
            true => f.emit(asm::ltrue(dst), span),
            false => f.emit(asm::lfalse(dst), span),
        }
    }

    Ok(NO_REG)
}

fn emit_expr_str(
    m: &mut State,
    str: Node<ast::Str>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    let f = f!(m);
    let v: &str = str.get();
    if let Some(dst) = dst.inner() {
        let id = f.literals.str(v, span)?;
        f.emit(asm::lstr(dst, id), span);
    }

    Ok(NO_REG)
}

fn emit_expr_nil(
    m: &mut State,
    nil: Node<ast::Nil>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    let f = f!(m);
    if let Some(dst) = dst.inner() {
        f.emit(asm::lnil(dst), span);
    }

    Ok(NO_REG)
}

#[cfg(test)]
mod tests;
