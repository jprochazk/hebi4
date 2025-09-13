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
use opcodes::{FnId, Imm8, Imm16, Imm24s, Instruction, Lit, Reg, asm, i24};
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

// TODO: for any stmt list, iterate over it twice, and pre-declare all functions
// in a special table. that table is then used to patch `fastcall` instructions
// with real function IDs once those IDs are resolved.
// in order to resolve the ID, the function must be emitted. the patching can
// be done at the end of every scope, it does not need to wait until all functions
// have been resolved, because it is impossible to call a function by function id
// unless it is declared within the current scope stack

/*

# emit_func(main) -> immediately declares `outer`, _then_ emits the body of `main`

# emit_func(outer) -> declares `inner`, emits `outer`
                   -> does NOT declare `inner_block`! it's not a direct child.
fn outer() do
    # emit_func(inner) -> emits `inner`
    fn inner() do
        does_not_exist()  # error
    end
    do
        # emit_func(inner_block) -> emits `inner_block`
        fn inner_block() do end

        # `inner_block` goes out of scope
    end

    outer()  # ok, recursion
    inner()  # ok
    inner_block()  # error

    # `inner` goes out of scope
end

outer()  # ok
inner()  # error

*/

pub fn emit(ast: &Ast) -> Result<Chunk> {
    let buf = &Bump::new();
    let mut m = State {
        buf,
        func_stack: vec![in buf],
        func_table: FunctionTable::new(buf),
    };

    // note: `main` cannot be called, so does not need a symbol
    let main = m
        .func_table
        .reserve("@main", Span::empty())
        .expect("should not fail to reserve main");

    let f = emit_func(
        &mut m,
        main,
        "@main".into(),
        Span::empty(),
        NodeList::empty(ast),
        &[],
        ast.root().body(),
    )?;

    m.func_table.define(main, f);

    Ok(Chunk::new(main, m.func_table.finish()))
}

struct State<'a> {
    buf: &'a Bump,
    func_stack: Vec<'a, FunctionState<'a>>,
    func_table: FunctionTable<'a>,
}

struct FunctionTable<'a> {
    entries: Vec<'a, FunctionTableEntry<'a>>,
}

enum FunctionTableEntry<'a> {
    Occupied(FuncInfo),
    Reserved(Cow<'a, str>, Span),
}

impl<'a> FunctionTable<'a> {
    fn new(buf: &'a Bump) -> Self {
        Self {
            entries: vec![in buf],
        }
    }

    fn reserve(&mut self, name: impl Into<Cow<'a, str>>, span: Span) -> Result<FnId> {
        let id = self.entries.len();
        if id >= u16::MAX as usize {
            return error(format!("too many functions, maximum is {}", u16::MAX), span).into();
        }
        let id = id as u16;

        self.entries
            .push(FunctionTableEntry::Reserved(name.into(), span));
        Ok(unsafe { FnId::new_unchecked(id) })
    }

    fn define(&mut self, id: FnId, info: FuncInfo) {
        use FunctionTableEntry as E;

        match self.entries.get_mut(id.zx()) {
            Some(E::Occupied(..)) => panic!("ICE: function defined twice"),
            Some(E::Reserved(name, span)) => {
                assert!(
                    info.name() == name,
                    "ICE: function definition mismatch: expected {name:?} for fn id {id}, got {:?}\n\
                    NOTE: function was declared at {span}",
                    info.name()
                );
            }
            None => {
                panic!("ICE: function finished before declaration");
            }
        }

        self.entries[id.zx()] = E::Occupied(info);
    }

    fn finish(self) -> Vec<'a, FuncInfo> {
        let mut out = Vec::with_capacity_in(self.entries.len(), self.entries.bump());
        for entry in self.entries {
            use FunctionTableEntry as E;
            match entry {
                E::Occupied(func_info) => out.push(func_info),
                E::Reserved(name, span) => {
                    panic!("ICE: function declared but not defined: {name:?} at {span}");
                }
            }
        }
        out
    }
}

struct FunctionState<'a> {
    name: Cow<'a, str>,
    scopes: Vec<'a, Scope<'a>>,
    ra: RegAlloc,
    code: Vec<'a, Instruction>,
    literals: Literals<'a>,
    loop_: Option<Loop<'a>>,

    dbg: FunctionDebug<'a>,
}

struct Scope<'a> {
    last_reg_in_prev_scope: u8,
    symbols: Vec<'a, Symbol<'a>>,
    undefined_functions: Vec<'a, FnId>,
}

impl<'a> Scope<'a> {
    fn push(&mut self, symbol: Symbol<'a>) {
        self.symbols.push(symbol)
    }
}

struct FunctionDebug<'a> {
    spans: Vec<'a, Span>,
    locals: Vec<'a, vm::dbg::Local>,
}

struct RegAlloc {
    current: u8,
    num: u8,
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

    fn alloc_n(&mut self, n: u8, span: Span) -> Result<RegRange> {
        if n == 0 {
            return error("cannot allocate zero registers", span).into();
        }
        if self.current as usize + n as usize >= u8::MAX as usize {
            return error("too many registers", span).into();
        }

        let start = unsafe { Reg::new_unchecked(self.current) };

        self.current += n;
        if self.current > self.num {
            self.num = self.current;
        }

        Ok(RegRange { start, n })
    }

    fn is_at_top(&self, r: Reg) -> bool {
        self.current == r.get() + 1
    }

    fn free(&mut self, r: Reg) {
        if self.current != r.get() + 1 {
            panic!("registers freed out of order");
        }
        self.current = r.get();
    }

    fn reset_to(&mut self, r: u8) {
        self.current = r;
    }
}

#[derive(Clone, Copy)]
struct RegRange {
    start: Reg,
    n: u8,
}

impl RegRange {
    fn empty() -> Self {
        Self {
            start: ZERO_REG,
            n: 0,
        }
    }
}

struct RegRangeIter {
    start: u8,
    end: u8,
    n: u8,
}

impl IntoIterator for RegRange {
    type Item = Reg;

    type IntoIter = RegRangeIter;

    fn into_iter(self) -> Self::IntoIter {
        RegRangeIter {
            start: self.start.get(),
            end: self.start.get() + self.n,
            n: self.n,
        }
    }
}

impl Iterator for RegRangeIter {
    type Item = Reg;

    fn next(&mut self) -> Option<Self::Item> {
        if self.n == 0 {
            return None;
        }

        let n = self.n;
        self.n -= 1;

        Some(unsafe { Reg::new_unchecked(self.end - n) })
    }
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
        self.scopes.push(Scope {
            last_reg_in_prev_scope: self.ra.current,
            symbols: vec![in buf],
            undefined_functions: vec![in buf],
        });
    }

    fn end_scope(&mut self) {
        let scope = self
            .scopes
            .pop()
            .expect("`end_scope` called without any scopes");
        self.ra.reset_to(scope.last_reg_in_prev_scope);
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

    fn reg_n(&mut self, n: u8, span: Span) -> Result<RegRange> {
        self.ra.alloc_n(n, span)
    }

    fn rfree(&mut self, reg: Reg) {
        self.ra.free(reg);
    }

    fn declare_local(&mut self, name: &'a str, reg: Reg, span: Span) {
        let scope = self
            .scopes
            .last_mut()
            .expect("must always have at least one scope");
        scope.push(Symbol::Local {
            name: name.into(),
            span,
            reg,
        });
        self.dbg.locals.push(vm::dbg::Local { span, reg });
    }

    fn declare_function(&mut self, name: impl Into<Cow<'a, str>>, arity: u8, span: Span, id: FnId) {
        let scope = self
            .scopes
            .last_mut()
            .expect("must always have at least one scope");
        scope.push(Symbol::Function {
            name: name.into(),
            arity,
            span,
            id,
        });
    }

    fn resolve(&self, name: &str) -> Option<&Symbol<'a>> {
        for scope in self.scopes.iter().rev() {
            for symbol in scope.symbols.iter().rev() {
                if symbol.name() == name {
                    return Some(symbol);
                }
            }
        }

        None
    }

    fn resolve_in_scope(&self, name: &str) -> Option<&Symbol<'a>> {
        for symbol in self.scopes.last()?.symbols.iter().rev() {
            if symbol.name() == name {
                return Some(symbol);
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

    fn reg_n(&mut self, n: u8, span: Span) -> Result<RegRange> {
        f!(self).reg_n(n, span)
    }

    fn rfree(&mut self, reg: Reg) {
        f!(self).rfree(reg);
    }

    fn declare_local(&mut self, name: &'a str, reg: Reg, span: Span) {
        f!(self).declare_local(name, reg, span)
    }

    fn resolve(&self, name: &str) -> Option<&Symbol<'a>> {
        for f in self.func_stack.iter().rev() {
            if let Some(symbol) = f.resolve(name) {
                return Some(symbol);
            }
        }

        None
    }

    fn resolve_in_scope(&self, name: &str) -> Option<&Symbol<'a>> {
        if let Some(symbol) = f!(&self).resolve_in_scope(name) {
            return Some(symbol);
        }

        None
    }

    fn emit(&mut self, inst: Instruction, span: Span) {
        f!(self).emit(inst, span)
    }

    /// Declare a function which has not yet been emitted
    ///
    /// This assign it an ID and declares a symbol with its name
    fn declare_function(
        &mut self,
        name: impl Into<Cow<'a, str>>,
        arity: u8,
        span: Span,
    ) -> Result<FnId> {
        let name = name.into();
        let id = self.func_table.reserve(name.clone(), span)?;
        f!(self).declare_function(name, arity, span, id);
        Ok(id)
    }

    /// Define an function once that it's been emitted
    fn define_function(&mut self, id: FnId, f: FuncInfo) {
        self.func_table.define(id, f);
    }
}

#[repr(align(16))]
enum Symbol<'a> {
    Local {
        name: Cow<'a, str>,
        span: Span,
        reg: Reg,
    },
    Function {
        name: Cow<'a, str>,
        arity: u8,
        span: Span,
        id: FnId,
    },
}

impl<'a> Symbol<'a> {
    fn name(&self) -> &str {
        match self {
            Symbol::Local { name, span, reg } => name.as_ref(),
            Symbol::Function {
                name,
                arity,
                span,
                id,
            } => name.as_ref(),
        }
    }
}

fn emit_func<'a>(
    m: &mut State<'a>,
    id: FnId,
    name: Cow<'a, str>,
    span: Span,
    params: NodeList<'a, Ident>,
    param_spans: &'a [Span],
    body: NodeList<'a, Stmt>,
) -> Result<FuncInfo> {
    if params.len() > 100 {
        return error(
            "too many parameters, maximum is 100",
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
            m.declare_local(param.get(), dst, span);
        }

        let _ = emit_stmt_list_with_tail(m, body, Some(ret_reg).into())?;

        m.emit(asm::ret(), NO_SPAN);
        m.end_scope();
    }
    let f = m.func_stack.pop().expect("function stack is empty");

    Ok(FuncInfo::new(
        f.name.into_owned(),
        params.len() as u8,
        f.ra.num,
        f.code.into_iter().collect(),
        f.literals.flat.into_iter().collect(),
        crate::vm::dbg::FuncDebugInfo {
            spans: f.dbg.spans.into_iter().collect(),
            locals: f.dbg.locals.into_iter().collect(),
        },
    ))
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

const ZERO_REG: Reg = unsafe { Reg::new_unchecked(0) };

fn emit_stmt_list_with_tail<'a>(
    m: &mut State<'a>,
    list: NodeList<'a, Stmt>,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    let (stmt_list, tail) = match list.last().map(|node| node.kind()) {
        Some(ast::StmtKind::StmtExpr(tail)) => (list.slice(0..list.len() - 1).unwrap(), Some(tail)),
        _ => (list, None),
    };

    emit_stmt_list(m, stmt_list)?;

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

fn emit_stmt_list<'a>(m: &mut State<'a>, list: NodeList<'a, Stmt>) -> Result<()> {
    // 1. declare all functions in the stmt list
    let mut undefined_functions = vec![in m.buf];
    for stmt in list {
        if let ast::StmtKind::FuncDecl(node) = stmt.kind() {
            let id = m.declare_function(
                node.name().get(),
                node.params().len() as u8,
                node.name_span(),
            )?;
            undefined_functions.push(id);
        }
    }

    // undefined functions needs to be in reverse order of declaration
    undefined_functions.reverse();

    f!(m)
        .scopes
        .last_mut()
        .expect("some scope")
        .undefined_functions = undefined_functions;

    // 2. then process the stmt list
    // as the stmt list is traversed, we come across `FuncDecl` again,
    // at which point we _define_ the given function.
    // to get its id, we `pop` from the `undefined_functions` list,
    // because we are guaranteed to process them in the same order.
    for stmt in list {
        emit_stmt(m, stmt)?;
    }

    Ok(())
}

fn emit_stmt<'a>(m: &mut State<'a>, stmt: Node<'a, Stmt>) -> Result<()> {
    match stmt.kind() {
        ast::StmtKind::Var(node) => emit_stmt_var(m, node)?,
        ast::StmtKind::Loop(node) => emit_stmt_loop(m, node)?,
        ast::StmtKind::FuncDecl(node) => emit_stmt_func(m, node)?,
        ast::StmtKind::StmtExpr(node) => {
            let _ = emit_expr(m, node.inner(), node.inner_span(), NO_REG)?;
        }
    }

    Ok(())
}

fn emit_stmt_var<'a>(m: &mut State<'a>, var: Node<'a, ast::Var>) -> Result<()> {
    let (redeclaration, dst) = match m.resolve_in_scope(var.name().get()) {
        Some(Symbol::Local { reg, .. }) => (true, *reg),
        _ => (false, m.reg(var.name_span())?),
    };
    emit_expr_into(m, var.value(), var.value_span(), dst)?;
    if !redeclaration {
        m.declare_local(var.name().get(), dst, var.name_span());
    }

    Ok(())
}

fn emit_stmt_loop<'a>(m: &mut State<'a>, loop_: Node<'a, ast::Loop>) -> Result<()> {
    let prev_loop = m.begin_loop();
    m.begin_scope();
    emit_stmt_list(m, loop_.body())?;
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

fn emit_stmt_func<'a>(m: &mut State<'a>, func: Node<'a, ast::FuncDecl>) -> Result<()> {
    let id = f!(m)
        .scopes
        .last_mut()
        .expect("some scope")
        .undefined_functions
        .pop()
        .expect("some function");

    let f = emit_func(
        m,
        id,
        func.name().get().into(),
        func.name_span(),
        func.params(),
        func.params_spans(),
        func.body().body(),
    )?;

    m.define_function(id, f);

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
        ast::ExprKind::Call(node) => emit_expr_call(m, node, span, dst),
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
    match m
        .resolve(get.name().get())
        .ok_or_else(|| error("could not resolve name", get.name_span()))?
    {
        Symbol::Local { reg, .. } => Ok(MaybeReg(Some(*reg))),
        Symbol::Function { id, .. } => {
            todo!("function to variable")
        }
    }
}

fn emit_expr_set_var<'a>(
    m: &mut State<'a>,
    set: Node<'a, ast::SetVar>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    // variable must exist at this point
    match m
        .resolve(set.base().name().get())
        .ok_or_else(|| error("could not resolve name", set.base().name_span()))?
    {
        // when adding other symbols, remember to error out here,
        // only local variables may be assigned to
        Symbol::Local { reg: dst, .. } => {
            emit_expr_into(m, set.value(), span, *dst)?;
        }
        Symbol::Function { .. } => return error("cannot assign to function", span).into(),
    };

    Ok(NO_REG)
}

fn emit_expr_call<'a>(
    m: &mut State<'a>,
    call: Node<'a, ast::Call>,
    span: Span,
    dst: MaybeReg,
) -> Result<MaybeReg> {
    enum Base {
        NeedsMov { base: Reg, to: Reg },
        InPlace(Reg),
        Unused(Reg),
    }

    impl Base {
        /// register to write to
        fn dst(&self) -> Reg {
            match self {
                Self::NeedsMov { base, to } => *base,
                Self::InPlace(reg) => *reg,
                Self::Unused(reg) => *reg,
            }
        }

        /// register where return value is placed
        fn out(&self) -> Reg {
            match self {
                Base::NeedsMov { base, to } => *to,
                Base::InPlace(reg) => *reg,
                Base::Unused(reg) => *reg,
            }
        }
    }

    // prepare registers
    let (base, args, reset) = if call.args().is_empty() {
        match dst.inner() {
            // argument-less call; can be used directly no matter where it is
            Some(reg) => (Base::InPlace(reg), RegRange::empty(), None),

            // argument-less call with unused value, so we didn't receive
            // a register. allocate for callee/ret
            None => {
                let reset = f!(m).ra.current;
                (
                    Base::Unused(m.reg(call.callee_span())?),
                    RegRange::empty(),
                    Some(reset),
                )
            }
        }
    } else {
        match dst.inner() {
            // NOTE: num of args is `1..=100`.
            Some(base) => {
                if f!(m).ra.is_at_top(base) {
                    // can be used directly; allocate args above it.
                    let reset = f!(m).ra.current;
                    let args = m.reg_n(call.args().len() as u8, span)?;
                    (Base::InPlace(base), args, Some(reset))
                } else {
                    // cannot be used directly. we have to allocate including dst,
                    // and then emit a mov after the call.
                    let reset = f!(m).ra.current;
                    (
                        Base::NeedsMov {
                            base: m.reg(span)?,
                            to: base,
                        },
                        m.reg_n(call.args().len() as u8, span)?,
                        Some(reset),
                    )
                }
            }

            None => {
                // unused value. allocate for callee/ret and args
                let reset = f!(m).ra.current;
                (
                    Base::Unused(m.reg(span)?),
                    m.reg_n(call.args().len() as u8, span)?,
                    Some(reset),
                )
            }
        }
    };

    // TODO: specialize "method" calls (get prop -> call in a single instruction)
    // ast::ExprKind::GetField(node) => todo!(),
    if let ast::ExprKind::GetVar(node) = call.callee().kind()
        && let Some(Symbol::Function { id, arity, .. }) = m.resolve(node.name().get())
    {
        // calling a function declaration directly - this is definitely a function
        // we can check arity right here and then the VM doesn't have to type check
        // OR arity check anymore

        let id = *id;
        let arity = *arity;
        let nargs = call.args().len() as u8;
        if nargs != arity {
            return error(
                format!("invalid number of arguments, expected {arity} but got {nargs}"),
                span,
            )
            .into();
        }

        for ((reg, value), span) in args.into_iter().zip(call.args()).zip(call.args_spans()) {
            emit_expr_into(m, value, *span, reg)?;
        }

        m.emit(asm::fastcall(base.dst(), id), span);
    } else {
        // maybe callable? only VM can know for sure

        let nargs = call.args().len() as u8;
        emit_expr_into(m, call.callee(), call.callee_span(), base.dst())?;
        for ((reg, value), span) in args.into_iter().zip(call.args()).zip(call.args_spans()) {
            emit_expr_into(m, value, *span, reg)?;
        }

        m.emit(
            asm::call(base.dst(), unsafe { Imm8::new_unchecked(nargs) }),
            span,
        );
    }

    if let Base::NeedsMov { base, to } = &base {
        m.emit(asm::mov(*to, *base), span);
    }

    if let Some(reset) = reset {
        f!(m).ra.reset_to(reset);
    }

    Ok(MaybeReg(Some(base.out())))
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
