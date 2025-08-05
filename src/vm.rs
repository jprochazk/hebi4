#![allow(unsafe_op_in_unsafe_fn)]

mod array;
mod disasm;

use beef::lean::Cow;

use std::marker::PhantomData;

use array::{DynArray, DynList};

use crate::{
    codegen::opcodes::*,
    error::{Error, Result, error},
    span::Span,
};

pub struct Chunk {
    main: FnId,
    functions: *const [FuncInfo],
}

impl Chunk {
    pub(crate) fn new(main: FnId, functions: Vec<FuncInfo>) -> Self {
        Self {
            main,
            functions: Box::into_raw(functions.into_boxed_slice()),
        }
    }
}

impl Drop for Chunk {
    fn drop(&mut self) {
        let _ = unsafe { Box::from_raw(self.functions.cast_mut()) };
    }
}

// TODO: colocate data (?)
pub struct FuncInfo {
    name: Cow<'static, str>,
    nparams: u8,
    nstack: u8,
    code: *const [Instruction],
    literals: *const [Literal],
    dbg: Option<*const dbg::FuncDebugInfo>,
}

impl FuncInfo {
    pub(crate) fn new(
        name: impl Into<Cow<'static, str>>,
        nparams: u8,
        nstack: u8,
        code: Vec<Instruction>,
        literals: Vec<Literal>,
        dbg: dbg::FuncDebugInfo,
    ) -> Self {
        Self {
            name: name.into(),
            nparams,
            nstack,
            code: Box::into_raw(code.into_boxed_slice()),
            literals: Box::into_raw(literals.into_boxed_slice()),
            dbg: Some(Box::into_raw(Box::new(dbg))),
        }
    }
}

// The pointers are just `Box`, but stored raw so that we can also point them to static data.
unsafe impl Send for FuncInfo {}
unsafe impl Sync for FuncInfo {}

impl Drop for FuncInfo {
    fn drop(&mut self) {
        let _ = unsafe { Box::from_raw(self.code.cast_mut()) };
        let _ = unsafe { Box::from_raw(self.literals.cast_mut()) };
        if let Some(dbg) = self.dbg {
            let _ = unsafe { Box::from_raw(dbg.cast_mut()) };
        }
    }
}

pub mod dbg {
    use super::*;

    pub struct FuncDebugInfo {
        pub(crate) spans: Box<[Span]>,
        pub(crate) locals: Box<[Local]>,
    }

    pub struct Local {
        pub(crate) span: Span,
        pub(crate) reg: Reg,
    }
}

// NOTE: `Value` and `Literal` must be partialy bit-compatible
// TODO: check this at compile time

#[repr(C, u64)]
pub enum Literal {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
}

#[derive(Default, Clone, Copy)]
#[repr(C, u64)]
pub enum Value {
    #[default]
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
    Object(Object) = 4,
}

impl Literal {
    #[inline]
    pub fn int(&self) -> Option<i64> {
        match self {
            Self::Int(v) => Some(*v),
            _ => None,
        }
    }

    #[inline]
    pub fn float(&self) -> Option<f64> {
        match self {
            Self::Float(v) => Some(*v),
            _ => None,
        }
    }
}

type Private = PhantomData<()>;

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Object {
    _private: Private,
}

#[derive(Clone, Copy)]
struct FuncInfoPtr(*const FuncInfo);

impl FuncInfoPtr {
    #[inline]
    unsafe fn code(self) -> Ip {
        Ip(
            (*self.0)
                .code
                .cast::<Instruction>() // drop the length,
                .cast::<RawInstruction>(), // _then_ cast to raw
        )
    }

    #[inline]
    unsafe fn literals(self) -> Lp {
        Lp(
            (*self.0).literals.cast::<Literal>(), // drop the length
        )
    }

    #[inline]
    unsafe fn nstack(self) -> u8 {
        (*self.0).nstack
    }
}

impl Sp {
    #[inline(always)]
    pub unsafe fn at(self, r: Reg) -> *mut Value {
        self.0.offset(r.sz())
    }
}

impl Jt {
    #[inline(always)]
    pub unsafe fn at(self, inst: RawInstruction) -> OpaqueOp {
        self.0.offset(inst.tag as isize).read()
    }
}

impl Ip {
    #[inline]
    unsafe fn offset_from_unsigned(self, other: Self) -> usize {
        (self.0).offset_from_unsigned(other.0)
    }

    #[inline]
    unsafe fn offset(self, n: usize) -> Self {
        Ip((self.0).offset(n as isize))
    }

    #[inline(always)]
    unsafe fn next(self) -> Self {
        Self(self.0.offset(1))
    }

    #[inline(always)]
    unsafe fn get(self) -> RawInstruction {
        self.0.read()
    }
}

#[derive(Clone, Copy)]
struct CallFrame {
    callee: FuncInfoPtr,

    /// Stack base of _this_ frame.
    stack_base: u32,

    /// Address of the next instruction to execute
    /// after returning from _this_ call frame.
    ///
    /// Points into the previous call frame's callee,
    /// if there is one.
    return_addr: u32,
}

#[derive(Clone, Copy)]
struct CallFramePtr(*mut CallFrame);

impl CallFramePtr {
    #[inline]
    unsafe fn raw(self) -> *mut CallFrame {
        self.0
    }

    #[inline]
    unsafe fn callee(self) -> FuncInfoPtr {
        (*self.0).callee
    }

    #[inline]
    unsafe fn stack_base(self) -> u32 {
        (*self.0).stack_base
    }

    #[inline]
    unsafe fn return_addr(self) -> u32 {
        (*self.0).return_addr
    }
}

impl Default for CallFrame {
    fn default() -> Self {
        static BAD: FuncInfo = FuncInfo {
            name: Cow::const_str("@bad"),
            nparams: 0,
            nstack: 0,
            code: &[asm::trap()],
            literals: &[],
            dbg: None,
        };

        CallFrame {
            callee: FuncInfoPtr(&BAD),
            stack_base: 0,
            return_addr: 0,
        }
    }
}

#[repr(C)]
pub(crate) struct Context {
    vm: *mut Vm,
    current_module: *const Chunk,
    error: *mut Option<Error>,
    current_frame: CallFrame,
}

impl Ctx {
    #[inline]
    unsafe fn current_frame(self) -> CallFramePtr {
        CallFramePtr(&raw mut (*self.0).current_frame)
    }

    #[inline]
    unsafe fn push_frame(self, frame: CallFrame) {
        let frames = &mut (*(*self.0).vm).frames;
        frames.push(frame);
    }

    #[inline]
    unsafe fn pop_frame_unchecked(self) -> CallFrame {
        let frames = &mut (*(*self.0).vm).frames;
        frames.pop_unchecked()
    }

    #[inline]
    unsafe fn has_enough_stack_space(self, stack_base: usize, nstack: usize) -> bool {
        let stack = &mut (*(*self.0).vm).stack;
        let remaining = stack.remaining(stack_base);
        remaining >= (nstack as isize)
    }

    #[inline]
    unsafe fn stack_at(self, stack_base: usize) -> Sp {
        let stack = &mut (*(*self.0).vm).stack;
        Sp(stack.offset(stack_base))
    }

    #[inline]
    unsafe fn get_function_in_current_module(self, id: FnId) -> FuncInfoPtr {
        let chunk = (*self.0).current_module;
        // remove len
        let functions = (*chunk).functions.cast::<FuncInfo>();
        FuncInfoPtr(functions.offset(id.sz()))
    }
}

enum VmError {
    DivisionByZero,
}

impl VmError {
    fn with_context(&self, ctx: &Context) -> Error {
        let span = Span::empty(); // TODO
        match self {
            VmError::DivisionByZero => error("division by zero", span),
        }
    }
}

impl From<VmError> for Error {
    fn from(value: VmError) -> Self {
        match value {
            VmError::DivisionByZero => todo!(),
        }
    }
}

static JT: JumpTable = jump_table! {
    nop,
    mov,
    lnil,
    lsmi,
    ltrue,
    lfalse,
    lint,
    lnum,
    lstr,
    lcli,
    lfni,
    larr,
    lobj,
    jmp,
    istrue,
    istruec,
    isfalse,
    isfalsec,
    islt,
    isle,
    isgt,
    isge,
    iseq,
    isne,
    iseqs,
    isnes,
    iseqn,
    isnen,
    iseqp,
    isnep,
    addvv,
    addvn,
    addnv,
    subvv,
    subvn,
    subnv,
    mulvv,
    mulvn,
    mulnv,
    divvv,
    divvn,
    divnv,
    unm,
    not,
    call,
    fastcall,
    ret,
    stop,
    trap,
};

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Control {
    Yield = 0,
    Error = 1,

    #[cfg(debug_assertions)]
    Continue(Sp, Lp, Ip),
}

#[inline(always)]
unsafe fn dispatch_current(jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    #[cfg(debug_assertions)]
    {
        Control::Continue(sp, lp, ip)
    }

    #[cfg(not(debug_assertions))]
    {
        let inst = ip.get();
        let op = jt.at(inst);
        op(inst, jt, sp, lp, ip, ctx)
    }
}

#[inline(always)]
unsafe fn dispatch_next(jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    let ip = ip.next();
    dispatch_current(jt, sp, lp, ip, ctx)
}

#[inline(always)]
unsafe fn nop(args: Nop, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    dispatch_next(jt, sp, lp, ip, ctx)
}

#[inline(always)]
unsafe fn mov(args: Mov, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn lnil(args: Lnil, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn lsmi(args: Lsmi, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    *sp.at(args.dst) = Value::Int(args.v.get() as i64);

    dispatch_next(jt, sp, lp, ip, ctx)
}

#[inline(always)]
unsafe fn lint(args: Lint, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn lnum(args: Lnum, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn lstr(args: Lstr, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn ltrue(args: Ltrue, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn lfalse(args: Lfalse, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn lcli(args: Lcli, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn lfni(args: Lfni, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn larr(args: Larr, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn lobj(args: Lobj, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn jmp(args: Jmp, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn istrue(args: Istrue, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn istruec(args: Istruec, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn isfalse(args: Isfalse, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn isfalsec(args: Isfalsec, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn islt(args: Islt, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn isle(args: Isle, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn isgt(args: Isgt, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn isge(args: Isge, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn iseq(args: Iseq, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn isne(args: Isne, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn iseqs(args: Iseqs, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn isnes(args: Isnes, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn iseqn(args: Iseqn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn isnen(args: Isnen, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn iseqp(args: Iseqp, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn isnep(args: Isnep, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn addvv(args: Addvv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn addvn(args: Addvn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn addnv(args: Addnv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn subvv(args: Subvv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn subvn(args: Subvn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn subnv(args: Subnv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn mulvv(args: Mulvv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn mulvn(args: Mulvn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn mulnv(args: Mulnv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn divvv(args: Divvv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn divvn(args: Divvn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn divnv(args: Divnv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn unm(args: Unm, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn not(args: Not, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn call(args: Call, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    todo!()
}

#[inline(always)]
unsafe fn fastcall(args: Fastcall, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    let ret = args.dst;
    let callee = ctx.get_function_in_current_module(args.id);

    let (sp, lp, ip) = do_call(callee, ret, ip, ctx);

    dispatch_current(jt, sp, lp, ip, ctx)
}

#[inline(always)]
unsafe fn ret(args: Ret, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    let (sp, lp, ip) = return_from_call(ctx);

    dispatch_current(jt, sp, lp, ip, ctx)
}

#[inline(always)]
unsafe fn stop(args: Stop, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    Control::Yield
}

#[inline(always)]
unsafe fn trap(args: Trap, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
    unreachable!();
}

/// call procedure:
/// 1. grow stack if needed
/// 2. allocate new call frame
/// 3. jump to start of callee
///
/// new call frame's stack overlaps with the current frame's stack
/// example: assuming 3 args, with ret at r6:
/// ```ignore
///   frame N:   [ 0 1 2 3 4 5 6 7 8 9 ]
///                            ^ ^
///                            | args
///                            ret
///   frame N+1: [ 6 7 8 9 ... ]
///                ^ ^
///                | args
///                ret
/// ```
/// `r0` in the new frame will be in the same location as `r6`
/// in the previous frame.
#[inline(always)]
unsafe fn do_call(callee: FuncInfoPtr, ret: Reg, ip: Ip, ctx: Ctx) -> (Sp, Lp, Ip) {
    // See doc comment.
    let stack_base = ctx.current_frame().stack_base() + (ret.get() as u32);

    // Return addr points to the next instruction after the call instruction.
    let current_function_start = ctx.current_frame().callee().code();
    let return_addr = 1 + (ip.offset_from_unsigned(current_function_start) as u32);

    let new_frame = CallFrame {
        callee,
        stack_base,
        return_addr,
    };

    let sp: Sp = maybe_grow_stack(ctx, &new_frame);
    let lp: Lp = new_frame.callee.literals();
    let ip: Ip = new_frame.callee.code();

    let prev_frame = core::ptr::replace(ctx.current_frame().raw(), new_frame);
    ctx.push_frame(prev_frame);

    (sp, lp, ip)
}

#[inline(always)]
unsafe fn maybe_grow_stack(ctx: Ctx, new_frame: &CallFrame) -> Sp {
    let new_stack_base = new_frame.stack_base as usize;
    let new_frame_size = new_frame.callee.nstack() as usize;
    if !ctx.has_enough_stack_space(new_stack_base, new_frame_size) {
        grow_stack(ctx, new_frame)
    }

    ctx.stack_at(new_stack_base)
}

#[inline(never)]
#[cold]
unsafe fn grow_stack(ctx: Ctx, new_frame: &CallFrame) {
    // NOTE: We allocate more than we need here,
    // capacity doubles each time anyway.
    let stack = &mut (*(*ctx.0).vm).stack;
    stack.grow(new_frame.callee.nstack() as usize)
}

#[inline(always)]
unsafe fn return_from_call(ctx: Ctx) -> (Sp, Lp, Ip) {
    // Only called from `ret`, meaning we are guaranteed to have
    // at least the one call frame which is currently being executed.

    let returning_from = core::ptr::replace(ctx.current_frame().raw(), ctx.pop_frame_unchecked());
    let returning_to = ctx.current_frame();

    let stack_base = returning_to.stack_base() as usize;
    let return_addr = returning_from.return_addr as usize;

    let sp: Sp = ctx.stack_at(stack_base);
    let lp: Lp = returning_to.callee().literals();
    let ip: Ip = returning_to.callee().code().offset(return_addr);

    (sp, lp, ip)
}

type Invariant<'a> = PhantomData<fn(&'a ()) -> &'a ()>;

pub struct Module<'gc> {
    _lifetime: Invariant<'gc>,
}

pub struct Vm {
    stack: DynArray<Value>,
    frames: DynList<CallFrame>,
}

impl Vm {
    pub fn new() -> Self {
        // 1 MiB
        const INITIAL_STACK_SIZE: usize = (1024 * 1024) / std::mem::size_of::<Value>();
        const STACK_DEPTH: usize = INITIAL_STACK_SIZE / 16;

        Vm {
            stack: DynArray::new(INITIAL_STACK_SIZE),
            frames: DynList::new(STACK_DEPTH),
        }
    }

    #[inline(always)]
    pub fn with<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(Runtime<'gc>) -> R,
        R: 'static,
    {
        f(Runtime {
            vm: self,
            _lifetime: PhantomData,
        })
    }
}

pub struct Runtime<'gc> {
    vm: *mut Vm,

    _lifetime: Invariant<'gc>,
}

// TODO: expose managed values in a safe way.
// First use-case for that: snapshots of values.
//   -> This also implies stringification using the runtime, but we'd like to:
//      1. Execute the module, yielding a value
//      2. "Stash" that value, returning it from `Vm::with`
//      3. Using a 2nd call to `Vm::with`, stringify the value
//         -> This may or may not actually require the VM, but we should
//            at least pretend that it does.
pub struct Gc<'gc, T>(*mut T, Invariant<'gc>);

impl<'gc> Runtime<'gc> {
    /// Run the code once.
    pub fn run_once(&mut self, chunk: Chunk) -> Result<()> {
        unsafe {
            let mut error = None;
            let entry = [
                asm::fastcall(Reg::new_unchecked(0), chunk.main),
                asm::stop(),
            ];
            let entry = core::mem::ManuallyDrop::new(FuncInfo {
                name: "@entry".into(),
                nparams: 0,
                nstack: 1,
                code: &entry,
                literals: &[],
                dbg: None,
            });

            let mut ctx = Context {
                vm: self.vm,
                current_module: &raw const chunk,
                error: &raw mut error,
                current_frame: CallFrame {
                    callee: FuncInfoPtr(&*entry),
                    stack_base: 0,
                    return_addr: 0,
                },
            };
            let ctx: Ctx = Ctx(&mut ctx);

            let jt: Jt = JT.as_ptr();
            let sp: Sp = ctx.stack_at(0);
            let lp: Lp = ctx.current_frame().callee().literals();
            let ip: Ip = ctx.current_frame().callee().code();

            // In debug mode, fall back to loop+match.
            #[cfg(debug_assertions)]
            {
                let mut sp: Sp = sp;
                let mut lp: Lp = lp;
                let mut ip: Ip = ip;

                loop {
                    let inst = ip.get();
                    let op = jt.at(inst);
                    match op(inst, jt, sp, lp, ip, ctx) {
                        Control::Yield => return Ok(()),
                        Control::Error => return Err(error.unwrap()),

                        #[cfg(debug_assertions)]
                        Control::Continue(new_sp, new_lp, new_ip) => {
                            sp = new_sp;
                            lp = new_lp;
                            ip = new_ip;
                            continue;
                        }
                    }
                }
            }

            // Release mode uses tail calls.
            #[cfg(not(debug_assertions))]
            {
                match dispatch_current(jt, sp, lp, ip, ctx) {
                    Control::Yield => return Ok(()),
                    Control::Error => return Err(error.unwrap()),
                }
            }
        }
    }

    // /// Compile a module, which gives you access to functions
    // /// declared within it.
    // ///
    // /// This will execute the top-level code in the chunk.
    // pub fn compile(&mut self, chunk: Chunk) -> Result<Gc<'gc, Module>> {
    //     todo!()
    // }
}

#[cfg(test)]
mod tests;
