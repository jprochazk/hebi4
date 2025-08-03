#![allow(unsafe_op_in_unsafe_fn)]

use crate::{
    codegen::opcodes::*,
    error::{Error, error},
    span::Span,
};

pub struct Module {
    pub(crate) main: Lit,
    pub(crate) literals: Box<[Literal]>,
}

// TODO: colocate data (?)
pub struct FuncInfo {
    pub(crate) name: Box<str>,
    pub(crate) nparams: u8,
    pub(crate) nstack: u8,
    pub(crate) code: Box<[Instruction]>,
    pub(crate) dbg: Box<dbg::FuncDebugInfo>,
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
    Int(i64) = 0,
    Float(f64) = 1,

    Func(Box<FuncInfo>) = 4,
}

#[repr(C, u64)]
pub enum Value {
    Int(i64) = 0,
    Float(f64) = 1,
}

struct RuntimeModule {
    module: *const Module,
    // TODO: module vars
}

struct RuntimeFunction {
    info: *const FuncInfo,
}

#[repr(C)]
pub struct Context {
    current_module: *mut RuntimeModule,
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

static JT: JumpTable = JumpTable {
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
};

op! {
    unsafe extern "?" fn nop(args: Nop, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn mov(args: Mov, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lnil(args: Lnil, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lsmi(args: Lsmi, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lint(args: Lint, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lnum(args: Lnum, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lstr(args: Lstr, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn ltrue(args: Ltrue, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lfalse(args: Lfalse, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lcli(args: Lcli, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lfni(args: Lfni, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn larr(args: Larr, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lobj(args: Lobj, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn jmp(args: Jmp, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn istrue(args: Istrue, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn istruec(args: Istruec, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isfalse(args: Isfalse, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isfalsec(args: Isfalsec, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn islt(args: Islt, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isle(args: Isle, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isgt(args: Isgt, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isge(args: Isge, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn iseq(args: Iseq, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isne(args: Isne, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn iseqs(args: Iseqs, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isnes(args: Isnes, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn iseqn(args: Iseqn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isnen(args: Isnen, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn iseqp(args: Iseqp, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isnep(args: Isnep, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn addvv(args: Addvv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn addvn(args: Addvn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn addnv(args: Addnv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn subvv(args: Subvv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn subvn(args: Subvn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn subnv(args: Subnv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn mulvv(args: Mulvv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn mulvn(args: Mulvn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn mulnv(args: Mulnv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn divvv(args: Divvv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn divvn(args: Divvn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn divnv(args: Divnv, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn unm(args: Unm, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn not(args: Not, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn call(args: Call, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn fastcall(args: Fastcall, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn ret(args: Ret, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
        todo!()
    }
}
