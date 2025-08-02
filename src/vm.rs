#![allow(unsafe_op_in_unsafe_fn)]

use crate::{
    codegen::opcodes::*,
    error::{Error, error},
    span::Span,
};

#[repr(u8)]
pub enum Value {
    Int(i64),
    Float(f64),
}

pub struct Context {}

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
    unsafe extern "?" fn nop(args: Nop, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn mov(args: Mov, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lnil(args: Lnil, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lsmi(args: Lsmi, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lint(args: Lint, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lnum(args: Lnum, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lstr(args: Lstr, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn ltrue(args: Ltrue, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lfalse(args: Lfalse, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lcli(args: Lcli, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lfni(args: Lfni, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn larr(args: Larr, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn lobj(args: Lobj, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn jmp(args: Jmp, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn istrue(args: Istrue, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn istruec(args: Istruec, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isfalse(args: Isfalse, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isfalsec(args: Isfalsec, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn islt(args: Islt, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isle(args: Isle, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isgt(args: Isgt, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isge(args: Isge, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn iseq(args: Iseq, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isne(args: Isne, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn iseqs(args: Iseqs, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isnes(args: Isnes, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn iseqn(args: Iseqn, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isnen(args: Isnen, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn iseqp(args: Iseqp, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn isnep(args: Isnep, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn addvv(args: Addvv, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn addvn(args: Addvn, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn addnv(args: Addnv, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn subvv(args: Subvv, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn subvn(args: Subvn, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn subnv(args: Subnv, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn mulvv(args: Mulvv, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn mulvn(args: Mulvn, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn mulnv(args: Mulnv, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn divvv(args: Divvv, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn divvn(args: Divvn, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn divnv(args: Divnv, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn unm(args: Unm, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn not(args: Not, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn call(args: Call, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn fastcall(args: Fastcall, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}

op! {
    unsafe extern "?" fn ret(args: Ret, jt: JumpTablePtr, sp: StackPtr, ip: InstructionPtr, ctx: ContextPtr) -> Control {
        todo!()
    }
}
