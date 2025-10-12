#![allow(dead_code, unused_macros)] // may be used in output
#![allow(unsafe_op_in_unsafe_fn)]

struct Nop;

#[repr(C)]
pub struct JumpTable {
    nop: Handler<Nop>,
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Opcode {
    Nop = 0,
}

//file-start

mod private {
    pub trait Sealed {}
}

use super::{Context, Control, Literal, ValueRaw};

pub trait OperandPack: private::Sealed + Sized {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Sp(pub(crate) *mut ValueRaw);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ip(pub(crate) *const Insn);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Insn(u32);

impl Insn {
    #[inline(always)]
    pub fn tag(self) -> isize {
        (self.0 & 0xFF) as isize
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Jt(pub(crate) *const OpaqueHandler);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Lp(pub(crate) *const Literal);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ctx(pub(crate) *mut Context);

const _: () = {
    use std::mem::align_of;
    assert!(align_of::<JumpTable>() == align_of::<[OpaqueHandler; 1]>());
};

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Hash)]
#[repr(transparent)]
pub struct u24(u32);

impl u24 {
    pub const MAX: u24 = u24(0xFFFFFF);
    pub const MIN: u24 = u24(0);
    pub const ZERO: u24 = u24(0);

    #[inline]
    pub const fn new(v: u32) -> Self {
        if v > Self::MAX.get() {
            panic!("value is out of bounds for u24");
        }

        unsafe { Self::new_unchecked(v) }
    }

    #[inline]
    pub const unsafe fn new_unchecked(v: u32) -> Self {
        debug_assert!(v <= Self::MAX.get());
        Self(v)
    }

    #[inline]
    pub const fn get(self) -> u32 {
        self.0
    }

    #[inline]
    pub const fn as_i24(self) -> i24 {
        i24(self)
    }
}

impl PartialEq for u24 {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&self.get(), &other.get())
    }
}

impl Eq for u24 {}

impl PartialOrd for u24 {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        PartialOrd::partial_cmp(&self.get(), &other.get())
    }
}

impl Ord for u24 {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Ord::cmp(&self.get(), &other.get())
    }
}

impl std::fmt::Debug for u24 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <u32 as std::fmt::Debug>::fmt(&self.get(), f)
    }
}

impl std::fmt::Display for u24 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <u32 as std::fmt::Display>::fmt(&self.get(), f)
    }
}

impl TryFrom<usize> for u24 {
    type Error = ();

    #[inline]
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value > u24::MAX.zx() as usize {
            return Err(());
        }

        Ok(unsafe { u24::new_unchecked(value as u32) })
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Hash)]
#[repr(transparent)]
pub struct i24(u24);

impl i24 {
    pub const MAX: i24 = i24(unsafe { u24::new_unchecked(16_777_215) });
    pub const MIN: i24 = i24(u24::ZERO);
    pub const ZERO: i24 = i24(unsafe { u24::new_unchecked(8_388_608) });

    const OFFSET: u32 = 8_388_608; // 2^23, which is i24::MAX + 1

    #[inline]
    pub const fn new(v: i32) -> Self {
        if v > Self::MAX.get() || v < Self::MIN.get() {
            panic!("value is out of bounds for i24");
        }

        unsafe { Self::new_unchecked(v) }
    }

    #[inline]
    pub const unsafe fn new_unchecked(v: i32) -> Self {
        Self(u24::new_unchecked((v as u32).wrapping_add(Self::OFFSET)))
    }

    #[inline]
    pub const fn get(self) -> i32 {
        self.0.get().wrapping_sub(Self::OFFSET) as i32
    }

    #[inline]
    pub const fn as_u24(self) -> u24 {
        self.0
    }
}

impl PartialEq for i24 {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl Eq for i24 {}

impl PartialOrd for i24 {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl Ord for i24 {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl std::fmt::Debug for i24 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <i32 as std::fmt::Debug>::fmt(&self.get(), f)
    }
}

impl std::fmt::Display for i24 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <i32 as std::fmt::Display>::fmt(&self.get(), f)
    }
}

impl TryFrom<isize> for i24 {
    type Error = ();

    #[inline]
    fn try_from(value: isize) -> Result<Self, Self::Error> {
        if value > i24::MAX.get() as isize || value < i24::MIN.get() as isize {
            return Err(());
        }

        Ok(unsafe { i24::new_unchecked(value as i32) })
    }
}

impl From<u24> for i24 {
    #[inline(always)]
    fn from(value: u24) -> Self {
        value.as_i24()
    }
}

impl From<i24> for u24 {
    #[inline(always)]
    fn from(value: i24) -> Self {
        value.as_u24()
    }
}

#[cfg(not(windows))]
macro_rules! Handler {
    (
        fn ($($ty:ty),* $(,)?) -> $ret:ty
    ) => {
        unsafe extern "C" fn($($ty),*) -> $ret
    };
}

#[cfg(windows)]
macro_rules! Handler {
    (
        fn ($($ty:ty),* $(,)?) -> $ret:ty
    ) => {
        unsafe extern "sysv64" fn($($ty),*) -> $ret
    };
}

#[cfg(not(windows))]
macro_rules! handler {
    (
        unsafe extern "?" fn $name:ident($($i:ident : $ty:ty),* $(,)?) -> $ret:ty $body:block
    ) => {
        unsafe extern "C" fn $name($($i:$ty),*) -> $ret $body
    };
}

#[cfg(windows)]
macro_rules! handler {
    (
        unsafe extern "?" fn $name:ident($($i:ident : $ty:ty),* $(,)?) -> $ret:ty $body:block
    ) => {
        unsafe extern "sysv64" fn $name($($i:$ty),*) -> $ret $body
    };
}

macro_rules! jump_table {
    {
        $($op:ident),* $(,)?
    } => {
        JumpTable {
            $($op: {
                type __Operands = $crate::codegen::opcodes::__operands::$op;
                const __OP: unsafe fn(__Operands, Jt, Sp, Lp, Ip, Ctx) -> Control = $op;

                {
                    handler! {
                        unsafe extern "?" fn $op(args: Insn, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
                            let args: __Operands = core::mem::transmute(args);
                            __OP(args, jt, sp, lp, ip, ctx)
                        }
                    }

                    $op
                }
            }),*
        }
    };
}

pub type Handler<Operands> = Handler!(fn(Operands, Jt, Sp, Lp, Ip, Ctx) -> Control);

pub type OpaqueHandler = Handler!(fn(Insn, Jt, Sp, Lp, Ip, Ctx) -> Control);

macro_rules! declare_operand_type {
    ($name:ident, $ty:ident, $fmt:literal) => {
        #[derive(Debug, Clone, Copy)]
        #[repr(transparent)]
        #[must_use = "unused operand"]
        pub struct $name($ty);

        impl $name {
            #[inline(always)]
            pub fn get(self) -> $ty {
                self.0
            }

            #[inline(always)]
            pub const unsafe fn new_unchecked(v: $ty) -> Self {
                Self(v)
            }

            #[inline(always)]
            pub fn zx(self) -> usize {
                self.0.zx()
            }

            #[inline(always)]
            pub fn sz(self) -> isize {
                self.0.sz()
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, $fmt, self.0)
            }
        }
    };
}

trait ExtendEx: Sized {
    fn zx(self) -> usize;
    fn sz(self) -> isize;
}

macro_rules! extend_primitive {
    ($ty:ty) => {
        impl ExtendEx for $ty {
            #[inline(always)]
            fn zx(self) -> usize {
                self as usize
            }

            #[inline(always)]
            fn sz(self) -> isize {
                self as isize
            }
        }
    };
}

extend_primitive!(u8);
extend_primitive!(u16);
extend_primitive!(i16);

impl ExtendEx for u24 {
    #[inline(always)]
    fn zx(self) -> usize {
        self.get() as usize
    }

    #[inline(always)]
    fn sz(self) -> isize {
        self.get() as isize
    }
}

impl ExtendEx for i24 {
    #[inline(always)]
    fn zx(self) -> usize {
        self.get() as usize
    }

    #[inline(always)]
    fn sz(self) -> isize {
        self.get() as isize
    }
}

macro_rules! declare_operand_types {
    (
        $(
            $name:ident($ty:ident) = $fmt:literal
        ),* $(,)?
    ) => {
        $(
            declare_operand_type!($name, $ty, $fmt);
        )*
    };
}

declare_operand_types! {
    Reg(u8) = "r{}",

    Mvar(u16) = "m{}",
    Cap(u8) = "c{}",

    Lit(u16) = "l{}",
    Lit8(u8) = "l{}",

    FnId(u16) = "fn{}",

    Imm8(u8) = "{}",
    Imm16(u16) = "{}",
    Imm16s(i16) = "{}",
    Imm24(u24) = "{}",
    Imm24s(i24) = "{}",
}

#[allow(non_snake_case)]
#[inline(always)]
fn op_abc(op: Opcode, a: u8, b: u8, c: u8) -> Insn {
    let mut v = 0u32;
    v |= op as u32;
    v |= (a as u32) << 8;
    v |= (b as u32) << 16;
    v |= (c as u32) << 24;
    Insn(v)
}

#[allow(non_snake_case)]
#[inline(always)]
fn op_aB(op: Opcode, a: u8, B: u16) -> Insn {
    let mut v = 0u32;
    v |= op as u32;
    v |= (a as u32) << 8;
    v |= (B as u32) << 16;
    Insn(v)
}

#[allow(non_snake_case)]
#[inline(always)]
fn op_aS(op: Opcode, a: u8, B: i16) -> Insn {
    let mut v = 0u32;
    v |= op as u32;
    v |= (a as u32) << 8;
    v |= (B as u32) << 16;
    Insn(v)
}

#[allow(non_snake_case)]
#[inline(always)]
fn op_A(op: Opcode, A: u24) -> Insn {
    let mut v = 0u32;
    v |= op as u32;
    v |= (A.0) << 8;
    Insn(v)
}

#[allow(non_snake_case)]
#[inline(always)]
fn op_S(op: Opcode, A: i24) -> Insn {
    let mut v = 0u32;
    v |= op as u32;
    v |= (A.0.0) << 8;
    Insn(v)
}

impl Opcode {
    #[inline(always)]
    fn zx(self) -> usize {
        self as usize
    }
}

#[allow(non_snake_case)]
impl Insn {
    #[inline(always)]
    pub fn op(self) -> Opcode {
        unsafe { ::core::mem::transmute((self.0 & 0xFF) as u8) }
    }

    #[inline(always)]
    fn a(self) -> u8 {
        ((self.0 >> 8) & 0xFF) as u8
    }

    #[inline(always)]
    fn b(self) -> u8 {
        ((self.0 >> 16) & 0xFF) as u8
    }

    #[inline(always)]
    fn c(self) -> u8 {
        (self.0 >> 24) as u8
    }

    #[inline(always)]
    fn B(self) -> u16 {
        (self.0 >> 16) as u16
    }

    #[inline(always)]
    fn A(self) -> u24 {
        u24((self.0 >> 8) as u32)
    }
}

// ...
