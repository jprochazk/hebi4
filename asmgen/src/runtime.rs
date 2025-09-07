#![allow(dead_code, unused_macros)] // may be used in output
#![allow(unsafe_op_in_unsafe_fn)]

struct Nop;

#[repr(C)]
pub struct JumpTable {
    nop: Op<Nop>,
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
pub struct Ip(pub(crate) *const RawInstruction);

#[derive(Clone, Copy)]
#[repr(C, align(4))]
pub struct RawInstruction {
    pub(crate) tag: u8,
    pub(crate) payload: [u8; 3],
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Jt(pub(crate) *const OpaqueOp);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Lp(pub(crate) *const Literal);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ctx(pub(crate) *mut Context);

const _: () = {
    use std::mem::align_of;
    assert!(align_of::<JumpTable>() == align_of::<[OpaqueOp; 1]>());
};

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Hash)]
#[repr(C, packed)]
pub struct u24([u8; 3]);

impl u24 {
    pub const MAX: u24 = u24([255; 3]);
    pub const MIN: u24 = u24([0; 3]);
    pub const ZERO: u24 = u24([0; 3]);

    #[inline]
    pub const fn new(v: u32) -> Self {
        if v > Self::MAX.get() {
            panic!("value is out of bounds for u24");
        }

        unsafe { Self::new_unchecked(v) }
    }

    #[inline]
    pub const unsafe fn new_unchecked(v: u32) -> Self {
        let [a, b, c, _] = v.to_le_bytes();
        Self([a, b, c])
    }

    #[inline]
    pub const fn get(self) -> u32 {
        let [a, b, c] = self.0;
        u32::from_le_bytes([a, b, c, 0])
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

#[cfg(not(windows))]
macro_rules! Op {
    (
        fn ($($ty:ty),* $(,)?) -> $ret:ty
    ) => {
        unsafe extern "C" fn($($ty),*) -> $ret
    };
}

#[cfg(windows)]
macro_rules! Op {
    (
        fn ($($ty:ty),* $(,)?) -> $ret:ty
    ) => {
        unsafe extern "sysv64" fn($($ty),*) -> $ret
    };
}

#[cfg(not(windows))]
macro_rules! op {
    (
        unsafe extern "?" fn $name:ident($($i:ident : $ty:ty),* $(,)?) -> $ret:ty $body:block
    ) => {
        unsafe extern "C" fn $name($($i:$ty),*) -> $ret $body
    };
}

#[cfg(windows)]
macro_rules! op {
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
                    op! {
                        unsafe extern "?" fn $op(args: RawInstruction, jt: Jt, sp: Sp, lp: Lp, ip: Ip, ctx: Ctx) -> Control {
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

pub type Op<Operands> = Op!(fn(Operands, Jt, Sp, Lp, Ip, Ctx) -> Control);

pub type OpaqueOp = Op!(fn(RawInstruction, Jt, Sp, Lp, Ip, Ctx) -> Control);

macro_rules! declare_operand_type {
    ($name:ident, $ty:ident, $fmt:literal) => {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        #[must_use = "unused operand"]
        pub struct $name($ty);

        impl $name {
            #[inline(always)]
            pub fn get(self) -> $ty {
                self.0
            }

            #[inline(always)]
            pub unsafe fn new_unchecked(v: $ty) -> Self {
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

    Lit(u16) = "l{}",
    Lit8(u8) = "l{}",

    FnId(u16) = "fn{}",

    Imm8(u8) = "{}",
    Imm16(i16) = "{}",
    Imm24(u24) = "{}",
}

const fn assert_bit_equal<A, B>(a: &A, b: &B) {
    let a: [u32; 1] = unsafe { (a as *const A as *const [u32; 1]).read() };
    let b: [u32; 1] = unsafe { (b as *const B as *const [u32; 1]).read() };
    if a[0] != b[0] {
        panic!("not bit equal");
    }
}

// ...
