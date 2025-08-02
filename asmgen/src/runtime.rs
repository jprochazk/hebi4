struct Nop;

#[repr(C)]
pub struct JumpTable {
    nop: Op<Nop>,
}

//file-start

mod private {
    pub trait Sealed {}
}

use super::{Context, Value};

pub trait OperandPack: private::Sealed + Sized {}

#[repr(transparent)]
pub struct StackPtr(*mut Value);

#[repr(transparent)]
pub struct InstructionPtr(*const u32);

#[repr(transparent)]
pub struct JumpTablePtr(*const OpaqueOp);

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Control {
    Yield = 0,
    Error = 1,
}

const _: () = {
    use std::mem::align_of;
    assert!(align_of::<JumpTable>() == align_of::<[OpaqueOp; 1]>());
};

#[repr(transparent)]
pub struct ContextPtr(*mut Context);

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

pub type Op<Operands> =
    Op!(fn(Operands, JumpTablePtr, StackPtr, InstructionPtr, ContextPtr) -> Control);

#[repr(transparent)]
pub struct Operands(u32);

pub type OpaqueOp =
    Op!(fn(Operands, JumpTablePtr, StackPtr, InstructionPtr, ContextPtr) -> Control);

macro_rules! declare_operand_type {
    ($name:ident, $ty:ident) => {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        pub struct $name($ty);

        impl $name {
            #[inline(always)]
            pub fn get(self) -> $ty {
                self.0
            }

            #[inline(always)]
            pub fn new(v: $ty) -> Self {
                Self(v)
            }
        }
    };
    ($name:ident, $ty:ident, $zx_ty:ident) => {
        declare_operand_type!($name, $ty);

        impl $name {
            #[inline(always)]
            pub fn zx(self) -> $zx_ty {
                self.0 as $zx_ty
            }
        }
    };
}

macro_rules! declare_operand_types {
    (
        $(
            $name:ident( $ty:ident $(as $zx_ty:ident)? )
        ),* $(,)?
    ) => {
        $(
            declare_operand_type!($name, $ty $(, $zx_ty)?);
        )*
    };
}

declare_operand_types! {
    Reg(u8 as usize),

    Lit(u16 as usize),
    Lit8(u8 as usize),

    Imm8(u8 as usize),
    Imm16(i16),
    Imm24(u24),
}

const fn assert_bit_equal<A, B>(a: &A, b: &B) {
    let a: [u32; 1] = unsafe { (a as *const A as *const [u32; 1]).read() };
    let b: [u32; 1] = unsafe { (b as *const B as *const [u32; 1]).read() };
    if a[0] != b[0] {
        panic!("not bit equal");
    }
}

// ...
