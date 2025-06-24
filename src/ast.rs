pub mod nodes;

use crate::{
    intern::{Interner, simple::SimpleInterner},
    token::Tokens,
};
use nodes::{Packed, u24, u56};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum AssignOp {
    None,
    Add,
    Sub,
    Mul,
    Div,
}

#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum InfixOp {
    Or, And,
    Eq, Ne,
    Gt, Ge, Lt, Le,
    Add, Sub,
    Mul, Div
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PrefixOp {
    Minus,
    Not,
}

macro_rules! roundtrip_u24_op {
    ($T:ident) => {
        impl $T {
            fn from_u24(v: u24) -> Self {
                let v = v.get() as u8;
                unsafe { core::mem::transmute(v) }
            }

            fn into_u24(self) -> u24 {
                let v = self as u32;
                unsafe { u24::new_unchecked(v) }
            }
        }
    };
}

roundtrip_u24_op!(AssignOp);
roundtrip_u24_op!(InfixOp);
roundtrip_u24_op!(PrefixOp);

declare_intern_id!(StrId);
declare_intern_id!(IdentId);
declare_intern_id!(FloatId);

pub struct Ast {
    root: Packed,
    nodes: Vec<Packed>,
    strings: Interner<StrId>,
    idents: Interner<IdentId>,
    floats: SimpleInterner<FloatId, f64>,
}

pub struct AstBuilder {
    nodes: Vec<Packed>,
    strings: Interner<StrId>,
    idents: Interner<IdentId>,
    floats: SimpleInterner<FloatId, f64>,
}

impl AstBuilder {
    pub(crate) fn new(tokens: &Tokens<'_>) -> Self {
        Self {
            // le shrug
            nodes: Vec::with_capacity(tokens.len() / 2),
            strings: Interner::with_capacity(tokens.len() / 16),
            idents: Interner::with_capacity(tokens.len() / 8),

            // really spitballin here
            floats: SimpleInterner::with_capacity(128),
        }
    }

    pub(crate) fn build(self, root: Packed) -> Ast {
        todo!()
    }

    fn append(&mut self, nodes: &[Packed]) -> u32 {
        todo!()
    }
}

macro_rules! roundtrip_u56_id {
    ($T:ident) => {
        impl $T {
            fn from_u56(v: u56) -> Self {
                use crate::intern::Intern as _;
                let v = v.get() as u32;
                unsafe { Self::from_index(v) }
            }

            fn into_u56(self) -> u56 {
                use crate::intern::Intern as _;
                let v = v.index();
                unsafe { u56::new_unchecked(v as u64) }
            }
        }
    };
}

roundtrip_u56_id!(StrId);
roundtrip_u56_id!(IdentId);
roundtrip_u56_id!(FloatId);
