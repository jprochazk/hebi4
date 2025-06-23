pub mod nodes;

use crate::{intern::Interner, token::Tokens};
use nodes::Packed;

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

declare_intern_id!(StrId);
declare_intern_id!(IdentId);

pub struct Ast {
    root: Packed,
    nodes: Vec<Packed>,
    strings: Interner<StrId>,
    idents: Interner<IdentId>,
}

pub struct AstBuilder {
    nodes: Vec<Packed>,
    strings: Interner<StrId>,
    idents: Interner<IdentId>,
}

impl AstBuilder {
    pub(crate) fn new(tokens: &Tokens<'_>) -> Self {
        Self {
            // le shrug
            nodes: Vec::with_capacity(tokens.len() / 2),
            strings: Interner::with_capacity(tokens.len() / 16),
            idents: Interner::with_capacity(tokens.len() / 8),
        }
    }

    pub(crate) fn build(self, root: Packed) -> Ast {
        todo!()
    }
}
