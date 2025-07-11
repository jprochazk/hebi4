pub mod nodes;

pub use nodes::*;

use crate::{
    intern::{Interner, simple::SimpleInterner},
    span::{Span, Spanned},
    token::Tokens,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AssignOp {
    None,
    Add,
    Sub,
    Mul,
    // TODO: integer division
    Div,
}

impl AssignOp {
    pub fn debug(&self, _: &Ast) -> impl std::fmt::Debug + '_ {
        self
    }
}

#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum InfixOp {
    Or, And,
    Eq, Ne,
    Gt, Ge, Lt, Le,
    Add, Sub,
    // TODO: integer division
    Mul, Div
}

impl InfixOp {
    pub fn debug(&self, _: &Ast) -> impl std::fmt::Debug + '_ {
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PrefixOp {
    Minus,
    Not,
}

impl PrefixOp {
    pub fn debug(&self, _: &Ast) -> impl std::fmt::Debug + '_ {
        self
    }
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

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct f64n(u64);

impl f64n {
    #[inline]
    pub fn new(v: f64) -> Self {
        Self(v.to_bits())
    }

    #[inline]
    pub fn get(self) -> f64 {
        f64::from_bits(self.0)
    }
}

impl f64n {
    pub fn debug(&self, _: &Ast) -> impl std::fmt::Debug + '_ {
        self
    }
}

impl std::fmt::Debug for f64n {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.get(), f)
    }
}

roundtrip_u24_op!(AssignOp);
roundtrip_u24_op!(InfixOp);
roundtrip_u24_op!(PrefixOp);

declare_intern_id!(pub StrId);
declare_intern_id!(pub IdentId);
declare_intern_id!(pub FloatId);

impl StrId {
    pub fn debug<'a>(&self, ast: &'a Ast) -> impl std::fmt::Debug + 'a {
        ast.strings.get(*self).unwrap()
    }
}

impl IdentId {
    pub fn debug<'a>(&self, ast: &'a Ast) -> impl std::fmt::Debug + 'a {
        ast.idents.get(*self).unwrap()
    }
}

impl FloatId {
    pub fn debug<'a>(&self, ast: &'a Ast) -> impl std::fmt::Debug + 'a {
        ast.floats.get(*self).unwrap()
    }
}

pub use private::Ast;
mod private {
    use super::*;

    pub struct Ast {
        root: Option<Root>,
        pub(crate) nodes: Vec<Packed>,
        pub(crate) spans: Vec<Span>,
        pub(crate) strings: Interner<StrId>,
        pub(crate) idents: Interner<IdentId>,
        pub(crate) floats: SimpleInterner<FloatId, f64n>,
    }

    impl Ast {
        pub(crate) fn new(tokens: &Tokens<'_>) -> Self {
            Self {
                root: None,

                // le shrug
                nodes: Vec::with_capacity(tokens.len() / 2),
                spans: Vec::with_capacity(tokens.len() / 2),
                strings: Interner::with_capacity(tokens.len() / 16),
                idents: Interner::with_capacity(tokens.len() / 8),

                // really spitballin here
                floats: SimpleInterner::with_capacity(128),
            }
        }

        pub(crate) fn set_root(&mut self, root: Root) {
            assert!(self.root.is_none());
            self.root = Some(root);
        }

        pub(crate) fn root(&self) -> Node<'_, Root> {
            let root = self.root.as_ref().unwrap();
            Node {
                ast: self,
                node: root,
            }
        }

        pub(crate) fn intern_ident(&mut self, ident: &str) -> IdentId {
            self.idents.intern(ident)
        }

        pub(crate) fn intern_str(&mut self, str: &str) -> StrId {
            self.strings.intern(str)
        }

        pub(crate) fn intern_float(&mut self, v: f64) -> FloatId {
            self.floats.intern(f64n::new(v))
        }

        pub(super) fn append(&mut self, nodes: &[Spanned<Packed>]) -> u32 {
            let index = self.nodes.len() as u32;

            self.spans.reserve(nodes.len());
            self.nodes.reserve(nodes.len());

            let spans_mem = self.spans.spare_capacity_mut();
            let nodes_mem = self.nodes.spare_capacity_mut();
            for (i, node) in nodes.iter().enumerate() {
                unsafe {
                    spans_mem.get_unchecked_mut(i).write(node.span);
                    nodes_mem.get_unchecked_mut(i).write(node.into_inner());
                }
            }

            unsafe {
                let new_len = self.nodes.len() + nodes.len();
                self.spans.set_len(new_len);
                self.nodes.set_len(new_len);
            }

            index
        }
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
                let v = self.index();
                unsafe { u56::new_unchecked(v as u64) }
            }
        }
    };
}

roundtrip_u56_id!(StrId);
roundtrip_u56_id!(IdentId);
roundtrip_u56_id!(FloatId);

impl std::fmt::Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.root().debug(self).fmt(f)
    }
}
