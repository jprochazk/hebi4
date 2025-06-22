use std::num::NonZeroU32;

use crate::{intern::Interner, token::Tokens};

#[derive(Clone, Copy, Debug)]
pub struct Packed {
    tag_and_length: NonZeroU32,
    payload: u32,

    #[cfg(debug_assertions)]
    node_kind: DebugNodeKind,
}

/// ## Safety
///
/// `Self` must be ABI-compatible with `Packed`.
///
/// This should be implemented for `Packed` wrapper types,
/// but those types must be `#[repr(transparent)]`.
pub unsafe trait PackedAbi: private::Sealed {
    unsafe fn from_packed_unchecked(v: Packed) -> Self;
    fn into_packed(self) -> Packed;
}

unsafe impl PackedAbi for Packed {
    unsafe fn from_packed_unchecked(v: Packed) -> Self {
        v
    }

    fn into_packed(self) -> Packed {
        self
    }
}

#[derive(Default, Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct NodeIndex(u32);

impl NodeIndex {
    pub fn get(&self) -> u32 {
        self.0
    }
}

#[derive(Clone, Copy, Debug)]
pub enum DebugNodeKind {
    LengthIndex,
    Index,
    InlineValue,
    TagOnly,
}

const TAG_MASK: u32 = 0b11111111_00000000_00000000_00000000;
const LEN_MASK: u32 = 0b00000000_11111111_11111111_11111111;

pub const U24_MAX: u32 = (2u64.pow(24) - 1) as u32;
pub const U56_MAX: u64 = 2u64.pow(56) - 1;

impl Packed {
    #[inline]
    fn with_length_index(tag: Tag, length: u32, index: NodeIndex) -> Self {
        // max len is u24
        assert!(length <= U24_MAX);

        // SAFETY: `tag` can never be 0, so neither can this whole expression:
        let tag_and_length = unsafe { NonZeroU32::new_unchecked(((tag as u32) << 24) | length) };

        Self {
            tag_and_length,
            payload: index.get(),

            #[cfg(debug_assertions)]
            node_kind: DebugNodeKind::LengthIndex,
        }
    }

    #[inline]
    fn with_index(tag: Tag, index: NodeIndex) -> Self {
        // SAFETY: `tag` can never be 0, so neither can this whole expression:
        let tag_and_length = unsafe { NonZeroU32::new_unchecked((tag as u32) << 24) };

        Self {
            tag_and_length,
            payload: index.get(),

            #[cfg(debug_assertions)]
            node_kind: DebugNodeKind::Index,
        }
    }

    /// A node with an inline value.
    ///
    /// The limits for the value are `N > 0 && N < u56::MAX`.
    #[inline]
    fn with_value(tag: Tag, value: u64) -> Self {
        assert!(value <= U56_MAX);

        let hi = ((value & 0xFFFFFFFF00000000) >> 32) as u32;
        let lo = (value & 0x00000000FFFFFFFF) as u32;

        // SAFETY: `tag` can never be 0, so neither can this whole expression:
        let tag_and_length = unsafe { NonZeroU32::new_unchecked(((tag as u32) << 24) | hi) };
        let payload = lo;

        Self {
            tag_and_length,
            payload,

            #[cfg(debug_assertions)]
            node_kind: DebugNodeKind::InlineValue,
        }
    }

    /// A node which is represented only by its tag.
    #[inline]
    fn tag_only(tag: Tag) -> Self {
        // SAFETY: `tag` can never be 0, so neither can this whole expression:
        let tag_and_length = unsafe { NonZeroU32::new_unchecked((tag as u32) << 24) };

        Self {
            tag_and_length,
            payload: 0,

            #[cfg(debug_assertions)]
            node_kind: DebugNodeKind::TagOnly,
        }
    }

    #[inline]
    fn tag(&self) -> Tag {
        let tag = ((self.tag_and_length.get() & TAG_MASK) >> 24) as u8;

        unsafe { std::mem::transmute::<u8, Tag>(tag) }
    }

    /// Number of subnodes.
    ///
    /// Only available if constructed with [`Packed::with_length_index`].
    #[inline]
    fn length(&self) -> u32 {
        debug_assert!(matches!(self.node_kind, DebugNodeKind::LengthIndex));

        self.tag_and_length.get() & LEN_MASK
    }

    /// Number of subnodes.
    ///
    /// Only available if constructed with [`Packed::with_length_index`] or [`Packed::with_index`].
    #[inline]
    fn index(&self) -> NodeIndex {
        debug_assert!(matches!(
            self.node_kind,
            DebugNodeKind::LengthIndex | DebugNodeKind::Index
        ));

        NodeIndex(self.payload)
    }

    /// Inline value.
    ///
    /// Only available if constructed with [`Packed::with_value`].
    #[inline]
    fn value(&self) -> u64 {
        debug_assert!(matches!(self.node_kind, DebugNodeKind::InlineValue));

        let hi = (self.tag_and_length.get() & LEN_MASK) as u64;
        let lo = (self.payload) as u64;

        (hi << 32) | lo
    }
}

impl private::Sealed for Packed {}
impl Tagged for Packed {
    #[inline]
    fn base_tag(&self) -> Tag {
        self.tag()
    }

    #[inline]
    fn tag_range() -> std::ops::RangeInclusive<u8> {
        0..=u8::MAX
    }
}

// Statically assert on sizes
#[cfg(debug_assertions)]
const _: () = {
    let _ = std::mem::transmute::<[u8; 12], Packed>;
    let _ = std::mem::transmute::<Option<Packed>, Packed>;
};

#[cfg(not(debug_assertions))]
const _: () = {
    let _ = std::mem::transmute::<[u8; 8], Packed>;
    let _ = std::mem::transmute::<Option<Packed>, Packed>;
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Tag {
    Root = 1,

    // reserved: 15-16

    // Stmt
    StmtVar = 16,
    StmtFn = 17,
    StmtLoop = 18,
    StmtExpr = 19,

    // reserved: 20-127

    // Expr
    ExprReturn = 128,
    ExprBreak = 129,
    ExprContinue = 130,

    ExprIf = 131,
    ExprDo = 132,
    ExprFn = 133,

    ExprAssign = 134,

    ExprInfix = 135,
    ExprPrefix = 136,

    ExprCall = 137,
    ExprIndex = 138,
    ExprField = 139,

    ExprArray = 140,
    ExprObject = 141,
    ExprInt = 142,
    ExprFloat = 143,
    ExprBool = 144,
    ExprStr = 145,
    ExprNil = 146,

    ExprUse = 147,

    // reserved: 147-244

    // Misc
    Ident = 245,
    // reserved: 246-255
    None = 255,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum StmtKind {
    Var = Tag::StmtVar as u8,
    Fn = Tag::StmtFn as u8,
    Loop = Tag::StmtLoop as u8,

    // TODO: make `Stmt` superset of `Expr`?
    Expr = Tag::StmtExpr as u8,
}

const STMT_MIN: u8 = StmtKind::Var as u8;
const STMT_MAX: u8 = StmtKind::Expr as u8;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct Stmt(Packed);

unsafe impl PackedAbi for Stmt {
    unsafe fn from_packed_unchecked(v: Packed) -> Self {
        debug_assert!(v.tag() as u8 >= STMT_MIN && v.tag() as u8 <= STMT_MAX);
        Self(v)
    }

    fn into_packed(self) -> Packed {
        self.0
    }
}

impl Stmt {
    #[inline]
    pub fn kind(&self) -> StmtKind {
        let tag = self.0.tag();
        debug_assert!(tag as u8 >= STMT_MIN && tag as u8 <= STMT_MAX);
        unsafe { std::mem::transmute::<Tag, StmtKind>(tag) }
    }

    #[inline]
    pub fn unpack<'ast, T: Node<NodeKind = Stmt>>(&self, ast: &'ast Ast) -> T::Output<'ast> {
        assert!(self.base_tag() == T::TAG);
        unsafe { self.unpack_unchecked::<T>(ast) }
    }

    #[inline]
    pub unsafe fn unpack_unchecked<'ast, T: Node<NodeKind = Stmt>>(
        &self,
        ast: &'ast Ast,
    ) -> T::Output<'ast> {
        debug_assert!(self.base_tag() == T::TAG);
        unsafe { T::unpack_unchecked(*self, ast) }
    }
}

impl private::Sealed for Stmt {}
impl Tagged for Stmt {
    #[inline]
    fn base_tag(&self) -> Tag {
        self.0.tag()
    }

    #[inline]
    fn tag_range() -> std::ops::RangeInclusive<u8> {
        STMT_MIN..=STMT_MAX
    }
}

impl Packed {
    #[inline]
    fn as_stmt(self) -> Option<Stmt> {
        if (self.tag() as u8) < STMT_MIN || (self.tag() as u8) > STMT_MAX {
            return None;
        }

        Some(Stmt(self))
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum ExprKind {
    Return = Tag::ExprReturn as u8,
    Break = Tag::ExprBreak as u8,
    Continue = Tag::ExprContinue as u8,

    If = Tag::ExprIf as u8,
    Do = Tag::ExprDo as u8,
    Fn = Tag::ExprFn as u8,

    Assign = Tag::ExprAssign as u8,

    Infix = Tag::ExprInfix as u8,
    Prefix = Tag::ExprPrefix as u8,

    Call = Tag::ExprCall as u8,
    Index = Tag::ExprIndex as u8,
    Field = Tag::ExprField as u8,

    Array = Tag::ExprArray as u8,
    Object = Tag::ExprObject as u8,
    Int = Tag::ExprInt as u8,
    Float = Tag::ExprFloat as u8,
    Bool = Tag::ExprBool as u8,
    Str = Tag::ExprStr as u8,
    Nil = Tag::ExprNil as u8,

    Use = Tag::ExprUse as u8,
}

const EXPR_MIN: u8 = ExprKind::Return as u8;
const EXPR_MAX: u8 = ExprKind::Use as u8;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct Expr(Packed);

unsafe impl PackedAbi for Expr {
    #[inline]
    unsafe fn from_packed_unchecked(v: Packed) -> Self {
        debug_assert!(v.tag() as u8 >= EXPR_MIN && v.tag() as u8 <= EXPR_MAX);
        Self(v)
    }

    #[inline]
    fn into_packed(self) -> Packed {
        self.0
    }
}

impl Expr {
    #[inline]
    pub fn kind(&self) -> ExprKind {
        let tag = self.0.tag();
        debug_assert!(tag as u8 >= EXPR_MIN && tag as u8 <= EXPR_MAX);
        unsafe { std::mem::transmute::<Tag, ExprKind>(tag) }
    }

    #[inline]
    pub fn unpack<'ast, T: Node<NodeKind = Expr>>(&self, ast: &'ast Ast) -> T::Output<'ast> {
        assert!(self.base_tag() == T::TAG);
        unsafe { self.unpack_unchecked::<T>(ast) }
    }

    #[inline]
    pub unsafe fn unpack_unchecked<'ast, T: Node<NodeKind = Expr>>(
        &self,
        ast: &'ast Ast,
    ) -> T::Output<'ast> {
        unsafe { T::unpack_unchecked(*self, ast) }
    }
}

impl Packed {
    #[inline]
    fn as_expr(self) -> Option<Expr> {
        if (self.tag() as u8) < EXPR_MIN || (self.tag() as u8) > EXPR_MAX {
            return None;
        }

        Some(Expr(self))
    }
}

impl private::Sealed for Expr {}
impl Tagged for Expr {
    #[inline]
    fn base_tag(&self) -> Tag {
        self.0.tag()
    }

    #[inline]
    fn tag_range() -> std::ops::RangeInclusive<u8> {
        EXPR_MIN..=EXPR_MAX
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum MiscKind {
    Ident = Tag::Ident as u8,
}

const MISC_MIN: u8 = MiscKind::Ident as u8;
const MISC_MAX: u8 = MiscKind::Ident as u8;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct Misc(Packed);

unsafe impl PackedAbi for Misc {
    #[inline]
    unsafe fn from_packed_unchecked(v: Packed) -> Self {
        debug_assert!(v.tag() as u8 >= MISC_MIN && v.tag() as u8 <= MISC_MIN);
        Self(v)
    }

    #[inline]
    fn into_packed(self) -> Packed {
        self.0
    }
}

impl Misc {
    #[inline]
    pub fn kind(&self) -> MiscKind {
        let tag = self.0.tag();
        debug_assert!(tag as u8 >= MISC_MIN && tag as u8 <= MISC_MAX);
        unsafe { std::mem::transmute::<Tag, MiscKind>(tag) }
    }

    #[inline]
    pub unsafe fn unpack_unchecked<'ast, T: Node<NodeKind = Misc>>(
        &self,
        ast: &'ast Ast,
    ) -> T::Output<'ast> {
        unsafe { T::unpack_unchecked(*self, ast) }
    }
}

impl private::Sealed for Misc {}
impl Tagged for Misc {
    #[inline]
    fn base_tag(&self) -> Tag {
        self.0.tag()
    }

    #[inline]
    fn tag_range() -> std::ops::RangeInclusive<u8> {
        MISC_MIN..=MISC_MAX
    }
}

impl Packed {
    #[inline]
    fn as_misc(self) -> Option<Misc> {
        if (self.tag() as u8) < MISC_MIN || (self.tag() as u8) > MISC_MAX {
            return None;
        }

        Some(Misc(self))
    }
}

mod private {
    pub trait Sealed {}
}

pub trait Tagged: private::Sealed {
    fn base_tag(&self) -> Tag;
    fn tag_range() -> std::ops::RangeInclusive<u8>;
}

declare_intern_id!(Str);
declare_intern_id!(Ident);

pub struct Ast {
    root: Packed,
    nodes: Vec<Packed>,
    strings: Interner<Str>,
    idents: Interner<Ident>,
}

impl Ast {
    #[inline]
    pub fn root(&self) -> node::Root<'_> {
        assert!(self.root.tag() == Tag::Root);
        unsafe { node::Root::unpack_unchecked(self.root, self) }
    }

    /// Retrieves a contiguous slice of [`Packed`].
    #[inline]
    fn slice(&self, index: NodeIndex, length: u32) -> Option<&[Packed]> {
        let start = index.0 as usize;
        self.nodes.get(start..start + length as usize)
    }

    #[inline]
    fn components<const N: usize>(&self, index: NodeIndex) -> Option<[Packed; N]> {
        let start = index.get() as usize;
        match self.nodes.get(start..start + N) {
            Some(slice) => Some(unsafe { slice.try_into().unwrap_unchecked() }),
            None => None,
        }
    }

    #[inline]
    fn components_with_tail<const N: usize>(
        &self,
        index: NodeIndex,
        tail_length: u32,
    ) -> Option<([Packed; N], &[Packed])> {
        let start = index.get() as usize;
        match self.nodes.get(start..start + N + tail_length as usize) {
            Some(slice) => {
                let components = unsafe { slice[..N].try_into().unwrap_unchecked() };
                let tail = &slice[N..];
                Some((components, tail))
            }
            None => None,
        }
    }
}

pub struct AstBuilder {
    nodes: Vec<Packed>,
    strings: Interner<Str>,
    idents: Interner<Ident>,
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
        debug_assert!(root.tag() == Tag::Root);

        let Self {
            nodes,
            strings,
            idents,
        } = self;

        Ast {
            root,
            nodes,
            strings,
            idents,
        }
    }

    /// Inserts a single `node`.
    #[inline]
    fn insert_packed(&mut self, node: Packed) -> NodeIndex {
        let index = self.nodes.len();
        self.nodes.push(node);
        NodeIndex(index as u32)
    }

    /// Inserts `nodes` and returns the index of the first one.
    ///
    /// The nodes are guaranteed to be contiguous, meaning they may be
    /// retrieved with `ast.span`.
    #[inline]
    fn insert_packed_contiguous(&mut self, nodes: &[Packed]) -> Option<(NodeIndex, u32)> {
        if nodes.is_empty() {
            return None;
        }

        let total_len = self.nodes.len() + nodes.len();
        if total_len > u32::MAX as usize {
            panic!("exceeded maximum number of AST nodes");
        }

        let index = NodeIndex(self.nodes.len() as u32);
        self.nodes.reserve(nodes.len());

        // going through `extend` is awful
        let dst = self.nodes.spare_capacity_mut();
        dst[0].write(nodes[0]);
        for i in 1..nodes.len() {
            unsafe {
                dst.get_unchecked_mut(i).write(*nodes.get_unchecked(i));
            }
        }
        unsafe {
            self.nodes.set_len(total_len);
        }

        Some((index, nodes.len() as u32))
    }
}

mod debug;
