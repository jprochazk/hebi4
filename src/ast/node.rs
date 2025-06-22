use std::marker::PhantomData;

use super::{Ast, AstBuilder, Expr, Node, Packed, PackedAbi, Stmt, Tag, Tagged, private};
use crate::intern::Intern as _;

#[macro_use]
mod macros;

declare_node! {
    #[kind(Packed), tag(Root)]
    pub struct Root<'a> {
        #[tail]
        pub body: &'a [Stmt],
    }
}

impl Root<'_> {
    pub fn body(&self) -> impl ExactSizeIterator<Item = Stmt> + '_ {
        self.body.iter().copied()
    }
}

pub mod stmt {
    use super::*;

    declare_node! {
        #[kind(Stmt), tag(StmtVar)]
        pub struct Var {
            pub name: Ident,
            pub value: super::Expr,
        }
    }

    declare_node! {
        #[kind(Stmt), tag(StmtFn)]
        pub struct Fn<'a> {
            pub name: Ident,
            pub body: super::Expr,
            #[tail]
            pub params: &'a [Ident],
        }
    }

    impl Fn<'_> {
        pub fn params(&self) -> impl ExactSizeIterator<Item = Ident> + '_ {
            self.params.iter().copied()
        }
    }

    declare_node! {
        #[kind(Stmt), tag(StmtLoop)]
        pub struct Loop<'a> {
            #[tail]
            pub body: &'a [Stmt],
        }
    }

    impl Loop<'_> {
        pub fn body(&self) -> impl ExactSizeIterator<Item = Stmt> + '_ {
            self.body.iter().copied()
        }
    }

    declare_node! {
        #[kind(Stmt), tag(StmtExpr)]
        pub struct Expr {
            pub inner: super::Expr,
        }
    }
}

pub mod expr {
    use crate::ast::U56_MAX;

    use super::*;

    declare_node! {
        #[kind(Expr), tag(ExprReturn)]
        pub struct Return {
            pub value: Opt<Expr>,
        }
    }

    declare_node! {
        #[kind(Expr), tag(ExprBreak)]
        pub struct Break {}
    }

    declare_node! {
        #[kind(Expr), tag(ExprContinue)]
        pub struct Continue {}
    }

    declare_node! {
        #[kind(Expr), tag(ExprIndex)]
        pub struct Index {
            pub key: Expr,
        }
    }

    declare_node! {
        #[kind(Expr), tag(ExprField)]
        pub struct Field {
            pub key: Ident,
        }
    }

    declare_node! {
        #[kind(Expr), tag(ExprDo)]
        pub struct Do<'a> {
            #[tail]
            pub body: &'a [Stmt],
        }
    }

    impl Do<'_> {
        pub fn body(&self) -> impl ExactSizeIterator<Item = Stmt> + '_ {
            self.body.iter().copied()
        }
    }

    pub struct Infix {
        pub lhs: Expr,
        pub rhs: Expr,
        pub op: InfixOp,
    }

    #[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
    #[repr(u8)]
    pub enum InfixOp {
        Or = 1,
        And,
        Eq,
        Ne,
        Gt,
        Ge,
        Lt,
        Le,
        Add,
        Sub,
        Mul,
        Div,
    }

    impl InfixOp {
        unsafe fn from_raw(length: u32) -> Self {
            unsafe { std::mem::transmute::<_, InfixOp>((length >> 16) as u8) }
        }

        fn into_raw(self) -> u32 {
            (self as u32) << 16
        }
    }

    impl private::Sealed for Infix {}
    impl Node for Infix {
        type NodeKind = Expr;

        type Output<'ast> = Infix;

        const TAG: Tag = Tag::ExprInfix;

        unsafe fn unpack_unchecked<'ast>(
            node: Self::NodeKind,
            ast: &'ast crate::ast::Ast,
        ) -> Self::Output<'ast> {
            debug_assert!(node.base_tag() == Tag::ExprInfix);

            let packed = node.into_packed();
            let [lhs, rhs] = unsafe { ast.components(packed.index()).unwrap_unchecked() };
            let lhs = unsafe { <Expr>::from_packed_unchecked(lhs) };
            let rhs = unsafe { <Expr>::from_packed_unchecked(rhs) };
            let op = unsafe { InfixOp::from_raw(packed.length()) };

            Infix { lhs, rhs, op }
        }

        fn pack_into(&self, ast: &mut AstBuilder) -> Self::NodeKind {
            let tag = Tag::ExprInfix;

            unsafe {
                Expr::from_packed_unchecked({
                    let index = {
                        let index = ast.insert_packed(self.lhs.into_packed());
                        ast.insert_packed(self.rhs.into_packed());
                        index
                    };
                    let length = self.op.into_raw();
                    Packed::with_length_index(tag, length, index)
                })
            }
        }
    }

    pub struct Prefix {
        pub rhs: Expr,
        pub op: PrefixOp,
    }

    #[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
    #[repr(u8)]
    pub enum PrefixOp {
        Minus = 1,
        Not,
    }

    impl PrefixOp {
        unsafe fn from_raw(length: u32) -> Self {
            unsafe { std::mem::transmute::<_, PrefixOp>((length >> 16) as u8) }
        }

        fn into_raw(self) -> u32 {
            (self as u32) << 16
        }
    }

    impl private::Sealed for Prefix {}
    impl Node for Prefix {
        type NodeKind = Expr;

        type Output<'ast> = Prefix;

        const TAG: Tag = Tag::ExprPrefix;

        unsafe fn unpack_unchecked<'ast>(
            node: Self::NodeKind,
            ast: &'ast crate::ast::Ast,
        ) -> Self::Output<'ast> {
            debug_assert!(node.base_tag() == Tag::ExprPrefix);

            let packed = node.into_packed();
            let [rhs] = unsafe { ast.components(packed.index()).unwrap_unchecked() };
            let rhs = unsafe { <Expr>::from_packed_unchecked(rhs) };
            let op = unsafe { PrefixOp::from_raw(packed.length()) };

            Prefix { rhs, op }
        }

        fn pack_into(&self, ast: &mut AstBuilder) -> Self::NodeKind {
            let tag = Tag::ExprPrefix;

            unsafe {
                Expr::from_packed_unchecked({
                    let index = ast.insert_packed(self.rhs.into_packed());
                    let length = self.op.into_raw();
                    Packed::with_length_index(tag, length, index)
                })
            }
        }
    }

    // TODO: dedupe Str, Ident, Int, etc. somehow?
    #[derive(Clone, Copy)]
    #[repr(transparent)]
    pub struct Str(Packed);

    unsafe impl PackedAbi for Str {
        #[inline]
        unsafe fn from_packed_unchecked(v: Packed) -> Self {
            debug_assert!(v.tag() == Tag::ExprStr);
            debug_assert!(TryInto::<u32>::try_into(v.value()).is_ok());
            Str(v)
        }

        #[inline]
        fn into_packed(self) -> Packed {
            self.0
        }
    }

    impl Str {
        #[inline]
        pub fn as_str<'ast>(&self, ast: &'ast Ast) -> Option<&'ast str> {
            ast.strings.get(self.id())
        }

        #[inline]
        pub unsafe fn as_str_unchecked<'ast>(&self, ast: &'ast Ast) -> &'ast str {
            debug_assert!(ast.strings.get(self.id()).is_some());
            unsafe { ast.strings.get(self.id()).unwrap_unchecked() }
        }
    }

    impl Str {
        #[inline]
        fn id(&self) -> crate::ast::Str {
            unsafe { crate::ast::Str::from_index(self.0.value() as u32) }
        }

        pub(super) fn from_id(id: crate::ast::Str) -> Self {
            Self(Packed::with_value(Tag::ExprStr, id.index() as u64))
        }
    }

    impl private::Sealed for Str {}
    impl Tagged for Str {
        #[inline]
        fn base_tag(&self) -> Tag {
            Tag::ExprStr
        }

        #[inline]
        fn tag_range() -> std::ops::RangeInclusive<u8> {
            Tag::ExprStr as u8..=Tag::ExprStr as u8
        }
    }

    impl Node for Str {
        type NodeKind = Expr;

        type Output<'ast> = Str;

        const TAG: Tag = Tag::ExprStr;

        #[inline]
        unsafe fn unpack_unchecked<'ast>(
            node: Self::NodeKind,
            _: &'ast crate::ast::Ast,
        ) -> Self::Output<'ast> {
            debug_assert!(node.base_tag() == Tag::ExprStr);
            Self(node.into_packed())
        }

        #[inline]
        fn pack_into(&self, _: &mut AstBuilder) -> Self::NodeKind {
            unsafe { Expr::from_packed_unchecked(self.0) }
        }
    }

    #[derive(Clone, Copy)]
    #[repr(transparent)]
    pub struct Int(Packed);

    impl Int {
        pub fn parse(v: &str) -> Option<Self> {
            v.parse::<u64>().ok().and_then(|v| {
                if v <= U56_MAX {
                    Some(Self::from_u56(v))
                } else {
                    None
                }
            })
        }

        pub fn try_from_u56(v: u64) -> Option<Self> {
            if v <= U56_MAX {
                Some(Self::from_u56(v))
            } else {
                None
            }
        }

        /// ## Panics
        ///
        /// Panics if `v` is greater than `U56_MAX`.
        pub fn from_u56(v: u64) -> Self {
            Self(Packed::with_value(Tag::ExprInt, v))
        }

        pub fn into_u56(self) -> u64 {
            self.0.value()
        }
    }

    unsafe impl PackedAbi for Int {
        #[inline]
        unsafe fn from_packed_unchecked(v: Packed) -> Self {
            debug_assert!(v.tag() == Tag::ExprInt);
            Int(v)
        }

        #[inline]
        fn into_packed(self) -> Packed {
            self.0
        }
    }

    impl private::Sealed for Int {}
    impl Tagged for Int {
        #[inline]
        fn base_tag(&self) -> Tag {
            Tag::ExprInt
        }

        #[inline]
        fn tag_range() -> std::ops::RangeInclusive<u8> {
            Tag::ExprInt as u8..=Tag::ExprInt as u8
        }
    }

    impl Node for Int {
        type NodeKind = Expr;

        type Output<'ast> = Int;

        const TAG: Tag = Tag::ExprInt;

        #[inline]
        unsafe fn unpack_unchecked<'ast>(
            node: Self::NodeKind,
            _: &'ast crate::ast::Ast,
        ) -> Self::Output<'ast> {
            debug_assert!(node.base_tag() == Tag::ExprInt);
            Self(node.into_packed())
        }

        #[inline]
        fn pack_into(&self, _: &mut AstBuilder) -> Self::NodeKind {
            unsafe { Expr::from_packed_unchecked(self.0) }
        }
    }
}

impl AstBuilder {
    #[inline]
    pub fn intern_str(&mut self, s: &str) -> expr::Str {
        expr::Str::from_id(self.strings.intern(s))
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ident(Packed);

unsafe impl PackedAbi for Ident {
    #[inline]
    unsafe fn from_packed_unchecked(v: Packed) -> Self {
        debug_assert!(v.tag() == Tag::Ident);
        debug_assert!(TryInto::<u32>::try_into(v.value()).is_ok());
        Ident(v)
    }

    #[inline]
    fn into_packed(self) -> Packed {
        self.0
    }
}

impl Ident {
    #[inline]
    pub fn as_str<'ast>(&self, ast: &'ast Ast) -> Option<&'ast str> {
        let id = self.id();
        ast.idents.get(id)
    }

    #[inline]
    pub unsafe fn as_str_unchecked<'ast>(&self, ast: &'ast Ast) -> &'ast str {
        debug_assert!(ast.idents.get(self.id()).is_some());
        unsafe { ast.idents.get(self.id()).unwrap_unchecked() }
    }
}

impl Ident {
    #[inline]
    fn id(&self) -> crate::ast::Ident {
        unsafe { crate::ast::Ident::from_index(self.0.value() as u32) }
    }

    fn from_id(id: crate::ast::Ident) -> Self {
        Self(Packed::with_value(Tag::Ident, id.index() as u64))
    }
}

impl AstBuilder {
    #[inline]
    pub fn intern_ident(&mut self, ident: &str) -> Ident {
        Ident::from_id(self.idents.intern(ident))
    }
}

impl private::Sealed for Ident {}
impl Tagged for Ident {
    #[inline]
    fn base_tag(&self) -> Tag {
        Tag::Ident
    }

    #[inline]
    fn tag_range() -> std::ops::RangeInclusive<u8> {
        Tag::Ident as u8..=Tag::Ident as u8
    }
}

/// Like [`Option`], but ABI-compatible with [`Packed`], and uses the
/// packed tag as a discriminator ([`Tag::None`]).
///
/// Instead of handling this directly, convert it to an `Option<T>` first.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Opt<T>(Packed, PhantomData<fn(T) -> T>);

impl<T> Opt<T> {
    pub fn is_some(&self) -> bool {
        self.0.tag() != Tag::None
    }

    pub fn is_none(&self) -> bool {
        self.0.tag() == Tag::None
    }
}

impl<T: PackedAbi + Copy> From<Option<T>> for Opt<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(v) => Self::some(v),
            None => Self::none(),
        }
    }
}

impl<T: PackedAbi + Copy> From<Opt<T>> for Option<T> {
    fn from(value: Opt<T>) -> Self {
        if value.is_some() {
            Some(unsafe { T::from_packed_unchecked(value.0) })
        } else {
            None
        }
    }
}

impl<T> private::Sealed for Opt<T> {}
impl<T: PackedAbi + Copy> Opt<T> {
    pub fn some(v: T) -> Self {
        Self(v.into_packed(), PhantomData)
    }

    pub fn none() -> Self {
        Self(Packed::tag_only(Tag::None), PhantomData)
    }
}

unsafe impl<T: PackedAbi + Copy> PackedAbi for Opt<T> {
    unsafe fn from_packed_unchecked(v: Packed) -> Self {
        Opt(v, PhantomData)
    }

    fn into_packed(self) -> Packed {
        self.0
    }
}
