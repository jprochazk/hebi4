use super::{AstBuilder, Expr, Node, Packed, PackedAbiCompatible, Stmt, Tag, Tagged, private};
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

pub mod stmt {
    use super::*;

    declare_node! {
        #[kind(Stmt), tag(StmtVar)]
        pub struct Var {
            pub name: Ident,
            pub value: Expr,
        }
    }

    declare_node! {
        #[kind(Stmt), tag(StmtFn)]
        pub struct Fn<'a> {
            pub name: Ident,
            pub body: Expr,
            #[tail]
            pub params: &'a [Ident],
        }
    }
}

pub mod expr {
    use super::*;

    #[derive(Clone, Copy)]
    #[repr(transparent)]
    pub struct Str(Packed);

    unsafe impl PackedAbiCompatible for Str {}

    impl Str {
        #[inline]
        pub fn as_str<'ast>(&self, ast: &'ast AstBuilder) -> Option<&'ast str> {
            ast.strings.get(self.id())
        }

        #[inline]
        pub unsafe fn as_str_unchecked<'ast>(&self, ast: &'ast AstBuilder) -> &'ast str {
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

        #[inline]
        unsafe fn _from_packed_unchecked(v: Packed) -> Self {
            debug_assert!(v.tag() == Tag::ExprStr);
            debug_assert!(TryInto::<u32>::try_into(v.value()).is_ok());
            Str(v)
        }

        #[inline]
        fn _into_packed(self) -> Packed {
            self.0
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

        #[inline]
        unsafe fn unpack_unchecked<'ast>(
            node: Self::NodeKind,
            _: &'ast crate::ast::Ast,
        ) -> Self::Output<'ast> {
            debug_assert!(node.base_tag() == Tag::ExprStr);
            Self(node._into_packed())
        }

        #[inline]
        fn pack_into(&self, _: &mut AstBuilder) -> Self::NodeKind {
            unsafe { Expr::_from_packed_unchecked(self.0) }
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

unsafe impl PackedAbiCompatible for Ident {}

impl Ident {
    #[inline]
    pub fn as_str<'ast>(&self, ast: &'ast AstBuilder) -> Option<&'ast str> {
        ast.idents.get(self.id())
    }

    #[inline]
    pub unsafe fn as_str_unchecked<'ast>(&self, ast: &'ast AstBuilder) -> &'ast str {
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

    #[inline]
    unsafe fn _from_packed_unchecked(v: Packed) -> Self {
        debug_assert!(v.tag() == Tag::Ident);
        debug_assert!(TryInto::<u32>::try_into(v.value()).is_ok());
        Ident(v)
    }

    #[inline]
    fn _into_packed(self) -> Packed {
        self.0
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
