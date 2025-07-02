#![allow(dead_code)] // may be used in output

// This file contains the "dependencies" for the generated output.
// They are placed here because they also require parts of the generated output to exist,
// which is also why this prelude with placeholders exists.
// Some things are here for no reason other than it seemed more convenient that way at
// the time of writing.

pub struct Ast {}
pub struct IdentId {}
pub struct StrId {}
pub struct FloatId {}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Spanned<T> {
    inner: T,
    pub span: Span,
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeKind {
    Root = 1,
    None = 255,
}

impl NodeKind {
    const fn all() -> &'static [(&'static str, u8)] {
        &[]
    }
}

struct Root(Packed);
struct Stmt(Packed);
struct Expr(Packed);

impl TryFrom<Packed> for Root {
    type Error = ();

    fn try_from(value: Packed) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl TryFrom<Packed> for Stmt {
    type Error = ();

    fn try_from(value: Packed) -> Result<Self, Self::Error> {
        todo!()
    }
}

impl TryFrom<Packed> for Expr {
    type Error = ();

    fn try_from(value: Packed) -> Result<Self, Self::Error> {
        todo!()
    }
}

// code before this marker is not included in the generated output
//file-start

mod private {
    pub trait Sealed {}
}
use private::Sealed;

use std::marker::PhantomData;

trait DebugWithAstExt {
    fn debug(self, ast: &Ast) -> impl std::fmt::Debug + '_;
}

impl DebugWithAstExt for f32 {
    fn debug(self, _: &Ast) -> impl std::fmt::Debug + '_ {
        self
    }
}

impl DebugWithAstExt for bool {
    fn debug(self, _: &Ast) -> impl std::fmt::Debug + '_ {
        self
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Hash)]
#[repr(packed)]
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

impl u24 {
    pub fn debug(self, _: &Ast) -> impl std::fmt::Debug + '_ {
        self
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Hash)]
#[repr(packed)]
pub struct u56([u8; 7]);

impl u56 {
    pub const MAX: u56 = u56([255; 7]);
    pub const MIN: u56 = u56([0; 7]);
    pub const ZERO: u56 = u56([0; 7]);

    #[inline]
    pub const fn new(v: u64) -> Self {
        if v > Self::MAX.get() {
            panic!("value is out of bounds for u56");
        }

        unsafe { Self::new_unchecked(v) }
    }

    #[inline]
    pub const unsafe fn new_unchecked(v: u64) -> Self {
        let [a, b, c, d, e, f, g, _] = v.to_le_bytes();
        Self([a, b, c, d, e, f, g])
    }

    #[inline]
    pub const fn get(self) -> u64 {
        let [a, b, c, d, e, f, g] = self.0;
        u64::from_le_bytes([a, b, c, d, e, f, g, 0])
    }

    fn from_u56(v: u56) -> Self {
        v
    }

    fn into_u56(self) -> u56 {
        self
    }
}

impl PartialEq for u56 {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&self.get(), &other.get())
    }
}

impl Eq for u56 {}

impl PartialOrd for u56 {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        PartialOrd::partial_cmp(&self.get(), &other.get())
    }
}

impl Ord for u56 {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Ord::cmp(&self.get(), &other.get())
    }
}

impl std::fmt::Debug for u56 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <u64 as std::fmt::Debug>::fmt(&self.get(), f)
    }
}

impl u56 {
    pub fn debug(self, _: &Ast) -> impl std::fmt::Debug + '_ {
        self
    }
}

trait IntoU56 {
    fn into_u56(self) -> u56;
}

trait FromU56 {
    fn from_u56(v: u56) -> Self;
}

impl IntoU56 for bool {
    fn into_u56(self) -> u56 {
        unsafe { u56::new_unchecked(self as u64) }
    }
}

impl FromU56 for bool {
    #[allow(unnecessary_transmutes)]
    fn from_u56(v: u56) -> Self {
        unsafe { core::mem::transmute(v.get() as u8) }
    }
}

impl IntoU56 for f32 {
    fn into_u56(self) -> u56 {
        unsafe { u56::new_unchecked(self.to_bits() as u64) }
    }
}

impl FromU56 for f32 {
    fn from_u56(v: u56) -> Self {
        f32::from_bits(v.get() as u32)
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Packed {
    repr: PackedRepr,
    #[cfg(debug_assertions)]
    debug_tag: DebugPackedReprTag,
}

impl Packed {
    #[inline]
    pub fn kind(&self) -> NodeKind {
        // SAFETY: `kind` is present in every union variant
        let kind = unsafe { self.repr.kind_only.kind.get() };
        debug_assert!(NodeKind::all().iter().map(|(_, v)| *v).any(|v| v == kind));

        // SAFETY: All writes of `kind` are in this file, and we know
        // that what's written is always a valid bitpattern of `NodeKind`.
        unsafe { core::mem::transmute(kind) }
    }

    #[inline]
    fn kind_only(kind: NodeKind) -> Self {
        Self {
            repr: PackedRepr {
                kind_only: KindOnly {
                    kind: unsafe { core::num::NonZero::new_unchecked(kind as u8) },
                    _padding: u56::ZERO,
                },
            },
            debug_tag: DebugPackedReprTag::KindOnly,
        }
    }

    #[inline]
    fn fixed_arity(kind: NodeKind, index: u32) -> Self {
        Self {
            repr: PackedRepr {
                fixed_arity: FixedArity {
                    kind: unsafe { core::num::NonZero::new_unchecked(kind as u8) },
                    _padding: u24::ZERO,
                    index,
                },
            },
            debug_tag: DebugPackedReprTag::FixedArity,
        }
    }

    #[inline]
    fn fixed_arity_inline(kind: NodeKind, value: u24, index: u32) -> Self {
        Self {
            repr: PackedRepr {
                fixed_arity_inline: FixedArity_Inline {
                    kind: unsafe { core::num::NonZero::new_unchecked(kind as u8) },
                    value,
                    index,
                },
            },
            debug_tag: DebugPackedReprTag::FixedArity_Inline,
        }
    }

    #[inline]
    fn variable_arity(kind: NodeKind, length: u24, index: u32) -> Self {
        Self {
            repr: PackedRepr {
                variable_arity: VariableArity {
                    kind: unsafe { core::num::NonZero::new_unchecked(kind as u8) },
                    length,
                    index,
                },
            },
            debug_tag: DebugPackedReprTag::VariableArity,
        }
    }

    #[inline]
    fn mixed_arity(kind: NodeKind, length: u24, index: u32) -> Self {
        Self {
            repr: PackedRepr {
                mixed_arity: MixedArity {
                    kind: unsafe { core::num::NonZero::new_unchecked(kind as u8) },
                    tail_length: length,
                    index,
                },
            },
            debug_tag: DebugPackedReprTag::MixedArity,
        }
    }

    #[inline]
    fn inline(kind: NodeKind, value: u56) -> Self {
        Self {
            repr: PackedRepr {
                inline: Inline {
                    kind: unsafe { core::num::NonZero::new_unchecked(kind as u8) },
                    value,
                },
            },
            debug_tag: DebugPackedReprTag::Inline,
        }
    }

    #[inline]
    unsafe fn as_kind_only(&self) -> &KindOnly {
        debug_assert!(self.debug_tag == DebugPackedReprTag::KindOnly);
        unsafe { &self.repr.kind_only }
    }

    #[inline]
    unsafe fn as_fixed_arity(&self) -> &FixedArity {
        debug_assert!(self.debug_tag == DebugPackedReprTag::FixedArity);
        unsafe { &self.repr.fixed_arity }
    }

    #[inline]
    unsafe fn as_fixed_arity_inline(&self) -> &FixedArity_Inline {
        debug_assert!(self.debug_tag == DebugPackedReprTag::FixedArity_Inline);
        unsafe { &self.repr.fixed_arity_inline }
    }

    #[inline]
    unsafe fn as_variable_arity(&self) -> &VariableArity {
        debug_assert!(self.debug_tag == DebugPackedReprTag::VariableArity);
        unsafe { &self.repr.variable_arity }
    }

    #[inline]
    unsafe fn as_mixed_arity(&self) -> &MixedArity {
        debug_assert!(self.debug_tag == DebugPackedReprTag::MixedArity);
        unsafe { &self.repr.mixed_arity }
    }

    #[inline]
    unsafe fn as_inline(&self) -> &Inline {
        debug_assert!(self.debug_tag == DebugPackedReprTag::Inline);
        unsafe { &self.repr.inline }
    }
}

const _: () = {
    let _ = core::mem::transmute::<Option<Packed>, Packed>;
};

#[allow(non_camel_case_types)]
#[cfg(debug_assertions)]
#[derive(Debug, Clone, Copy, PartialEq)]
enum DebugPackedReprTag {
    KindOnly,
    FixedArity,
    FixedArity_Inline,
    VariableArity,
    MixedArity,
    Inline,
}

#[derive(Clone, Copy)]
union PackedRepr {
    kind_only: KindOnly,
    fixed_arity: FixedArity,
    fixed_arity_inline: FixedArity_Inline,
    variable_arity: VariableArity,
    mixed_arity: MixedArity,
    inline: Inline,
}

type RawKind = core::num::NonZero<u8>;

#[derive(Clone, Copy)]
#[repr(C)]
struct KindOnly {
    kind: RawKind,
    _padding: u56,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct FixedArity {
    kind: RawKind,
    _padding: u24,
    index: u32,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
#[repr(C)]
struct FixedArity_Inline {
    kind: RawKind,
    value: u24,
    index: u32,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct VariableArity {
    kind: RawKind,
    length: u24,
    index: u32,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct MixedArity {
    kind: RawKind,
    tail_length: u24,
    index: u32,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct Inline {
    kind: RawKind,
    value: u56,
}

/// Marker for types which are transparent wrappers over [`Packed`].
pub unsafe trait PackedAbi: Sealed + Sized + Copy {
    /// Check if `kind` matches `Self`'s kind.
    fn check_kind(kind: NodeKind) -> bool;
}

/// Conversion traits to/from [`Packed`].
pub trait PackedNode: PackedAbi {
    /// SAFETY:
    /// - `Self` must be a transparent wrapper over `Packed`.
    /// - `v` must have the same kind as `Self`.
    unsafe fn from_packed(v: Packed) -> Self;

    fn into_packed(v: Self) -> Packed;

    /// SAFETY:
    /// - All nodes in `v` must have the same kind as `Self`.
    /// - `Self` must be a transparent wrapper over `Packed`.
    unsafe fn from_packed_slice(v: &[Packed]) -> &[Self];

    fn into_packed_slice(v: &[Self]) -> &[Packed];

    /// SAFETY:
    /// - `Self` must be a transparent wrapper over `Packed`.
    /// - `Spanned` is a repr(C) struct, meaning it has the
    ///   same layout for the same `T`.
    unsafe fn from_spanned_packed(v: Spanned<Packed>) -> Spanned<Self>;

    fn into_spanned_packed(v: Spanned<Self>) -> Spanned<Packed>;

    /// SAFETY:
    /// - `Self` must be a transparent wrapper over `Packed`.
    /// - All nodes in `v` must have the same kind as `Self`.
    /// - `Spanned` is a repr(C) struct, meaning it has the
    ///   same layout for the same `T`.
    unsafe fn from_spanned_packed_slice(v: &[Spanned<Packed>]) -> &[Spanned<Self>];

    fn into_spanned_packed_slice(v: &[Spanned<Self>]) -> &[Spanned<Packed>];
}

impl<T: PackedAbi> PackedNode for T {
    unsafe fn from_packed(v: Packed) -> Self {
        debug_assert!(T::check_kind(v.kind()));
        unsafe { core::mem::transmute_copy(&v) }
    }

    fn into_packed(v: Self) -> Packed {
        unsafe { core::mem::transmute_copy(&v) }
    }

    unsafe fn from_packed_slice(v: &[Packed]) -> &[Self] {
        debug_assert!(v.iter().all(|v| T::check_kind(v.kind())));
        unsafe { core::mem::transmute(v) }
    }

    fn into_packed_slice(v: &[Self]) -> &[Packed] {
        unsafe { core::mem::transmute(v) }
    }

    unsafe fn from_spanned_packed(v: Spanned<Packed>) -> Spanned<Self> {
        debug_assert!(T::check_kind(v.kind()));
        unsafe { core::mem::transmute_copy(&v) }
    }

    fn into_spanned_packed(v: Spanned<Self>) -> Spanned<Packed> {
        unsafe { core::mem::transmute_copy(&v) }
    }

    unsafe fn from_spanned_packed_slice(v: &[Spanned<Packed>]) -> &[Spanned<Self>] {
        debug_assert!(v.iter().all(|v| T::check_kind(v.kind())));
        unsafe { core::mem::transmute(v) }
    }

    fn into_spanned_packed_slice(v: &[Spanned<Self>]) -> &[Spanned<Packed>] {
        unsafe { core::mem::transmute(v) }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NodeCastError;
impl std::fmt::Display for NodeCastError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid cast between node types")
    }
}
impl std::error::Error for NodeCastError {}

pub trait Pack {
    type Node;

    fn pack(self, ast: &mut Ast) -> Self::Node;
}

pub trait Unpack {
    type Node<'a>;

    /// # Safety
    /// - The node and all its subnodes must exist within the given `ast`.
    ///
    /// The only way to ensure this is to use [`Pack`] with [`AstBuilder`],
    /// and then [`AstBuilder::build`].
    unsafe fn unpack<'a>(self, ast: &'a Ast) -> Self::Node<'a>;
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Opt<T>(Packed, PhantomData<T>);

impl<T: Sealed> Sealed for Opt<T> {}
/// SAFETY: `Opt` is a transparent wrapper over `Packed`.
unsafe impl<T: PackedAbi> PackedAbi for Opt<T> {
    fn check_kind(kind: NodeKind) -> bool {
        kind == NodeKind::None || T::check_kind(kind)
    }
}

impl<T: PackedAbi> Opt<T> {
    #[inline]
    pub fn some(v: T) -> Self {
        Self(T::into_packed(v), PhantomData)
    }

    #[inline]
    pub fn none() -> Self {
        Self(Packed::kind_only(NodeKind::None), PhantomData)
    }

    #[inline]
    pub fn is_some(&self) -> bool {
        !self.is_none()
    }

    #[inline]
    pub fn is_none(&self) -> bool {
        matches!(self.0.kind(), NodeKind::None)
    }

    #[inline]
    pub fn unwrap(&self) -> T {
        if self.is_none() {
            panic!("unwrapped an Opt::None value");
        }
        unsafe { T::from_packed(self.0) }
    }

    #[inline]
    pub unsafe fn unwrap_unchecked(&self) -> T {
        debug_assert!(self.is_some());
        unsafe { T::from_packed(self.0) }
    }

    #[inline]
    pub fn into_option(self) -> Option<T> {
        self.into()
    }
}

impl<T: PackedAbi> From<Opt<T>> for Option<T> {
    fn from(value: Opt<T>) -> Self {
        if value.is_some() {
            Some(unsafe { value.unwrap_unchecked() })
        } else {
            None
        }
    }
}

impl<T: PackedAbi> From<Option<T>> for Opt<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => Self::some(value),
            None => Self::none(),
        }
    }
}

struct DebugIter<T>(T);

impl<T: Clone + Iterator<Item = I>, I: std::fmt::Debug> std::fmt::Debug for DebugIter<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.0.clone()).finish()
    }
}
