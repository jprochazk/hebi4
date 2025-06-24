use super::{AssignOp, Ast, AstBuilder, FloatId, IdentId, InfixOp, PrefixOp, StrId};

mod private {
    pub trait Sealed {}
}
use private::Sealed;

use std::marker::PhantomData;

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

    fn from_u24(v: u24) -> Self {
        v
    }

    fn into_u24(self) -> u24 {
        self
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

trait IntoU56 {
    fn into_u56(self) -> u56;
}

impl IntoU56 for bool {
    fn into_u56(self) -> u56 {
        unsafe { u56::new_unchecked(self as u64) }
    }
}

impl IntoU56 for f32 {
    fn into_u56(self) -> u56 {
        unsafe { u56::new_unchecked(self.to_bits() as u64) }
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
    fn fixed_arity_inline(kind: NodeKind, inline: u24, index: u32) -> Self {
        Self {
            repr: PackedRepr {
                fixed_arity_inline: FixedArity_Inline {
                    kind: unsafe { core::num::NonZero::new_unchecked(kind as u8) },
                    inline,
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
            debug_tag: DebugPackedReprTag::FixedArity_Inline,
        }
    }

    #[inline]
    fn mixed_arity(kind: NodeKind, length: u24, index: u32) -> Self {
        Self {
            repr: PackedRepr {
                mixed_arity: MixedArity {
                    kind: unsafe { core::num::NonZero::new_unchecked(kind as u8) },
                    length,
                    index,
                },
            },
            debug_tag: DebugPackedReprTag::FixedArity_Inline,
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
            debug_tag: DebugPackedReprTag::FixedArity_Inline,
        }
    }
}

const _: () = {
    let _ = core::mem::transmute::<Option<Packed>, Packed>;
};

#[allow(non_camel_case_types)]
#[cfg(debug_assertions)]
#[derive(Debug, Clone, Copy)]
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
    inline: u24,
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
    length: u24,
    index: u32,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct Inline {
    kind: RawKind,
    value: u56,
}

/// Marker for types which are transparent wrappers over [`Packed`].
pub trait PackedAbi: Sealed + Sized + Copy {}

/// Conversion traits to/from [`Packed`].
pub trait PackedNode: PackedAbi {
    fn from_packed(v: Packed) -> Self;
    fn into_packed(v: Self) -> Packed;
    fn from_packed_slice(v: &[Packed]) -> &[Self];
    fn into_packed_slice(v: &[Self]) -> &[Packed];
}

impl<T: PackedAbi> PackedNode for T {
    fn from_packed(v: Packed) -> Self {
        // SAFETY: `Self` is a transparent wrapper over `Packed`.
        unsafe { core::mem::transmute_copy(&v) }
    }

    fn into_packed(v: Self) -> Packed {
        // SAFETY: `Self` is a transparent wrapper over `Packed`.
        unsafe { core::mem::transmute_copy(&v) }
    }

    fn from_packed_slice(v: &[Packed]) -> &[Self] {
        // SAFETY: `Self` is a transparent wrapper over `Packed`.
        unsafe { core::mem::transmute(v) }
    }

    fn into_packed_slice(v: &[Self]) -> &[Packed] {
        // SAFETY: `Self` is a transparent wrapper over `Packed`.
        unsafe { core::mem::transmute(v) }
    }
}

pub trait Pack {
    type Parts<'a>;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self;
}

pub trait Unpack {
    type Parts<'a>;

    fn unpack<'a>(self, ast: &'a Ast) -> Self::Parts<'a>;
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Opt<T>(Packed, PhantomData<T>);

impl<T: Sealed> Sealed for Opt<T> {}
impl<T: PackedAbi> PackedAbi for Opt<T> {}

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
        T::from_packed(self.0)
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum NodeKind {
    Root = 1,
    Var = 2,
    Loop = 3,
    StmtExpr = 4,
    Return = 5,
    Break = 6,
    Continue = 7,
    IfSimple = 8,
    IfMulti = 9,
    Block = 10,
    Fn = 11,
    GetVar = 12,
    GetField = 13,
    GetIndex = 14,
    Call = 15,
    SetVar = 16,
    SetField = 17,
    SetIndex = 18,
    Infix = 19,
    Prefix = 20,
    Array = 21,
    Object = 22,
    Int = 23,
    Float32 = 24,
    Float64 = 25,
    Bool = 26,
    Str = 27,
    Nil = 28,
    Branch = 29,
    ObjectEntry = 30,
    Ident = 31,
    None = 255,
}

impl NodeKind {
    const fn all() -> &'static [(&'static str, u8)] {
        &[
            ("Var", 2),
            ("Loop", 3),
            ("StmtExpr", 4),
            ("Return", 5),
            ("Break", 6),
            ("Continue", 7),
            ("IfSimple", 8),
            ("IfMulti", 9),
            ("Block", 10),
            ("Fn", 11),
            ("GetVar", 12),
            ("GetField", 13),
            ("GetIndex", 14),
            ("Call", 15),
            ("SetVar", 16),
            ("SetField", 17),
            ("SetIndex", 18),
            ("Infix", 19),
            ("Prefix", 20),
            ("Array", 21),
            ("Object", 22),
            ("Int", 23),
            ("Float32", 24),
            ("Float64", 25),
            ("Bool", 26),
            ("Str", 27),
            ("Nil", 28),
            ("Branch", 29),
            ("ObjectEntry", 30),
            ("Ident", 31),
        ]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum StmtKind {
    Var = NodeKind::Var as u8,
    Loop = NodeKind::Loop as u8,
    StmtExpr = NodeKind::StmtExpr as u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ExprKind {
    Return = NodeKind::Return as u8,
    Break = NodeKind::Break as u8,
    Continue = NodeKind::Continue as u8,
    IfSimple = NodeKind::IfSimple as u8,
    IfMulti = NodeKind::IfMulti as u8,
    Block = NodeKind::Block as u8,
    Fn = NodeKind::Fn as u8,
    GetVar = NodeKind::GetVar as u8,
    GetField = NodeKind::GetField as u8,
    GetIndex = NodeKind::GetIndex as u8,
    Call = NodeKind::Call as u8,
    SetVar = NodeKind::SetVar as u8,
    SetField = NodeKind::SetField as u8,
    SetIndex = NodeKind::SetIndex as u8,
    Infix = NodeKind::Infix as u8,
    Prefix = NodeKind::Prefix as u8,
    Array = NodeKind::Array as u8,
    Object = NodeKind::Object as u8,
    Int = NodeKind::Int as u8,
    Float32 = NodeKind::Float32 as u8,
    Float64 = NodeKind::Float64 as u8,
    Bool = NodeKind::Bool as u8,
    Str = NodeKind::Str as u8,
    Nil = NodeKind::Nil as u8,
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Stmt(Packed);
impl Sealed for Stmt {}
impl PackedAbi for Stmt {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Expr(Packed);
impl Sealed for Expr {}
impl PackedAbi for Expr {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Var(Packed);
impl Sealed for Var {}
impl PackedAbi for Var {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Loop(Packed);
impl Sealed for Loop {}
impl PackedAbi for Loop {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct StmtExpr(Packed);
impl Sealed for StmtExpr {}
impl PackedAbi for StmtExpr {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Return(Packed);
impl Sealed for Return {}
impl PackedAbi for Return {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Break(Packed);
impl Sealed for Break {}
impl PackedAbi for Break {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Continue(Packed);
impl Sealed for Continue {}
impl PackedAbi for Continue {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct IfSimple(Packed);
impl Sealed for IfSimple {}
impl PackedAbi for IfSimple {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct IfMulti(Packed);
impl Sealed for IfMulti {}
impl PackedAbi for IfMulti {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Block(Packed);
impl Sealed for Block {}
impl PackedAbi for Block {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Fn(Packed);
impl Sealed for Fn {}
impl PackedAbi for Fn {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct GetVar(Packed);
impl Sealed for GetVar {}
impl PackedAbi for GetVar {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct GetField(Packed);
impl Sealed for GetField {}
impl PackedAbi for GetField {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct GetIndex(Packed);
impl Sealed for GetIndex {}
impl PackedAbi for GetIndex {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Call(Packed);
impl Sealed for Call {}
impl PackedAbi for Call {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct SetVar(Packed);
impl Sealed for SetVar {}
impl PackedAbi for SetVar {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct SetField(Packed);
impl Sealed for SetField {}
impl PackedAbi for SetField {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct SetIndex(Packed);
impl Sealed for SetIndex {}
impl PackedAbi for SetIndex {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Infix(Packed);
impl Sealed for Infix {}
impl PackedAbi for Infix {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Prefix(Packed);
impl Sealed for Prefix {}
impl PackedAbi for Prefix {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Array(Packed);
impl Sealed for Array {}
impl PackedAbi for Array {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Object(Packed);
impl Sealed for Object {}
impl PackedAbi for Object {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Int(Packed);
impl Sealed for Int {}
impl PackedAbi for Int {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Float32(Packed);
impl Sealed for Float32 {}
impl PackedAbi for Float32 {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Float64(Packed);
impl Sealed for Float64 {}
impl PackedAbi for Float64 {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Bool(Packed);
impl Sealed for Bool {}
impl PackedAbi for Bool {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Str(Packed);
impl Sealed for Str {}
impl PackedAbi for Str {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Nil(Packed);
impl Sealed for Nil {}
impl PackedAbi for Nil {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Branch(Packed);
impl Sealed for Branch {}
impl PackedAbi for Branch {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ObjectEntry(Packed);
impl Sealed for ObjectEntry {}
impl PackedAbi for ObjectEntry {}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ident(Packed);
impl Sealed for Ident {}
impl PackedAbi for Ident {}

pub struct VarParts {
    pub name: Ident,
    pub value: Expr,
}

pub struct LoopParts<'a> {
    pub body: &'a [Stmt],
}

pub struct StmtExprParts {
    pub inner: Expr,
}

pub struct ReturnParts {
    pub value: Opt<Expr>,
}

pub struct BreakParts {}

pub struct ContinueParts {}

pub struct IfSimpleParts {
    pub cond: Expr,
    pub then: Expr,
    pub else_: Opt<Expr>,
}

pub struct IfMultiParts<'a> {
    pub tail: Block,
    pub branches: &'a [Branch],
}

pub struct BlockParts<'a> {
    pub body: &'a [Stmt],
}

pub struct FnParts<'a> {
    pub name: Ident,
    pub body: Block,
    pub params: &'a [Ident],
}

pub struct GetVarParts {
    pub name: Ident,
}

pub struct GetFieldParts {
    pub parent: Expr,
    pub key: Ident,
}

pub struct GetIndexParts {
    pub parent: Expr,
    pub key: Expr,
}

pub struct CallParts<'a> {
    pub callee: Expr,
    pub args: &'a [Expr],
}

pub struct SetVarParts {
    pub name: Ident,
    pub value: Expr,
    pub op: AssignOp,
}

pub struct SetFieldParts {
    pub parent: Expr,
    pub key: Ident,
    pub value: Expr,
    pub op: AssignOp,
}

pub struct SetIndexParts {
    pub parent: Expr,
    pub key: Expr,
    pub value: Expr,
    pub op: AssignOp,
}

pub struct InfixParts {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op: InfixOp,
}

pub struct PrefixParts {
    pub rhs: Expr,
    pub op: PrefixOp,
}

pub struct ArrayParts<'a> {
    pub items: &'a [Expr],
}

pub struct ObjectParts<'a> {
    pub entries: &'a [ObjectEntry],
}

pub struct IntParts {
    pub value: u56,
}

pub struct Float32Parts {
    pub value: f32,
}

pub struct Float64Parts {
    pub value: FloatId,
}

pub struct BoolParts {
    pub value: bool,
}

pub struct StrParts {
    pub value: StrId,
}

pub struct NilParts {}

pub struct BranchParts {
    pub cond: Expr,
    pub then: Expr,
}

pub struct ObjectEntryParts {
    pub key: Expr,
    pub value: Expr,
}

pub struct IdentParts {
    pub id: IdentId,
}

impl Pack for Var {
    type Parts<'a> = VarParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let VarParts { name, value } = parts;
        let name = <_>::into_packed(name);
        let value = <_>::into_packed(value);
        let index = ast.append(&[name, value]);
        Self::from_packed(Packed::fixed_arity(NodeKind::Var, index))
    }
}

impl Pack for Loop {
    type Parts<'a> = LoopParts<'a>;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let LoopParts { body } = parts;
        let length = body.len();
        if length > u24::MAX.get() as usize {
            panic!("length is out of bounds for u24");
        }
        let length = u24::new(length as u32);
        let body = <_>::into_packed_slice(body);
        let index = ast.append(body);
        Self::from_packed(Packed::variable_arity(NodeKind::Loop, length, index))
    }
}

impl Pack for StmtExpr {
    type Parts<'a> = StmtExprParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let StmtExprParts { inner } = parts;
        let inner = <_>::into_packed(inner);
        let index = ast.append(&[inner]);
        Self::from_packed(Packed::fixed_arity(NodeKind::StmtExpr, index))
    }
}

impl Pack for Return {
    type Parts<'a> = ReturnParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let ReturnParts { value } = parts;
        let value = <_>::into_packed(value);
        let index = ast.append(&[value]);
        Self::from_packed(Packed::fixed_arity(NodeKind::Return, index))
    }
}

impl Pack for Break {
    type Parts<'a> = BreakParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let _ = (parts, ast);
        Self::from_packed(Packed::kind_only(NodeKind::Break))
    }
}

impl Pack for Continue {
    type Parts<'a> = ContinueParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let _ = (parts, ast);
        Self::from_packed(Packed::kind_only(NodeKind::Continue))
    }
}

impl Pack for IfSimple {
    type Parts<'a> = IfSimpleParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let IfSimpleParts { cond, then, else_ } = parts;
        let cond = <_>::into_packed(cond);
        let then = <_>::into_packed(then);
        let else_ = <_>::into_packed(else_);
        let index = ast.append(&[cond, then, else_]);
        Self::from_packed(Packed::fixed_arity(NodeKind::IfSimple, index))
    }
}

impl Pack for IfMulti {
    type Parts<'a> = IfMultiParts<'a>;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let IfMultiParts { tail, branches } = parts;
        let tail = <_>::into_packed(tail);
        let index = ast.append(&[tail]);

        let length = branches.len();
        if length > u24::MAX.get() as usize {
            panic!("length is out of bounds for u24");
        }
        let length = u24::new(length as u32);
        let branches = <_>::into_packed_slice(branches);
        let _ = ast.append(branches);

        Self::from_packed(Packed::mixed_arity(NodeKind::IfMulti, length, index))
    }
}

impl Pack for Block {
    type Parts<'a> = BlockParts<'a>;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let BlockParts { body } = parts;
        let length = body.len();
        if length > u24::MAX.get() as usize {
            panic!("length is out of bounds for u24");
        }
        let length = u24::new(length as u32);
        let body = <_>::into_packed_slice(body);
        let index = ast.append(body);
        Self::from_packed(Packed::variable_arity(NodeKind::Block, length, index))
    }
}

impl Pack for Fn {
    type Parts<'a> = FnParts<'a>;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let FnParts { name, body, params } = parts;
        let name = <_>::into_packed(name);
        let body = <_>::into_packed(body);
        let index = ast.append(&[name, body]);

        let length = params.len();
        if length > u24::MAX.get() as usize {
            panic!("length is out of bounds for u24");
        }
        let length = u24::new(length as u32);
        let params = <_>::into_packed_slice(params);
        let _ = ast.append(params);

        Self::from_packed(Packed::mixed_arity(NodeKind::Fn, length, index))
    }
}

impl Pack for GetVar {
    type Parts<'a> = GetVarParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let GetVarParts { name } = parts;
        let name = <_>::into_packed(name);
        let index = ast.append(&[name]);
        Self::from_packed(Packed::fixed_arity(NodeKind::GetVar, index))
    }
}

impl Pack for GetField {
    type Parts<'a> = GetFieldParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let GetFieldParts { parent, key } = parts;
        let parent = <_>::into_packed(parent);
        let key = <_>::into_packed(key);
        let index = ast.append(&[parent, key]);
        Self::from_packed(Packed::fixed_arity(NodeKind::GetField, index))
    }
}

impl Pack for GetIndex {
    type Parts<'a> = GetIndexParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let GetIndexParts { parent, key } = parts;
        let parent = <_>::into_packed(parent);
        let key = <_>::into_packed(key);
        let index = ast.append(&[parent, key]);
        Self::from_packed(Packed::fixed_arity(NodeKind::GetIndex, index))
    }
}

impl Pack for Call {
    type Parts<'a> = CallParts<'a>;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let CallParts { callee, args } = parts;
        let callee = <_>::into_packed(callee);
        let index = ast.append(&[callee]);

        let length = args.len();
        if length > u24::MAX.get() as usize {
            panic!("length is out of bounds for u24");
        }
        let length = u24::new(length as u32);
        let args = <_>::into_packed_slice(args);
        let _ = ast.append(args);

        Self::from_packed(Packed::mixed_arity(NodeKind::Call, length, index))
    }
}

impl Pack for SetVar {
    type Parts<'a> = SetVarParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let SetVarParts { op, name, value } = parts;
        let name = <_>::into_packed(name);
        let value = <_>::into_packed(value);
        let index = ast.append(&[name, value]);
        let op: u24 = op.into_u24();
        Self::from_packed(Packed::fixed_arity_inline(NodeKind::SetVar, op, index))
    }
}

impl Pack for SetField {
    type Parts<'a> = SetFieldParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let SetFieldParts {
            op,
            parent,
            key,
            value,
        } = parts;
        let parent = <_>::into_packed(parent);
        let key = <_>::into_packed(key);
        let value = <_>::into_packed(value);
        let index = ast.append(&[parent, key, value]);
        let op: u24 = op.into_u24();
        Self::from_packed(Packed::fixed_arity_inline(NodeKind::SetField, op, index))
    }
}

impl Pack for SetIndex {
    type Parts<'a> = SetIndexParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let SetIndexParts {
            op,
            parent,
            key,
            value,
        } = parts;
        let parent = <_>::into_packed(parent);
        let key = <_>::into_packed(key);
        let value = <_>::into_packed(value);
        let index = ast.append(&[parent, key, value]);
        let op: u24 = op.into_u24();
        Self::from_packed(Packed::fixed_arity_inline(NodeKind::SetIndex, op, index))
    }
}

impl Pack for Infix {
    type Parts<'a> = InfixParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let InfixParts { op, lhs, rhs } = parts;
        let lhs = <_>::into_packed(lhs);
        let rhs = <_>::into_packed(rhs);
        let index = ast.append(&[lhs, rhs]);
        let op: u24 = op.into_u24();
        Self::from_packed(Packed::fixed_arity_inline(NodeKind::Infix, op, index))
    }
}

impl Pack for Prefix {
    type Parts<'a> = PrefixParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let PrefixParts { op, rhs } = parts;
        let rhs = <_>::into_packed(rhs);
        let index = ast.append(&[rhs]);
        let op: u24 = op.into_u24();
        Self::from_packed(Packed::fixed_arity_inline(NodeKind::Prefix, op, index))
    }
}

impl Pack for Array {
    type Parts<'a> = ArrayParts<'a>;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let ArrayParts { items } = parts;
        let length = items.len();
        if length > u24::MAX.get() as usize {
            panic!("length is out of bounds for u24");
        }
        let length = u24::new(length as u32);
        let items = <_>::into_packed_slice(items);
        let index = ast.append(items);
        Self::from_packed(Packed::variable_arity(NodeKind::Array, length, index))
    }
}

impl Pack for Object {
    type Parts<'a> = ObjectParts<'a>;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let ObjectParts { entries } = parts;
        let length = entries.len();
        if length > u24::MAX.get() as usize {
            panic!("length is out of bounds for u24");
        }
        let length = u24::new(length as u32);
        let entries = <_>::into_packed_slice(entries);
        let index = ast.append(entries);
        Self::from_packed(Packed::variable_arity(NodeKind::Object, length, index))
    }
}

impl Pack for Int {
    type Parts<'a> = IntParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let IntParts { value } = parts;
        let value: u56 = value.into_u56();
        Self::from_packed(Packed::inline(NodeKind::Int, value))
    }
}

impl Pack for Float32 {
    type Parts<'a> = Float32Parts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let Float32Parts { value } = parts;
        let value: u56 = value.into_u56();
        Self::from_packed(Packed::inline(NodeKind::Float32, value))
    }
}

impl Pack for Float64 {
    type Parts<'a> = Float64Parts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let Float64Parts { value } = parts;
        let value: u56 = value.into_u56();
        Self::from_packed(Packed::inline(NodeKind::Float64, value))
    }
}

impl Pack for Bool {
    type Parts<'a> = BoolParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let BoolParts { value } = parts;
        let value: u56 = value.into_u56();
        Self::from_packed(Packed::inline(NodeKind::Bool, value))
    }
}

impl Pack for Str {
    type Parts<'a> = StrParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let StrParts { value } = parts;
        let value: u56 = value.into_u56();
        Self::from_packed(Packed::inline(NodeKind::Str, value))
    }
}

impl Pack for Nil {
    type Parts<'a> = NilParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let _ = (parts, ast);
        Self::from_packed(Packed::kind_only(NodeKind::Nil))
    }
}

impl Pack for Branch {
    type Parts<'a> = BranchParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let BranchParts { cond, then } = parts;
        let cond = <_>::into_packed(cond);
        let then = <_>::into_packed(then);
        let index = ast.append(&[cond, then]);
        Self::from_packed(Packed::fixed_arity(NodeKind::Branch, index))
    }
}

impl Pack for ObjectEntry {
    type Parts<'a> = ObjectEntryParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let ObjectEntryParts { key, value } = parts;
        let key = <_>::into_packed(key);
        let value = <_>::into_packed(value);
        let index = ast.append(&[key, value]);
        Self::from_packed(Packed::fixed_arity(NodeKind::ObjectEntry, index))
    }
}

impl Pack for Ident {
    type Parts<'a> = IdentParts;

    fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {
        let IdentParts { id } = parts;
        let id: u56 = id.into_u56();
        Self::from_packed(Packed::inline(NodeKind::Ident, id))
    }
}
