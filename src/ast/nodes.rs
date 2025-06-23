use super::{Ast, AstBuilder, IdentId, StrId};


mod private {
    pub trait Sealed {}
}
use private::Sealed;

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Hash)]
#[repr(packed)]
pub struct u24([u8; 3]);

impl u24 {
    pub const MAX: u24 = u24([255; 3]);
    pub const MIN: u24 = u24([0; 3]);

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

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Hash)]
#[repr(packed)]
pub struct u56([u8; 7]);

impl u56 {
    pub const MAX: u56 = u56([255; 7]);
    pub const MIN: u56 = u56([0; 7]);

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

#[derive(Clone, Copy)]
pub struct Packed {
    repr: PackedRepr,
    #[cfg(debug_assertions)]
    debug_tag: DebugPackedReprTag,
}

const _: () = {
    let _ = core::mem::transmute::<Option<Packed>, Packed>;
};

#[allow(non_camel_case_types)]
#[cfg(debug_assertions)]
#[derive(Debug, Clone, Copy)]
enum DebugPackedReprTag {
    FixedArity,
    FixedArity_Inline,
    VariableArity,
    MixedArity,
    Inline,
}

#[derive(Clone, Copy)]
union PackedRepr {
    fixed_arity: FixedArity,
    variable_arity: VariableArity,
    mixed_arity: MixedArity,
}

type RawTag = core::num::NonZero<u8>;

#[derive(Clone, Copy)]
#[repr(align(8))]
struct FixedArity {
    tag: RawTag,
    value: u24,
    index: u32,
}

#[derive(Clone, Copy)]
#[repr(align(8))]
struct VariableArity {
    tag: RawTag,
    length: u24,
    index: u32,
}

#[derive(Clone, Copy)]
#[repr(align(8))]
struct MixedArity {
    tag: RawTag,
    length: u24,
    index: u32,
}

#[derive(Clone, Copy)]
#[repr(align(8))]
struct InlineValue {
    tag: RawTag,
    value: u56,
}

pub trait PackedAbi: Sealed + Sized + Copy {}

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

