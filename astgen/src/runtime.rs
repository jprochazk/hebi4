struct Ast {}
struct AstBuilder {}
struct IdentId {}
struct StrId {}

// code before this marker is not included in the generated output
//file-start

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
