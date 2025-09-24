//! "Thin" buffers which store length on the heap.
//!
//! - [`ThinStr`]
//!
//! Implementation copied from: https://github.com/thomcc/thin_str/blob/d0367e0e876104dfa9860bec45f10b3e6e107ecd/src/lib.rs
//! Original code is dual licensed under MIT/Apache 2.0, (C) github.com/thomcc

use std::{
    alloc::{self, Layout},
    ptr::{self, NonNull},
};

/// String which takes up only `usize` bytes on the stack by
/// storing length as a prefix of the data on the heap.
///
/// Empty strings do not allocate.
#[repr(transparent)]
pub struct ThinStr(NonNull<Repr>);

#[repr(C)]
struct Repr {
    len: usize,
    data: [u8; 0],
}

impl Drop for ThinStr {
    #[inline]
    fn drop(&mut self) {
        let len = self.len();
        if len > 0 {
            let layout = ThinStr::layout_for(len);
            unsafe {
                let ptr: *mut Repr = self.0.as_ptr();
                std::alloc::dealloc(ptr.cast::<u8>(), layout);
            }
        }
    }
}

const EMPTY: ThinStr = const {
    const REPR: Repr = Repr { len: 0, data: [] };
    unsafe { ThinStr(NonNull::new_unchecked((&REPR) as *const Repr as *mut Repr)) }
};

unsafe impl Send for ThinStr {}
unsafe impl Sync for ThinStr {}

impl Clone for ThinStr {
    #[inline]
    fn clone(&self) -> Self {
        Self::new(self.as_str())
    }
}

impl ThinStr {
    /// Create a new empty string.
    #[inline]
    pub const fn empty() -> Self {
        EMPTY
    }

    /// SAFETY: `initializer` must initialize the first `len` bytes of the
    /// pointer with UTF-8.
    #[inline]
    unsafe fn new_init_with(len: usize, initializer: impl FnOnce(*mut u8)) -> Self {
        if len == 0 {
            return EMPTY;
        }
        let layout = Self::layout_for(len);

        let ptr: NonNull<Repr> = NonNull::new(alloc::alloc(layout).cast())
            .unwrap_or_else(|| alloc::handle_alloc_error(layout));

        let szp = ptr.as_ptr().cast::<usize>();
        ptr::write(szp, len);

        let bufstart = szp.add(1).cast::<u8>();
        initializer(bufstart);

        debug_assert_eq!((*ptr.as_ptr()).data.as_ptr() as usize, bufstart as usize);

        ThinStr(ptr)
    }

    /// Create a new string with the same contents as `s`.
    #[inline]
    pub fn new(s: &str) -> Self {
        unsafe {
            let len = s.len();
            Self::new_init_with(len, |buf| {
                ptr::copy_nonoverlapping(s.as_bytes().as_ptr(), buf, len)
            })
        }
    }

    /// Create a new string with all bytes initialized to zero.
    #[inline]
    pub fn new_zeroed(len: usize) -> Self {
        unsafe { Self::new_init_with(len, |buf| buf.write_bytes(0u8, len)) }
    }

    #[inline]
    fn layout_for(len: usize) -> Layout {
        assert!(len != 0);
        Layout::from_size_align(
            size_of::<Repr>().checked_add(len).unwrap(),
            align_of::<Repr>(),
        )
        .unwrap()
    }

    /// How long is the string, in bytes.
    #[inline]
    pub fn len(&self) -> usize {
        unsafe { (self.0.as_ptr() as *const usize).read() }
    }

    /// Returns true if the len is zero.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// We can't specialize the real to_string, but at least we can help out
    /// people whose muscle memory for <string-like-thing> -> String is 'call
    /// to_string()'
    #[inline]
    #[allow(clippy::inherent_to_string_shadow_display)]
    pub fn to_string(&self) -> String {
        String::from(self.as_str())
    }

    // Note: miri hates it if we get this via `data.as_ptr()` :/
    #[inline]
    fn data_ptr(&self) -> *const u8 {
        unsafe { (self.0.as_ptr() as *const u8).add(size_of::<usize>()) }
    }

    #[inline]
    fn data_ptr_mut(&mut self) -> *mut u8 {
        unsafe { (self.0.as_ptr() as *mut u8).add(size_of::<usize>()) }
    }

    /// Access the string's byte array.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        let len = self.len();
        unsafe { core::slice::from_raw_parts(self.data_ptr(), len) }
    }

    /// Get the underlying byte array mutably. It's unsound to write into this
    /// unless you ensure that it remains valid UTF8 after your writes.
    ///
    /// # Safety
    /// Caller must not write non-utf8 bytes
    #[inline]
    pub unsafe fn as_mut_bytes(&mut self) -> &mut [u8] {
        let len = self.len();
        core::slice::from_raw_parts_mut(self.data_ptr_mut(), len)
    }

    #[inline]
    pub fn as_mut_str(&mut self) -> &mut str {
        unsafe {
            let bytes = self.as_mut_bytes();
            #[cfg(any(test, miri))]
            {
                assert!(core::str::from_utf8(bytes).is_ok());
            }
            core::str::from_utf8_unchecked_mut(bytes)
        }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        let bytes = self.as_bytes();
        #[cfg(any(test, miri))]
        {
            assert!(core::str::from_utf8(bytes).is_ok());
        }
        unsafe { core::str::from_utf8_unchecked(bytes) }
    }
}

impl core::str::FromStr for ThinStr {
    type Err = core::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::new(s))
    }
}

impl core::ops::DerefMut for ThinStr {
    #[inline]
    fn deref_mut(&mut self) -> &mut str {
        self.as_mut_str()
    }
}
impl core::ops::Deref for ThinStr {
    type Target = str;
    #[inline]
    fn deref(&self) -> &str {
        self.as_str()
    }
}
impl AsRef<str> for ThinStr {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl AsMut<str> for ThinStr {
    #[inline]
    fn as_mut(&mut self) -> &mut str {
        self.as_mut_str()
    }
}

impl From<&str> for ThinStr {
    #[inline]
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

impl From<std::string::String> for ThinStr {
    #[inline]
    fn from(s: std::string::String) -> Self {
        Self::new(&s)
    }
}

impl Default for ThinStr {
    #[inline]
    fn default() -> Self {
        Self::empty()
    }
}

impl core::fmt::Debug for ThinStr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(self.as_str(), f)
    }
}

impl core::fmt::Display for ThinStr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self.as_str(), f)
    }
}

impl PartialEq for ThinStr {
    #[inline]
    fn eq(&self, o: &ThinStr) -> bool {
        PartialEq::eq(self.as_str(), o.as_str())
    }
    #[inline]
    fn ne(&self, o: &ThinStr) -> bool {
        PartialEq::ne(self.as_str(), o.as_str())
    }
}

impl Eq for ThinStr {}

impl PartialOrd for ThinStr {
    #[inline]
    fn partial_cmp(&self, o: &ThinStr) -> Option<core::cmp::Ordering> {
        Some(Ord::cmp(self.as_str(), o.as_str()))
    }
    #[inline]
    fn lt(&self, o: &ThinStr) -> bool {
        PartialOrd::lt(self.as_str(), o.as_str())
    }
    #[inline]
    fn le(&self, o: &ThinStr) -> bool {
        PartialOrd::le(self.as_str(), o.as_str())
    }
    #[inline]
    fn gt(&self, o: &ThinStr) -> bool {
        PartialOrd::gt(self.as_str(), o.as_str())
    }
    #[inline]
    fn ge(&self, o: &ThinStr) -> bool {
        PartialOrd::ge(self.as_str(), o.as_str())
    }
}

impl Ord for ThinStr {
    #[inline]
    fn cmp(&self, o: &ThinStr) -> core::cmp::Ordering {
        Ord::cmp(self.as_str(), o.as_str())
    }
}

impl core::hash::Hash for ThinStr {
    #[inline]
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl core::borrow::Borrow<str> for ThinStr {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

macro_rules! impl_index {
    ($($t:ty),+ $(,)?) => {$(
        impl core::ops::Index<$t> for ThinStr {
            type Output = str;
            #[inline]
            fn index(&self, idx: $t) -> &str {
                core::ops::Index::index(self.as_str(), idx)
            }
        }
        impl core::ops::IndexMut<$t> for ThinStr {
            #[inline]
            fn index_mut(&mut self, idx: $t) -> &mut str {
                self.as_mut_str().index_mut(idx)
            }
        }
    )+};
}

impl_index! {
    core::ops::RangeFull,
    core::ops::Range<usize>,
    core::ops::RangeTo<usize>,
    core::ops::RangeFrom<usize>,
    core::ops::RangeInclusive<usize>,
    core::ops::RangeToInclusive<usize>,
}

macro_rules! impl_eq {
    ($lhs:ty, $rhs: ty) => {
        impl<'a, 'b> PartialEq<$rhs> for $lhs {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool {
                PartialEq::eq(&self[..], &other[..])
            }
            #[inline]
            fn ne(&self, other: &$rhs) -> bool {
                PartialEq::ne(&self[..], &other[..])
            }
        }
        impl<'a, 'b> PartialEq<$lhs> for $rhs {
            #[inline]
            fn eq(&self, other: &$lhs) -> bool {
                PartialEq::eq(&self[..], &other[..])
            }
            #[inline]
            fn ne(&self, other: &$lhs) -> bool {
                PartialEq::ne(&self[..], &other[..])
            }
        }
    };
}
impl_eq!(ThinStr, str);
impl_eq!(ThinStr, &'a str);
impl_eq!(&'a ThinStr, str);

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_thinness() {
        assert_eq!(
            core::mem::size_of::<ThinStr>(),
            core::mem::size_of::<usize>()
        );
        assert_eq!(
            core::mem::size_of::<Option<ThinStr>>(),
            core::mem::size_of::<usize>()
        );
    }

    #[test]
    #[allow(clippy::redundant_clone)]
    fn test_create() {
        let xyz = ThinStr::empty();
        let abc = ThinStr::new("foo").clone();
        assert_eq!(xyz.len(), 0);
        assert_eq!(xyz, "");
        assert!(xyz.is_empty());
        assert_eq!(abc.len(), 3);
        assert_eq!(abc, "foo");
        let xyz2 = xyz.clone();
        assert_eq!(xyz2.len(), 0);
        assert_eq!(xyz2, "");
        let s = ThinStr::new("abcde");
        assert_eq!(s.len(), 5);
        let t: ThinStr = "foo".into();
        assert_ne!(t, s);
        let q = s.clone();
        assert_eq!(s, q);

        let z3 = ThinStr::new_zeroed(3);
        assert_eq!(z3, "\0\0\0");

        let z3 = ThinStr::new_zeroed(0);
        assert_eq!(z3, "");
    }

    #[test]
    fn test_ord() {
        let mut v: [ThinStr; 3] = ["foo".into(), "bar".into(), "quux".into()];
        v.sort();
        assert_eq!(v[0], "bar");
        assert_eq!(v[1], "foo");
        assert_eq!(v[2], "quux");
    }

    #[test]
    fn test_indexing() {
        let s = ThinStr::new("abcde");
        assert_eq!(&s[..], "abcde");
        assert_eq!(&s[1..], "bcde");
        assert_eq!(&s[1..3], "bc");
        assert_eq!(&s[1..=3], "bcd");
        assert_eq!(&s[..=3], "abcd");
        let mut y = ThinStr::new("abcde");
        y.as_mut_str().make_ascii_uppercase();
        assert_eq!(y, "ABCDE");

        fn ascii_upper<I>(s: &ThinStr, i: I) -> ThinStr
        where
            ThinStr: core::ops::IndexMut<I, Output = str>,
        {
            let mut s2 = s.clone();
            (&mut s2[i]).make_ascii_uppercase();
            s2
        }

        assert_eq!(ascii_upper(&s, ..), "ABCDE");
        assert_eq!(ascii_upper(&s, 1..), "aBCDE");
        assert_eq!(ascii_upper(&s, 1..3), "aBCde");
        assert_eq!(ascii_upper(&s, 1..=3), "aBCDe");
        assert_eq!(ascii_upper(&s, ..=3), "ABCDe");
        let mut t = ThinStr::empty();
        t.make_ascii_uppercase();
        assert_eq!(t, "");
    }

    #[test]
    fn test_fmt_conv() {
        assert_eq!(ThinStr::from(std::string::String::from("abcd")), "abcd");
        assert_eq!(
            std::string::String::from("abcd").as_str(),
            ThinStr::from("abcd")
        );
        assert_eq!("1234", format!("{}", ThinStr::new("1234")));
        assert_eq!("\"1234\"", format!("{:?}", ThinStr::new("1234")));
    }
}
