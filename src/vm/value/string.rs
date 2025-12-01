use std::ops::Deref;

use rustc_hash::FxBuildHasher;

use crate::vm::gc::{GcPtr, GcRef, GcRoot, GcUninitRoot, Heap, Trace, Tracer};

// TODO: intern all strings?
#[repr(align(16))]
pub struct Str {
    pub(crate) hash: u64,
    pub(crate) inner: String,
}

impl Str {
    #[inline(never)]
    pub fn alloc(heap: &Heap, s: &str) -> GcPtr<Self> {
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self {
                hash: heap.string_hasher().hash_str(s),
                inner: s.to_owned(),
            });
        })
    }

    #[inline(never)]
    pub fn new<'a>(heap: &Heap, root: GcUninitRoot<'a>, s: &str) -> GcRoot<'a, Self> {
        let ptr = Self::alloc(heap, s);
        unsafe { root.init_raw(ptr) }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl GcRef<'_, Str> {
    #[inline]
    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }
}

impl AsRef<str> for GcRef<'_, Str> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<std::path::Path> for GcRef<'_, Str> {
    fn as_ref(&self) -> &std::path::Path {
        self.as_str().as_ref()
    }
}

impl Deref for Str {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

unsafe impl Trace for Str {
    vtable!(Str);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        _ = tracer;
    }
}

impl std::fmt::Display for GcRef<'_, Str> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

impl std::fmt::Debug for GcRef<'_, Str> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.as_str(), f)
    }
}

#[derive(Default)]
pub(crate) struct StringHasher(FxBuildHasher);

impl StringHasher {
    pub fn hash_str(&self, key: &str) -> u64 {
        use std::hash::{BuildHasher as _, Hash as _, Hasher as _};

        let state = &mut self.0.build_hasher();
        key.hash(state);
        state.finish()
    }
}
