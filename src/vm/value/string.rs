use rustc_hash::FxBuildHasher;

use crate::vm::gc::{GcPtr, GcRef, Heap, Trace, Tracer};

// TODO: intern all strings?
#[repr(align(16))]
pub struct String {
    pub(crate) hash: u64,
    pub(crate) inner: std::string::String,
}

impl String {
    #[inline(never)]
    pub fn alloc(heap: &Heap, s: &str) -> GcPtr<Self> {
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self {
                hash: heap.string_hasher().hash_str(s),
                inner: s.to_owned(),
            });
        })
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl GcRef<'_, String> {
    #[inline]
    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }
}

unsafe impl Trace for String {
    vtable!(String);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        _ = tracer;
    }
}

impl std::fmt::Display for GcRef<'_, String> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

impl std::fmt::Debug for GcRef<'_, String> {
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
