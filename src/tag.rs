use std::{num::NonZeroUsize, ptr::NonNull};

/// Tagged pointer.
#[repr(transparent)]
pub struct TaggedPtr<T: Sized>(NonNull<T>);

// has a niche
const _: () =
    assert!(std::mem::size_of::<TaggedPtr<()>>() == std::mem::size_of::<Option<TaggedPtr<()>>>());

impl<T: Sized> TaggedPtr<T> {
    const ALIGN: usize = core::mem::align_of::<T>();
    const MASK: usize = Self::ALIGN - 1;

    const __INVALID_ALIGNMENT_CHECK: () =
        assert!(TaggedPtr::<T>::ALIGN >= 2 && TaggedPtr::<T>::ALIGN.is_power_of_two());

    /// Only uses first `Self::ALIGN` bits of `tag`.
    #[inline(always)]
    pub fn new(ptr: NonNull<T>, tag: usize) -> Self {
        let _ = Self::__INVALID_ALIGNMENT_CHECK;

        Self(ptr).with_tag(tag)
    }

    #[inline(always)]
    pub fn with_tag(self, tag: usize) -> Self {
        Self(self.0.map_addr(
            #[inline]
            |addr| {
                let addr = (addr.get() & !Self::MASK) | (tag & Self::MASK);
                // SAFETY: guaranteed to still be non-zero, as the original address is non-zero.
                unsafe { NonZeroUsize::new_unchecked(addr) }
            },
        ))
    }

    /// Get the first `Self::ALIGN` bits.
    #[inline(always)]
    pub fn tag(self) -> usize {
        self.0.addr().get() & Self::MASK
    }

    /// Get the ptr.
    #[inline(always)]
    pub fn ptr(self) -> NonNull<T> {
        self.0.map_addr(|addr| {
            let addr = addr.get() & !Self::MASK;
            // SAFETY: guaranteed to still be non-zero, as the original address is non-zero.
            unsafe { NonZeroUsize::new_unchecked(addr) }
        })
    }

    /// Get the ptr without checking the tag.
    ///
    /// This assumes that all tag bits are `0`.
    #[inline(always)]
    pub unsafe fn assume_zero_tag(self) -> NonNull<T> {
        let ptr = self.0;
        debug_assert!((ptr.addr().get() & Self::MASK) == 0);
        ptr
    }
}

impl<T: Sized> Clone for TaggedPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Sized> Copy for TaggedPtr<T> {}
