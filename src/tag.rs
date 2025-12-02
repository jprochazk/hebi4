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

    /// Only uses first `Self::ALIGN` bits of `tag`.
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
        self.0.as_ptr().addr() & Self::MASK
    }

    /// Get the ptr.
    #[inline(always)]
    pub fn ptr(self) -> NonNull<T> {
        // SAFETY: guaranteed to still be non-null, as the original address is non-null.
        unsafe { NonNull::new_unchecked(self.0.as_ptr().map_addr(|addr| addr & !Self::MASK)) }
    }
}

impl<T: Sized> Clone for TaggedPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Sized> Copy for TaggedPtr<T> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[repr(align(2))]
    struct Align2(u8);

    #[repr(align(4))]
    struct Align4(u32);

    #[repr(align(8))]
    struct Align8(u64);

    #[repr(align(16))]
    struct Align16([u64; 2]);

    #[test]
    fn test_basic_tag_operations() {
        let value = Box::new(42u64);
        let ptr = NonNull::from(Box::leak(value));

        // Create tagged pointer with tag = 5
        let tagged = TaggedPtr::new(ptr, 5);
        assert_eq!(tagged.tag(), 5);
        assert_eq!(tagged.ptr(), ptr);

        // Clean up
        unsafe { drop(Box::from_raw(ptr.as_ptr())) };
    }

    #[test]
    fn test_tag_masking() {
        let value = Box::new(Align8(123));
        let ptr = NonNull::from(Box::leak(value));

        // Align8 gives us 3 bits for tags (values 0-7)
        // Try to set tag = 15 (0b1111), should only use lower 3 bits (0b111 = 7)
        let tagged = TaggedPtr::new(ptr, 0b1111);
        assert_eq!(tagged.tag(), 0b111); // Only lower 3 bits used

        unsafe { drop(Box::from_raw(ptr.as_ptr())) };
    }

    #[test]
    fn test_update_tag() {
        let value = Box::new(Align4(100));
        let ptr = NonNull::from(Box::leak(value));

        let tagged = TaggedPtr::new(ptr, 1);
        assert_eq!(tagged.tag(), 1);

        // Update tag to 2
        let tagged = tagged.with_tag(2);
        assert_eq!(tagged.tag(), 2);
        assert_eq!(tagged.ptr(), ptr); // Pointer unchanged

        // Update tag to 3
        let tagged = tagged.with_tag(3);
        assert_eq!(tagged.tag(), 3);
        assert_eq!(tagged.ptr(), ptr);

        unsafe { drop(Box::from_raw(ptr.as_ptr())) };
    }

    #[test]
    fn test_pointer_preservation() {
        let value = Box::new(Align16([0xDEADBEEF, 0xCAFEBABE]));
        let ptr = NonNull::from(Box::leak(value));
        let original_addr = ptr.as_ptr() as usize;

        // Set various tags
        for tag in 0..16 {
            let tagged = TaggedPtr::new(ptr, tag);
            let extracted_ptr = tagged.ptr();
            let extracted_addr = extracted_ptr.as_ptr() as usize;

            // The extracted pointer should match the original
            assert_eq!(extracted_addr, original_addr);
            assert_eq!(tagged.tag(), tag & 0xF); // Mask to available bits
        }

        unsafe { drop(Box::from_raw(ptr.as_ptr())) };
    }

    #[test]
    fn test_different_alignments() {
        // Align2: 1 bit for tag (0-1)
        {
            let value = Box::new(Align2(5));
            let ptr = NonNull::from(Box::leak(value));
            let tagged = TaggedPtr::new(ptr, 1);
            assert_eq!(tagged.tag(), 1);
            assert_eq!(tagged.ptr(), ptr);
            unsafe { drop(Box::from_raw(ptr.as_ptr())) };
        }

        // Align4: 2 bits for tag (0-3)
        {
            let value = Box::new(Align4(10));
            let ptr = NonNull::from(Box::leak(value));
            let tagged = TaggedPtr::new(ptr, 3);
            assert_eq!(tagged.tag(), 3);
            assert_eq!(tagged.ptr(), ptr);
            unsafe { drop(Box::from_raw(ptr.as_ptr())) };
        }

        // Align8: 3 bits for tag (0-7)
        {
            let value = Box::new(Align8(20));
            let ptr = NonNull::from(Box::leak(value));
            let tagged = TaggedPtr::new(ptr, 7);
            assert_eq!(tagged.tag(), 7);
            assert_eq!(tagged.ptr(), ptr);
            unsafe { drop(Box::from_raw(ptr.as_ptr())) };
        }
    }

    #[test]
    fn test_zero_tag() {
        let value = Box::new(Align8(77));
        let ptr = NonNull::from(Box::leak(value));

        let tagged = TaggedPtr::new(ptr, 0);
        assert_eq!(tagged.tag(), 0);
        assert_eq!(tagged.ptr(), ptr);

        unsafe { drop(Box::from_raw(ptr.as_ptr())) };
    }

    #[test]
    fn test_max_tag_value() {
        let value = Box::new(Align8(55));
        let ptr = NonNull::from(Box::leak(value));

        // For Align8, max tag is 7 (0b111)
        let tagged = TaggedPtr::new(ptr, 7);
        assert_eq!(tagged.tag(), 7);
        assert_eq!(tagged.ptr(), ptr);

        unsafe { drop(Box::from_raw(ptr.as_ptr())) };
    }

    #[test]
    fn test_sequential_tag_updates() {
        let value = Box::new(Align8(33));
        let ptr = NonNull::from(Box::leak(value));
        let mut tagged = TaggedPtr::new(ptr, 0);

        for expected_tag in 0..8 {
            tagged = tagged.with_tag(expected_tag);
            assert_eq!(tagged.tag(), expected_tag);
            assert_eq!(tagged.ptr(), ptr);
        }

        unsafe { drop(Box::from_raw(ptr.as_ptr())) };
    }
}
