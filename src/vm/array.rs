//! Heap-allocated arrays.
//!
//! These are unsafe alternatives to `Vec<T>` and `Box<[T]>`.
//!
//! The idea behind using these instead of `Vec` and/or boxed slices is
//! drastically reduced generated code size, and more aggressive optimization.
//!
//! See the doc comment in the `vm` module for more information.

use std::alloc::{Layout, alloc_zeroed as allocate_zeroed, dealloc as deallocate};

/// A dynamically-sized heap-allocated array.
///
/// Does NOT initialize its contents, but _does_
/// keep track of which part of it is initialized.
///
/// Only supports `push` and `pop` operations.
pub struct DynStack<T: Sized + Copy> {
    inner: DynArray<T>,
    length: usize,
}

impl<T: Sized + Copy> DynStack<T> {
    /// Panics if `initial_capacity` is not a power of two.
    pub fn new(initial_capacity: usize) -> Self {
        Self {
            inner: DynArray::new(initial_capacity),
            length: 0,
        }
    }

    /// Push a value onto the stack.
    ///
    /// This grows the array if necessary.
    // No pointers are handed out by the stack, so calling
    // `push` is always safe.
    #[inline]
    pub fn push(&mut self, value: T) {
        unsafe {
            if self.inner.remaining(self.length) == 0 {
                self.inner.grow(1);
            }

            self.inner.offset(self.length).write(value);
            self.length += 1;
        }
    }

    /// Pop a value from the stack.
    ///
    /// Assumes length is non-zero.
    ///
    /// # Safety
    /// - There must be a value at the top of the stack.
    #[inline]
    pub unsafe fn pop_unchecked(&mut self) -> T {
        self.length -= 1;
        self.inner.offset(self.length).read()
    }
}

/// A statically-sized heap-allocated array.
///
/// Does NOT initialize its contents, and does
/// not keep track of which part of it is initialized.
///
/// The user assumes this responsibility.
pub struct DynArray<T: Sized + Copy> {
    base: *mut T,
    capacity: usize,
}

impl<T: Sized + Copy> DynArray<T> {
    /// Panics if `initial_capacity` is not a power of two.
    #[inline]
    pub fn new(initial_capacity: usize) -> Self {
        assert!(initial_capacity.is_power_of_two());

        unsafe {
            let capacity = initial_capacity;
            let base = allocate_zeroed(Self::layout(capacity)).cast::<T>();
            Self { base, capacity }
        }
    }

    /// Returns a pointer to item at `offset`.
    ///
    /// The item is not guaranteed to be initialized,
    /// it must be initialized by the user before being read!
    #[inline]
    pub fn offset(&self, offset: usize) -> *mut T {
        debug_assert!(offset < self.capacity);
        unsafe { self.base.add(offset) }
    }

    /// How many items this array can store.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Assuming the array holds items up to `offset`,
    /// returns how many bytes are remaining.
    ///
    /// The array does not guarantee to hold items up to `offset`.
    #[inline]
    pub fn remaining(&self, offset: usize) -> isize {
        (self.capacity as isize) - (offset as isize)
    }

    /// Grow the stack.
    ///
    /// Calling this invalidates any pointers to the array created by
    /// [`Self::offset`] before the call.
    #[inline(never)]
    #[cold]
    pub unsafe fn grow(&mut self, additional: usize) {
        let old_capacity = self.capacity;
        let old_base = self.base;

        let new_capacity = (old_capacity + additional).next_power_of_two();
        let new_base = allocate_zeroed(Self::layout(new_capacity)).cast::<T>();

        core::ptr::copy_nonoverlapping(old_base, new_base, old_capacity);
        deallocate(old_base.cast(), Self::layout(old_capacity));

        self.base = new_base;
        self.capacity = new_capacity;
    }

    #[inline]
    fn layout(capacity: usize) -> Layout {
        unsafe { Layout::array::<T>(capacity).unwrap_unchecked() }
    }
}

impl<T: Sized + Copy> Drop for DynArray<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe { deallocate(self.base.cast(), Self::layout(self.capacity)) }
    }
}
