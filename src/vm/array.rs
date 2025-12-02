//! Heap-allocated arrays.
//!
//! These are unsafe alternatives to `Vec<T>` and `Box<[T]>`.
//!
//! The idea behind using these instead of `Vec` and/or boxed slices is
//! drastically reduced generated code size, and more aggressive optimization.
//!
//! See the doc comment in the `vm` module for more information.

use std::alloc::{Layout, alloc_zeroed, dealloc, handle_alloc_error};

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

    /// Set length to `new_len`.
    ///
    /// `new_len` must be less than or equal to current length.
    #[inline]
    pub unsafe fn set_length(&mut self, new_len: usize) {
        debug_assert!(self.length >= new_len);
        self.length = new_len;
    }

    /// Push a value onto the stack.
    ///
    /// This grows the array if necessary.
    // No pointers are handed out by the stack, so calling
    // `push` is always safe.
    #[inline]
    pub fn push(&mut self, value: T) {
        if self.inner.remaining(self.length) == 0 {
            let old_cap = self.inner.capacity;
            unsafe { self.inner.grow(1) }
            let new_cap = self.inner.capacity;
        }

        unsafe { self.inner.offset(self.length).write(value) };
        self.length += 1;
    }

    /// Pop a value from the stack.
    ///
    /// Assumes length is non-zero.
    ///
    /// # Safety
    /// - There must be a value at the top of the stack.
    #[inline]
    pub unsafe fn pop_unchecked(&mut self) -> T {
        debug_assert!(self.length > 0);
        self.length -= 1;
        self.inner.offset(self.length).read()
    }

    /// Read the top value.
    ///
    /// Assumes length is non-zero.
    ///
    /// # Safety
    /// - There must be a value at the top of the stack.
    #[inline]
    pub unsafe fn top_unchecked(&mut self) -> *mut T {
        debug_assert!(self.length > 0);
        self.inner.base.add(self.length - 1)
    }

    /// Read the `n`th value.
    ///
    /// Assumes length is non-zero.
    ///
    /// # Safety
    /// - There must be a value at the top of the stack.
    #[inline]
    pub unsafe fn at_unchecked(&mut self, n: usize) -> *mut T {
        debug_assert!(self.length > n);
        self.inner.base.add(n)
    }

    #[inline]
    pub fn top(&mut self) -> Option<*mut T> {
        if self.length == 0 {
            return None;
        }

        Some(unsafe { self.top_unchecked() })
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.length
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    #[inline]
    pub fn iter(&self) -> DynStackIter<'_, T> {
        DynStackIter {
            stack: self,
            front: 0,
            back: self.len().wrapping_sub(1),
        }
    }

    #[inline]
    pub unsafe fn iter_raw(this: *const Self) -> DynStackIterRaw<T> {
        DynStackIterRaw {
            stack: this,
            front: 0,
            back: (*this).length.wrapping_sub(1),
        }
    }
}

pub struct DynStackIter<'a, T: Sized + Copy> {
    stack: &'a DynStack<T>,
    front: usize,
    back: usize,
}

impl<'a, T: Sized + Copy> Iterator for DynStackIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.front >= self.stack.length {
            return None;
        }

        let index = self.front;
        self.front = self.front.wrapping_add(1);

        Some(unsafe { &*self.stack.inner.offset(index) })
    }
}

impl<'a, T: Sized + Copy> DoubleEndedIterator for DynStackIter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.back >= self.stack.length {
            return None;
        }

        let index = self.back;
        self.back = self.back.wrapping_sub(1);

        Some(unsafe { &*self.stack.inner.offset(index) })
    }
}

pub struct DynStackIterRaw<T: Sized + Copy> {
    stack: *const DynStack<T>,
    front: usize,
    back: usize,
}

impl<T: Sized + Copy> Iterator for DynStackIterRaw<T> {
    type Item = *mut T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.front >= unsafe { (*self.stack).length } {
            return None;
        }

        let index = self.front;
        self.front = self.front.wrapping_add(1);

        Some(unsafe { (*self.stack).inner.offset(index) })
    }
}

impl<T: Sized + Copy> DoubleEndedIterator for DynStackIterRaw<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.back >= unsafe { (*self.stack).length } {
            return None;
        }

        let index = self.back;
        self.back = self.back.wrapping_sub(1);

        Some(unsafe { (*self.stack).inner.offset(index) })
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

        let capacity = initial_capacity;
        let layout = Self::layout(capacity);
        // NOTE: `alloc_zeroed` is load-bearing:
        // `0` is a valid `ValueRaw`, and we need the stack to
        // not contain any uninitialized memory, otherwise the
        // GC isn't allowed to trace it.
        let base = unsafe { alloc_zeroed(layout).cast::<T>() };
        if base.is_null() {
            handle_alloc_error(layout);
        }
        Self { base, capacity }
    }

    /// Returns a pointer to item at `offset`.
    ///
    /// The item is not guaranteed to be initialized,
    /// it must be initialized by the user before being read!
    #[inline]
    pub unsafe fn offset(&self, offset: usize) -> *mut T {
        debug_assert!(offset < self.capacity, "{} < {}", offset, self.capacity);
        self.base.add(offset)
    }

    /// How many items this array can store.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Assuming the array holds items up to `offset`,
    /// returns how many slots are remaining.
    ///
    /// The array does not guarantee to hold items up to `offset`.
    #[inline]
    pub fn remaining(&self, offset: usize) -> isize {
        (self.capacity as isize) - (offset as isize)
    }

    /// Grow the array.
    ///
    /// Calling this invalidates any pointers to the array created by
    /// [`Self::offset`] before the call.
    #[cold]
    #[inline]
    pub unsafe fn grow(&mut self, additional: usize) {
        let old_capacity = self.capacity;
        let old_base = self.base;

        let new_capacity = (old_capacity + additional).next_power_of_two();
        let new_layout = Self::layout(new_capacity);
        // NOTE: `alloc_zeroed` is load-bearing:
        // `0` is a valid `ValueRaw`, and we need the array to
        // not contain any uninitialized memory, otherwise the
        // GC isn't allowed to trace it.
        let new_base = alloc_zeroed(new_layout).cast::<T>();
        if new_base.is_null() {
            handle_alloc_error(new_layout);
        }

        core::ptr::copy_nonoverlapping(old_base, new_base, old_capacity);
        dealloc(old_base.cast(), Self::layout(old_capacity));

        self.base = new_base;
        self.capacity = new_capacity;
    }

    /// Sets all values in the range `keep..self.capacity` to zero.
    ///
    /// This does not reallocate the array. It is equivalent to a memset.
    ///
    /// To use this safely, `T` must have a valid bit pattern of `0`.
    #[inline]
    pub unsafe fn drain_to_end(&mut self, keep: usize) {
        let count = self.capacity - keep;
        if count == 0 {
            return;
        }
        self.offset(keep).write_bytes(0, count);
    }

    #[inline]
    fn layout(capacity: usize) -> Layout {
        unsafe { Layout::array::<T>(capacity).unwrap_unchecked() }
    }
}

impl<T: Sized + Copy> Drop for DynArray<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe { dealloc(self.base.cast(), Self::layout(self.capacity)) }
    }
}
