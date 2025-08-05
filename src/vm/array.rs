use std::alloc::{Layout, alloc_zeroed as allocate_zeroed, dealloc as deallocate};

pub struct DynList<T: Sized + Default + Copy> {
    inner: DynArray<T>,
    length: usize,
}

impl<T: Sized + Default + Copy> DynList<T> {
    /// `initial_capacity` must be a power of two.
    pub fn new(initial_capacity: usize) -> Self {
        Self {
            inner: DynArray::new(initial_capacity),
            length: 0,
        }
    }

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

    #[inline]
    pub unsafe fn pop_unchecked(&mut self) -> T {
        self.length -= 1;
        self.inner.offset(self.length).read()
    }
}

pub struct DynArray<T: Sized + Default + Copy> {
    base: *mut T,
    capacity: usize,
}

impl<T: Sized + Default + Copy> DynArray<T> {
    /// `initial_capacity` must be a power of two.
    #[inline]
    pub fn new(initial_capacity: usize) -> Self {
        assert!(initial_capacity.is_power_of_two());

        unsafe {
            let capacity = initial_capacity;
            let base = allocate_zeroed(Self::layout(capacity)).cast::<T>();
            for i in 0..capacity {
                base.add(i).write(T::default());
            }
            Self { base, capacity }
        }
    }

    #[inline]
    pub fn offset(&self, offset: usize) -> *mut T {
        debug_assert!(offset < self.capacity);
        unsafe { self.base.add(offset) }
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    #[inline]
    pub fn remaining(&self, offset: usize) -> isize {
        (self.capacity as isize) - (offset as isize)
    }

    /// Grow the stack.
    ///
    /// This invalidates any pointers to the old array.
    #[inline(never)]
    #[cold]
    pub unsafe fn grow(&mut self, additional: usize) {
        let old_capacity = self.capacity;
        let old_base = self.base;

        let new_capacity = (old_capacity + additional).next_power_of_two();
        let new_base = allocate_zeroed(Self::layout(new_capacity)).cast::<T>();

        core::ptr::copy_nonoverlapping(old_base, new_base, old_capacity);
        for i in old_capacity..new_capacity {
            new_base.add(i).write(T::default());
        }
        deallocate(old_base.cast(), Self::layout(old_capacity));

        self.base = new_base;
        self.capacity = new_capacity;
    }

    #[inline]
    fn layout(capacity: usize) -> Layout {
        unsafe { Layout::array::<T>(capacity).unwrap_unchecked() }
    }
}

impl<T: Sized + Default + Copy> Drop for DynArray<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe { deallocate(self.base.cast(), Self::layout(self.capacity)) }
    }
}
