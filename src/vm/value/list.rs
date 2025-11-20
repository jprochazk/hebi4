use super::{
    super::gc::{GcRef, GcRefMut, Trace, Tracer, ValueRef, ValueRoot},
    ValueRaw,
};
use crate::vm::gc::{GcPtr, GcRoot, GcUninitRoot, Heap};

#[repr(align(16))]
pub struct List {
    pub(crate) items: Vec<ValueRaw>,
}

impl List {
    #[inline(never)]
    pub fn alloc(heap: &Heap, capacity: usize) -> GcPtr<Self> {
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self {
                items: Vec::with_capacity(capacity),
            });
        })
    }

    /// Allocate a `List` initialized to `len` `nil`s.
    #[inline(never)]
    pub fn alloc_zeroed(heap: &Heap, len: usize) -> GcPtr<Self> {
        heap.alloc_no_gc(|ptr| unsafe {
            let mut items = Vec::with_capacity(len);
            {
                let items = items.spare_capacity_mut();
                for i in 0..len {
                    items.get_unchecked_mut(i).write(ValueRaw::Nil);
                }
            }
            items.set_len(len);

            (*ptr).write(Self { items });
        })
    }

    pub fn new_zeroed<'a>(heap: &Heap, root: GcUninitRoot<'a>, len: usize) -> GcRoot<'a, Self> {
        let ptr = Self::alloc_zeroed(heap, len);
        unsafe { root.init_raw(ptr) }
    }

    pub fn new<'a>(heap: &Heap, root: GcUninitRoot<'a>, capacity: usize) -> GcRoot<'a, Self> {
        let ptr = Self::alloc(heap, capacity);
        unsafe { root.init_raw(ptr) }
    }
}

unsafe impl Trace for List {
    vtable!(List);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        for item in &self.items {
            tracer.visit_value(*item);
        }
    }
}

impl<'a> GcRef<'a, List> {
    #[inline]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.items.capacity()
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<ValueRef<'a>> {
        GcRef::map_value_opt(self, |this| this.items.get(index))
    }

    #[inline]
    pub unsafe fn get_unchecked(&self, index: usize) -> ValueRef<'a> {
        GcRef::map_value(self, |this| this.items.get_unchecked(index))
    }

    #[inline]
    pub fn iter(&self) -> ListIter<'a> {
        ListIter {
            list: *self,
            index: 0,
        }
    }
}

impl<'a> IntoIterator for GcRef<'a, List> {
    type Item = ValueRef<'a>;

    type IntoIter = ListIter<'a>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct ListIter<'a> {
    pub(crate) list: GcRef<'a, List>,
    pub(crate) index: usize,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = ValueRef<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.list.get(self.index) {
            Some(v) => {
                self.index += 1;

                Some(v)
            }

            None => {
                // fused iterator
                self.index = usize::MAX;
                None
            }
        }
    }
}

impl<'a> std::iter::FusedIterator for ListIter<'a> {}

#[derive(Debug, Clone)]
pub struct IndexOutOfBounds(usize);

impl std::error::Error for IndexOutOfBounds {}

impl std::fmt::Display for IndexOutOfBounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "index out of bounds: {}", self.0)
    }
}

impl<'a> GcRefMut<'a, List> {
    /// Append `value` to the list.
    #[inline]
    pub fn push(&mut self, value: ValueRoot<'_>) {
        // unrooted pointer is transitively rooted by `self`
        self.items.push(value.raw());
    }

    /// Set `self[index]` to `value`.
    #[inline]
    pub fn set(&mut self, index: usize, value: ValueRoot<'_>) -> Result<(), IndexOutOfBounds> {
        if index >= self.items.len() {
            return Err(IndexOutOfBounds(index));
        }

        // SAFETY: we just checked that the index is in bounds
        unsafe {
            *self.items.get_unchecked_mut(index) = value.raw();
        }

        Ok(())
    }

    /// Set `self[index]` to `value`.
    ///
    /// ## Safety
    ///
    /// - `value` must still be alive.
    /// - `index` must be in bounds.
    #[inline]
    pub unsafe fn set_raw_unchecked(&mut self, index: usize, value: ValueRaw) {
        debug_assert!(index < self.items.len());
        unsafe {
            *self.items.get_unchecked_mut(index) = value;
        }
    }

    /// Resizes `self` to exactly `new_size`, filling empty slots with `nil`.
    ///
    /// Assumes that `self` will _grow_, and that there is enough capacity for it.
    ///
    /// ## Safety
    ///
    /// - `self.len() <= new_size`
    /// - `self.capacity() >= new_size`
    #[inline]
    pub unsafe fn resize_grow_unchecked(&mut self, new_size: usize) {
        debug_assert!(self.items.len() <= new_size);

        let additional = new_size - self.items.len();

        let items = self.items.spare_capacity_mut();
        for i in 0..additional {
            items.get_unchecked_mut(i).write(ValueRaw::Nil);
        }

        unsafe {
            self.items.set_len(new_size);
        }
    }
}

impl std::fmt::Debug for GcRef<'_, List> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for value in self.iter() {
            list.entry(&value);
        }
        list.finish()
    }
}

impl std::fmt::Debug for GcRefMut<'_, List> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        // Not using `iter`, because that requires `GcRef` which can't
        // be produced from `&GcRefMut`, it needs an owned `GcRefMut`.
        for value in &self.items {
            // SAFETY: transitively rooted
            let value = unsafe { value.as_ref() };
            list.entry(&value);
        }
        list.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::gc::{Heap, ValueRef, ValueRoot, let_root};

    #[test]
    fn list() {
        let heap = &mut Heap::new();

        {
            let_root!(in heap; list);
            let list = List::new(heap, list, 128);

            list.as_mut(heap).push(ValueRoot::Int(10));

            // `list` is safe from collection
            assert_eq!(heap.stats().bytes(), core::mem::size_of::<List>());
            heap.collect_no_external_roots();
            assert_eq!(heap.stats().bytes(), core::mem::size_of::<List>());

            {
                let list = list.as_ref(heap);
                assert_eq!(list.len(), 1);
                assert_eq!(list.capacity(), 128);
                assert!(matches!(list.get(0), Some(ValueRef::Int(10))));
            }
        }

        // `list` will be deallocated
        heap.collect_no_external_roots();
        assert_eq!(heap.stats().bytes(), 0);
        assert_eq!(heap.stats().collections(), 2);
    }
}
