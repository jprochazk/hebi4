use crate::vm::gc::Gc;

use super::gc::{
    ObjectType, Ref, RefMut, Rootable, Trace, Tracer, ValueRef, ValueRefMut, ValueRoot,
};

// NOTE: `Value` must be bit-compatible with `Literal`,
// so that `Literal` can be directly treated as a `Value`

#[repr(C, u64)]
pub enum Literal {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
}

#[derive(Default, Clone, Copy)]
#[repr(C, u64)]
pub enum ValueRaw {
    #[default]
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
    List(Gc<List>) = 4,
    Table(Gc<Table>) = 5,
    UData(Gc<UData>) = 6,
}

pub struct List {
    items: Vec<ValueRaw>,
}

impl List {
    /// ## Panics
    ///
    /// if `capacity` is not a power of two
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            items: Vec::with_capacity(capacity),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.items.capacity()
    }

    #[inline]
    pub unsafe fn push_unchecked(&mut self, value: ValueRoot<'_>) {
        std::hint::assert_unchecked(self.items.spare_capacity_mut().len() > 0);

        let len = self.items.len();
        self.items
            .spare_capacity_mut()
            .get_unchecked_mut(0)
            .write(value.raw());
        self.items.set_len(len + 1);
    }

    #[inline]
    pub fn push(&mut self, value: ValueRoot<'_>) {
        self.items.push(value.raw());
    }
}

impl Rootable for List {
    const TYPE: ObjectType = ObjectType::List;
}

unsafe impl Trace for List {
    unsafe fn trace(&self, tracer: &Tracer) {
        for item in &self.items {
            tracer.visit_value(*item);
        }
    }
}

impl<'a> Ref<'a, List> {
    fn get<'this>(&'this self, index: usize) -> Option<ValueRef<'this>> {
        let Some(v) = self.items.get(index) else {
            return None;
        };

        let v = match v {
            ValueRaw::Nil => ValueRef::Nil,
            ValueRaw::Bool(v) => ValueRef::Bool(*v),
            ValueRaw::Int(v) => ValueRef::Int(*v),
            ValueRaw::Float(v) => ValueRef::Float(*v),
            ValueRaw::List(gc) => ValueRef::List(unsafe { Ref::new_unchecked(gc.as_ref()) }),
            ValueRaw::Table(gc) => ValueRef::Table(unsafe { Ref::new_unchecked(gc.as_ref()) }),
            ValueRaw::UData(gc) => ValueRef::UData(unsafe { Ref::new_unchecked(gc.as_ref()) }),
        };

        Some(v)
    }
}

impl<'a> RefMut<'a, List> {
    fn get_mut<'this>(&'this mut self, index: usize) -> Option<ValueRefMut<'this>> {
        let Some(v) = self.items.get(index) else {
            return None;
        };

        let v = match v {
            ValueRaw::Nil => ValueRefMut::Nil,
            ValueRaw::Bool(v) => ValueRefMut::Bool(*v),
            ValueRaw::Int(v) => ValueRefMut::Int(*v),
            ValueRaw::Float(v) => ValueRefMut::Float(*v),
            ValueRaw::List(gc) => {
                ValueRefMut::List(unsafe { RefMut::new_unchecked(gc.as_ref_mut()) })
            }
            ValueRaw::Table(gc) => {
                ValueRefMut::Table(unsafe { RefMut::new_unchecked(gc.as_ref_mut()) })
            }
            ValueRaw::UData(gc) => {
                ValueRefMut::UData(unsafe { RefMut::new_unchecked(gc.as_ref_mut()) })
            }
        };

        Some(v)
    }
}

pub struct Table {
    // TODO
}

impl Rootable for Table {
    const TYPE: ObjectType = ObjectType::Table;
}

unsafe impl Trace for Table {
    unsafe fn trace(&self, tracer: &Tracer) {
        todo!()
    }
}

pub struct Closure {
    // TODO
}

impl Rootable for Closure {
    const TYPE: ObjectType = ObjectType::Closure;
}

unsafe impl Trace for Closure {
    unsafe fn trace(&self, tracer: &Tracer) {
        todo!()
    }
}

pub struct UData {
    // TODO
}

impl Rootable for UData {
    const TYPE: ObjectType = ObjectType::UData;
}

unsafe impl Trace for UData {
    unsafe fn trace(&self, tracer: &Tracer) {
        _ = tracer;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::vm::gc::Heap;

    #[test]
    fn alloc_array() {
        let heap = &mut Heap::new();

        {
            let_root_unchecked![
                unsafe in heap;
                list = heap.alloc_no_gc(List::new(128))
            ];

            list.as_ref_mut(heap).push(ValueRoot::Int(10));

            // `list` is safe from collection
            assert_eq!(heap.stats().bytes(), core::mem::size_of::<List>());
            heap.collect();
            assert_eq!(heap.stats().bytes(), core::mem::size_of::<List>());

            {
                let list = list.as_ref(heap);
                assert_eq!(list.len(), 1);
                assert_eq!(list.capacity(), 128);
                assert!(matches!(list.get(0), Some(ValueRef::Int(10))));
            }
        }

        // `list` will be deallocated
        heap.collect();
        assert_eq!(heap.stats().bytes(), 0);
    }
}
