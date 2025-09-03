use crate::vm::gc::Gc;

use super::{
    array::DynArray,
    gc::{ObjectType, Rootable, Trace, Tracer},
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
pub enum Value {
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
    /// Determines which part of the list is safe to read
    ///
    /// `items[..length]`
    len: usize,

    /// Backing storage
    items: DynArray<Value>,
}

impl List {
    /// ## Panics
    ///
    /// if `capacity` is not a power of two
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            len: 0,
            items: DynArray::new(capacity),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.items.capacity()
    }
}

impl Rootable for List {
    const TYPE: ObjectType = ObjectType::List;
}

unsafe impl Trace for List {
    unsafe fn trace(&self, tracer: &Tracer) {
        let len = self.len;
        let base = self.items.offset(0);
        let slice = std::slice::from_raw_parts(base, len);

        for item in slice {
            tracer.visit_value(*item);
        }
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

        let list = unsafe { root_unchecked![in heap; heap.alloc_no_gc(List::new(128))] };
        heap.collect(); // `list` is safe from collection

        {
            let list = list.as_ref(heap);
            assert_eq!(list.len(), 0);
            assert_eq!(list.capacity(), 128);
        }
    }
}
