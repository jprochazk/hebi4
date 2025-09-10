use std::hash::{BuildHasher, Hash as _, Hasher as _};

use hashbrown::{HashMap, hash_map::RawEntryMut};
use rustc_hash::FxBuildHasher;

use crate::vm::gc::Gc;

use super::gc::{Heap, ObjectKind, Ref, RefMut, Root, Trace, Tracer, ValueRef, ValueRoot};

// NOTE: `Value` must be bit-compatible with `Literal`,
// so that `Literal` can be directly treated as a `Value`

#[repr(C, u64)]
pub enum Literal {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,

    String(std::string::String),
}

#[derive(Default, Clone, Copy)]
#[repr(C, u64)]
pub enum ValueRaw {
    #[default]
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,

    String(Gc<String>),
    List(Gc<List>),
    Table(Gc<Table>),
    UData(Gc<UData>),
}

#[repr(align(16))]
pub struct String {
    inner: std::string::String,
}

impl String {
    #[inline]
    pub(crate) fn alloc(heap: &Heap, s: &str) -> Gc<Self> {
        let this = Self {
            inner: s.to_owned(),
        };

        heap.alloc_no_gc(this)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl Ref<'_, String> {
    #[inline]
    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }
}

unsafe impl Trace for String {
    const KIND: ObjectKind = ObjectKind::String;

    unsafe fn trace(&self, tracer: &Tracer) {
        _ = tracer;
    }
}

#[repr(align(16))]
pub struct List {
    items: Vec<ValueRaw>,
}

impl List {
    #[inline]
    pub(crate) fn alloc(heap: &Heap, capacity: usize) -> Gc<Self> {
        let this = Self {
            items: Vec::with_capacity(capacity),
        };

        heap.alloc_no_gc(this)
    }
}

unsafe impl Trace for List {
    const KIND: ObjectKind = ObjectKind::List;

    unsafe fn trace(&self, tracer: &Tracer) {
        for item in &self.items {
            tracer.visit_value(*item);
        }
    }
}

impl<'a> Ref<'a, List> {
    #[inline]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.items.capacity()
    }

    fn get<'this>(&'this self, index: usize) -> Option<ValueRef<'this>> {
        let Some(v) = self.items.get(index) else {
            return None;
        };

        let v = match v {
            ValueRaw::Nil => ValueRef::Nil,
            ValueRaw::Bool(v) => ValueRef::Bool(*v),
            ValueRaw::Int(v) => ValueRef::Int(*v),
            ValueRaw::Float(v) => ValueRef::Float(*v),
            ValueRaw::String(gc) => ValueRef::String(unsafe { gc.as_ref() }),
            ValueRaw::List(gc) => ValueRef::List(unsafe { gc.as_ref() }),
            ValueRaw::Table(gc) => ValueRef::Table(unsafe { gc.as_ref() }),
            ValueRaw::UData(gc) => ValueRef::UData(unsafe { gc.as_ref() }),
        };

        Some(v)
    }
}

#[derive(Debug, Clone)]
pub struct IndexOutOfBounds(usize);

impl std::error::Error for IndexOutOfBounds {}
impl std::fmt::Display for IndexOutOfBounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "index out of bounds: {}", self.0)
    }
}

impl<'a> RefMut<'a, List> {
    #[inline]
    pub fn push(&mut self, value: ValueRoot<'_>) {
        self.items.push(value.raw());
    }

    /// Push without checking if we have capacity for another value.
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
}

struct Opaque(u32);

#[repr(align(16))]
pub struct Table {
    map: HashMap<Opaque, (), ()>,
    kv: Vec<(Gc<String>, ValueRaw)>,
    hasher: FxBuildHasher,
}

#[inline]
fn hash_str(hasher: &impl BuildHasher, key: &str) -> u64 {
    let state = &mut hasher.build_hasher();
    key.hash(state);
    state.finish()
}

impl Table {
    #[inline]
    pub fn alloc(heap: &Heap, capacity: usize) -> Gc<Self> {
        let this = Self {
            map: HashMap::with_capacity_and_hasher(capacity, ()),
            kv: Vec::with_capacity(capacity),
            hasher: FxBuildHasher::default(),
        };

        heap.alloc_no_gc(this)
    }
}

pub trait AsStr {
    fn as_str(&self) -> &str;
}

impl AsStr for &str {
    fn as_str(&self) -> &str {
        self
    }
}

impl AsStr for &std::string::String {
    fn as_str(&self) -> &str {
        self
    }
}

impl AsStr for Ref<'_, String> {
    fn as_str(&self) -> &str {
        self.as_str()
    }
}

impl<'a> Ref<'a, Table> {
    #[inline]
    pub fn get<K>(&self, key: K) -> Option<ValueRef<'a>>
    where
        K: AsStr,
    {
        let this: &Table = &*self;
        let key = key.as_str();
        let hash = hash_str(&this.hasher, key);
        let entry = this.map.raw_entry().from_hash(hash, |&Opaque(index)| {
            // SAFETY: `index` is guaranteed to be valid
            let (stored_key, _) = unsafe { this.kv.get_unchecked(index as usize) };
            // SAFETY: dereferencing inner Gc'd pointers is safe,
            // because we are rooted (by `RefMut`), and the table
            // is traced.
            let stored_key = unsafe { stored_key.as_ref() };

            stored_key.as_str() == key
        });

        let Some((&Opaque(index), &())) = entry else {
            return None;
        };

        // SAFETY: `index` is guaranteed to be valid
        let (_, v) = unsafe { this.kv.get_unchecked(index as usize) };

        let v = match v {
            ValueRaw::Nil => ValueRef::Nil,
            ValueRaw::Bool(v) => ValueRef::Bool(*v),
            ValueRaw::Int(v) => ValueRef::Int(*v),
            ValueRaw::Float(v) => ValueRef::Float(*v),
            ValueRaw::String(gc) => ValueRef::String(unsafe { gc.as_ref() }),
            ValueRaw::List(gc) => ValueRef::List(unsafe { gc.as_ref() }),
            ValueRaw::Table(gc) => ValueRef::Table(unsafe { gc.as_ref() }),
            ValueRaw::UData(gc) => ValueRef::UData(unsafe { gc.as_ref() }),
        };

        Some(v)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.kv.len()
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.kv.capacity()
    }
}

impl RefMut<'_, Table> {
    #[inline]
    pub fn insert(&mut self, key: &Root<'_, String>, value: ValueRoot<'_>) {
        let this: &mut Table = &mut *self;
        let key = unsafe { key.as_ptr().as_ref() };

        let hash = hash_str(&this.hasher, key.as_str());
        let entry = this.map.raw_entry_mut().from_hash(hash, |&Opaque(index)| {
            // SAFETY: `index` is guaranteed to be valid
            let (stored_key, _) = unsafe { this.kv.get_unchecked(index as usize) };
            // SAFETY: dereferencing inner Gc'd pointers is safe,
            // because we are rooted (by `RefMut`), and the table
            // is traced.
            let stored_key = unsafe { stored_key.as_ref() };

            stored_key.as_str() == key.as_str()
        });

        match entry {
            RawEntryMut::Occupied(entry) => {
                let index = entry.key().0 as usize;
                // SAFETY: `index` is guaranteed to be valid
                let (k, v) = unsafe { this.kv.get_unchecked_mut(index) };
                *k = key.as_ptr();
                *v = value.raw();
            }
            RawEntryMut::Vacant(entry) => {
                let index = this.kv.len() as u32;
                // NOTE: the newly stored key is rooted at least for the duration
                // of this call to `insert`, proven by being passed in as `Ref`
                this.kv.push((key.as_ptr(), value.raw()));
                entry.insert_with_hasher(hash, Opaque(index), (), |&Opaque(index)| {
                    // SAFETY: `index` is guaranteed to be valid, because we just pushed it into `kv`
                    let (stored_key, _) = unsafe { this.kv.get_unchecked(index as usize) };
                    // SAFETY: we're accessing the key which is still rooted by caller
                    let stored_key = unsafe { stored_key.as_ref() };

                    hash_str(&this.hasher, stored_key.as_str())
                });
            }
        }
    }
}

unsafe impl Trace for Table {
    const KIND: ObjectKind = ObjectKind::Table;

    unsafe fn trace(&self, tracer: &Tracer) {
        for (key, value) in &self.kv {
            tracer.visit(*key);
            tracer.visit_value(*value);
        }
    }
}

pub struct Closure {
    // TODO
}

unsafe impl Trace for Closure {
    const KIND: ObjectKind = ObjectKind::Closure;

    unsafe fn trace(&self, tracer: &Tracer) {
        todo!()
    }
}

pub struct UData {
    // TODO
}

unsafe impl Trace for UData {
    const KIND: ObjectKind = ObjectKind::UData;

    unsafe fn trace(&self, tracer: &Tracer) {
        _ = tracer;
    }
}

#[macro_export]
macro_rules! string {
    (in $heap:ident; $string:ident = $str:expr) => {
        // SAFETY: the allocated object is immediately rooted
        let_root_unchecked!(
            unsafe in $heap;
            $string = $crate::vm::value::String::alloc($heap, $str)
        );
    };
}

#[macro_export]
macro_rules! list {
    (in $heap:ident; $list:ident = $capacity:expr) => {
        // SAFETY: the allocated object is immediately rooted
        let_root_unchecked!(
            unsafe in $heap;
            $list = $crate::vm::value::List::alloc($heap, $capacity)
        );
    };
}

#[macro_export]
macro_rules! table {
    (in $heap:ident; $table:ident = $capacity:expr) => {
        // SAFETY: the allocated object is immediately rooted
        let_root_unchecked!(
            unsafe in $heap;
            $table = $crate::vm::value::Table::alloc($heap, $capacity)
        );
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::vm::gc::Heap;

    #[test]
    fn list() {
        let heap = &mut Heap::new();

        {
            list!(in heap; list = 128);

            list.as_mut(heap).push(ValueRoot::Int(10));

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
        assert_eq!(heap.stats().collections(), 2);
    }

    #[test]
    fn table() {
        let heap = &mut Heap::new();

        {
            table!(in heap; table = 128);

            string!(in heap; string_a = "a");
            table.as_mut(heap).insert(&string_a, ValueRoot::Int(10));

            // `table` is safe from collection
            assert_eq!(
                heap.stats().bytes(),
                core::mem::size_of::<Table>() + core::mem::size_of::<String>()
            );
            heap.collect();
            assert_eq!(
                heap.stats().bytes(),
                core::mem::size_of::<Table>() + core::mem::size_of::<String>()
            );

            {
                let table = table.as_ref(heap);
                assert_eq!(table.len(), 1);
                assert_eq!(table.capacity(), 128);

                assert!(matches!(table.get("a"), Some(ValueRef::Int(10))));

                assert!(matches!(
                    table.get(string_a.as_ref(heap)),
                    Some(ValueRef::Int(10))
                ));
            }

            // insert another entry
            string!(in heap; string_b = "b");
            table.as_mut(heap).insert(&string_b, ValueRoot::Int(20));

            // `table` is safe from collection
            assert_eq!(
                heap.stats().bytes(),
                core::mem::size_of::<Table>() + (core::mem::size_of::<String>() * 2)
            );
            heap.collect();
            assert_eq!(
                heap.stats().bytes(),
                core::mem::size_of::<Table>() + (core::mem::size_of::<String>() * 2)
            );

            {
                let table = table.as_ref(heap);
                assert_eq!(table.len(), 2);
                assert_eq!(table.capacity(), 128);

                assert!(matches!(table.get("a"), Some(ValueRef::Int(10))));
                assert!(matches!(table.get("b"), Some(ValueRef::Int(20))));

                assert!(matches!(
                    table.get(string_a.as_ref(heap)),
                    Some(ValueRef::Int(10))
                ));
                assert!(matches!(
                    table.get(string_b.as_ref(heap)),
                    Some(ValueRef::Int(20))
                ));
            }
        }

        // `table` and `string` will be deallocated
        heap.collect();
        assert_eq!(heap.stats().bytes(), 0);
        assert_eq!(heap.stats().collections(), 3);
    }
}
