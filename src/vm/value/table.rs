use hashbrown::{HashMap, hash_map::RawEntryMut};

use super::{Str, StringHasher, ValueRaw};
use crate::vm::gc::{
    GcPtr, GcRef, GcRefMut, GcRoot, GcUninitRoot, Heap, Trace, Tracer, ValueRef, ValueRoot,
};

pub(crate) struct Opaque(u32);

#[repr(align(16))]
pub struct Table {
    pub(crate) map: HashMap<Opaque, (), ()>,
    pub(crate) kv: Vec<(GcPtr<Str>, ValueRaw)>,
    pub(crate) hash: StringHasher,
}

impl Table {
    #[inline(never)]
    pub fn alloc(heap: &Heap, capacity: usize) -> GcPtr<Self> {
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self {
                map: HashMap::with_capacity_and_hasher(capacity, ()),
                kv: Vec::with_capacity(capacity),
                hash: StringHasher::default(),
            });
        })
    }

    #[inline(never)]
    pub fn new<'a>(heap: &Heap, root: GcUninitRoot<'a>, capacity: usize) -> GcRoot<'a, Self> {
        let ptr = Self::alloc(heap, capacity);
        unsafe { root.init_raw(ptr) }
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

impl AsStr for &String {
    fn as_str(&self) -> &str {
        self
    }
}

impl AsStr for GcRef<'_, Str> {
    fn as_str(&self) -> &str {
        self.as_str()
    }
}

impl<'a> GcRef<'a, Table> {
    // TODO: string interning, pre-hashing
    #[inline]
    pub fn get<K>(&self, key: K) -> Option<ValueRef<'a>>
    where
        K: AsStr,
    {
        let this: &Table = &*self;
        let key = key.as_str();
        let hash = this.hash.hash_str(key);
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

        // SAFETY: transitively rooted
        let v = unsafe { v.as_ref() };

        Some(v)
    }

    // TODO: string interning
    #[inline]
    pub(crate) unsafe fn get_raw(&self, key: GcPtr<Str>) -> Option<ValueRaw> {
        let this: &Table = &*self;
        let hash = key.as_ref().hash;
        let entry = this.map.raw_entry().from_hash(hash, |&Opaque(index)| {
            // SAFETY: `index` is guaranteed to be valid
            let (stored_key, _) = unsafe { this.kv.get_unchecked(index as usize) };
            // SAFETY: dereferencing inner Gc'd pointers is safe,
            // because we are rooted (by `RefMut`), and the table
            // is traced.
            let stored_key = unsafe { stored_key.as_ref() };

            stored_key.as_str() == key.as_ref().as_str()
        });

        let Some((&Opaque(index), &())) = entry else {
            return None;
        };

        // SAFETY: `index` is guaranteed to be valid
        let (_, v) = unsafe { this.kv.get_unchecked(index as usize) };

        Some(*v)
    }

    #[inline]
    pub fn entry(&self, index: usize) -> Option<(GcRef<'a, Str>, ValueRef<'a>)> {
        // TODO: can this use `Ref::map`?
        let this: &Table = &*self;

        let Some((k, v)) = this.kv.get(index) else {
            return None;
        };

        // SAFETY: transitively rooted
        let k = unsafe { k.as_ref() };
        let v = unsafe { v.as_ref() };

        Some((k, v))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.kv.len()
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.kv.capacity()
    }

    #[inline]
    pub fn entries(&self) -> TableEntries<'a> {
        TableEntries {
            table: *self,
            index: 0,
        }
    }
}

impl<'a> IntoIterator for GcRef<'a, Table> {
    type Item = (GcRef<'a, Str>, ValueRef<'a>);

    type IntoIter = TableEntries<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries()
    }
}

impl GcRefMut<'_, Table> {
    /// `self[key] = value`
    #[inline(never)]
    pub fn insert(&mut self, key: &GcRoot<'_, Str>, value: ValueRoot<'_>) {
        unsafe { self.insert_raw(key.as_ptr(), value.raw()) }
    }

    /// `self[key] = value`
    ///
    /// Assumes that `key` and `value` are alive.
    ///
    /// ## Safety
    ///
    /// - `key` and `value` must be alive
    #[inline(always)]
    pub unsafe fn insert_raw(&mut self, key: GcPtr<Str>, value: ValueRaw) {
        let this: &mut Table = &mut *self;
        let hash = key.as_ref().hash;
        let entry = this.map.raw_entry_mut().from_hash(hash, |&Opaque(index)| {
            // SAFETY: `index` is guaranteed to be valid
            let (stored_key, _) = unsafe { this.kv.get_unchecked(index as usize) };
            // SAFETY: dereferencing inner Gc'd pointers is safe,
            // because we are rooted (by `RefMut`), and the table
            // is traced.
            let stored_key = unsafe { stored_key.as_ref() };

            stored_key.as_str() == key.as_ref().as_str()
        });

        match entry {
            RawEntryMut::Occupied(entry) => {
                let index = entry.key().0 as usize;
                // SAFETY: `index` is guaranteed to be valid
                let (k, v) = unsafe { this.kv.get_unchecked_mut(index) };
                *k = key;
                *v = value;
            }
            RawEntryMut::Vacant(entry) => {
                let index = this.kv.len() as u32;
                // NOTE: the newly stored key is rooted at least for the duration
                // of this call to `insert`, proven by being passed in as `Ref`
                this.kv.push((key, value));
                entry.insert_with_hasher(hash, Opaque(index), (), |&Opaque(index)| {
                    // SAFETY: `index` is guaranteed to be valid, because we just pushed it into `kv`
                    let (stored_key, _) = unsafe { this.kv.get_unchecked(index as usize) };
                    // SAFETY: we're accessing the key which is still rooted by caller
                    let stored_key = unsafe { stored_key.as_ref() };

                    this.hash.hash_str(stored_key.as_str())
                });
            }
        }
    }
}

pub struct TableEntries<'a> {
    pub(crate) table: GcRef<'a, Table>,
    pub(crate) index: usize,
}

impl<'a> Iterator for TableEntries<'a> {
    type Item = (GcRef<'a, Str>, ValueRef<'a>);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.table.entry(self.index) {
            Some(entry) => {
                self.index += 1;

                Some(entry)
            }

            None => {
                // fused iterator
                self.index = usize::MAX;
                None
            }
        }
    }
}

impl<'a> std::iter::FusedIterator for TableEntries<'a> {}

unsafe impl Trace for Table {
    vtable!(Table);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        for (key, value) in &self.kv {
            tracer.visit(*key);
            tracer.visit_value(*value);
        }
    }
}

impl std::fmt::Debug for GcRef<'_, Table> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut map = f.debug_map();
        for (k, v) in self.entries() {
            map.entry(&k, &v);
        }
        map.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::gc::{Heap, ValueRef, ValueRoot, let_root};

    #[test]
    fn table() {
        let heap = &mut Heap::new();

        {
            let_root!(in heap; table);
            let table = Table::new(heap, table, 128);

            let_root!(in heap; string_a);
            let string_a = Str::new(heap, string_a, "a");
            table.as_mut(heap).insert(&string_a, ValueRoot::Int(10));

            // `table` is safe from collection
            assert_eq!(
                heap.stats().bytes(),
                core::mem::size_of::<Table>() + core::mem::size_of::<Str>()
            );
            heap.collect_no_external_roots();
            assert_eq!(
                heap.stats().bytes(),
                core::mem::size_of::<Table>() + core::mem::size_of::<Str>()
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
            let_root!(in heap; string_b);
            let string_b = Str::new(heap, string_b, "b");
            table.as_mut(heap).insert(&string_b, ValueRoot::Int(20));

            // `table` is safe from collection
            assert_eq!(
                heap.stats().bytes(),
                core::mem::size_of::<Table>() + (core::mem::size_of::<Str>() * 2)
            );
            heap.collect_no_external_roots();
            assert_eq!(
                heap.stats().bytes(),
                core::mem::size_of::<Table>() + (core::mem::size_of::<Str>() * 2)
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
        heap.collect_no_external_roots();
        assert_eq!(heap.stats().bytes(), 0);
        assert_eq!(heap.stats().collections(), 3);
    }
}
