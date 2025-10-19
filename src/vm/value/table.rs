use hashbrown::{HashMap, hash_map::RawEntryMut};

use super::{String, StringHasher, ValueRaw};
use crate::vm::gc::{Gc, Heap, Ref, RefMut, Root, Trace, Tracer, ValueRef, ValueRoot};

pub(crate) struct Opaque(u32);

#[repr(align(16))]
pub struct Table {
    pub(crate) map: HashMap<Opaque, (), ()>,
    pub(crate) kv: Vec<(Gc<String>, ValueRaw)>,
    pub(crate) hash: StringHasher,
}

impl Table {
    #[inline(never)]
    pub fn alloc(heap: &Heap, capacity: usize) -> Gc<Self> {
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self {
                map: HashMap::with_capacity_and_hasher(capacity, ()),
                kv: Vec::with_capacity(capacity),
                hash: StringHasher::default(),
            });
        })
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

    #[inline]
    pub fn entry(&self, index: usize) -> Option<(Ref<'a, String>, ValueRef<'a>)> {
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

impl<'a> IntoIterator for Ref<'a, Table> {
    type Item = (Ref<'a, String>, ValueRef<'a>);

    type IntoIter = TableEntries<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries()
    }
}

impl RefMut<'_, Table> {
    /// `self[key] = value`
    #[inline(never)]
    pub fn insert(&mut self, key: &Root<'_, String>, value: ValueRoot<'_>) {
        unsafe { self.insert_raw(key.as_ptr(), value.raw()) }
    }

    /// `self[key] = value`
    ///
    /// Assumes that `key` and `value` are alive.
    ///
    /// ## Safety
    ///
    /// - `key` and `value` must be alive
    #[inline(never)]
    pub unsafe fn insert_raw(&mut self, key: Gc<String>, value: ValueRaw) {
        let this: &mut Table = &mut *self;
        let key = unsafe { key.as_ref() };

        let hash = this.hash.hash_str(key.as_str());
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
                *v = value;
            }
            RawEntryMut::Vacant(entry) => {
                let index = this.kv.len() as u32;
                // NOTE: the newly stored key is rooted at least for the duration
                // of this call to `insert`, proven by being passed in as `Ref`
                this.kv.push((key.as_ptr(), value));
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
    pub(crate) table: Ref<'a, Table>,
    pub(crate) index: usize,
}

impl<'a> Iterator for TableEntries<'a> {
    type Item = (Ref<'a, String>, ValueRef<'a>);

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
