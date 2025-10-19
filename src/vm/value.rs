#[macro_use]
mod macros;

use crate::vm::gc::Gc;

// TODO: string allocation strategy
//
// Pass a handle to the GC into the compiler, so it can intern GC'd strings.
// That _does_ mean you can't arbitrary move the module around, across threads,
// etc., and to move it as such you'd have to serialize it first.
//
// Alternatively, canonicalize at "module load" time, as in have a separate
// step where the module is given to a VM instance, which then moves interned
// strings onto the GC heap, resulting in a "loaded" or "runtime" module.
// That's then what's actually passed around in all the places.
// Note that functions have back-pointers to their modules, which complicates
// things.
//
// Tradeoffs...

#[derive(Default, Clone, Copy)]
#[repr(C, u64)]
pub enum ValueRaw {
    #[default]
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,

    String(Gc<string::String>),
    List(Gc<list::List>),
    Table(Gc<table::Table>),
    UserData(Gc<userdata::UserData>),
}

impl ValueRaw {
    /// Coerce `self` to a boolean:
    ///
    /// - `nil` is always `false`
    /// - bools are returned as-is
    /// - `int` is false if `== 0`
    /// - `float` is false if `== 0.0`
    /// - objects are always `true`
    #[inline]
    pub(crate) fn coerce_bool(self) -> bool {
        match self {
            ValueRaw::Nil => false,
            ValueRaw::Bool(v) => v,
            ValueRaw::Int(v) => v != 0,
            ValueRaw::Float(v) => v != 0.0,
            ValueRaw::String(gc) => true,
            ValueRaw::List(gc) => true,
            ValueRaw::Table(gc) => true,
            ValueRaw::UserData(gc) => true,
        }
    }

    #[inline]
    pub(crate) fn type_name(self) -> &'static str {
        match self {
            ValueRaw::Nil => "nil",
            ValueRaw::Bool(_) => "bool",
            ValueRaw::Int(_) => "int",
            ValueRaw::Float(_) => "float",
            ValueRaw::String(gc) => "str",
            ValueRaw::List(gc) => "list",
            ValueRaw::Table(gc) => "table",
            ValueRaw::UserData(gc) => "udata",
        }
    }
}

pub mod closure;
pub mod function;
pub mod list;
pub mod module;
pub mod string;
pub mod table;
pub mod userdata;

pub(crate) use self::string::StringHasher;

#[allow(unused_imports)] // re-exports
pub use self::{
    closure::Closure,
    function::FunctionProto,
    list::{List, ListIter},
    module::ModuleProto,
    string::String,
    table::{Table, TableEntries},
    userdata::UserData,
};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::gc::{Heap, ValueRef, ValueRoot};

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

    #[test]
    fn heap_collect_on_drop() {
        // `heap` frees all managed objects on drop
        let heap = &mut Heap::new();
        list!(in heap; v = 0);
    }
}
