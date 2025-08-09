//! Garbage collector
//!
//! # Explicit goals
//! - Fully safe API
//! - Reasonably efficient
//! - Easy to use
//!
//! # Non-goals
//! - General-purpose API
//! - Extensible
//! - Fully safe implementation
//!
//! # GC rooting api:
//! - Root<'a, T> => rooted on the stack
//! - Persistent<T> => rooted via a special list
//!   - `Rc<T>`, but GC-managed.
//! - Handle<'a, T>, HandleMut<'a, T> => rooted _somehow_
//!   - Both `Root<'a, T>` and `Persistent<'a, T>` yield handles,
//!     given a shared/exclusive reference to the heap.
//!   - Allows for write barriers to exist, opening up incremental collection
//!     at some point in the future.
//!     - Ideally the write barriers would already exist and be called,
//!       even if they are no-ops, just to prove that it can work.
//! - Gc<T> => internal "raw pointer" type, not exposed in any way,
//!   unsafe to use.
//!   - e.g. `Value` stores this, builtin arrays hold a list of them, etc.
//!   - requires unsafe `Trace` impl to be correct when stored
//!   - can be dereferenced using unsafe code, or through "handle projection" (?)
//!
//! # GC algorithm:
//! - precise, stop-the-world
//! - FUTURE: want incremental, so rooting API must support
//!   write barriers.
//!
//! Tracing begins from roots:
//! - VM state (stack, globals?, modules?, closures?)
//! - Stack roots
//! - Persistent handles
//!
//! # Allocator:
//! - Each on-heap object is a separate `Box::new`.
//! - FUTURE: allocate in per-type free list arenas,
//!           with inline bitmap for metadata (e.g. color bits)
//!           instead of storing per-object in header.
//!
//! # Type hierarchy:
//! - Primitives
//!   - nil, bool, i64, f64
//!   - Stored on stack, no GC involvement
//!   - Does not need heap for access
//! - Built in objects
//!   - Array, Table
//!   - Stored on heap, GC header type tag supports a subset of these directly
//!     - Tracing these is cheap, only a `match obj.tag` + static dispatch
//! - GCBox
//!   - Rust `struct`, wrapped in GC-managed allocation
//!     - Cannot be used to expose anything to script code, only passed around
//!   - These cannot be traced, meaning no `Trace` derive
//!     - Would require `unsafe Trace` to be exposed, don't want that,
//!       even via derive.
//!     - Every type under the sun would have to implement `Trace`, extremely
//!       annoying due to orphan rule.
//!   - Instead, users must store `Persistent` handles if they want to store
//!     objects in their own structs.
//!     - These are considered roots, so are traced without the user's
//!       involvement.
//!     - Big downside is that RC is slow, and these handles aren't super cheap.
//!     - Storing `Vec<Persistent<T>>` is probably worst case, user should store
//!       `Persistent<Array>` instead, and eat the dynamic dispatch cost.
//!   - Uses `Drop` for "finalization"
//!     - Because dereferencing handles requires heap access, users cannot
//!       dereference them in `drop` impls, so `Drop` types are safe to
//!       wrap in `UserData`
//!   - Carve out separate sweep paths for user data which does not need `drop`
//! - UTable
//!   - `GCBox` with exposed getters, setters, methods (?)
//!
//! NOTE: I didn't realise just how much simpler the GC implementation becomes
//! when there is no user-defined `trace`, and no way to store "direct handles"
//! to GC objects in user-defined structs. Wow!

use std::{cell::UnsafeCell, marker::PhantomData, pin::Pin};

// TODO:
// - Start with a `safe-gc` like API
// - Introduce unchecked getters
// - Use stack roots instead of RootSet by default
// - Ability to promote objects to "permanent"
//   - Similar to `safe_gc::Root`
// - Ability to traverse external collections of pointers
//   - Important for VM stack
//   - NOTE: call frames should probably keep closures alive!

// for tracing from heterogenous list, need some kind of type tag.
//
// if we guarantee that user-defined types cannot store objects without
// persistent handle indirection, then we don't need to trace arbitrary
// user data, and can have a bounded set of possible types which need to
// be traced for interior pointers.
pub(crate) struct HeapData {
    _marker: PhantomData<()>,
}

impl HeapData {
    #[inline]
    pub(crate) fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

#[repr(C)]
pub struct Heap {
    data: *mut HeapData,
}

impl Heap {
    #[inline]
    pub(crate) fn new(data: *mut HeapData) -> Self {
        Self { data }
    }
}

#[repr(C)]
pub struct Gc<T: Sized + 'static> {
    ptr: *mut Thing<T>,
}

impl<T: Sized + 'static> Gc<T> {
    /// Directly wrap a pointer.
    ///
    /// ## Safety
    /// - The pointer must point to a heap-allocated _object_.
    /// - The pointer must have been produced by `Gc::into_raw`.
    #[inline]
    pub(crate) unsafe fn from_raw(ptr: *mut T) -> Gc<T> {
        let offset = core::mem::offset_of!(Thing<T>, thing);
        let ptr = ptr.cast::<u8>().sub(offset).cast::<Thing<T>>();

        Gc { ptr }
    }

    /// Unwrap a managed pointer.
    ///
    /// This operation is always safe, but using the resulting
    /// pointer has the same requirements as [`Gc::get`] or [`Gc::get_mut`].
    #[inline]
    pub(crate) fn into_raw(this: Gc<T>) -> *mut T {
        unsafe { UnsafeCell::raw_get(&raw const (*this.ptr).thing) }
    }

    /// Get a shared reference to the object.
    ///
    /// ## Safety
    /// - The object must have been allocated by the given `heap`.
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    #[inline]
    pub(crate) unsafe fn get(self, heap: &Heap) -> &T {
        _ = heap;

        let ptr = unsafe { &raw const (*self.ptr).thing };
        let ptr = UnsafeCell::raw_get(ptr);

        unsafe { &*ptr }
    }

    /// Get an exclusive reference to the object.
    ///
    /// ## Safety
    /// - The object must have been allocated by the given `heap`.
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    #[inline]
    pub(crate) unsafe fn get_mut(self, heap: &mut Heap) -> &mut T {
        _ = heap;

        let ptr = unsafe { &raw const (*self.ptr).thing };
        let ptr = UnsafeCell::raw_get(ptr);

        unsafe { &mut *ptr }
    }
}

/// Stores pointers used to traverse stack roots
struct RootBase {
    list: *mut *mut RootBase,
    prev: *mut RootBase,
}

/// Internal API
///
/// Value produced by the [`root`] macro, only transitively
/// reachable through a [`Root`]. The place itself becomes
/// pinned, and is appended to the list of stack roots.
#[doc(hidden)]
#[repr(C)]
pub struct StackRoot<T: Sized + 'static> {
    base: RootBase,
    ptr: Gc<T>,
}

impl<T: Sized + 'static> StackRoot<T> {
    /// Internal API
    #[doc(hidden)]
    #[inline]
    pub unsafe fn new(list: *mut *mut RootBase, ptr: Gc<T>) -> Self {
        Self { base: todo!(), ptr }
    }
}

#[repr(C)]
pub struct Root<'a, T: Sized + 'static> {
    place: Pin<&'a mut StackRoot<T>>,
}

impl<'a, T: Sized + 'static> Root<'a, T> {
    /// Internal API
    #[doc(hidden)]
    #[inline]
    pub unsafe fn new(place: Pin<&'a mut StackRoot<T>>) -> Self {
        Self { place }
    }
}

#[repr(C)]
struct GcHeader {
    marked: bool,
    next: *mut GcHeader,
}

#[repr(C)]
struct Thing<T: Sized + 'static> {
    header: GcHeader,
    thing: UnsafeCell<T>,
}

impl<T: 'static> Gc<T> {}

impl<T: 'static> Clone for Gc<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: 'static> Copy for Gc<T> {}

/// The GC must know how to traverse types in order to manage them.
///
/// ## Safety
///
/// All interior pointers must be marked.
pub unsafe trait Trace: Sized + 'static {
    /// ## Safety
    ///
    /// All interior pointers must be marked.
    unsafe fn trace(this: Gc<Self>, tracer: &dyn Tracer);
}

/// A type which knows how to mark GC-managed objects.
pub trait Tracer: private::Sealed {
    fn mark(&self, ptr: Gc<Any>);
}

#[repr(transparent)]
pub struct Any {
    _marker: PhantomData<()>,
}

mod private {
    pub trait Sealed {}
}

#[doc(hidden)]
pub trait __MustNotImplTrace<T> {
    fn f() {}
}

impl<T: Sized> __MustNotImplTrace<()> for T {
    fn f() {}
}

#[allow(dead_code)]
struct __Invalid;

impl<T: Sized + Trace> __MustNotImplTrace<__Invalid> for T {
    fn f() {}
}

#[macro_export]
#[doc(hidden)]
macro_rules! __must_not_impl_trace {
    ($T:ty) => {
        const _: () = {
            let _ = <$T as $crate::__macro::__MustNotImplTrace<_>>::f;
        };
    };
}

#[doc(hidden)]
pub const fn must_impl_trace<T: Trace>() {}

#[macro_export]
#[doc(hidden)]
macro_rules! __must_impl_trace {
    ($T:ty) => {
        const _: () = {
            let _ = $crate::__macro::must_impl_trace::<$T>;
        };
    };
}

struct Test {}
