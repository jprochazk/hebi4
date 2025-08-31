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
//! - UTable
//!   - Rust `struct`, wrapped in GC-managed allocation
//!   - May expose getters, setters, methods (?)
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
//!
//! NOTE: I didn't realise just how much simpler the GC implementation becomes
//! when there is no user-defined `trace`, and no way to store "direct handles"
//! to GC objects in user-defined structs. Wow!

use std::{
    cell::UnsafeCell,
    marker::{PhantomData, PhantomPinned},
    pin::Pin,
};

#[repr(C)]
pub(crate) struct HeapData {
    roots: UnsafeCell<RootKindArray<RootList>>,
    _marker: PhantomData<()>,
}

impl HeapData {
    #[inline]
    pub(crate) fn new() -> Self {
        Self {
            roots: UnsafeCell::new(RootKindArray::default()),
            _marker: PhantomData,
        }
    }

    #[inline]
    fn raw_roots(this: *mut HeapData) -> *mut RootKindArray<RootList> {
        unsafe {
            let cell = &raw mut (*this).roots;
            UnsafeCell::raw_get(cell)
        }
    }
}

#[repr(C)]
pub struct Heap {
    data: *mut HeapData,

    #[cfg(debug_assertions)]
    heap_id: HeapId,
}

impl Heap {
    #[inline]
    pub(crate) fn new(data: *mut HeapData) -> Self {
        Self {
            data,

            #[cfg(debug_assertions)]
            heap_id: HeapId::new(),
        }
    }

    #[cfg(debug_assertions)]
    #[doc(hidden)]
    #[inline]
    pub fn id(&self) -> HeapId {
        self.heap_id
    }
}

#[cfg(debug_assertions)]
#[doc(hidden)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct HeapId(usize);

#[cfg(debug_assertions)]
impl HeapId {
    #[doc(hidden)]
    pub fn new() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};

        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

        Self(NEXT_ID.fetch_add(1, Ordering::Relaxed))
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

#[repr(transparent)]
struct GcHeader {
    tagged: *mut GcHeader,
}

impl GcHeader {
    #[inline]
    fn new(kind: RootKind, next: *mut GcHeader) -> Self {
        let marked = false as usize;
        let kind = kind as usize;

        let tag = marked | kind;

        debug_assert!(tag & 0b001 == marked);
        debug_assert!(tag & 0b110 == kind);

        Self {
            tagged: next.wrapping_add(tag),
        }
    }

    #[inline]
    fn marked(self) -> bool {
        self.tagged.addr() & 1 == 1
    }

    #[inline]
    fn mark(self, v: bool) -> Self {
        GcHeader {
            tagged: self
                .tagged
                .map_addr(|addr| (addr & !0b1).wrapping_add(v as usize)),
        }
    }

    #[inline]
    fn kind(self) -> RootKind {
        let raw = (self.tagged.addr() & 0b110) as u8;
        unsafe { core::mem::transmute(raw) }
    }

    #[inline]
    fn next(self) -> *mut GcHeader {
        let tag = self.tagged.addr() & 0b111;
        self.tagged.map_addr(|addr| addr.wrapping_sub(tag))
    }
}

const _: () = {
    assert!(
        std::mem::size_of::<*mut ()>() == 8 && std::mem::align_of::<*mut ()>() == 8,
        "only 64-bit platforms are supported"
    );
};

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
    unsafe fn trace(this: Gc<Self>, tracer: &Tracer);
}

/// A type which knows how to mark GC-managed objects.
pub struct Tracer {}

impl Tracer {
    fn mark(&self, ptr: Gc<Any>) {}
}

#[repr(transparent)]
pub struct Any {
    _marker: PhantomData<()>,
}

mod private {
    pub trait Sealed {}
}

// ## Roots
//
// The rooting API uses a stack-allocated linked list,
// segregated by type.
//
// For all intents and purposes, the root kind is the
// same as the rooted object's type tag.
//
// If we guarantee that user-defined types cannot store objects without
// persistent handle indirection, then we don't need to trace arbitrary
// user data, and can have a bounded set of possible types which need to
// be traced for interior pointers.

/// Declare an enum for each possible kind of root,
/// and an array type which uses the enum as its index.
///
/// The array is always fully initialized, removing the need
/// for any bounds checking.
macro_rules! root_kind {
    (
        #[array($array:ident)]
        #[$($attr:meta),*]
        $vis:vis enum $name:ident {
            $($variant:ident = $v:expr),*
            $(,)?
        }
    ) => {
        #[$($attr),*]
        $vis enum $name {
            $($variant = $v),*
        }

        impl $name {
            pub const LENGTH: usize = root_kind!(@count $($variant)*);
        }

        #[repr(transparent)]
        $vis struct $array<T: Sized>([T; $name::LENGTH]);

        impl<T: Sized> $array<T> {
            #[inline]
            pub fn new(values: [T; $name::LENGTH]) -> Self {
                Self(values)
            }

            #[inline]
            pub fn get(&self, index: $name) -> &T {
                unsafe {
                    self.0.get_unchecked(index as usize)
                }
            }

            #[inline]
            pub fn get_mut(&mut self, index: $name) -> &mut T {
                unsafe {
                    self.0.get_unchecked_mut(index as usize)
                }
            }

            #[inline]
            pub fn raw_get(this: *const $array<T>, index: $name) -> *const T {
                unsafe {
                    let inner = &raw const (*this).0;
                    inner.cast::<T>().add(index as usize)
                }
            }

            #[inline]
            pub fn raw_get_mut(this: *mut $array<T>, index: $name) -> *mut T {
                unsafe {
                    let inner = &raw mut (*this).0;
                    inner.cast::<T>().add(index as usize)
                }
            }
        }

        impl<T: Sized + Default> $array<T> {
            #[inline]
            fn default() -> Self {
                Self::new(core::array::from_fn(|_| T::default()))
            }
        }

        impl<T: Sized + Default> core::ops::Index<$name> for $array<T> {
            type Output = T;

            #[inline]
            fn index(&self, index: $name) -> &T {
                self.get(index)
            }
        }

        impl<T: Sized + Default> core::ops::IndexMut<$name> for $array<T> {
            #[inline]
            fn index_mut(&mut self, index: $name) -> &mut T {
                self.get_mut(index)
            }
        }
    };

    (@count) => {0};
    (@count $ident:ident $($tail:ident)*) => {
        1 + root_kind!(@count $($tail)*)
    };
}

root_kind! {
    #[array(RootKindArray)]
    #[repr(u8)]
    pub enum RootKind {
        Array = 0b000,
        Table = 0b010,
        UTable = 0b100,
    }
}

/// Internal API
///
/// Represents a rootable thing. Used to map a type
/// to its root kind.
#[doc(hidden)]
pub trait Rootable: private::Sealed + Sized + 'static {
    const KIND: RootKind;
}

/// Stores pointers used to traverse stack roots.
///
/// Each node stores a pointer to the head of the list,
/// and a pointer to the previous node.
#[repr(C)]
struct RootBase {
    list: *mut RootList,
    prev: *mut RootBase,
}

/// A linked list of stack roots.
///
/// Treated as a stack. Roots are pushed to the front,
/// and popped from the front, in LIFO order.
#[repr(C)]
struct RootList {
    head: *mut RootBase,
}

impl RootList {
    #[inline]
    fn head(this: *mut RootList) -> *mut *mut RootBase {
        unsafe { &raw mut (*this).head }
    }

    #[inline]
    fn base(this: *mut RootList) -> RootBase {
        unsafe {
            RootBase {
                list: this,
                prev: (*this).head,
            }
        }
    }
}

impl Default for RootList {
    fn default() -> Self {
        Self {
            head: core::ptr::null_mut(),
        }
    }
}

/// Internal API
///
/// A root list node, holds a direct pointer to a GC-managed object.
///
/// Not useful on its own, needs to be pinned and wrapped in a `Root`.
#[doc(hidden)]
#[repr(C)]
pub struct StackRoot<T: Rootable> {
    base: RootBase,
    ptr: Gc<T>,
    pinned: PhantomPinned,
}

impl<T: Rootable> StackRoot<T> {
    /// Internal API
    ///
    /// SAFETY:
    /// - `ptr` must still be live, but not necessarily reachable
    /// - `ptr` must have been allocated via `heap`
    #[doc(hidden)]
    #[inline]
    pub unsafe fn from_heap_ptr(heap: &Heap, ptr: Gc<T>) -> Self {
        let roots = HeapData::raw_roots(heap.data);
        let list = RootKindArray::raw_get_mut(roots, T::KIND);
        let base = RootList::base(list);

        Self {
            base,
            ptr,
            pinned: PhantomPinned,
        }
    }

    /// Cast `self` to a `RootBase`.
    #[inline]
    fn root_base(self: Pin<&mut Self>) -> *mut RootBase {
        unsafe {
            // SAFETY: we do not move out of `self`
            &raw mut self.get_unchecked_mut().base
        }
    }

    /// Append a pinned stack root to its root list.
    ///
    /// SAFETY:
    /// - `self` must remain pinned.
    /// - no other stack root can be appended to any root list
    ///   between the construction of `self` and calling this
    ///   function.
    #[inline]
    unsafe fn _append_to_root_list(self: Pin<&mut Self>) {
        let head = RootList::head(self.base.list);

        // SAFETY: we are pinned, so it's safe to append ourselves
        // to the root list.
        unsafe {
            *head = self.root_base();
        }
    }

    /// Remove a pinned stack root from its root list.
    ///
    /// SAFETY: `self` must be head of the root list.
    #[inline]
    unsafe fn _remove_from_root_list(self: Pin<&mut Self>) {
        let head = RootList::head(self.base.list);
        let prev = self.base.prev;
        let this = self.root_base();

        unsafe {
            debug_assert!(*head == this, "roots removed out of order");

            *head = prev;
        }
    }
}

impl<T: Rootable> Drop for StackRoot<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            // SAFETY:
            // - roots are dropped in LIFO order.
            // - `self` will not be moved after a call to `drop`
            Pin::new_unchecked(self)._remove_from_root_list();
        }
    }
}

#[repr(C)]
pub struct Root<'a, T: Rootable> {
    /// Internal API.
    ///
    /// Accessing this field is undefined behavior,
    /// use [`Root::get`] and [`Root::get_mut`] instead.
    #[doc(hidden)]
    pub __place: Pin<&'a mut StackRoot<T>>,

    /// Internal API.
    ///
    /// Accessing this field is undefined behavior.
    #[doc(hidden)]
    #[cfg(debug_assertions)]
    pub __heap_id: HeapId,
}

impl<'a, T: Rootable> Root<'a, T> {
    /// Dereference the rooted pointer. Grants shared access to the object.
    #[inline]
    pub fn get<'v>(&self, heap: &'v Heap) -> &'v T {
        debug_assert!(heap.id() == self.__heap_id);

        // SAFETY:
        // - The object is guaranteed to live for the duration of this borrow:
        //   - No collection may happen while the object is borrowed,
        //     because `heap.collect` requires unique access to the `heap`
        // - No mutable reference may exist at the same time as a shared reference
        //   - Shared access is gated by shared access to the `heap`
        //   - Only other shared references to `T` exist while the borrow is active
        unsafe { self.__place.ptr.get(heap) }
    }

    // TODO: instead of returning `&mut T`, return `Write<'a, T>`, which:
    // - Doesn't allow moving out of `T` (with mem::replace and similar)
    // - Automatically triggers write barriers on write access
    //
    // Object never exist on the stack (other than for the moment when they're constructed),
    // and rootable objects don't implement `Clone` or `Copy`, so it's impossible to move
    // out of a mutable reference to a rootable `T`. Or at least that _should_ be the case,
    // but we'd rather be 100% sure by gating mutable access behind a smart pointer of
    // some kind. Being able to trigger write barriers that way is a nice side-effect.

    /// Dereference the rooted pointer. Grants unique mutable access to the object.
    #[inline]
    pub fn get_mut<'v>(&self, heap: &'v mut Heap) -> &'v mut T {
        debug_assert!(heap.id() == self.__heap_id);

        // SAFETY:
        // - The object is guaranteed to live for the duration of this borrow:
        //   - No collection may happen while the object is borrowed,
        //     because `heap.collect` requires unique access to the `heap`
        // - The returned mutable reference is unique for the given object:
        //   - Unique access is gated by unique access to the `heap`
        //   - Only one heap may be active on a given thread at once
        //   - Heaps, roots, and GC'd pointers are not `Send` or `Sync`
        //   - No other references exist to `T` while the borrow is active
        unsafe { self.__place.ptr.get_mut(heap) }
    }
}

/// Create a root on the stack.
///
/// Rooting a reclaimed object is undefined behavior.
/// Inversely, _not_ rooting an allocated object is undefined behavior.
///
/// To use this correctly, you must only allocate using `alloc_no_gc`,
/// and _immediately_ root the resulting object.
///
/// ## Safety
///
/// - The object pointed to by `$ptr` must still be live at the time of initialization.
#[macro_export]
macro_rules! root_unchecked {
    [in $heap:ident; $ptr:ident] => {
        $crate::__macro::Root {
            // the `pin!` macro hoists values into the enclosing scope (via `super let`)
            // before pinning them, so this stack root is guaranteed to be dropped
            __place: std::pin::pin!(
                $crate::__macro::StackRoot::from_heap_ptr($heap, $ptr)
            ),

            #[cfg(debug_assertions)]
            __heap_id: ($heap).id(),
        }
    };
}

// TODO: root projection

impl Rootable for () {
    const KIND: RootKind = RootKind::Array;
}

impl private::Sealed for () {}

fn test(heap: &mut Heap, ptr: Gc<()>) {
    let v0 = unsafe { root_unchecked![in heap; ptr] };
    let v1 = unsafe { root_unchecked![in heap; ptr] };

    let r0_1 = v0.get(heap);
    let r0_2 = v0.get(heap);
    println!("{:?}", *r0_1);
}
