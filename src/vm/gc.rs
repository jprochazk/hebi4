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
//!   - List, Table
//!   - Stored on heap, GC header type tag supports a subset of these directly
//!     - Tracing these is cheap, only a `match obj.tag` + static dispatch
//! - UData
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
//!       `Persistent<List>` instead, and eat the dynamic dispatch cost.
//!   - Uses `Drop` for "finalization"
//!     - Because dereferencing handles requires heap access, users cannot
//!       dereference them in `drop` impls, so `Drop` types are safe to
//!       wrap in `UserData`
//!   - Carve out separate sweep paths for user data which does not need `drop`
//!

// TODO: root projection
// TODO: re-rooting
// TODO: storing in objects

use std::{
    cell::{Cell, UnsafeCell},
    marker::{PhantomData, PhantomPinned},
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::null_mut,
};

use super::value::Value;

// # Heap

#[repr(C)]
pub struct Heap {
    /// boxed because root lists must have stable addresses
    roots: UnsafeCell<Box<RootList>>,

    /// objects are linked into a global list of all
    /// allocations, which is used during sweeping
    head: Cell<*mut GcHeader>,

    #[cfg(debug_assertions)]
    heap_id: HeapId,
}

impl Heap {
    #[inline]
    pub(crate) fn new() -> Self {
        Self {
            roots: UnsafeCell::new(Box::new(RootList::default())),
            head: Cell::new(null_mut()),

            #[cfg(debug_assertions)]
            heap_id: HeapId::new(),
        }
    }

    #[inline]
    pub(crate) fn alloc_no_gc<T: Rootable + Trace + 'static>(&self, value: T) -> Gc<T> {
        let ptr = Box::into_raw(Box::new(GcBox {
            header: GcHeader::new(T::TYPE, self.head.get()),
            value: UnsafeCell::new(value),
        }));

        // CAST: `GcBox` is a `repr(C)` struct with `GcHeader` as its first field
        self.head.set(ptr.cast());

        Gc { ptr }
    }

    #[inline]
    pub(crate) fn collect(&mut self) {
        mark_and_sweep::collect(self);
    }

    #[inline]
    fn roots(&self) -> *mut RootList {
        unsafe { &raw mut **self.roots.get() }
    }

    #[cfg(debug_assertions)]
    #[doc(hidden)]
    #[inline]
    pub fn id(&self) -> HeapId {
        self.heap_id
    }
}

mod mark_and_sweep;

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

// # Managed pointers

#[repr(C)]
pub struct Gc<T: Sized + 'static> {
    ptr: *mut GcBox<T>,
}

impl<T: Sized + 'static> Gc<T> {
    /// Wrap a previously managed pointer.
    ///
    /// ## Safety
    /// - The pointer must have been produced by `Gc::into_raw`.
    #[inline]
    pub(crate) unsafe fn from_raw(ptr: *mut T) -> Gc<T> {
        let offset = core::mem::offset_of!(GcBox<T>, value);
        // CAST: `GcBox` begins with
        let ptr = ptr.cast::<u8>().sub(offset).cast::<GcBox<T>>();

        Gc { ptr }
    }

    /// Unwrap a managed pointer, returning a raw pointer to the underlying object.
    ///
    /// This operation is always safe, but using the resulting
    /// pointer has the same requirements as [`Gc::get`] or [`Gc::get_mut`].
    #[inline]
    pub(crate) fn into_raw(this: Gc<T>) -> *mut T {
        unsafe { UnsafeCell::raw_get(&raw const (*this.ptr).value) }
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
    pub(crate) unsafe fn as_ref(self, heap: &Heap) -> &T {
        _ = heap;

        let ptr = unsafe { &raw const (*self.ptr).value };
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
    pub(crate) unsafe fn as_ref_mut(self, heap: &mut Heap) -> &mut T {
        _ = heap;

        let ptr = unsafe { &raw const (*self.ptr).value };
        let ptr = UnsafeCell::raw_get(ptr);

        unsafe { &mut *ptr }
    }

    #[inline]
    fn cast<U: Sized + 'static>(self) -> Gc<U> {
        Gc {
            ptr: self.ptr.cast(),
        }
    }

    #[inline]
    unsafe fn get_raw(self) -> *mut T {
        let ptr = unsafe { &raw mut (*self.ptr).value };
        UnsafeCell::raw_get(ptr)
    }

    #[inline]
    unsafe fn type_(self) -> ObjectType {
        let header = &raw mut (*self.ptr).header;
        header.read().type_()
    }

    #[inline]
    unsafe fn mark(self) {
        let header = &raw mut (*self.ptr).header;
        GcHeader::set_mark(header, true);
    }

    #[inline]
    unsafe fn is_marked(self) -> bool {
        let header = &raw mut (*self.ptr).header;
        header.read().marked()
    }
}

impl<T: Trace + Sized + 'static> Gc<T> {
    #[inline]
    unsafe fn trace(self, tracer: &Tracer) {
        (*self.get_raw()).trace(tracer)
    }
}

#[derive(Clone, Copy)]
#[repr(C, align(8))]
struct GcHeader {
    tagged: *mut GcHeader,
}

impl GcHeader {
    #[inline]
    fn new(tt: ObjectType, next: *mut GcHeader) -> Self {
        let marked = false as usize;
        let tt = tt as usize;

        let tag = marked | tt;

        debug_assert!(tag & 0b001 == marked);
        debug_assert!(tag & 0b110 == tt);

        Self {
            tagged: next.wrapping_add(tag),
        }
    }

    #[inline]
    fn into_parts(self) -> (*mut GcHeader, ObjectType, bool) {
        (self.next(), self.type_(), self.marked())
    }

    #[inline]
    fn marked(self) -> bool {
        self.tagged.addr() & 1 == 1
    }

    #[inline]
    fn type_(self) -> ObjectType {
        let raw = (self.tagged.addr() & 0b110) as u8;
        unsafe { core::mem::transmute(raw) }
    }

    #[inline]
    fn next(self) -> *mut GcHeader {
        let tag = self.tagged.addr() & 0b111;
        self.tagged.map_addr(|addr| addr.wrapping_sub(tag))
    }

    #[inline]
    unsafe fn set_mark(this: *mut GcHeader, v: bool) {
        let v = v as usize;
        (*this).tagged = (*this).tagged.map_addr(|addr| (addr & !0b1) | v);
    }

    #[inline]
    unsafe fn set_next(this: *mut GcHeader, next: *mut GcHeader) {
        let tag = (*this).tagged.addr() & 0b111;
        (*this).tagged = next.map_addr(|addr| (addr & !0b111) | tag);
    }
}

const _: () = {
    assert!(
        std::mem::size_of::<*mut ()>() == 8 && std::mem::align_of::<*mut ()>() == 8,
        "only 64-bit platforms are supported"
    );
};

#[repr(C, align(8))]
struct GcBox<T: Sized + 'static> {
    header: GcHeader,
    value: UnsafeCell<T>,
}

impl<T: 'static> Gc<T> {}

impl<T: 'static> Clone for Gc<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: 'static> Copy for Gc<T> {}

// # Tracing

/// The GC must know how to traverse types in order to manage them.
///
/// ## Safety
///
/// All interior pointers must be marked.
pub(crate) unsafe trait Trace: Sized + 'static {
    /// ## Safety
    ///
    /// All interior pointers must be marked.
    unsafe fn trace(&self, tracer: &Tracer);
}

/// A type which knows how to mark GC-managed objects.
pub(crate) struct Tracer {
    _marker: PhantomData<()>,
}

impl Tracer {
    #[inline]
    pub(crate) fn visit<T: Rootable + Trace>(&self, ptr: Gc<T>) {
        unsafe {
            if ptr.is_marked() {
                // cycle
                return;
            }
            ptr.mark();
            (*ptr.get_raw()).trace(self)
        }
    }

    #[inline]
    pub(crate) fn visit_value(&self, value: Value) {
        match value {
            Value::List(ptr) => self.visit(ptr),
            Value::Table(ptr) => self.visit(ptr),
            Value::UData(ptr) => self.visit(ptr),
            Value::Nil | Value::Bool(_) | Value::Int(_) | Value::Float(_) => {}
        }
    }
}

// ## Roots
//
// The rooting API uses a stack-allocated type-erased linked list.
// To trace through the roots, we iterate over the linked list,
// matching on each member's type tag.
//
// If we guarantee that user-defined types cannot store objects without
// persistent handle indirection, then we don't need to trace arbitrary
// user data, and can have a bounded set of possible types which need to
// be traced for interior pointers.

#[repr(u8)]
pub enum ObjectType {
    List = 0b000,
    Table = 0b010,
    Closure = 0b100,
    UData = 0b110,
}

/// Internal API
///
/// Represents a rootable thing. Used to map a type
/// to its root kind.
#[doc(hidden)]
pub(crate) trait Rootable: Sized + 'static {
    const TYPE: ObjectType;
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

    #[inline]
    unsafe fn iter<F>(this: *mut RootList, mut tracer: F)
    where
        F: FnMut(*mut RootBase),
    {
        let mut curr = (*this).head;
        while !curr.is_null() {
            tracer(curr);
            curr = (*curr).prev;
        }
    }
}

impl Default for RootList {
    fn default() -> Self {
        Self { head: null_mut() }
    }
}

/// Internal API
///
/// A root list node, holds a direct pointer to a GC-managed object.
///
/// Not useful on its own, needs to be pinned and wrapped in a `Root`.
#[doc(hidden)]
#[repr(C)]
pub struct StackRoot<T: Sized + 'static> {
    base: RootBase,
    ptr: Gc<T>,
    pinned: PhantomPinned,
}

impl<T: Rootable> StackRoot<T> {
    /// Internal API
    ///
    /// ## SAFETY:
    /// - `ptr` must still be live
    /// - `ptr` must have been allocated via `heap`
    #[doc(hidden)]
    #[inline]
    pub unsafe fn from_heap_ptr(heap: &Heap, ptr: Gc<T>) -> Self {
        let base = RootList::base(heap.roots());

        Self {
            base,
            ptr,
            pinned: PhantomPinned,
        }
    }
}

impl<T> StackRoot<T> {
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

impl<T> Drop for StackRoot<T> {
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
    /// Accessing this field is undefined behavior.
    ///
    /// Use the [`Root::get`] and [`Root::get_mut`] APIs to access
    /// the rooted object.

    // NOTE: There's not much a user can actually do with this field
    // without using unsafe code, so accessing it is safe, which is why
    // it can be exposed.
    #[doc(hidden)]
    pub __place: Pin<&'a mut StackRoot<T>>,

    /// Internal API.
    ///
    /// Accessing this field is undefined behavior.
    ///
    /// Only used for debug assertions.
    #[doc(hidden)]
    #[cfg(debug_assertions)]
    pub __heap_id: HeapId,
}

impl<'a, T: Rootable> Root<'a, T> {
    /// Dereference the rooted pointer. Grants shared access to the object.
    #[inline]
    pub fn as_ref<'v>(&self, heap: &'v Heap) -> Ref<'v, T> {
        debug_assert!(heap.id() == self.__heap_id);

        // SAFETY:
        // - The object is guaranteed to live for the duration of this borrow:
        //   - No collection may happen while the object is borrowed,
        //     because `heap.collect` requires unique access to the `heap`
        // - No mutable reference may exist at the same time as a shared reference
        //   - Shared access is gated by shared access to the `heap`
        //   - Only other shared references to `T` exist while the borrow is active
        Ref(unsafe { self.__place.ptr.as_ref(heap) })
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
    pub fn as_ref_mut<'v>(&self, heap: &'v mut Heap) -> RefMut<'v, T> {
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
        RefMut(unsafe { self.__place.ptr.as_ref_mut(heap) })
    }
}

/// A shared reference to a GC-managed `T`.
///
/// This type is used to certify that the reference comes from a live root,
/// which makes it safe to move around, re-root, and store as an object member.
///
/// Implements `DerefMut` so can be turned into a plain reference as needed.
#[derive(Clone, Copy)]
pub struct Ref<'a, T>(&'a T);

impl<'a, T: Rootable> Deref for Ref<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// A unique reference to a GC-managed `T`.
///
/// This type is used to certify that the reference comes from a live root,
/// which makes it safe to move around, re-root, and store as an object member.
///
/// Implements `Deref`/`DerefMut` so can be turned into a plain reference as needed.
pub struct RefMut<'a, T>(&'a mut T);

impl<'a, T: Rootable> Deref for RefMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, T: Rootable> DerefMut for RefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
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
    [in $heap:ident; $ptr:expr] => {
        $crate::__macro::Root {
            // the `pin!` macro hoists values into the enclosing scope (via `super let`)
            // it can't be moved, because:
            // - the original value is unreachable due to macro hygiene
            // - getting a mutable reference to the stack root requires unsafe code,
            //   because `StackRoot: !Unpin`.
            __place: std::pin::pin!(
                $crate::__macro::StackRoot::from_heap_ptr($heap, $ptr)
            ),

            #[cfg(debug_assertions)]
            __heap_id: ($heap).id(),
        }
    };
}
