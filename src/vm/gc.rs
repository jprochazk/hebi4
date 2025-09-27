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
//! - `Root<'a, T>` => rooted on the stack
//! - `Persistent<T>` => rooted via a special list
//!   - `Rc<T>`, but GC-managed.
//! - `Handle<'a, T>`, `HandleMut<'a, T>` => rooted _somehow_
//!   - Both `Root<'a, T>` and `Persistent<'a, T>` yield handles,
//!     given a shared/exclusive reference to the heap.
//!   - Allows for write barriers to exist, opening up incremental collection
//!     at some point in the future.
//!     - Ideally the write barriers would already exist and be called,
//!       even if they are no-ops, just to prove that it can work.
//! - `Gc<T>` => internal "raw pointer" type, not exposed in any way,
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
//!   - String, List, Table, Closure
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
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::null_mut,
};

use super::value::ValueRaw;

// # Heap

#[repr(C)]
pub struct Heap {
    /// boxed because root lists must have stable addresses
    roots: UnsafeCell<Box<RootList>>,

    /// objects are linked into a global list of all
    /// allocations, which is used during sweeping
    head: Cell<*mut GcHeader>,

    stats: HeapStats,

    #[cfg(debug_assertions)]
    heap_id: HeapId,
}

impl Heap {
    #[inline]
    pub(crate) fn new() -> Self {
        Self {
            roots: UnsafeCell::new(Box::new(RootList::default())),
            head: Cell::new(null_mut()),
            stats: HeapStats::default(),

            #[cfg(debug_assertions)]
            heap_id: HeapId::new(),
        }
    }

    // TODO: when to call `collect`?

    // NOTE: `&self` because it does not trigger a collection,
    // so it does not need to invalidate any shared references.
    #[inline]
    pub(crate) fn alloc_no_gc<T: Trace + 'static>(
        &self,
        init: impl FnOnce(*mut MaybeUninit<T>),
    ) -> Gc<T> {
        // We do manual piece-wise init, because allocation failure should _not_
        // cause the value to be dropped! Therefore it must be constructed _after_
        // we have a place to put it.
        let ptr = Box::into_raw(Box::<GcBox<T>>::new_uninit());

        unsafe {
            let header = ptr.cast::<MaybeUninit<GcHeader>>();
            (*header).write(GcHeader::new(T::KIND, self.head.get()));

            let value_ptr = ptr
                .cast::<u8>()
                .add(size_of::<GcHeader>())
                .cast::<MaybeUninit<T>>();
            init(value_ptr);
        }

        let ptr = ptr.cast::<GcBox<T>>();

        // CAST: `GcBox` is a `repr(C)` struct with `GcHeader` as its first field
        self.head.set(ptr.cast::<GcHeader>());

        self.stats.alloc(core::mem::size_of::<T>());

        Gc { ptr }
    }

    #[inline]
    pub(crate) fn collect(&mut self) {
        struct NoExternalRoots;
        impl ExternalRoots for NoExternalRoots {
            unsafe fn trace(&self, _: &Tracer) {}
        }

        self.collect_with_external_roots(&NoExternalRoots);
    }

    #[inline]
    pub(crate) fn collect_with_external_roots(&mut self, external_roots: &dyn ExternalRoots) {
        mark_and_sweep::collect(self, external_roots);
    }

    #[inline]
    pub fn stats(&self) -> &HeapStats {
        &self.stats
    }

    #[inline]
    fn roots(&self) -> *mut RootList {
        unsafe {
            let ptr: *mut Box<RootList> = UnsafeCell::raw_get(&self.roots);
            let ptr: *mut *mut RootList = core::mem::transmute(ptr);
            ptr.read()
        }
    }

    #[cfg(debug_assertions)]
    #[doc(hidden)]
    #[inline]
    pub fn id(&self) -> HeapId {
        self.heap_id
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe {
            mark_and_sweep::free_all(self);
        }
    }
}

#[derive(Default, Clone)]
pub struct HeapStats {
    alloc_bytes: Cell<usize>,
    collections: Cell<usize>,
}

impl HeapStats {
    /// Bytes directly held by managed pointers.
    ///
    /// Does not count backing storage of lists, tables, etc.
    #[inline]
    pub fn bytes(&self) -> usize {
        self.alloc_bytes.get()
    }

    /// How many times the GC ran
    #[inline]
    pub fn collections(&self) -> usize {
        self.collections.get()
    }

    #[inline]
    fn collect(&self) {
        self.collections.update(|v| v + 1)
    }

    #[inline]
    fn alloc(&self, bytes: usize) {
        self.alloc_bytes.update(|v| v + bytes)
    }

    #[inline]
    fn free(&self, bytes: usize) {
        self.alloc_bytes.update(|v| v - bytes)
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
    pub fn null() -> Self {
        Self {
            ptr: core::ptr::null_mut(),
        }
    }
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
    unsafe fn kind(self) -> ObjectKind {
        let header = &raw mut (*self.ptr).header;
        let header = UnsafeCell::raw_get(header);
        header.read().kind()
    }

    #[inline]
    unsafe fn set_mark(self, v: bool) {
        let header = &raw mut (*self.ptr).header;
        let header = UnsafeCell::raw_get(header);
        GcHeader::set_mark(header, v);
    }

    #[inline]
    unsafe fn is_marked(self) -> bool {
        let header = &raw mut (*self.ptr).header;
        let header = UnsafeCell::raw_get(header);
        header.read().marked()
    }
}

impl<T: Trace + Sized + 'static> Gc<T> {
    /// Get a shared reference to the object.
    ///
    /// ## Safety
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    /// - No `RefMut` must exist to the object
    #[inline]
    pub(crate) unsafe fn as_ref<'a>(self) -> Ref<'a, T> {
        unsafe { Ref::new_unchecked(self) }
    }

    /// Get an exclusive reference to the object.
    ///
    /// ## Safety
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    /// - The resulting reference must be _unique_ for the object
    #[inline]
    pub(crate) unsafe fn as_mut<'a>(self) -> RefMut<'a, T> {
        unsafe { RefMut::new_unchecked(self) }
    }

    /// ## Safety
    /// - The object must be alive.
    /// - No `&mut T` must exist to the object
    #[inline]
    pub(crate) unsafe fn as_rust_ref_very_unsafe<'a>(self) -> &'a T {
        &*UnsafeCell::raw_get(&raw mut (*self.ptr).value)
    }

    /// ## Safety
    /// - The object must be alive.
    /// - The resulting reference must be _unique_ for the object
    #[inline]
    pub(crate) unsafe fn as_rust_ref_mut_very_unsafe<'a>(self) -> &'a mut T {
        &mut *UnsafeCell::raw_get(&raw mut (*self.ptr).value)
    }

    #[inline]
    unsafe fn trace(self, tracer: &Tracer) {
        (*self.get_raw()).trace(tracer)
    }
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum ObjectKind {
    String = 0,
    List = 1,
    Table = 2,
    Closure = 3,
    UData = 4,
}

#[derive(Clone, Copy)]
#[repr(C, align(16))]
struct GcHeader {
    /// Each managed object is linked into a global list of all allocated values.
    /// Objects are linked into it during allocation, and unlinked during the sweep phase of GC
    /// after being freed.
    next: *mut GcHeader,
    kind: ObjectKind,
    marked: bool,
}

impl GcHeader {
    #[inline]
    fn new(kind: ObjectKind, next: *mut GcHeader) -> Self {
        Self {
            next,
            kind,
            marked: false,
        }
    }

    #[inline]
    fn into_parts(self) -> (*mut GcHeader, ObjectKind, bool) {
        (self.next(), self.kind(), self.marked())
    }

    #[inline]
    fn marked(self) -> bool {
        self.marked
    }

    #[inline]
    fn kind(self) -> ObjectKind {
        self.kind
    }

    #[inline]
    fn next(self) -> *mut GcHeader {
        self.next
    }

    #[inline]
    unsafe fn set_mark(this: *mut GcHeader, v: bool) {
        (*this).marked = v;
    }

    #[inline]
    unsafe fn set_next(this: *mut GcHeader, next: *mut GcHeader) {
        (*this).next = next;
    }
}

const _: () = {
    assert!(
        std::mem::size_of::<*mut ()>() == 8 && std::mem::align_of::<*mut ()>() == 8,
        "only 64-bit platforms are supported"
    );
};

// Think very carefully about changing the repr of this!
#[repr(C, align(8))]
struct GcBox<T: Sized + 'static> {
    header: UnsafeCell<GcHeader>,
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

/// Trace external roots, which is any place outside of
/// the object graph where `Gc` pointers may be placed
/// by the VM, and where they need to stay alive.
///
/// An example of this is the VM's stack, which holds live
/// values up to the top-most call frame.
pub(crate) trait ExternalRoots: 'static {
    unsafe fn trace(&self, tracer: &Tracer);
}

/// The GC must know how to traverse types in order to manage them.
///
/// ## Safety
///
/// All interior pointers must be marked.
///
/// `KIND` must refer to the `ObjectKind` variant corresponding to the type of `Self`.
pub(crate) unsafe trait Trace: Sized + 'static {
    const KIND: ObjectKind;

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
    pub(crate) fn visit<T: Trace>(&self, ptr: Gc<T>) {
        unsafe {
            if ptr.is_marked() {
                return; // cycle
            }
            ptr.set_mark(true);
            (*ptr.get_raw()).trace(self)
        }
    }

    #[inline]
    pub(crate) fn visit_value(&self, value: ValueRaw) {
        match value {
            ValueRaw::String(ptr) => self.visit(ptr),
            ValueRaw::List(ptr) => self.visit(ptr),
            ValueRaw::Table(ptr) => self.visit(ptr),
            ValueRaw::UData(ptr) => self.visit(ptr),
            ValueRaw::Nil | ValueRaw::Bool(_) | ValueRaw::Int(_) | ValueRaw::Float(_) => {}
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

/// Stores pointers used to traverse stack roots.
///
/// Each node stores a pointer to the head of the list,
/// and a pointer to the previous node.
#[repr(C)]
struct RootBase {
    list: *mut RootList,
    prev: *mut StackRoot<()>,
}

/// A linked list of stack roots.
///
/// Treated as a stack. Roots are pushed to the front,
/// and popped from the front, in LIFO order.
#[repr(C)]
struct RootList {
    head: *mut StackRoot<()>,
}

impl RootList {
    #[inline]
    fn head(this: *mut RootList) -> *mut *mut StackRoot<()> {
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
        F: FnMut(*mut StackRoot<()>),
    {
        let mut curr = (*this).head;
        while !curr.is_null() {
            tracer(curr);
            curr = (*curr).base.prev;
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

impl<T: Trace> StackRoot<T> {
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
    pub(crate) fn type_erase(self: Pin<&mut Self>) -> *mut StackRoot<()> {
        unsafe {
            let ptr = self.get_unchecked_mut() as *mut StackRoot<T>;
            ptr.cast::<StackRoot<()>>()
        }
    }

    /// Internal API
    // Append a pinned stack root to its root list.
    //
    // SAFETY:
    // - `self` must remain pinned.
    // - no other stack root can be appended to any root list
    //   between the construction of `self` and calling this
    //   function.
    #[doc(hidden)]
    #[inline]
    pub unsafe fn __append_to_root_list(mut self: Pin<&mut Self>) -> Pin<&mut Self> {
        let head = RootList::head(self.base.list);

        // SAFETY: we are pinned, so it's safe to append ourselves
        // to the root list.
        unsafe {
            *head = self.as_mut().type_erase();
        }

        self
    }

    /// Remove a pinned stack root from its root list.
    ///
    /// SAFETY: `self` must be head of the root list.
    #[inline]
    unsafe fn __remove_from_root_list(self: Pin<&mut Self>) {
        let head = RootList::head(self.base.list);
        let prev = self.base.prev;
        let this = self.type_erase();

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
            Pin::new_unchecked(self).__remove_from_root_list();
        }
    }
}

#[repr(C)]
pub struct Root<'a, T: Sized + 'static> {
    place: Pin<&'a mut StackRoot<T>>,

    #[cfg(debug_assertions)]
    heap_id: HeapId,
}

impl<'a, T: Trace> Root<'a, T> {
    /// Internal API
    #[doc(hidden)]
    #[inline]
    pub unsafe fn __new(heap: &Heap, mut place: Pin<&'a mut StackRoot<T>>) -> Self {
        place.as_mut().__append_to_root_list();

        Self {
            place,

            #[cfg(debug_assertions)]
            heap_id: heap.id(),
        }
    }

    /// Dereference the rooted pointer. Grants shared access to the object.
    #[inline]
    pub fn as_ref<'v>(&self, heap: &'v Heap) -> Ref<'v, T> {
        #[cfg(debug_assertions)]
        debug_assert!(heap.id() == self.heap_id);

        // SAFETY:
        // - The object is guaranteed to live for the duration of this borrow:
        //   - No collection may happen while the object is borrowed,
        //     because `heap.collect` requires unique access to the `heap`
        // - No mutable reference may exist at the same time as a shared reference
        //   - Shared access is gated by shared access to the `heap`
        //   - Only other shared references to `T` exist while the borrow is active
        unsafe { self.place.ptr.as_ref() }
    }

    /// Dereference the rooted pointer. Grants unique mutable access to the object.
    ///
    /// NOTE:
    /// - Mutable access requires borrowing the entire heap
    /// - No objects implement `Clone` or `Copy`
    /// - Object constructors return `Gc` pointers, so are never held on the stack
    ///
    /// Therefore it is impossible for the object to be moved out of in safe code.
    #[inline]
    pub fn as_mut<'v>(&self, heap: &'v mut Heap) -> RefMut<'v, T> {
        #[cfg(debug_assertions)]
        debug_assert!(heap.id() == self.heap_id);

        // SAFETY:
        // - The object is guaranteed to live for the duration of this borrow:
        //   - No collection may happen while the object is borrowed,
        //     because `heap.collect` requires unique access to the `heap`
        // - The returned mutable reference is unique for the given object:
        //   - Unique access is gated by unique access to the `heap`
        //   - Only one heap may be active on a given thread at once
        //   - Heaps, roots, and GC'd pointers are not `Send` or `Sync`
        //   - No other references exist to `T` while the borrow is active
        unsafe { self.place.ptr.as_mut() }
    }

    /// Retrieve the stored pointer.
    #[inline]
    pub fn as_ptr(&self) -> Gc<T> {
        self.place.ptr
    }

    /// Update the stored pointer.
    #[inline]
    pub unsafe fn set_ptr<U: Trace>(self, ptr: Gc<U>) -> Root<'a, U> {
        let mut this: Root<'a, U> = core::mem::transmute(self);
        this.place.as_mut().get_unchecked_mut().ptr = ptr;
        this
    }
}

// TODO: don't at all implement `deref` for `Ref` and `RefMut`, it shouldn't be necessary.

/// A shared reference to a GC-managed `T`.
///
/// This type is used to certify that the reference comes from a live root,
/// which makes it safe to move around, re-root, and store as an object member.
///
/// Implements `DerefMut` so can be turned into a plain reference as needed.
pub struct Ref<'a, T: 'static>(Gc<T>, PhantomData<fn(&'a T) -> &'a T>);

impl<'a, T> Clone for Ref<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'a, T> Copy for Ref<'a, T> {}

impl<'a, T: Trace + Sized + 'static> Ref<'a, T> {
    #[inline]
    pub fn map<U: Trace + Sized + 'static>(
        this: &Self,
        f: impl for<'v> FnOnce(&'v Ref<'a, T>) -> &'v Gc<U>,
    ) -> Ref<'a, U> {
        let ptr = *f(this);
        unsafe { ptr.as_ref() }
    }
}

impl<'a, T: Trace + Sized + 'static> Ref<'a, T> {
    #[inline]
    pub(crate) unsafe fn new_unchecked(ptr: Gc<T>) -> Self {
        Self(ptr, PhantomData)
    }

    #[inline]
    pub fn as_ptr(self) -> Gc<T> {
        self.0
    }
}

impl<'a, T: Trace + Sized + 'static> Deref for Ref<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_rust_ref_very_unsafe() }
    }
}

/// A unique reference to a GC-managed `T`.
///
/// This type is used to certify that the reference comes from a live root,
/// which makes it safe to move around, re-root, and store as an object member.
///
/// Implements `Deref`/`DerefMut` so can be turned into a plain reference as needed.
pub struct RefMut<'a, T: 'static>(Gc<T>, PhantomData<&'a mut T>);

impl<'a, T: Trace + Sized + 'static> RefMut<'a, T> {
    #[inline]
    pub fn map<U: Trace + Sized + 'static>(
        this: &mut Self,
        f: impl for<'v> FnOnce(&'v mut RefMut<'a, T>) -> &'v mut Gc<U>,
    ) -> RefMut<'a, U> {
        let ptr = *f(this);
        unsafe { ptr.as_mut() }
    }

    #[inline]
    pub fn as_ref(self) -> Ref<'a, T> {
        Ref(self.0, PhantomData)
    }
}

impl<'a, T: Trace + Sized + 'static> RefMut<'a, T> {
    #[inline]
    pub(crate) unsafe fn new_unchecked(ptr: Gc<T>) -> Self {
        Self(ptr, PhantomData)
    }

    #[inline]
    pub fn as_ptr(self) -> Gc<T> {
        self.0
    }
}

impl<'a, T: Trace + Sized + 'static> Deref for RefMut<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_rust_ref_very_unsafe() }
    }
}

impl<'a, T: Trace + Sized + 'static> DerefMut for RefMut<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_rust_ref_mut_very_unsafe() }
    }
}

#[repr(C, u64)]
pub enum ValueRoot<'a> {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,

    String(Root<'a, super::value::String>),
    List(Root<'a, super::value::List>),
    Table(Root<'a, super::value::Table>),
    UData(Root<'a, super::value::UData>),
}

impl<'a> ValueRoot<'a> {
    #[inline]
    pub fn raw(self) -> ValueRaw {
        match self {
            ValueRoot::Nil => ValueRaw::Nil,
            ValueRoot::Bool(v) => ValueRaw::Bool(v),
            ValueRoot::Int(v) => ValueRaw::Int(v),
            ValueRoot::Float(v) => ValueRaw::Float(v),
            ValueRoot::String(v) => ValueRaw::String(v.as_ptr()),
            ValueRoot::List(v) => ValueRaw::List(v.as_ptr()),
            ValueRoot::Table(v) => ValueRaw::Table(v.as_ptr()),
            ValueRoot::UData(v) => ValueRaw::UData(v.as_ptr()),
        }
    }
}

#[repr(C, u64)]
pub enum ValueRef<'a> {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,

    String(Ref<'a, super::value::String>),
    List(Ref<'a, super::value::List>),
    Table(Ref<'a, super::value::Table>),
    UData(Ref<'a, super::value::UData>),
}

impl<'a> ValueRef<'a> {
    #[inline]
    pub fn raw(self) -> ValueRaw {
        match self {
            ValueRef::Nil => ValueRaw::Nil,
            ValueRef::Bool(v) => ValueRaw::Bool(v),
            ValueRef::Int(v) => ValueRaw::Int(v),
            ValueRef::Float(v) => ValueRaw::Float(v),
            ValueRef::String(v) => ValueRaw::String(v.as_ptr()),
            ValueRef::List(v) => ValueRaw::List(v.as_ptr()),
            ValueRef::Table(v) => ValueRaw::Table(v.as_ptr()),
            ValueRef::UData(v) => ValueRaw::UData(v.as_ptr()),
        }
    }
}

// #[repr(C, u64)]
// pub enum ValueRefMut<'a> {
//     Nil = 0,
//     Bool(bool) = 1,
//     Int(i64) = 2,
//     Float(f64) = 3,

//     String(RefMut<'a, super::value::String>),
//     List(RefMut<'a, super::value::List>),
//     Table(RefMut<'a, super::value::Table>),
//     UData(RefMut<'a, super::value::UData>),
// }

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
macro_rules! let_root_unchecked {
    (unsafe in $heap:ident; $place:ident = $ptr:expr) => {
        let mut place = unsafe { $crate::__macro::StackRoot::from_heap_ptr($heap, $ptr) };
        let $place = unsafe {
            $crate::__macro::Root::__new($heap, ::core::pin::Pin::new_unchecked(&mut place))
        };
    };
}

// #[macro_export]
// macro_rules! reroot {
//     (in $heap:ident; $place:ident = $root:expr) => {
//         let
//     };
// }
