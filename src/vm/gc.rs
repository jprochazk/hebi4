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
//!   - List, Table, Closure
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

    #[inline]
    pub(crate) fn alloc_no_gc<T: Rootable + Trace + 'static>(&self, value: T) -> Gc<T> {
        let ptr = Box::into_raw(Box::new(GcBox {
            header: GcHeader::new(T::TYPE, self.head.get()),
            value: UnsafeCell::new(value),
        }));

        // CAST: `GcBox` is a `repr(C)` struct with `GcHeader` as its first field
        self.head.set(ptr.cast());

        self.stats.alloc(core::mem::size_of::<T>());

        Gc { ptr }
    }

    #[inline]
    pub(crate) fn collect(&mut self) {
        mark_and_sweep::collect(self);
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

#[derive(Default, Clone)]
pub struct HeapStats {
    alloc_bytes: Cell<usize>,
}

impl HeapStats {
    /// Bytes directly held by managed pointers.
    ///
    /// Does not count backing storage of lists, tables, etc.
    pub fn bytes(&self) -> usize {
        self.alloc_bytes.get()
    }

    fn alloc(&self, bytes: usize) {
        self.alloc_bytes.update(|v| v + bytes)
    }

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

    /// Get a shared reference to the object.
    ///
    /// ## Safety
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    #[inline]
    pub(crate) unsafe fn as_ref<'a>(self) -> &'a T {
        let ptr = unsafe { &raw const (*self.ptr).value };
        let ptr = UnsafeCell::raw_get(ptr);

        unsafe { &*ptr }
    }

    /// Get an exclusive reference to the object.
    ///
    /// ## Safety
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    #[inline]
    pub(crate) unsafe fn as_ref_mut<'a>(self) -> &'a mut T {
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

// NOTE: Changing `GcHeader` or `ObjectType` must be done carefully with intent!
//
// Alignment tells us where in memory an address may start. A 1-byte alignment means
// the value can reside anywhere in memory, a 4-byte alignment means the address must
// be cleanly divisible (the remainder is 0) by 4, etc.
//
// Here is a pointer to an 8-byte aligned value on x86-64 linux, in binary form:
//
// ```text,ignore
// 00000000_00000000_01010110_00010000_01000101_01101100_01110101_11001000
//                                                                     ^^^
// ```
//
// Those low bits are free real estate.
//
// `GcHeader` is 8-byte aligned, which means a `*mut GcHeader` always contains 3 unused bits.
// Unused in the sense that if we write something to them, and remember to mask off those bits
// before dereferencing the pointer, they can be used to store other things!
//
// (Yes, it's true that on most platforms the high 16 bits are also unused, but that depends on
// platform-specific details. Alignment is inherently portable)
//
// We use the first bit as the object's mark bit. The other two bits are used to store the `ObjectType`.
//
// In `ObjectType`'s numerical repr, we ensure the first bit is unused, so that
// reading the object type from `GcHeader`'s tagged pointer only requires masking off
// all other bits. For example, instead of `0b001` for `Table`, we use `0b010`.
//

#[repr(u8)]
pub enum ObjectType {
    List = 0b000,
    Table = 0b010,
    Closure = 0b100,
    UData = 0b110,
}

#[derive(Clone, Copy)]
#[repr(C, align(8))]
struct GcHeader {
    // Each managed object is linked into a global list of all allocated values.
    // Objects are linked into it during allocation, and unlinked during the sweep phase of GC
    // after being freed.
    //
    // This field stores a _tagged pointer_ to the next object in the list.
    tagged: *mut GcHeader,
}

impl GcHeader {
    #[inline]
    fn new(tt: ObjectType, next: *mut GcHeader) -> Self {
        let marked = false as usize;
        let tt = tt as usize;

        let tag = marked | tt;

        // Ensure that we aren't using more bits than we are supposed to
        debug_assert!(tag & 0b001 == marked);
        debug_assert!(tag & 0b110 == tt);

        Self {
            tagged: next.map_addr(|addr| addr.wrapping_add(tag)),
        }
    }

    #[inline]
    fn into_parts(self) -> (*mut GcHeader, ObjectType, bool) {
        (self.next(), self.type_(), self.marked())
    }

    #[inline]
    fn marked(self) -> bool {
        self.tagged.addr() & 0b1 == 0b1
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
                return; // cycle
            }
            ptr.mark();
            (*ptr.get_raw()).trace(self)
        }
    }

    #[inline]
    pub(crate) fn visit_value(&self, value: ValueRaw) {
        match value {
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

impl<'a, T: Rootable> Root<'a, T> {
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
        debug_assert!(heap.id() == self.heap_id);

        // SAFETY:
        // - The object is guaranteed to live for the duration of this borrow:
        //   - No collection may happen while the object is borrowed,
        //     because `heap.collect` requires unique access to the `heap`
        // - No mutable reference may exist at the same time as a shared reference
        //   - Shared access is gated by shared access to the `heap`
        //   - Only other shared references to `T` exist while the borrow is active
        Ref(unsafe { self.place.ptr.as_ref() })
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
        RefMut(unsafe { self.place.ptr.as_ref_mut() })
    }

    /// Retrieve the stored pointer.
    #[inline]
    pub fn as_ptr(&self) -> Gc<T> {
        self.place.ptr
    }

    /// Update the stored pointer.
    #[inline]
    pub unsafe fn set_ptr<U: Rootable>(self, ptr: Gc<U>) -> Root<'a, U> {
        let mut this: Root<'a, U> = core::mem::transmute(self);
        this.place.as_mut().get_unchecked_mut().ptr = ptr;
        this
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

impl<'a, T> Ref<'a, T> {
    #[inline]
    pub fn map<'v, U>(this: &'v Self, f: impl FnOnce(&'v T) -> &'v U) -> Ref<'v, U> {
        Ref(f(this.0))
    }
}

impl<'a, T: Rootable> Ref<'a, T> {
    #[inline]
    pub(crate) unsafe fn new_unchecked(ptr: &'a T) -> Self {
        Self(ptr)
    }
}

impl<'a, T> Deref for Ref<'a, T> {
    type Target = T;

    #[inline]
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

impl<'a, T> RefMut<'a, T> {
    #[inline]
    pub fn map<'v, U>(this: &'v mut Self, f: impl FnOnce(&'v mut T) -> &'v mut U) -> RefMut<'v, U> {
        RefMut(f(this.0))
    }
}

impl<'a, T: Rootable> RefMut<'a, T> {
    #[inline]
    pub(crate) unsafe fn new_unchecked(ptr: &'a mut T) -> Self {
        Self(ptr)
    }
}

impl<'a, T> Deref for RefMut<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, T> DerefMut for RefMut<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

#[repr(C, u64)]
pub enum ValueRoot<'a> {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
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
            ValueRoot::List(root) => ValueRaw::List(root.as_ptr()),
            ValueRoot::Table(root) => ValueRaw::Table(root.as_ptr()),
            ValueRoot::UData(root) => ValueRaw::UData(root.as_ptr()),
        }
    }
}

#[repr(C, u64)]
pub enum ValueRef<'a> {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
    List(Ref<'a, super::value::List>),
    Table(Ref<'a, super::value::Table>),
    UData(Ref<'a, super::value::UData>),
}

#[repr(C, u64)]
pub enum ValueRefMut<'a> {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
    List(RefMut<'a, super::value::List>),
    Table(RefMut<'a, super::value::Table>),
    UData(RefMut<'a, super::value::UData>),
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
macro_rules! let_root_unchecked {
    (unsafe in $heap:ident; $place:ident = $ptr:expr) => {
        let mut place = unsafe { $crate::__macro::StackRoot::from_heap_ptr($heap, $ptr) };
        let $place = unsafe {
            $crate::__macro::Root::__new($heap, ::core::pin::Pin::new_unchecked(&mut place))
        };
    };
}
