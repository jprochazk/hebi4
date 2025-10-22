//! Garbage collector

// TODO: root projection
// TODO: re-rooting
// TODO: storing in objects

use std::{
    cell::{Cell, UnsafeCell},
    marker::{PhantomData, PhantomPinned},
    mem::MaybeUninit,
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::{NonNull, null_mut},
};

use super::value::{StringHasher, ValueRaw};

// # Heap

// TODO: require `HeapGuard` instead of `Heap`,
// `HeapGuard` is acquired by `Heap::enter`, which guarantees
// no two heaps exist on the same thread at the same time.

#[repr(C)]
pub struct Heap {
    /// Boxed, because root lists must have stable addresses
    roots: UnsafeCell<Box<RootList>>,

    /// Objects are linked into a global list of all allocations,
    /// which is used to find dead object during the sweep phase.
    head: Cell<Option<NonNull<GcHeader>>>,

    stats: HeapStats,

    string_hasher: StringHasher,

    #[cfg(debug_assertions)]
    heap_id: HeapId,
}

impl Heap {
    #[doc(hidden)]
    pub unsafe fn __testing() -> Self {
        Self::new()
    }

    #[inline]
    pub(crate) fn new() -> Self {
        Self {
            roots: UnsafeCell::new(Box::new(RootList::default())),
            head: Cell::new(None),
            stats: HeapStats::default(),
            string_hasher: StringHasher::default(),

            #[cfg(debug_assertions)]
            heap_id: HeapId::new(),
        }
    }

    // TODO: when to call `collect`?

    // NOTE: `&self` because it does not trigger a collection,
    // so it does not need to invalidate any shared references.

    /// Allocate an object on the heap.
    ///
    /// The object itself may be initialized in-place in the `init` callback.
    #[inline]
    pub(crate) fn alloc_no_gc<T: Trace + 'static>(
        &self,
        init: impl FnOnce(*mut MaybeUninit<T>),
    ) -> GcPtr<T> {
        // We do manual piece-wise init, because allocation failure should _not_
        // cause the value to be dropped! Therefore it must be constructed _after_
        // we have a place to put it.
        let ptr = Box::into_raw(Box::<GcBox<T>>::new_uninit());

        unsafe {
            let header = ptr.cast::<MaybeUninit<GcHeader>>();
            (*header).write(GcHeader::new(T::vtable(), self.head.get()));

            let value_ptr = ptr
                .cast::<u8>()
                .add(size_of::<GcHeader>())
                .cast::<MaybeUninit<T>>();
            init(value_ptr);
        }

        let ptr = ptr.cast::<GcBox<T>>();
        let ptr = unsafe { NonNull::new_unchecked(ptr) };

        // CAST: `GcBox` is a `repr(C)` struct with `GcHeader` as its first field
        self.head.set(Some(ptr.cast::<GcHeader>()));

        self.stats.on_alloc(core::mem::size_of::<T>());

        GcPtr { ptr }
    }

    /// Trigger a full collection.
    ///
    /// No external roots are traced.
    #[inline]
    pub(crate) fn collect_no_external_roots(&mut self) {
        struct NoExternalRoots;
        impl ExternalRoots for NoExternalRoots {
            unsafe fn trace(&self, _: &Tracer) {}
        }

        unsafe { Self::collect_with_external_roots(self, NoExternalRoots) };
    }

    /// Trigger a full collection.
    ///
    /// Together with the heap's own stack roots, the given `external_roots`
    /// are also traced to find live objects.
    #[inline]
    pub(crate) unsafe fn collect_with_external_roots(
        this: *mut Heap,
        external_roots: impl ExternalRoots,
    ) {
        mark_and_sweep::collect(this, external_roots);
    }

    /// Get some statistics about the heap.
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
    pub(crate) fn id(&self) -> HeapId {
        self.heap_id
    }

    #[inline]
    pub(crate) fn string_hasher(&self) -> &StringHasher {
        &self.string_hasher
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe {
            mark_and_sweep::free_all(self);
        }
    }
}

/// Statistics about [`Heap`] usage.
#[derive(Default, Clone)]
pub struct HeapStats {
    alloc_bytes: Cell<usize>,
    collections: Cell<usize>,
}

impl HeapStats {
    /// Bytes directly used by managed objects.
    ///
    /// Only counts the object wrappers themselves, not any allocations they may contain.
    /// For example, for an object which contains a `Vec`, the data stored in the `Vec`
    /// is _not_ counted, because the GC does not manage those.
    #[inline]
    pub fn bytes(&self) -> usize {
        self.alloc_bytes.get()
    }

    /// How many times the GC ran a collection cycle.
    #[inline]
    pub fn collections(&self) -> usize {
        self.collections.get()
    }

    // Methods for updating heap stats:

    /// Called when a collection occurs.
    #[inline]
    fn on_collect(&self) {
        self.collections.update(|v| v + 1)
    }

    /// Called when a managed object is allocated by the mutator.
    #[inline]
    fn on_alloc(&self, bytes: usize) {
        self.alloc_bytes.update(|v| v + bytes)
    }

    /// Called when a managed object is freed during a sweep.
    #[inline]
    fn on_free(&self, bytes: usize) {
        self.alloc_bytes.update(|v| v - bytes)
    }
}

mod mark_and_sweep;

#[cfg(debug_assertions)]
#[doc(hidden)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct HeapId(usize);

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

/// Pointer to a object in the garbage-collected heap.
///
/// No guarantees are made about the liveness of the inner `T`.
///
/// Can only be dereferenced using `unsafe` code:
///
/// - [`GcPtr::as_ref`]
/// - [`GcPtr::as_mut`]
/// - [`GcPtr::as_rust_ref_very_unsafe`]
/// - [`GcPtr::as_rust_ref_mut_very_unsafe`]
///
/// To be able to use the above methods, the usual Rust aliasing rules
/// must be upheld:
///
/// - When creating a shared reference, no unique reference may exist.
/// - When creating a unique reference, no other references may exist.
///
/// A few of the raw pointer dereferencing guarantees are already upheld by `GcPtr`:
///
/// - This pointer is never null.
/// - The `T` is always properly aligned.
/// - `T` is "dereferenceable" in the sense that the memory range of
///   `size_of::<T>` starting at this pointer contains a valid instance of `T`,
///   and the instance is contained entirely within the original allocation.
#[repr(transparent)]
pub struct GcPtr<T: Sized + 'static> {
    ptr: NonNull<GcBox<T>>,
}

impl<T: Sized + 'static> GcPtr<T> {
    /// Wrap a previously managed pointer.
    ///
    /// ## Safety
    /// - The pointer must have been produced by `Gc::into_raw`.
    #[inline]
    pub(crate) unsafe fn from_raw(ptr: *mut T) -> GcPtr<T> {
        let offset = core::mem::offset_of!(GcBox<T>, value);
        // CAST: `GcBox` is a `repr(C)` struct with `T` as its 2nd field
        let ptr = ptr.cast::<u8>().sub(offset).cast::<GcBox<T>>();
        let ptr = NonNull::new_unchecked(ptr);

        GcPtr { ptr }
    }

    #[inline]
    pub(crate) unsafe fn into_raw(self) -> NonNull<T> {
        let ptr = unsafe { &raw const (*self.ptr.as_ptr()).value };
        NonNull::new_unchecked(UnsafeCell::raw_get(ptr))
    }

    #[inline]
    pub(crate) unsafe fn vt(self) -> &'static GcVtable {
        let header = &raw mut (*self.ptr.as_ptr()).header;
        let header = UnsafeCell::raw_get(header);
        header.read().vt()
    }

    #[inline]
    unsafe fn set_mark(self, v: bool) {
        let header = &raw mut (*self.ptr.as_ptr()).header;
        let header = UnsafeCell::raw_get(header);
        let header = NonNull::new_unchecked(header);
        GcHeader::set_mark(header, v);
    }

    #[inline]
    unsafe fn is_marked(self) -> bool {
        let header = &raw mut (*self.ptr.as_ptr()).header;
        let header = UnsafeCell::raw_get(header);
        header.read().marked()
    }
}

impl<T: Trace> GcPtr<T> {
    /// Get a shared reference to the object.
    ///
    /// ## Safety
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    /// - No unique reference to the object may already exist
    #[inline]
    pub unsafe fn as_ref<'a>(self) -> GcRef<'a, T> {
        GcRef::new_unchecked(self)
    }

    /// Get an exclusive reference to the object.
    ///
    /// ## Safety
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    /// - No other reference to the object may already exist
    #[inline]
    pub unsafe fn as_mut<'a>(self) -> GcRefMut<'a, T> {
        GcRefMut::new_unchecked(self)
    }

    /// ## Safety
    /// - The object must be alive.
    /// - No unique reference to the object may already exist
    #[inline]
    pub unsafe fn as_rust_ref_very_unsafe<'a>(self) -> &'a T {
        &*UnsafeCell::raw_get(&raw mut (*self.ptr.as_ptr()).value)
    }

    /// ## Safety
    /// - The object must be alive.
    /// - No other reference to the object may already exist
    #[inline]
    pub unsafe fn as_rust_ref_mut_very_unsafe<'a>(self) -> &'a mut T {
        &mut *UnsafeCell::raw_get(&raw mut (*self.ptr.as_ptr()).value)
    }

    #[inline]
    pub fn as_any(self) -> GcAnyPtr {
        GcAnyPtr {
            ptr: self.ptr.cast(),
        }
    }

    #[inline]
    unsafe fn trace(self, tracer: &Tracer) {
        self.into_raw().as_ref().trace(tracer)
    }
}

#[repr(C, align(16))]
pub(crate) struct GcVtable {
    /// Trace object's interior references
    pub trace: unsafe fn(*const (), *const Tracer),

    /// Drop and free the object
    pub free: unsafe fn(*mut ()) -> usize,

    /// Debug-print the object
    pub debug: unsafe fn(GcAnyPtr, &mut core::fmt::Formatter<'_>) -> core::fmt::Result,

    pub type_name: &'static str,
}

macro_rules! generate_vtable_for {
    ($T:ty) => {
        $crate::gc::GcVtable {
            trace: {
                unsafe fn _trace(this: *const (), tracer: *const $crate::gc::Tracer) {
                    <$T as $crate::gc::Trace>::trace(&*this.cast::<$T>(), &*tracer);
                }

                _trace
            },
            free: {
                unsafe fn _free(this: *mut ()) -> usize {
                    let _ = Box::from_raw(this.cast::<$crate::gc::GcBox<$T>>());
                    ::core::mem::size_of::<$T>()
                }

                _free
            },
            debug: {
                unsafe fn _debug(
                    this: $crate::gc::GcAnyPtr,
                    fmt: &mut ::core::fmt::Formatter<'_>,
                ) -> ::core::fmt::Result {
                    let this: $crate::gc::GcPtr<$T> = this.cast_unchecked();
                    let this = this.as_ref();
                    ::core::fmt::Debug::fmt(&this, fmt)
                }

                _debug
            },
            type_name: stringify!($T),
        }
    };
}

/// Tagged virtual table.
///
/// `GcVtable` is aligned to 16 bytes, so it has 4 free bits,
/// and we use the first one as the mark bit.
#[derive(Clone, Copy)]
#[repr(transparent)]
struct TaggedVt(NonNull<GcVtable>);

const _: () = assert!(TaggedVt::ALIGN >= 16 && TaggedVt::ALIGN.is_power_of_two());

impl TaggedVt {
    const ALIGN: usize = core::mem::align_of::<GcVtable>();
    const MASK: usize = Self::ALIGN - 1;

    #[inline]
    fn new(ptr: &'static GcVtable) -> Self {
        let ptr = ptr as *const GcVtable as *mut GcVtable;
        // SAFETY: pointer comes from a `&'static` ref.
        Self(unsafe { NonNull::new_unchecked(ptr) })
    }

    /// Only uses first `Self::ALIGN` bits of `tag`.
    #[inline]
    fn with_tag(self, tag: usize) -> Self {
        let tag = tag & Self::MASK;
        Self(self.0.map_addr(
            #[inline]
            |addr| {
                let addr = (addr.get() & !Self::MASK) | tag;
                // SAFETY: guaranteed to still be non-zero, as the original address is non-zero.
                unsafe { NonZeroUsize::new_unchecked(addr) }
            },
        ))
    }

    /// Get the first `Self::ALIGN` bits.
    #[inline]
    fn tag(self) -> usize {
        self.0.addr().get() & Self::MASK
    }

    /// Get the vtable ptr.
    #[inline]
    pub(crate) fn vt(self) -> &'static GcVtable {
        let ptr = self.0.map_addr(|addr| {
            let addr = addr.get() & !Self::MASK;
            // SAFETY: guaranteed to still be non-zero, as the original address is non-zero.
            unsafe { NonZeroUsize::new_unchecked(addr) }
        });

        // SAFETY: pointer comes from a `&'static` ref.
        unsafe { ptr.as_ref() }
    }
}

#[derive(Clone, Copy)]
#[repr(C, align(16))]
struct GcHeader {
    /// Each managed object is linked into a global list of all allocated values.
    /// Objects are linked into it during allocation, and unlinked during the sweep phase of GC
    /// after being freed.
    next: Option<NonNull<GcHeader>>,
    tagged_vt: TaggedVt,
}

impl GcHeader {
    #[inline]
    fn new(vtable: &'static GcVtable, next: Option<NonNull<GcHeader>>) -> Self {
        Self {
            next,
            tagged_vt: TaggedVt::new(vtable),
        }
    }

    #[inline]
    fn into_parts(self) -> (Option<NonNull<GcHeader>>, &'static GcVtable, bool) {
        (self.next(), self.vt(), self.marked())
    }

    #[inline]
    fn marked(self) -> bool {
        self.tagged_vt.tag() == 1
    }

    #[inline]
    fn vt(self) -> &'static GcVtable {
        self.tagged_vt.vt()
    }

    #[inline]
    fn next(self) -> Option<NonNull<GcHeader>> {
        self.next
    }

    #[inline]
    unsafe fn set_mark(this: NonNull<GcHeader>, v: bool) {
        let this = this.as_ptr();
        (*this).tagged_vt = (*this).tagged_vt.with_tag(v as usize);
    }

    #[inline]
    unsafe fn set_next(this: NonNull<GcHeader>, next: Option<NonNull<GcHeader>>) {
        let this = this.as_ptr();
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
pub(crate) struct GcBox<T: Sized + 'static> {
    header: UnsafeCell<GcHeader>,
    value: UnsafeCell<T>,
}

impl<T: 'static> GcPtr<T> {}

impl<T: 'static> Clone for GcPtr<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: 'static> Copy for GcPtr<T> {}

#[repr(transparent)]
pub struct GcAnyPtr {
    ptr: NonNull<GcBox<()>>,
}

impl GcAnyPtr {
    /// Get a shared reference to the object.
    ///
    /// ## Safety
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    /// - No unique reference to the object may already exist
    #[inline]
    pub unsafe fn as_ref<'a>(self) -> GcAnyRef<'a> {
        GcAnyRef::new_unchecked(self)
    }

    /// Get an exclusive reference to the object.
    ///
    /// ## Safety
    /// - The object must be alive.
    /// - For the resulting reference to be valid, the object must either be:
    ///   - Rooted
    ///   - Exist while the garbage collector is guaranteed not to run
    /// - No other reference to the object may already exist
    #[inline]
    pub unsafe fn as_mut<'a>(self) -> GcAnyRefMut<'a> {
        GcAnyRefMut::new_unchecked(self)
    }

    /// Cast to a `GcPtr<T>` after checking that `self`
    /// actually points to a `T`.
    #[inline]
    pub fn cast<T: Trace>(self) -> Option<GcPtr<T>> {
        if !self.is::<T>() {
            return None;
        }

        // SAFETY: `self` points to an instance of `T`.
        Some(unsafe { self.cast_unchecked() })
    }

    /// Cast to a `GcPtr<T>` without checking that `self`
    /// actually points to a `T`.
    ///
    /// ## Safety
    ///
    /// - This pointer must point to an instance of `T`.
    #[inline]
    pub unsafe fn cast_unchecked<T: Trace>(self) -> GcPtr<T> {
        GcPtr {
            ptr: self.ptr.cast(),
        }
    }

    #[inline]
    pub fn is<T: Trace>(self) -> bool {
        // SAFETY:
        // VTables are static, so comparing them for equality
        // is the same as comparing the types' `TypeId`s.
        let vt = unsafe { self.vt() };
        core::ptr::eq(vt, T::vtable())
    }

    /// Wrap a previously managed pointer.
    ///
    /// ## Safety
    /// - The pointer must have been produced by [`GcAnyPtr::into_raw`].
    #[inline]
    pub(crate) unsafe fn from_raw(ptr: *mut ()) -> GcAnyPtr {
        let offset = core::mem::offset_of!(GcBox<()>, value);
        // CAST: `GcBox` is a `repr(C)` struct with `T` as its 2nd field
        let ptr = ptr.cast::<u8>().sub(offset).cast::<GcBox<()>>();
        let ptr = NonNull::new_unchecked(ptr);

        GcAnyPtr { ptr }
    }

    #[inline]
    pub(crate) unsafe fn into_raw(self) -> NonNull<()> {
        let ptr = unsafe { &raw const (*self.ptr.as_ptr()).value };
        NonNull::new_unchecked(UnsafeCell::raw_get(ptr))
    }

    /// ## Safety
    ///
    /// - Object must be live.
    #[inline]
    pub(crate) unsafe fn type_name(self) -> &'static str {
        let header = &raw mut (*self.ptr.as_ptr()).header;
        let header = UnsafeCell::raw_get(header);
        header.read().vt().type_name
    }

    /// ## Safety
    ///
    /// - Object must be live.
    #[inline]
    pub(crate) unsafe fn vt(self) -> &'static GcVtable {
        let header = &raw mut (*self.ptr.as_ptr()).header;
        let header = UnsafeCell::raw_get(header);
        header.read().vt()
    }

    /// ## Safety
    ///
    /// - Object must be live.
    #[inline]
    unsafe fn set_mark(self, v: bool) {
        let header = &raw mut (*self.ptr.as_ptr()).header;
        let header = UnsafeCell::raw_get(header);
        let header = NonNull::new_unchecked(header);
        GcHeader::set_mark(header, v);
    }

    /// ## Safety
    ///
    /// - Object must be live.
    #[inline]
    unsafe fn is_marked(self) -> bool {
        let header = &raw mut (*self.ptr.as_ptr()).header;
        let header = UnsafeCell::raw_get(header);
        header.read().marked()
    }

    #[inline]
    unsafe fn trace(self, tracer: &Tracer) {
        (self.vt().trace)(self.into_raw().as_ptr().cast_const(), tracer);
    }
}

impl Clone for GcAnyPtr {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}
impl Copy for GcAnyPtr {}

// # Tracing

/// Trace external roots, which is any place outside of
/// the object graph where managed pointers may be exist,
/// and where the GC should be able to find them.
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
/// - `trace` must correctly trace all interior references.
/// - `vtable` must correctly refer to a vtable of `Self`.
///   the easiest way to ensure this is to use the [`vtable`] macro.
/// - `vtable` must additionally always return the same reference
///   to the same vtable, meaning the vtable _must_ be stored in a `static`.
///
/// Address equality of vtable pointers is used instead of `TypeId`-based
/// type checks.
pub(crate) unsafe trait Trace: Sized + 'static {
    fn vtable() -> &'static GcVtable;

    /// ## Safety
    ///
    /// All interior pointers must be marked.
    unsafe fn trace(&self, tracer: &Tracer);
}

macro_rules! vtable {
    ($T:ty) => {
        #[inline]
        fn vtable() -> &'static $crate::gc::GcVtable {
            static VT: $crate::gc::GcVtable = generate_vtable_for!($T);

            &VT
        }
    };
}

/// A type which knows how to mark GC-managed objects
/// and dynamically-typed values.
pub(crate) struct Tracer {
    _marker: PhantomData<()>,
}

impl Tracer {
    /// Visit an interior pointer.
    #[inline]
    pub(crate) fn visit<T: Trace>(&self, ptr: GcPtr<T>) {
        unsafe {
            if ptr.is_marked() {
                return; // cycle
            }
            ptr.set_mark(true);
            ptr.trace(self)
        }
    }

    #[inline]
    pub(crate) fn visit_any(&self, ptr: GcAnyPtr) {
        unsafe {
            if ptr.is_marked() {
                return; // cycle
            }
            ptr.set_mark(true);
            ptr.trace(self);
        }
    }

    /// Visit a value, which may contain an interior pointer.
    #[inline]
    pub(crate) fn visit_value(&self, value: ValueRaw) {
        match value {
            ValueRaw::Object(ptr) => self.visit_any(ptr),
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
///
/// Once constructed, should be pinned and never moved again.
#[doc(hidden)]
#[repr(C)]
pub struct StackRoot<T: Sized + 'static> {
    base: RootBase,
    ptr: GcPtr<T>,
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
    pub unsafe fn from_heap_ptr(heap: &Heap, ptr: GcPtr<T>) -> Self {
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
    // ## Safety
    //
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
    /// ## Safety
    ///
    /// - `self` must be head of the root list.
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

/// A pointer representing a rooted object.
///
/// This type is safe to move, because the underlying reference
/// is pinned to the stack.
#[repr(C)]
pub struct GcRoot<'a, T: Sized + 'static> {
    place: Pin<&'a mut StackRoot<T>>,

    #[cfg(debug_assertions)]
    heap_id: HeapId,
}

impl<'a, T: Trace> GcRoot<'a, T> {
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

    // Dereferencing roots: Rust's aliasing guarantees are upheld due to usage of the `Heap`
    // type here. In order to produce a direct reference to an object, the user must first
    // have a direct reference to the heap.

    /// Dereference the pointer, yielding a direct _shared_ reference to the object.
    ///
    /// Note that this requires shared access to the `heap`. That means it is not possible
    /// to hold a shared reference to an object `A`, and at the same time hold a unique
    /// reference to an object `B`, even though it is valid.
    ///
    /// To work around this issue, many APIs on builtin types accepts `Root`s directly.
    /// For example, appending a list to itself can be done by rooting the list twice,
    /// getting a mutable reference to the first root, and appending the second root:
    ///
    /// ```rust
    /// # use hebi4::{gc::{Heap, reroot}, value::list};
    /// # let heap = unsafe { &mut Heap::__testing() };
    /// list!(in heap; a0 = 0);
    /// reroot!(in heap; a1 = a0);
    /// a0.as_mut(heap).push(a1.as_any().into());
    /// ```
    #[inline]
    pub fn as_ref<'v>(&self, heap: &'v Heap) -> GcRef<'v, T> {
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

    // NOTE:
    // - Mutable access requires borrowing the entire heap
    // - No objects implement `Clone` or `Copy`
    // - Object constructors return `Gc` pointers, so are never held on the stack
    //
    // Therefore it is impossible for the object to be moved out of in safe code.

    /// Dereference the pointer, yielding a direct _unique_ reference to the object.
    #[inline]
    pub fn as_mut<'v>(&self, heap: &'v mut Heap) -> GcRefMut<'v, T> {
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

    #[inline]
    pub fn as_any(self) -> GcAnyRoot<'a> {
        GcAnyRoot(unsafe { core::mem::transmute(self) })
    }
}

impl<'a, T: Sized + 'static> GcRoot<'a, T> {
    /// Retrieve the stored pointer.
    #[inline]
    pub fn as_ptr(&self) -> GcPtr<T> {
        self.place.ptr
    }

    /// Update the stored pointer.
    #[inline]
    pub unsafe fn set_ptr<U: Trace>(self, ptr: GcPtr<U>) -> GcRoot<'a, U> {
        let mut this: GcRoot<'a, U> = core::mem::transmute(self);
        this.place.as_mut().get_unchecked_mut().ptr = ptr;
        this
    }
}

#[repr(transparent)]
pub struct GcAnyRoot<'a>(GcRoot<'a, ()>);

impl<'a> GcAnyRoot<'a> {
    /// Cast to a `GcPtr<T>` after checking that `self`
    /// actually points to a `T`.
    #[inline]
    pub fn cast<T: Trace>(self) -> Option<GcRoot<'a, T>> {
        if !self.is::<T>() {
            return None;
        }
        Some(unsafe { self.cast_unchecked() })
    }

    /// Cast to a `GcRoot<T>` without checking that `self`
    /// actually points to a `T`.
    ///
    /// ## Safety
    ///
    /// - This pointer must point to an instance of `T`.
    #[inline]
    pub unsafe fn cast_unchecked<T: Trace>(self) -> GcRoot<'a, T> {
        // SAFETY: `Self` is a `repr(transparent)` struct around `GcRoot`
        core::mem::transmute(self)
    }

    #[inline]
    pub fn is<T: Trace>(&self) -> bool {
        self.as_ptr().is::<T>()
    }

    /// Retrieve the stored pointer.
    #[inline]
    pub fn as_ptr(&self) -> GcAnyPtr {
        GcAnyPtr {
            ptr: self.0.as_ptr().ptr.cast(),
        }
    }

    /// Update the stored pointer.
    #[inline]
    pub unsafe fn set_ptr<T: Trace>(self, ptr: GcPtr<T>) -> GcAnyRoot<'a> {
        let mut this: GcRoot<'a, T> = core::mem::transmute(self.0);
        this.place.as_mut().get_unchecked_mut().ptr = ptr;
        let this: GcRoot<'a, ()> = core::mem::transmute(this);
        GcAnyRoot(this)
    }

    /// Update the stored pointer.
    #[inline]
    pub unsafe fn set_any_ptr(mut self, ptr: GcAnyPtr) -> GcAnyRoot<'a> {
        self.0.place.as_mut().get_unchecked_mut().ptr = GcPtr { ptr: ptr.ptr };
        self
    }

    #[inline]
    pub(crate) fn vt(&self) -> &'static GcVtable {
        // SAFETY: rooted
        unsafe { self.as_ptr().vt() }
    }
}

impl std::fmt::Debug for GcAnyRoot<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { (self.vt().debug)(self.as_ptr(), f) }
    }
}

/// A shared reference to a GC-managed `T`.
///
/// This type is used to certify that the reference comes from a live root,
/// which makes it safe to move around, re-root, and store as an object member.
///
/// Implements `DerefMut` so can be turned into a plain reference as needed.
#[repr(transparent)]
pub struct GcRef<'a, T: 'static>(GcPtr<T>, PhantomData<fn(&'a T) -> &'a T>);

impl<'a, T> Clone for GcRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'a, T> Copy for GcRef<'a, T> {}

impl<'a, T: Trace> GcRef<'a, T> {
    #[inline]
    pub fn map<U: Trace>(
        this: &Self,
        f: impl for<'v> FnOnce(&'v GcRef<'a, T>) -> &'v GcPtr<U>,
    ) -> GcRef<'a, U> {
        let ptr = *f(this);
        unsafe { ptr.as_ref() }
    }

    #[inline]
    pub fn map_opt<U: Trace>(
        this: &Self,
        f: impl for<'v> FnOnce(&'v GcRef<'a, T>) -> Option<&'v GcPtr<U>>,
    ) -> Option<GcRef<'a, U>> {
        let ptr = f(this);
        match ptr {
            Some(&ptr) => Some(unsafe { ptr.as_ref() }),
            None => None,
        }
    }
}

impl<'a, T: Trace> GcRef<'a, T> {
    #[inline]
    pub(crate) unsafe fn new_unchecked(ptr: GcPtr<T>) -> Self {
        Self(ptr, PhantomData)
    }

    #[inline]
    pub fn as_ptr(&self) -> GcPtr<T> {
        self.0
    }

    #[inline]
    pub fn as_any(self) -> GcAnyRef<'a> {
        GcAnyRef(self.0.as_any(), PhantomData)
    }
}

impl<'a, T: Trace> Deref for GcRef<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_rust_ref_very_unsafe() }
    }
}

#[repr(transparent)]
pub struct GcAnyRef<'a>(GcAnyPtr, PhantomData<fn(&'a ()) -> &'a ()>);

impl<'a> Clone for GcAnyRef<'a> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'a> Copy for GcAnyRef<'a> {}

impl<'a> GcAnyRef<'a> {
    #[inline]
    pub(crate) fn new_unchecked(ptr: GcAnyPtr) -> Self {
        Self(ptr, PhantomData)
    }

    #[inline]
    pub fn cast<T: Trace>(self) -> Option<GcRef<'a, T>> {
        let this = self.0.cast()?;
        Some(GcRef(this, PhantomData))
    }

    #[inline]
    pub unsafe fn cast_unchecked<T: Trace>(self) -> GcRef<'a, T> {
        let this = unsafe { self.0.cast_unchecked() };
        GcRef(this, PhantomData)
    }

    #[inline]
    pub fn is<T: Trace>(&self) -> bool {
        self.0.is::<T>()
    }

    #[inline]
    pub fn type_name(&self) -> &'static str {
        // SAFETY: `self` is rooted and guaranteed to be live
        unsafe { self.0.type_name() }
    }

    #[inline]
    pub fn as_ptr(&self) -> GcAnyPtr {
        self.0
    }

    #[inline]
    pub(crate) fn vt(&self) -> &'static GcVtable {
        // SAFETY: rooted
        unsafe { self.as_ptr().vt() }
    }
}

impl std::fmt::Debug for GcAnyRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { (self.vt().debug)(self.as_ptr(), f) }
    }
}

/// A unique reference to a GC-managed `T`.
///
/// This type is used to certify that the reference comes from a live root,
/// which makes it safe to move around, re-root, and store as an object member.
///
/// Implements `Deref`/`DerefMut` so can be turned into a plain reference as needed.
#[repr(transparent)]
pub struct GcRefMut<'a, T: 'static>(GcPtr<T>, PhantomData<&'a mut T>);

impl<'a, T: Trace> GcRefMut<'a, T> {
    #[inline]
    pub fn map<U: Trace>(
        this: &mut Self,
        f: impl for<'v> FnOnce(&'v mut GcRefMut<'a, T>) -> &'v mut GcPtr<U>,
    ) -> GcRefMut<'a, U> {
        let ptr = *f(this);
        unsafe { ptr.as_mut() }
    }

    #[inline]
    pub fn as_ref(self) -> GcRef<'a, T> {
        GcRef(self.0, PhantomData)
    }

    #[inline]
    pub fn as_any(self) -> GcAnyRefMut<'a> {
        GcAnyRefMut(self.0.as_any(), PhantomData)
    }
}

impl<'a, T: Trace> GcRefMut<'a, T> {
    #[inline]
    pub(crate) unsafe fn new_unchecked(ptr: GcPtr<T>) -> Self {
        Self(ptr, PhantomData)
    }

    #[inline]
    pub fn as_ptr(&self) -> GcPtr<T> {
        self.0
    }
}

impl<'a, T: Trace> Deref for GcRefMut<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_rust_ref_very_unsafe() }
    }
}

impl<'a, T: Trace> DerefMut for GcRefMut<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_rust_ref_mut_very_unsafe() }
    }
}

#[repr(transparent)]
pub struct GcAnyRefMut<'a>(GcAnyPtr, PhantomData<fn(&'a ()) -> &'a ()>);

impl<'a> Clone for GcAnyRefMut<'a> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'a> Copy for GcAnyRefMut<'a> {}

impl<'a> GcAnyRefMut<'a> {
    #[inline]
    pub(crate) unsafe fn new_unchecked(ptr: GcAnyPtr) -> Self {
        Self(ptr, PhantomData)
    }

    #[inline]
    pub fn cast<T: Trace>(self) -> Option<GcRefMut<'a, T>> {
        let this = self.0.cast()?;
        Some(GcRefMut(this, PhantomData))
    }

    #[inline]
    pub unsafe fn cast_unchecked<T: Trace>(self) -> GcRefMut<'a, T> {
        let this = unsafe { self.0.cast_unchecked() };
        GcRefMut(this, PhantomData)
    }

    #[inline]
    pub fn is<T: Trace>(&self) -> bool {
        self.0.is::<T>()
    }

    #[inline]
    pub fn as_ptr(self) -> GcAnyPtr {
        self.0
    }

    #[inline]
    pub fn as_ref(&self) -> GcAnyRef<'a> {
        GcAnyRef(self.0, PhantomData)
    }
}

impl<'a> From<()> for ValueRoot<'a> {
    #[inline(always)]
    fn from(v: ()) -> Self {
        Self::Nil
    }
}

impl<'a> From<bool> for ValueRoot<'a> {
    #[inline(always)]
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl<'a> From<i64> for ValueRoot<'a> {
    #[inline(always)]
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl<'a> From<f64> for ValueRoot<'a> {
    #[inline(always)]
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}

impl<'a> From<GcAnyRoot<'a>> for ValueRoot<'a> {
    #[inline(always)]
    fn from(v: GcAnyRoot<'a>) -> Self {
        Self::Object(v)
    }
}

#[repr(C, u64)]
pub enum ValueRoot<'a> {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
    Object(GcAnyRoot<'a>),
}

impl<'a> ValueRoot<'a> {
    #[inline]
    pub fn raw(self) -> ValueRaw {
        match self {
            ValueRoot::Nil => ValueRaw::Nil,
            ValueRoot::Bool(v) => ValueRaw::Bool(v),
            ValueRoot::Int(v) => ValueRaw::Int(v),
            ValueRoot::Float(v) => ValueRaw::Float(v),
            ValueRoot::Object(v) => ValueRaw::Object(v.as_ptr()),
        }
    }
}

impl<'a> std::fmt::Debug for ValueRoot<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Bool(v) => f.debug_tuple("Bool").field(v).finish(),
            Self::Int(v) => f.debug_tuple("Int").field(v).finish(),
            Self::Float(v) => f.debug_tuple("Float").field(v).finish(),
            Self::Object(v) => std::fmt::Debug::fmt(v, f),
        }
    }
}

#[repr(C, u64)]
pub enum ValueRef<'a> {
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
    Object(GcAnyRef<'a>),
}

impl<'a> ValueRef<'a> {
    #[inline]
    pub fn raw(self) -> ValueRaw {
        match self {
            ValueRef::Nil => ValueRaw::Nil,
            ValueRef::Bool(v) => ValueRaw::Bool(v),
            ValueRef::Int(v) => ValueRaw::Int(v),
            ValueRef::Float(v) => ValueRaw::Float(v),
            ValueRef::Object(v) => ValueRaw::Object(v.as_ptr()),
        }
    }
}

impl<'a> From<()> for ValueRef<'a> {
    #[inline(always)]
    fn from(v: ()) -> Self {
        Self::Nil
    }
}

impl<'a> From<bool> for ValueRef<'a> {
    #[inline(always)]
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl<'a> From<i64> for ValueRef<'a> {
    #[inline(always)]
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl<'a> From<f64> for ValueRef<'a> {
    #[inline(always)]
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}

impl<'a> From<GcAnyRef<'a>> for ValueRef<'a> {
    #[inline(always)]
    fn from(v: GcAnyRef<'a>) -> Self {
        Self::Object(v)
    }
}

impl<'a> std::fmt::Debug for ValueRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Bool(v) => f.debug_tuple("Bool").field(v).finish(),
            Self::Int(v) => f.debug_tuple("Int").field(v).finish(),
            Self::Float(v) => f.debug_tuple("Float").field(v).finish(),
            Self::Object(v) => std::fmt::Debug::fmt(v, f),
        }
    }
}

impl ValueRaw {
    #[inline]
    pub unsafe fn as_ref<'a>(self) -> ValueRef<'a> {
        match self {
            ValueRaw::Nil => ValueRef::Nil,
            ValueRaw::Bool(v) => ValueRef::Bool(v),
            ValueRaw::Int(v) => ValueRef::Int(v),
            ValueRaw::Float(v) => ValueRef::Float(v),
            ValueRaw::Object(v) => ValueRef::Object(v.as_ref()),
        }
    }
}

impl<'a, T: Trace> GcRef<'a, T> {
    #[inline]
    pub fn map_value(
        this: &Self,
        f: impl for<'v> FnOnce(&'v GcRef<'a, T>) -> &'v ValueRaw,
    ) -> ValueRef<'a> {
        let value = *f(this);
        unsafe { value.as_ref() }
    }

    #[inline]
    pub fn map_value_opt(
        this: &Self,
        f: impl for<'v> FnOnce(&'v GcRef<'a, T>) -> Option<&'v ValueRaw>,
    ) -> Option<ValueRef<'a>> {
        let value = f(this);
        match value {
            Some(value) => Some(unsafe { value.as_ref() }),
            None => None,
        }
    }
}

mod private {
    pub trait Sealed {}
}

pub trait Rooted<T>: private::Sealed {
    fn as_ptr(&self) -> GcPtr<T>;
}

impl<T: Trace> private::Sealed for GcRef<'_, T> {}
impl<T: Trace> Rooted<T> for GcRef<'_, T> {
    #[inline]
    fn as_ptr(&self) -> GcPtr<T> {
        GcRef::as_ptr(self)
    }
}

impl<T: Trace> private::Sealed for GcRefMut<'_, T> {}
impl<T: Trace> Rooted<T> for GcRefMut<'_, T> {
    #[inline]
    fn as_ptr(&self) -> GcPtr<T> {
        GcRefMut::as_ptr(self)
    }
}

impl<T: Trace> private::Sealed for GcRoot<'_, T> {}
impl<T: Trace> Rooted<T> for GcRoot<'_, T> {
    fn as_ptr(&self) -> GcPtr<T> {
        GcRoot::as_ptr(self)
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
//     UserData(RefMut<'a, super::value::UData>),
// }

#[macro_export]
#[doc(hidden)]
macro_rules! __let_root_unchecked {
    (unsafe in $heap:ident; $place:ident = $ptr:expr) => {
        let ptr = $ptr;
        let mut place = unsafe { $crate::gc::StackRoot::from_heap_ptr($heap, ptr) };
        let $place = unsafe {
            $crate::gc::GcRoot::__new($heap, ::core::pin::Pin::new_unchecked(&mut place))
        };
    };
}

/// Create a root on the stack.
///
/// ```rust
/// # use hebi4::{gc::{Heap, let_root_unchecked}, value::List};
/// # let heap = unsafe { &mut Heap::__testing() };
/// # let ptr = List::alloc(heap, 0);
/// let_root_unchecked!(unsafe in heap; obj = ptr);
/// ```
///
/// Rooting a dead object is immediate undefined behavior.
///
/// Inversely, _not_ rooting an allocated object may result
/// in undefined behavior, and should be well justified.
///
/// To use this correctly, you must only allocate using `alloc_no_gc`,
/// and _immediately_ root the resulting object.
///
/// ## Safety
///
/// - The object pointed to by `$ptr` must still be live
///   at the time of initialization.
pub use crate::__let_root_unchecked as let_root_unchecked;

#[macro_export]
#[doc(hidden)]
macro_rules! __reroot {
    (in $heap:ident; $place:ident = $ptr:expr) => {
        let ptr = $crate::gc::Rooted::as_ptr(&$ptr);
        $crate::gc::let_root_unchecked!(unsafe in $heap; $place = ptr);
    };
}

/// Create a root on the stack from an existing root.
///
/// Anything "rooted" may be used as the target pointer:
/// - [`GcRef`]
/// - [`GcRefMut`]
/// - [`Root`]
///
/// ```rust
/// # use hebi4::{gc::{Heap, reroot}, value::list};
/// # let heap = unsafe { &mut Heap::__testing() };
/// list!(in heap; a0 = 100);
/// reroot!(in heap; a1 = a0);
/// println!("{}", a1.as_ref(heap).capacity());
/// ```
pub use crate::__reroot as reroot;
