use super::*;

// Stop-the-world, mark and sweep GC.
pub(crate) unsafe fn collect(heap: *mut Heap, mut external_roots: impl ExternalRoots) {
    debug_print!("start");

    (*heap).stats.on_collect();
    mark(heap, &external_roots);
    sweep(heap, &mut external_roots);
}

// Mark:
// - For each root, add it to the worklist.
// - While the worklist is not empty:
//   - Pop an item off the worklist
//   - Mark it
//   - Trace through it, marking any linked objects
//
// Note that we do not actually use a worklist.
// Instead, we use depth-first recursive traversal.
unsafe fn mark(heap: *mut Heap, external_roots: &impl ExternalRoots) {
    debug_print!("start mark");

    let tracer = Tracer {
        _marker: PhantomData,
    };

    debug_print!("trace external roots");
    external_roots.trace(&tracer);

    debug_print!("trace stack roots");
    RootList::iter((*heap).roots(), |root| {
        let ptr = (*root).ptr;
        debug_print!("visit {:016x} ({})", ptr.into_raw().addr(), unsafe {
            ptr.vt().type_name
        });
        if ptr.is_marked() {
            debug_print!("cycle");
            return;
        }
        ptr.set_mark(true);

        unsafe {
            (ptr.vt().trace)(ptr.into_raw().as_ptr(), &tracer);
        }
    });

    debug_print!("end mark");
}

// Sweep:
// - For each allocated object:
//   - If it is marked, unmark it
//   - Otherwise, free it
//
// As we iterate through the heap, we also need to maintain
// the linked list of all allocated objects:
//
// - Store the last marked object `M`.
// - Whenever an object is freed `F`, update the header of `M` to point to `F.next`
unsafe fn sweep(heap: *mut Heap, external_roots: &mut impl ExternalRoots) {
    debug_print!("start sweep");

    let mut iter = (*heap).head.get();
    let mut last_marked: Option<NonNull<GcHeader>> = None;
    let mut freed_bytes = 0usize;

    while let Some(object) = iter {
        let (next, vt, marked) = (*object.as_ptr()).into_parts();

        if marked {
            // unmark
            GcHeader::set_mark(object, false);
            last_marked = Some(object);

            debug_print!("live {:016x} ({})", object.as_ptr().addr(), vt.type_name);
        } else {
            // unlink
            match last_marked {
                Some(prev_live) => GcHeader::set_next(prev_live, next),
                None => (*heap).head.set(next),
            }

            debug_print!("dead {:016x} ({})", object.as_ptr().addr(), vt.type_name);

            unsafe {
                freed_bytes += (vt.free)(object.as_ptr().cast::<()>());
            }
        }

        iter = next;
    }

    external_roots.clear_dead_roots();

    (*heap).stats.on_free(freed_bytes);

    debug_print!("end sweep");
}

// NOTE: This is called in `drop`, so we don't need to clear dead roots. They are inaccessible after this.
pub(crate) unsafe fn free_all(heap: *mut Heap) {
    debug_print!("drop");

    let mut iter = (*heap).head.get();
    let mut freed_bytes = 0usize;

    while let Some(object) = iter {
        let (next, vt, marked) = (*object.as_ptr()).into_parts();

        debug_print!("free {:016x} ({})", object.as_ptr().addr(), vt.type_name);

        unsafe {
            freed_bytes += (vt.free)(object.as_ptr().cast::<()>());
        }

        iter = next;
    }

    (*heap).stats.on_free(freed_bytes);
}
