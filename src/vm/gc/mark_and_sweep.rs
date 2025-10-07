use super::*;

// Stop-the-world, mark and sweep GC.
pub(crate) fn collect(heap: &mut Heap, external_roots: &dyn ExternalRoots) {
    heap.stats.collect();
    unsafe {
        mark(heap, external_roots);
        sweep(heap);
    }
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
unsafe fn mark(heap: &mut Heap, external_roots: &dyn ExternalRoots) {
    let tracer = Tracer {
        _marker: PhantomData,
    };

    external_roots.trace(&tracer);

    RootList::iter(heap.roots(), |root| {
        let ptr = (*root).ptr;
        if ptr.is_marked() {
            return;
        }
        ptr.set_mark(true);

        unsafe {
            (ptr.vt().trace)(ptr.get_raw(), &tracer);
        }
    });
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
unsafe fn sweep(heap: &mut Heap) {
    let mut iter = heap.head.get();
    let mut last_marked: Option<*mut GcHeader> = None;
    let mut freed_bytes = 0usize;

    while !iter.is_null() {
        let (next, vt, marked) = (*iter).into_parts();

        if marked {
            // unmark
            GcHeader::set_mark(iter, false);
            last_marked = Some(iter);
        } else {
            // unlink
            match last_marked {
                Some(prev_live) => GcHeader::set_next(prev_live, next),
                None => heap.head.set(next),
            }

            unsafe {
                freed_bytes += (vt.free)(iter.cast::<()>());
            }
        }

        iter = next;
    }

    heap.stats.free(freed_bytes);
}

pub(crate) unsafe fn free_all(heap: &mut Heap) {
    let mut iter = heap.head.get();
    let mut freed_bytes = 0usize;

    while !iter.is_null() {
        let (next, vt, marked) = (*iter).into_parts();

        unsafe {
            freed_bytes += (vt.free)(iter.cast::<()>());
        }

        iter = next;
    }

    heap.stats.free(freed_bytes);
}
