use super::*;
use crate::vm::value::*;

// changing the set of possible root kinds also means
// having to change everything here to support it
fn _update_reminder(v: ObjectType) {
    match v {
        ObjectType::List => {}
        ObjectType::Table => {}
        ObjectType::Closure => {}
        ObjectType::UData => {}
    }
}

// Stop-the-world, mark and sweep GC.
pub fn collect(heap: &mut Heap) {
    unsafe {
        mark(heap);
        sweep(heap);
    }
}

#[inline]
unsafe fn iter_roots<F>(heap: &mut Heap, mut f: F)
where
    F: FnMut(*mut StackRoot<()>),
{
    RootList::iter(heap.roots(), |root| f(root.cast::<StackRoot<()>>()));
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
unsafe fn mark(heap: &mut Heap) {
    let tracer = Tracer {
        _marker: PhantomData,
    };

    // TODO: reduce duplication (?)
    iter_roots(heap, |root| {
        let ptr = (*root).ptr;
        if ptr.is_marked() {
            return;
        }
        ptr.mark();

        match ptr.type_() {
            ObjectType::List => ptr.cast::<List>().trace(&tracer),
            ObjectType::Table => ptr.cast::<Table>().trace(&tracer),
            ObjectType::Closure => ptr.cast::<Closure>().trace(&tracer),
            ObjectType::UData => ptr.cast::<UData>().trace(&tracer),
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

    while !iter.is_null() {
        let (next, kind, marked) = (*iter).into_parts();

        if marked {
            GcHeader::set_mark(iter, false);
            last_marked = Some(iter);
        } else {
            match last_marked {
                Some(prev_live) => GcHeader::set_next(prev_live, next),
                None => heap.head.set(next),
            }

            match kind {
                ObjectType::List => {
                    let _ = Box::from_raw(iter.cast::<GcBox<List>>());
                }
                ObjectType::Table => {
                    let _ = Box::from_raw(iter.cast::<GcBox<Table>>());
                }
                ObjectType::Closure => {
                    let _ = Box::from_raw(iter.cast::<GcBox<Closure>>());
                }
                ObjectType::UData => {
                    let _ = Box::from_raw(iter.cast::<GcBox<UData>>());
                }
            }
        }

        iter = next;
    }
}
