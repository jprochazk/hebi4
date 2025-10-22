use crate::gc::{GcRef, Trace, Tracer};

pub struct Closure {
    // TODO
}

unsafe impl Trace for Closure {
    vtable!(Closure);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        todo!()
    }
}

impl std::fmt::Debug for GcRef<'_, Closure> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure").finish_non_exhaustive()
    }
}
