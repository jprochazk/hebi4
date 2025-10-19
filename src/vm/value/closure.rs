use super::super::gc::{Trace, Tracer};

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
