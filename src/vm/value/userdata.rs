use super::super::gc::{Trace, Tracer};

pub struct UserData {
    // TODO
}

unsafe impl Trace for UserData {
    vtable!(UserData);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        _ = tracer;
    }
}
