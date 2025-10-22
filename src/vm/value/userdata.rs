use crate::gc::{GcRef, Trace, Tracer};

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

impl std::fmt::Debug for GcRef<'_, UserData> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UserData").finish_non_exhaustive()
    }
}
