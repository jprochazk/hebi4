use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

use crate::vm::{
    gc::{GcPtr, GcRef, Trace},
    value::{Str, host_function::HostFunction},
};

pub struct NativeModuleProto {
    name: GcPtr<Str>,
    functions: Vec<GcPtr<HostFunction>>,
    functions_by_name: HashMap<String, usize, FxBuildHasher>,
}

impl<'a> GcRef<'a, NativeModuleProto> {
    pub fn name(&self) -> GcRef<'a, Str> {
        GcRef::map(self, |this| &this.name)
    }
}

unsafe impl Trace for NativeModuleProto {
    vtable!(NativeModuleProto);

    unsafe fn trace(&self, tracer: &crate::vm::gc::Tracer) {
        for f in &self.functions {
            tracer.visit(*f);
        }
    }
}

impl std::fmt::Debug for GcRef<'_, NativeModuleProto> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeModuleProto")
            .field("name", &self.name())
            .finish()
    }
}
