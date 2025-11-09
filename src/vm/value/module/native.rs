use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

use crate::{
    gc::GcAnyPtr,
    vm::{
        gc::{GcPtr, GcRef, Trace},
        value::{Str, host_function::HostFunction},
    },
};

pub struct NativeModuleProto {
    pub(crate) name: GcPtr<Str>,
    pub(crate) functions: Box<[GcPtr<HostFunction>]>,
    pub(crate) functions_by_name: HashMap<String, usize, FxBuildHasher>,
}

impl<'a> GcRef<'a, NativeModuleProto> {
    pub fn name(&self) -> GcRef<'a, Str> {
        GcRef::map(self, |this| &this.name)
    }

    pub(crate) unsafe fn get(&self, name: GcPtr<Str>) -> Option<GcAnyPtr> {
        // TODO: string interning
        let idx = self
            .functions_by_name
            .get(name.as_ref().as_str())
            .copied()?;
        unsafe {
            let func = *self.functions.get_unchecked(idx);
            Some(func.as_any())
        }
    }
}

unsafe impl Trace for NativeModuleProto {
    vtable!(NativeModuleProto);

    unsafe fn trace(&self, tracer: &crate::vm::gc::Tracer) {
        tracer.visit(self.name);
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
