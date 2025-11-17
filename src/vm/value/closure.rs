pub use crate::module::UpvalueDescriptor;
use crate::{
    module,
    vm::{
        gc::{GcPtr, GcRef, Heap, Trace, Tracer},
        value::{Function, ValueRaw},
    },
};

pub struct ClosureProto {
    pub(crate) func: GcPtr<Function>,
    pub(crate) upvalues: Box<[UpvalueDescriptor]>,
}

impl ClosureProto {
    #[inline(never)]
    pub fn alloc(
        heap: &Heap,
        func: GcPtr<Function>,
        upvalues: &[module::UpvalueDescriptor],
    ) -> GcPtr<Self> {
        let upvalues = upvalues.into();

        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self { func, upvalues });
        })
    }
}

unsafe impl Trace for ClosureProto {
    vtable!(ClosureProto);

    unsafe fn trace(&self, tracer: &Tracer) {
        tracer.visit(self.func);
    }
}

impl std::fmt::Debug for GcRef<'_, ClosureProto> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ClosureProto").finish_non_exhaustive()
    }
}

pub struct Closure {
    pub(crate) func: GcPtr<Function>,
    pub(crate) upvalues: Box<[ValueRaw]>,
}

impl Closure {
    #[inline(never)]
    pub fn alloc(heap: &Heap, proto: GcPtr<ClosureProto>) -> GcPtr<Self> {
        let upvalues =
            vec![ValueRaw::Nil; unsafe { proto.as_ref().upvalues.len() }].into_boxed_slice();

        heap.alloc_no_gc(|ptr| unsafe {
            let func = proto.as_ref().func;
            (*ptr).write(Self { func, upvalues });
        })
    }
}

impl<'a> GcRef<'a, Closure> {
    pub fn func(&self) -> GcRef<'a, Function> {
        GcRef::map(self, |this| &this.func)
    }
}

unsafe impl Trace for Closure {
    vtable!(Closure);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        tracer.visit(self.func);
        for uv in &self.upvalues {
            tracer.visit_value(*uv);
        }
    }
}

impl std::fmt::Debug for GcRef<'_, Closure> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure")
            .field("name", &self.func().name())
            .finish_non_exhaustive()
    }
}
