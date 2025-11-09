use crate::{
    codegen::opcodes::{Cap, Reg},
    module,
    vm::{
        gc::{GcPtr, GcRef, Heap, Trace, Tracer},
        value::{Function, ValueRaw},
    },
};

pub struct ClosureProto {
    pub(crate) func: GcPtr<Function>,
    pub(crate) capture_info: Box<[CaptureInfo]>,
}

impl ClosureProto {
    #[inline(never)]
    pub fn alloc(
        heap: &Heap,
        func: GcPtr<Function>,
        capture_info: &[module::CaptureInfo],
    ) -> GcPtr<Self> {
        let capture_info = capture_info
            .iter()
            .map(|i| match i {
                module::CaptureInfo::Reg(reg) => CaptureInfo::Reg(*reg),
                module::CaptureInfo::Cap(cap) => CaptureInfo::Cap(*cap),
            })
            .collect();

        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self { func, capture_info });
        })
    }
}

unsafe impl Trace for ClosureProto {
    vtable!(ClosureProto);

    unsafe fn trace(&self, tracer: &Tracer) {
        tracer.visit(self.func);
    }
}

#[derive(Clone, Copy)]
pub enum CaptureInfo {
    Reg(Reg),
    Cap(Cap),
}

impl std::fmt::Debug for GcRef<'_, ClosureProto> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ClosureProto").finish_non_exhaustive()
    }
}

pub struct Closure {
    pub(crate) func: GcPtr<Function>,
    pub(crate) captures: Box<[ValueRaw]>,
}

impl Closure {
    #[inline(never)]
    pub fn alloc(
        heap: &Heap,
        proto: GcPtr<ClosureProto>,
        captures: Box<[ValueRaw]>,
    ) -> GcPtr<Self> {
        debug_assert!(unsafe { proto.as_ref().capture_info.len() == captures.len() });

        heap.alloc_no_gc(|ptr| unsafe {
            let func = proto.as_ref().func;
            (*ptr).write(Self { func, captures });
        })
    }
}

unsafe impl Trace for Closure {
    vtable!(Closure);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        tracer.visit(self.func);
        for capture in &self.captures {
            tracer.visit_value(*capture);
        }
    }
}

impl std::fmt::Debug for GcRef<'_, Closure> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure").finish_non_exhaustive()
    }
}
