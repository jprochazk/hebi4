use std::{marker::PhantomData, rc::Rc};

use crate::{
    codegen::opcodes::{Reg, Sp, Vm},
    error::{Result, error_span},
    gc::{GcRoot, GcUninitRoot},
    span::Span,
    vm::{
        Invariant, Stdio,
        gc::{GcPtr, GcRef, Heap, Trace},
        value::{Str, ValueRaw},
    },
};

pub struct Context<'a> {
    vm: Vm,
    sp: Sp,
    nargs: u8,
    // _lifetime: PhantomData<&'a ()>,
    _lifetime: Invariant<'a>,
}

impl<'a> Context<'a> {
    pub(crate) fn new(vm: Vm, sp: Sp, nargs: u8) -> Self {
        Self {
            vm,
            sp,
            nargs,
            _lifetime: PhantomData,
        }
    }

    #[inline]
    pub(crate) fn args<const N: usize>(&self) -> Result<[ValueRaw; N]> {
        if (self.nargs as usize) < N {
            // TODO: span here
            return error_span("invalid number of arguments", Span::empty()).into();
        }

        let mut args = [const { ValueRaw::Nil }; N];
        for i in 0..N {
            unsafe {
                args[i] = self.arg_unchecked(i as u8);
            }
        }
        Ok(args)
    }

    #[inline]
    pub(crate) fn arg(&self, i: u8) -> Option<ValueRaw> {
        if i >= self.nargs {
            return None;
        }

        Some(unsafe { self.arg_unchecked(i) })
    }

    #[inline]
    pub(crate) unsafe fn arg_unchecked(&self, i: u8) -> ValueRaw {
        self.sp.at(Reg::new_unchecked(i + 1)).read()
    }

    #[inline]
    pub(crate) fn private_clone(&self) -> Self {
        Self {
            vm: self.vm,
            sp: self.sp,
            nargs: self.nargs,
            _lifetime: PhantomData,
        }
    }

    #[inline]
    pub fn heap_mut(&mut self) -> &mut Heap {
        unsafe { &mut *self.vm.heap() }
    }

    #[inline]
    pub fn heap(&self) -> &Heap {
        unsafe { &*self.vm.heap() }
    }

    #[inline]
    pub fn stdio(&mut self) -> &mut Stdio {
        unsafe { &mut *self.vm.stdio() }
    }
}

pub type HostFunctionCallback = Rc<dyn for<'a> Fn(Context<'a>) -> Result<ValueRaw> + 'static>;

pub type HostFunctionCallbackRaw = for<'a> fn(Context<'a>) -> Result<ValueRaw>;

pub struct HostFunction {
    pub(crate) name: GcPtr<Str>,
    pub(crate) arity: u8,
    pub(crate) f: HostFunctionCallback,
}

impl HostFunction {
    #[inline(never)]
    pub fn alloc(heap: &Heap, name: GcPtr<Str>, arity: u8, f: HostFunctionCallback) -> GcPtr<Self> {
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self { name, arity, f });
        })
    }

    #[inline(never)]
    pub fn new<'a>(
        heap: &mut Heap,
        root: GcUninitRoot<'a>,
        name: &GcRoot<'a, Str>,
        arity: u8,
        f: HostFunctionCallback,
    ) -> GcRoot<'a, Self> {
        let ptr = Self::alloc(heap, name.as_ptr(), arity, f);
        unsafe { root.init_raw(heap, ptr) }
    }
}

unsafe impl Trace for HostFunction {
    vtable!(HostFunction);

    unsafe fn trace(&self, tracer: &crate::vm::gc::Tracer) {
        tracer.visit(self.name);
    }
}

impl std::fmt::Debug for GcRef<'_, HostFunction> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = GcRef::map(self, |this| &this.name);
        f.debug_struct("HostFunction")
            .field("name", &name)
            .finish_non_exhaustive()
    }
}
