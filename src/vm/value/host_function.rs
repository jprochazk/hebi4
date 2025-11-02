use std::marker::PhantomData;

use crate::codegen::opcodes::{Reg, Sp, Vm};
use crate::error::{Result, error};
use crate::gc::{GcPtr, GcRef, Heap, Trace, ValueRef};
use crate::span::Span;
use crate::value::{String, ValueRaw};
use crate::vm::{Invariant, Stdio};

pub struct Context<'a> {
    vm: Vm,
    sp: Sp,
    nargs: u8,
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

    pub fn args<const N: usize>(&self) -> Result<[ValueRef<'a>; N]> {
        if (self.nargs as usize) < N {
            // TODO: span here
            return error("invalid number of arguments", Span::empty()).into();
        }

        let mut args = [const { ValueRef::Nil }; N];
        for i in 0..N {
            unsafe {
                args[i] = self.arg_unchecked(i as u8);
            }
        }
        Ok(args)
    }

    pub fn arg(&self, i: u8) -> Option<ValueRef<'a>> {
        if i >= self.nargs {
            return None;
        }

        Some(unsafe { self.sp.at(Reg::new_unchecked(i + 1)).read().as_ref() })
    }

    pub unsafe fn arg_unchecked(&self, i: u8) -> ValueRef<'a> {
        self.sp.at(Reg::new_unchecked(i + 1)).read().as_ref()
    }

    pub fn stdio(&mut self) -> &mut Stdio {
        unsafe { &mut *self.vm.stdio() }
    }
}

pub type HostFunctionCallback = for<'a> fn(Context<'a>) -> Result<ValueRaw>;

pub struct HostFunction {
    pub(crate) name: GcPtr<String>,
    pub(crate) arity: u8,
    pub(crate) f: HostFunctionCallback,
}

impl HostFunction {
    #[inline(never)]
    pub fn alloc(
        heap: &Heap,
        name: GcPtr<String>,
        arity: u8,
        f: HostFunctionCallback,
    ) -> GcPtr<Self> {
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self { name, arity, f });
        })
    }
}

unsafe impl Trace for HostFunction {
    vtable!(HostFunction);

    unsafe fn trace(&self, tracer: &crate::gc::Tracer) {
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
