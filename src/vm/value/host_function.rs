use std::{marker::PhantomData, rc::Rc};

use crate::{
    Error,
    codegen::opcodes::{Reg, Sp, Vm},
    error::{Result, error, error_span},
    gc::{GcRoot, GcUninitRoot, ValueRoot},
    module::{
        TryIntoHebiValueRaw,
        native::{Ret, TryIntoHebiArgs},
    },
    span::Span,
    value::{Closure, Function},
    vm::{
        CallFrame, Invariant, Stdio, VmError, dispatch_loop,
        gc::{GcPtr, GcRef, Heap, Trace},
        maybe_grow_stack, prepare_call, prepare_closure_call,
        value::{Str, ValueRaw},
    },
};

pub struct Context<'a> {
    vm: Vm,
    stack_base: usize,
    nargs: u8,
    // _lifetime: PhantomData<&'a ()>,
    _lifetime: Invariant<'a>,
}

impl<'a> Context<'a> {
    pub(crate) fn new(vm: Vm, stack_base: usize, nargs: u8) -> Self {
        Self {
            vm,
            stack_base,
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
        self.sp().at(Reg::new_unchecked(i + 1)).read()
    }

    #[doc(hidden)]
    #[inline]
    pub unsafe fn __unsafe_clone(&self) -> Self {
        Self {
            vm: self.vm,
            stack_base: self.stack_base,
            nargs: self.nargs,
            _lifetime: PhantomData,
        }
    }

    #[inline]
    pub(crate) fn sp(&self) -> Sp {
        unsafe { self.vm.stack_at(self.stack_base) }
    }

    #[inline]
    pub fn call<'v, Args: TryIntoHebiArgs>(
        &mut self,
        callee: &ValueRoot,
        args: Args,
        ret: GcUninitRoot<'v>,
    ) -> Result<ValueRoot<'v>> {
        let vm = self.vm;

        unsafe {
            // 1. allocate stack space for ret and args
            // new call frame will be above the current one, without overlap
            let current_callee = self
                .vm
                .current_frame()
                .callee()
                .into_host()
                .unwrap_unchecked();

            // the slot after args:
            //   [prev_frame.ret, a0, a1, .., aN, new_frame.ret]
            //                                    ^^^^^^^^^^^^^
            let ret_reg = Reg::new_unchecked(current_callee.as_ref().arity + 1);
            let nargs = Args::LEN;

            // context: type mismatch between the args. it seems like an off by one?

            // 2. do a generic call
            if let Some(callee) = callee.raw().into_object::<Function>() {
                if callee.as_ref().nparams != nargs {
                    return error(format!(
                        "invalid number of args, expected {} but got {}",
                        callee.as_ref().nparams,
                        nargs
                    ))
                    .into();
                }

                let (sp, ip) = prepare_call(callee, ret_reg, vm, 0);
                let stack_base = sp.ret().offset_from_unsigned(vm.stack_at(0).ret());

                let mut cx = Context::new(self.vm, stack_base, nargs);
                args.try_into_hebi_args(&mut cx)?;

                match dispatch_loop(vm, ip, stack_base) {
                    Ok(value) => {
                        debug_assert!(
                            vm.current_frame()
                                .callee()
                                .into_host()
                                .is_some_and(|c| { c.into_raw() == current_callee.into_raw() })
                        );

                        Ok(value.root(&mut *vm.heap(), ret))
                    }
                    Err(err) => {
                        let err = vm.take_error(err);
                        vm.unwind();
                        debug_assert!(
                            vm.current_frame()
                                .callee()
                                .into_host()
                                .is_some_and(|c| { c.into_raw() == current_callee.into_raw() })
                        );

                        Err(err)
                    }
                }
            } else if let Some(callee) = callee.raw().into_object::<Closure>() {
                if callee.as_ref().func.as_ref().nparams != nargs {
                    return error(format!(
                        "invalid number of args, expected {} but got {}",
                        callee.as_ref().func.as_ref().nparams,
                        nargs
                    ))
                    .into();
                }

                let (sp, ip) = prepare_closure_call(callee, ret_reg, vm, 0);
                let stack_base = sp.ret().offset_from_unsigned(vm.stack_at(0).ret());

                let mut cx = Context::new(self.vm, stack_base, nargs);
                args.try_into_hebi_args(&mut cx)?;

                match dispatch_loop(vm, ip, stack_base) {
                    Ok(value) => {
                        debug_assert!(
                            vm.current_frame()
                                .callee()
                                .into_host()
                                .is_some_and(|c| { c.into_raw() == current_callee.into_raw() })
                        );

                        Ok(value.root(&mut *vm.heap(), ret))
                    }
                    Err(err) => {
                        let err = vm.take_error(err);
                        vm.unwind();
                        debug_assert!(
                            vm.current_frame()
                                .callee()
                                .into_host()
                                .is_some_and(|c| { c.into_raw() == current_callee.into_raw() })
                        );

                        Err(err)
                    }
                }
            } else if let Some(callee) = callee.raw().into_object::<HostFunction>() {
                if callee.as_ref().arity != nargs {
                    return error(format!(
                        "invalid number of args, expected {} but got {}",
                        callee.as_ref().arity,
                        nargs
                    ))
                    .into();
                }

                maybe_grow_stack(
                    vm,
                    vm.current_frame().stack_base() as usize + ret_reg.zx(),
                    1 + nargs as usize,
                );

                let mut cx = {
                    let stack_base = vm.current_frame().stack_base() + (ret_reg.get() as u32);
                    let return_addr = 0; // return to host
                    debug_assert!(vm.has_enough_stack_space(
                        stack_base as usize,
                        1 + callee.as_ref().arity as usize
                    ));
                    vm.push_frame(CallFrame::host(callee, stack_base, return_addr));
                    Context::new(vm, stack_base as usize, nargs)
                };

                args.try_into_hebi_args(&mut cx)?;

                let result = (callee.as_ref().f)(cx);
                match result {
                    Ok(value) => {
                        debug_assert!(
                            vm.current_frame()
                                .callee()
                                .into_host()
                                .is_some_and(|c| { c.into_raw() == callee.into_raw() })
                        );
                        vm.pop_frame_unchecked();
                        debug_assert!(
                            vm.current_frame()
                                .callee()
                                .into_host()
                                .is_some_and(|c| { c.into_raw() == current_callee.into_raw() })
                        );

                        Ok(value.root(&mut *vm.heap(), ret))
                    }
                    Err(err) => {
                        vm.unwind();
                        debug_assert!(
                            vm.current_frame()
                                .callee()
                                .into_host()
                                .is_some_and(|c| { c.into_raw() == callee.into_raw() }),
                            "expected frame {} but got {}",
                            callee.as_ref().name.as_ref().as_str(),
                            vm.current_frame().name().as_ref().as_str(),
                        );
                        vm.pop_frame_unchecked();
                        debug_assert!(
                            vm.current_frame()
                                .callee()
                                .into_host()
                                .is_some_and(|c| { c.into_raw() == current_callee.into_raw() })
                        );

                        let err = vm.take_error(VmError::Host);
                        return Err(err);
                    }
                }
            } else {
                return error("this value cannot be called").into();
            }
        }
    }

    /// Return a managed value.
    ///
    /// Note that just calling this is not enough: The resulting `Ret`
    /// type _must_ be returned from the function.
    #[inline]
    pub fn ret<T>(mut self, v: T) -> Result<Ret<'a>>
    where
        T: TryIntoHebiValueRaw,
    {
        // SAFETY: rooted by `v` while being written,
        // and by stack afterwards.
        unsafe {
            let v = v.try_into_hebi_value_raw(&mut self)?;
            *self.sp().ret() = v;
            Ok(Ret::new())
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

    #[doc(hidden)]
    #[inline]
    pub unsafe fn __write_error(&mut self, error: Error) {
        unsafe { self.vm.write_error(error) }
    }
}

impl std::ops::Deref for Context<'_> {
    type Target = Heap;

    fn deref(&self) -> &Self::Target {
        self.heap()
    }
}

impl std::ops::DerefMut for Context<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.heap_mut()
    }
}

pub(crate) type HostFunctionCallback =
    Rc<dyn for<'a> Fn(Context<'a>) -> Result<ValueRaw, ()> + 'static>;

pub(crate) type HostFunctionCallbackRaw = for<'a> fn(Context<'a>) -> Result<ValueRaw, ()>;

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
        heap: &Heap,
        root: GcUninitRoot<'a>,
        name: &GcRoot<'a, Str>,
        arity: u8,
        f: HostFunctionCallback,
    ) -> GcRoot<'a, Self> {
        let ptr = Self::alloc(heap, name.as_ptr(), arity, f);
        unsafe { root.init_raw(ptr) }
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
