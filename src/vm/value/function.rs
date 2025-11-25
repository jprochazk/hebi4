use std::{ptr::NonNull, sync::Arc};

use super::Str;
use crate::{
    codegen::opcodes::{Insn, Ip, Lp},
    module::FuncDebugInfo,
    vm::{
        gc::{GcPtr, GcRef, GcRefMut, Trace},
        value::{ModuleProto, ValueRaw},
    },
};

#[repr(align(16))]
pub struct Function {
    pub(crate) name: GcPtr<Str>,
    pub(crate) nparams: u8,
    pub(crate) nstack: u8,
    pub(crate) code: Box<[Insn]>,
    pub(crate) literals: Box<[ValueRaw]>,
    pub(crate) module: GcPtr<ModuleProto>,
    pub(crate) dbg: Option<Arc<FuncDebugInfo>>,
}

impl<'a> GcRef<'a, Function> {
    #[inline]
    pub fn name(&self) -> GcRef<'a, Str> {
        GcRef::map(self, |this| &this.name)
    }

    #[inline]
    pub(crate) fn arity(&self) -> usize {
        self.nparams as usize
    }

    #[inline]
    pub(crate) fn stack_size(&self) -> usize {
        self.nstack as usize
    }

    #[inline]
    pub(crate) fn code(&self) -> &[Insn] {
        &self.code[..]
    }

    #[inline]
    pub(crate) fn literals(&self) -> &[ValueRaw] {
        &self.literals[..]
    }

    #[inline]
    pub(crate) fn module(&self) -> GcRef<'a, ModuleProto> {
        GcRef::map(self, |this| &this.module)
    }

    #[inline]
    pub(crate) fn dbg(&self) -> Option<&FuncDebugInfo> {
        match &self.dbg {
            Some(dbg) => Some(&**dbg),
            None => None,
        }
    }
}

impl<'a> GcRefMut<'a, Function> {
    #[inline]
    pub(crate) fn code_mut(&mut self) -> &mut [Insn] {
        &mut self.code[..]
    }
}

impl Function {
    #[inline]
    pub(crate) unsafe fn code_raw(ptr: GcPtr<Function>) -> Ip {
        let ptr = &raw mut *(*ptr.into_raw().as_ptr()).code;
        let ptr = ptr as *mut Insn;
        Ip(NonNull::new_unchecked(ptr))
    }

    #[inline]
    pub(crate) unsafe fn literals_raw(ptr: GcPtr<Function>) -> Lp {
        let ptr: *mut [ValueRaw] = &raw mut *(*ptr.into_raw().as_ptr()).literals;
        let ptr = ptr as *mut ValueRaw;
        Lp(NonNull::new_unchecked(ptr))
    }
}

unsafe impl Trace for Function {
    vtable!(Function);

    unsafe fn trace(&self, tracer: &crate::vm::gc::Tracer) {
        tracer.visit(self.name);
        for literal in self.literals.iter().copied() {
            tracer.visit_value(literal);
        }
        // almost certainly this has already been visited, but just in case:
        tracer.visit(self.module);
    }
}

impl std::fmt::Debug for GcRef<'_, Function> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // SAFETY: interior pointers are transitively rooted
        f.debug_struct("Function")
            .field("name", &self.name())
            .field("nparams", &self.nparams)
            .field("nstack", &self.nstack)
            .field("code", &self.code.len())
            .field("literals", &self.literals.len())
            .field("module", &self.module().name())
            .finish()
    }
}
