use std::sync::Arc;

use super::String;
use crate::{
    ValueRaw,
    codegen::opcodes::Insn,
    module::FuncDebugInfo,
    vm::{
        gc::{Gc, Ref, RefMut, Trace},
        value::ModuleProto,
    },
};

#[repr(align(16))]
pub struct FunctionProto {
    pub(crate) name: Gc<String>,
    pub(crate) nparams: u8,
    pub(crate) nstack: u8,
    pub(crate) code: Box<[Insn]>,
    pub(crate) literals: Box<[ValueRaw]>,
    pub(crate) module: Gc<ModuleProto>,
    pub(crate) dbg: Option<Arc<FuncDebugInfo>>,
}

impl<'a> Ref<'a, FunctionProto> {
    #[inline]
    pub fn name(&self) -> Ref<'a, String> {
        Ref::map(self, |this| &this.name)
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
    pub(crate) fn module(&self) -> Ref<'a, ModuleProto> {
        Ref::map(self, |this| &this.module)
    }

    #[inline]
    pub(crate) fn dbg(&self) -> Option<&FuncDebugInfo> {
        match &self.dbg {
            Some(dbg) => Some(&**dbg),
            None => None,
        }
    }
}

impl<'a> RefMut<'a, FunctionProto> {
    #[inline]
    pub(crate) fn code_mut(&mut self) -> &mut [Insn] {
        &mut self.code[..]
    }
}

unsafe impl Trace for FunctionProto {
    vtable!(FunctionProto);

    unsafe fn trace(&self, tracer: &crate::vm::gc::Tracer) {
        tracer.visit(self.name);
        for literal in self.literals.iter().copied() {
            tracer.visit_value(literal);
        }
        // almost certainly this has already been visited, but just in case:
        tracer.visit(self.module);
    }
}
