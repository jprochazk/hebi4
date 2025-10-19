use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

use super::{FunctionProto, ValueRaw};
use crate::{
    codegen::opcodes::{FnId, Insn, Reg, asm},
    module::{FuncInfo, Literal, Module},
    vm::gc::{Gc, Heap, Ref, RefMut, Root, Trace},
    vm::value::String,
};

pub struct ModuleProto {
    pub(crate) module_id: ModuleId,
    pub(crate) entrypoint: Option<Gc<FunctionProto>>,
    pub(crate) functions: Box<[Gc<FunctionProto>]>,
    pub(crate) module_vars: Box<[ValueRaw]>,
}

impl<'a> Ref<'a, ModuleProto> {
    #[inline]
    pub(crate) fn get_function(&self, id: FnId) -> Option<Ref<'a, FunctionProto>> {
        Ref::map_opt(self, |this| this.functions.get(id.zx()))
    }

    #[inline]
    pub(crate) unsafe fn get_function_unchecked(&self, id: FnId) -> Ref<'a, FunctionProto> {
        Ref::map(self, |this| unsafe {
            this.functions.get_unchecked(id.zx())
        })
    }

    #[inline]
    pub(crate) fn entrypoint(&self) -> Ref<'a, FunctionProto> {
        Ref::map(self, |this| match &this.entrypoint {
            Some(entrypoint) => entrypoint,
            // SAFETY: after initialization, `entrypoint` is never `None`
            None => unsafe { core::hint::unreachable_unchecked() },
        })
    }

    #[inline]
    pub(crate) fn functions(&self) -> Functions<'a> {
        Functions {
            module: *self,
            index: 0,
        }
    }
}

pub(crate) struct Functions<'a> {
    module: Ref<'a, ModuleProto>,
    index: usize,
}

impl<'a> Iterator for Functions<'a> {
    type Item = Ref<'a, FunctionProto>;

    fn next(&mut self) -> Option<Self::Item> {
        Ref::map_opt(&self.module, |module| {
            match module.functions.get(self.index) {
                Some(function) => {
                    self.index += 1;
                    Some(function)
                }
                None => None,
            }
        })
    }
}

impl<'a> RefMut<'a, ModuleProto> {}

unsafe impl Trace for ModuleProto {
    vtable!(ModuleProto);

    unsafe fn trace(&self, tracer: &crate::vm::gc::Tracer) {
        tracer.visit(self.entrypoint.unwrap_unchecked());

        for function in self.functions.iter().copied() {
            tracer.visit(function);
        }

        for v in self.module_vars.iter().copied() {
            tracer.visit_value(v);
        }
    }
}

#[derive(Default)]
pub(crate) struct ModuleRegistry {
    modules: Vec<Gc<ModuleProto>>,
    by_name: HashMap<std::string::String, ModuleId, FxBuildHasher>,
}

impl ModuleRegistry {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn add(&mut self, heap: &mut Heap, module: &Module) -> Gc<ModuleProto> {
        let id = ModuleId(self.modules.len() as u32);
        let name = module.name().to_owned();
        let module = canonicalize(heap, module, id);

        self.modules.push(module);
        self.by_name.insert(name, id);

        module
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = Gc<ModuleProto>> {
        self.modules.iter().copied()
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub(crate) struct ModuleId(u32);

// `ret`, assumes that it always returns _into_ something.
// That allows `ret` to `pop_frame_unchecked`, removing a
// branch from a very hot code path.
// For the `main` entrypoint of a module, this is what it
// will `ret` into. All it does is call the module's `main`,
// and then halt the interpreter.
const ENTRYPOINT: [Insn; 2] = unsafe {
    [
        // call the entrypoint
        asm::fastcall(Reg::new_unchecked(0), FnId::new_unchecked(0)),
        // halt the VM
        asm::stop(),
    ]
};

fn canonicalize(heap: &mut Heap, info: &Module, id: ModuleId) -> Gc<ModuleProto> {
    let_root_unchecked!(unsafe in heap; module = heap.alloc_no_gc(|ptr| unsafe {
        (*ptr).write(ModuleProto {
            module_id: id,
            entrypoint: None,
            functions: Box::new([]),
            // TODO: module vars
            module_vars: Box::new([ValueRaw::Nil; 0]),
        });
    }));

    let entrypoint = generate_entrypoint(heap, info.name(), &module);
    module.as_mut(heap).entrypoint = Some(entrypoint);

    let mut functions = Vec::new();
    functions.reserve_exact(info.functions().len());
    for function in info.functions() {
        let function = canonicalize_function(heap, function, &module);
        functions.push(function);
    }
    let functions = functions.into_boxed_slice();

    module.as_mut(heap).functions = functions;

    module.as_ptr()
}

fn canonicalize_function(
    heap: &mut Heap,
    function: &FuncInfo,
    module: &Root<'_, ModuleProto>,
) -> Gc<FunctionProto> {
    string!(in heap; name = function.name());

    let name = name.as_ptr();
    let code = function.code().into();
    let literals = canonicalize_literals(heap, function.literals());
    let module = module.as_ptr();

    let_root_unchecked!(unsafe in heap; function = heap.alloc_no_gc(|ptr| unsafe {
        (*ptr).write(FunctionProto {
            name,
            nparams: 0,
            nstack: 1,
            code,
            literals,
            module,
            dbg: None,
        });
    }));

    function.as_ptr()
}

fn canonicalize_literals(heap: &mut Heap, literals: &[Literal]) -> Box<[ValueRaw]> {
    let mut out = Vec::new();
    out.reserve_exact(literals.len());
    for literal in literals {
        let value = match literal {
            Literal::Nil => ValueRaw::Nil,
            Literal::Bool(v) => ValueRaw::Bool(*v),
            Literal::Int(v) => ValueRaw::Int(*v),
            Literal::Float(v) => ValueRaw::Float(*v),
            Literal::String(v) => ValueRaw::String(String::alloc(heap, v.as_str())),
        };
        out.push(value);
    }
    out.into_boxed_slice()
}

fn generate_entrypoint(
    heap: &Heap,
    name: &str,
    module: &Root<'_, ModuleProto>,
) -> Gc<FunctionProto> {
    string!(in heap; name = &format!("{name}#start"));
    let_root_unchecked!(unsafe in heap; f = heap.alloc_no_gc(|ptr| unsafe {
        (*ptr).write(FunctionProto {
            name: name.as_ptr(),
            nparams: 0,
            nstack: 1,
            code: Box::new(ENTRYPOINT),
            literals: Box::new([]),
            module: module.as_ptr(),
            dbg: None,
        });
    }));

    f.as_ptr()
}
