mod native;

use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

use super::{Function, ValueRaw};
use crate::{
    codegen::opcodes::{FnId, Insn, Reg, asm},
    gc::Tracer,
    module::{FuncInfo, Literal, Module, NativeModule},
    value::host_function::HostFunction,
    vm::{
        gc::{GcPtr, GcRef, GcRefMut, GcRoot, GcUninitRoot, Heap, Trace, let_root},
        value::{Str, closure::ClosureProto, module::native::NativeModuleProto},
    },
};

pub struct ModuleProto {
    pub(crate) name: GcPtr<Str>,
    pub(crate) entrypoint: Option<GcPtr<Function>>,
    pub(crate) functions: Box<[GcPtr<Function>]>,
    pub(crate) module_vars: Box<[ValueRaw]>,
}

impl<'a> GcRef<'a, ModuleProto> {
    #[inline]
    pub(crate) fn get_function(&self, id: FnId) -> Option<GcRef<'a, Function>> {
        GcRef::map_opt(self, |this| this.functions.get(id.zx()))
    }

    #[inline]
    pub(crate) unsafe fn get_function_unchecked(&self, id: FnId) -> GcRef<'a, Function> {
        GcRef::map(self, |this| unsafe {
            this.functions.get_unchecked(id.zx())
        })
    }

    #[inline]
    pub(crate) fn entrypoint(&self) -> GcRef<'a, Function> {
        GcRef::map(self, |this| match &this.entrypoint {
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

    #[inline]
    pub fn name(&self) -> GcRef<'a, Str> {
        GcRef::map(self, |this| &this.name)
    }
}

pub(crate) struct Functions<'a> {
    module: GcRef<'a, ModuleProto>,
    index: usize,
}

impl<'a> Iterator for Functions<'a> {
    type Item = GcRef<'a, Function>;

    fn next(&mut self) -> Option<Self::Item> {
        GcRef::map_opt(&self.module, |module| {
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

impl<'a> GcRefMut<'a, ModuleProto> {}

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

impl<'a> std::fmt::Debug for GcRef<'a, ModuleProto> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleProto")
            .field("name", &self.name())
            .field("functions", &self.functions.len())
            .field("module_vars", &self.module_vars.len())
            .finish()
    }
}

#[derive(Default)]
pub(crate) struct ModuleRegistry {
    modules: HashMap<String, GcPtr<ModuleProto>, FxBuildHasher>,
    native_modules: HashMap<String, GcPtr<NativeModuleProto>>,
}

impl ModuleRegistry {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn add(&mut self, heap: &mut Heap, module: &Module) -> GcPtr<ModuleProto> {
        let name = module.name().to_owned();

        let_root!(in heap; m);
        let m = canonicalize(heap, m, module);
        self.modules.insert(name, m.as_ptr());
        m.as_ptr()
    }

    pub(crate) fn add_native(&mut self, heap: &mut Heap, module: &NativeModule) {
        let name = module.name().to_owned();

        let_root!(in heap; m);
        let m = unsafe {
            let_root!(in heap; name_root);
            let name = Str::new(heap, name_root, name.as_str());

            let (functions, functions_by_name) = {
                let mut functions = Box::new_uninit_slice(module.num_functions());
                let mut functions_by_name: HashMap<String, usize, _> = Default::default();

                for (i, ((func_name, orig_func), func_slot)) in
                    module.functions().zip(functions.iter_mut()).enumerate()
                {
                    // TODO: improve root in loop ergonomics
                    let_root!(in heap; func_name_root);
                    let func_name = Str::new(heap, func_name_root, func_name);

                    let_root!(in heap; func_root);
                    let func = HostFunction::new(
                        heap,
                        func_root,
                        &func_name,
                        orig_func.arity(),
                        orig_func.callback().clone(),
                    );

                    func_slot.write(func.as_ptr());
                    functions_by_name.insert(orig_func.name().to_owned(), i);
                }

                (functions.assume_init(), functions_by_name)
            };

            m.init_raw(
                heap,
                heap.alloc_no_gc(|ptr| {
                    (*ptr).write(NativeModuleProto {
                        name: name.as_ptr(),
                        functions,
                        functions_by_name,
                    });
                }),
            )
        };
        self.native_modules.insert(name, m.as_ptr());
    }

    pub(crate) unsafe fn get(&self, spec: GcPtr<Str>) -> Option<GcPtr<NativeModuleProto>> {
        // TODO: string interning
        self.native_modules.get(spec.as_ref().as_str()).copied()
    }

    pub(crate) fn trace(&self, tracer: &Tracer) {
        for m in self.modules.values() {
            tracer.visit(*m);
        }

        for m in self.native_modules.values() {
            tracer.visit(*m)
        }
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub(crate) struct ModuleId(u32);

pub struct ImportProto {
    spec: GcPtr<Str>,
    bindings: ImportBindings,
}

impl ImportProto {
    pub(crate) fn alloc(heap: &Heap, spec: GcPtr<Str>, bindings: ImportBindings) -> GcPtr<Self> {
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self { spec, bindings });
        })
    }
}

pub enum ImportBindings {
    Bare(Reg),
    Named(Box<[(GcPtr<Str>, Reg)]>),
}

impl<'a> GcRef<'a, ImportProto> {
    pub fn spec(&self) -> GcRef<'a, Str> {
        GcRef::map(self, |this| &this.spec)
    }

    pub fn bindings(&self) -> &ImportBindings {
        &self.bindings
    }
}

unsafe impl Trace for ImportProto {
    vtable!(ImportProto);

    unsafe fn trace(&self, tracer: &crate::vm::gc::Tracer) {
        tracer.visit(self.spec);

        match &self.bindings {
            ImportBindings::Bare(reg) => {}
            ImportBindings::Named(items) => {
                for (key, _) in items {
                    tracer.visit(*key);
                }
            }
        }
    }
}

impl std::fmt::Debug for GcRef<'_, ImportProto> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ImportProto")
            .field("spec", &self.spec())
            .finish_non_exhaustive()
    }
}

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

fn canonicalize<'a>(
    heap: &mut Heap,
    root: GcUninitRoot<'a>,
    info: &Module,
) -> GcRoot<'a, ModuleProto> {
    let_root!(in heap; name);
    let name = Str::new(heap, name, info.name());

    let module = unsafe {
        root.init_raw(
            heap,
            heap.alloc_no_gc(|ptr| {
                (*ptr).write(ModuleProto {
                    name: name.as_ptr(),
                    entrypoint: None,
                    functions: Box::new([]),
                    // TODO: module vars
                    module_vars: Box::new([ValueRaw::Nil; 0]),
                });
            }),
        )
    };

    let_root!(in heap; entrypoint);
    let entrypoint = generate_entrypoint(heap, entrypoint, info.name(), &module);
    module.as_mut(heap).entrypoint = Some(entrypoint.as_ptr());

    let mut functions = Vec::new();
    functions.reserve_exact(info.functions().len());
    for info in info.functions() {
        let_root!(in heap; function);
        let function = canonicalize_function(heap, function, info, &module);
        functions.push(function.as_ptr());
    }
    for i in 0..functions.len() {
        let literals = canonicalize_literals(heap, &functions, info.functions()[i].literals());
        unsafe {
            functions[i].as_mut().literals = literals;
        }
    }
    let functions = functions.into_boxed_slice();

    module.as_mut(heap).functions = functions;

    module
}

fn canonicalize_function<'a>(
    heap: &mut Heap,
    root: GcUninitRoot<'a>,
    function: &FuncInfo,
    module: &GcRoot<'a, ModuleProto>,
) -> GcRoot<'a, Function> {
    let_root!(in heap; name);
    let name = Str::new(heap, name, function.name());

    let name = name.as_ptr();
    let code = function.code().into();
    let literals = Box::new([]);
    let module = module.as_ptr();
    let dbg = function.dbg().cloned();

    unsafe {
        root.init_raw(
            heap,
            heap.alloc_no_gc(|ptr| {
                (*ptr).write(Function {
                    name,
                    nparams: 0,
                    nstack: 1,
                    code,
                    literals,
                    module,
                    dbg,
                });
            }),
        )
    }
}

fn canonicalize_literals(
    heap: &mut Heap,
    functions: &[GcPtr<Function>],
    literals: &[Literal],
) -> Box<[ValueRaw]> {
    let mut out = Vec::new();
    out.reserve_exact(literals.len());
    for literal in literals {
        let value = match literal {
            Literal::Nil => ValueRaw::Nil,
            Literal::Bool(v) => ValueRaw::Bool(*v),
            Literal::Int(v) => ValueRaw::Int(*v),
            Literal::Float(v) => ValueRaw::Float(*v),
            Literal::String(v) => ValueRaw::Object(Str::alloc(heap, v.as_str()).as_any()),
            Literal::ClosureInfo(v) => ValueRaw::Object(
                ClosureProto::alloc(heap, functions[v.func.zx()], &v.capture_info).as_any(),
            ),
            Literal::ImportInfo(v) => match v {
                crate::module::ImportInfo::Bare { spec, dst } => {
                    let_root!(in heap; spec_root);
                    let spec = Str::new(heap, spec_root, spec);
                    let bindings = ImportBindings::Bare(*dst);
                    ValueRaw::Object(ImportProto::alloc(heap, spec.as_ptr(), bindings).as_any())
                }
                crate::module::ImportInfo::Named { spec, bindings } => {
                    let_root!(in heap; spec_root);
                    let spec = Str::new(heap, spec_root, spec);

                    let bindings = ImportBindings::Named(
                        bindings
                            .iter()
                            .map(|(key, dst)| {
                                // TODO: root in loop ergonomics
                                let_root!(in heap; key_root);
                                let key = Str::new(heap, key_root, key);

                                (key.as_ptr(), *dst)
                            })
                            .collect(),
                    );
                    ValueRaw::Object(ImportProto::alloc(heap, spec.as_ptr(), bindings).as_any())
                }
            },
        };
        out.push(value);
    }
    out.into_boxed_slice()
}

fn generate_entrypoint<'a>(
    heap: &mut Heap,
    root: GcUninitRoot<'a>,
    name: &str,
    module: &GcRoot<'a, ModuleProto>,
) -> GcRoot<'a, Function> {
    let_root!(in heap; name_gc);
    let name = Str::new(heap, name_gc, &format!("{name}#start"));

    unsafe {
        root.init_raw(
            heap,
            heap.alloc_no_gc(|ptr| {
                (*ptr).write(Function {
                    name: name.as_ptr(),
                    nparams: 0,
                    nstack: 1,
                    code: Box::new(ENTRYPOINT),
                    literals: Box::new([]),
                    module: module.as_ptr(),
                    dbg: None,
                });
            }),
        )
    }
}
