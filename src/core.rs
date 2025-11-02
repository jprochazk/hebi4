mod fmt;
use crate::{
    codegen::opcodes::HostId,
    gc::{GcPtr, Heap},
    value::{
        host_function::{HostFunction, HostFunctionCallback},
        string::String,
    },
};
use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;
use std::sync::LazyLock;

#[derive(Clone, Copy)]
pub struct CoreLib {
    functions: &'static [CoreFunction],
    by_name: &'static LazyLock<HashMap<&'static str, HostId, FxBuildHasher>>,
}

// TODO: nicer bindings with fromvalue/intovalue
macro_rules! functions {
    ($($module:ident :: $name:ident ($arity:expr)),* $(,)?) => {{
        static BY_NAME: LazyLock<HashMap<&'static str, HostId, FxBuildHasher>> = LazyLock::new(|| {
            let mut map = HashMap::default();

            let mut id = 0u16;
            $(
                map.insert(stringify!($name), unsafe { HostId::new_unchecked(id) });
                id += 1;
            )*

            let _ = id;

            map
        });

        CoreLib {
            functions: &[
                $(
                    CoreFunction {
                        name: stringify!($name),
                        arity: $arity,
                        f: $crate::core::$module::$name,
                    }
                )*
            ],
            by_name: &BY_NAME
        }
    }};
}

// TODO: better sync for arity
static CORE_LIB: CoreLib = functions! {
    fmt::print(1)
};

impl CoreLib {
    pub fn get() -> &'static Self {
        &CORE_LIB
    }

    pub fn find(&self, name: &str) -> Option<(HostId, &'static CoreFunction)> {
        let id = self.by_name.get(name).copied()?;
        Some((id, &self.functions[id.zx()]))
    }

    pub fn entry(&self, id: HostId) -> &'static CoreFunction {
        unsafe { self.functions.get_unchecked(id.zx()) }
    }

    #[inline]
    pub fn callback_for(&self, id: HostId) -> HostFunctionCallback {
        unsafe { self.functions.get_unchecked(id.zx()).f }
    }
}

pub struct CoreFunction {
    pub name: &'static str,
    pub arity: u8,
    f: HostFunctionCallback,
}

pub(crate) struct RuntimeCoreLib {
    pub functions: Box<[GcPtr<HostFunction>]>,
}

impl RuntimeCoreLib {
    pub(crate) fn init(heap: &Heap) -> Self {
        let functions = CoreLib::get()
            .functions
            .iter()
            .map(|function| {
                // TODO: string interning
                let name = String::alloc(heap, function.name);

                HostFunction::alloc(heap, name, function.arity, function.f)
            })
            .collect();

        Self { functions }
    }
}
