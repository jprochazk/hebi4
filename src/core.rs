use std::sync::LazyLock;

use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

use crate::{
    codegen::opcodes::HostId,
    gc::Tracer,
    vm::{
        gc::{GcPtr, Heap},
        value::{
            host_function::{HostFunction, HostFunctionCallback},
            string::Str,
        },
    },
};

#[derive(Clone, Copy)]
struct CoreLibData {
    functions: &'static LazyLock<Box<[CoreFunction]>>,
    by_name: &'static LazyLock<HashMap<&'static str, HostId, FxBuildHasher>>,
}

macro_rules! functions {
    ($($module:ident :: $name:ident),* $(,)?) => {{

        static FUNCTIONS: LazyLock<Box<[CoreFunction]>> = LazyLock::new(|| {
            [$(
                CoreFunction {
                    name: stringify!($name),
                    arity: $crate::module::native::arity_of(&$crate::core::$module::$name),
                    f: $crate::__function_shim!($crate::core::$module::$name),
                },
            )*].into_iter().collect()
        });

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

        CoreLibData {
            functions: &FUNCTIONS,
            by_name: &BY_NAME
        }
    }};
}

pub struct CoreLib {
    functions: &'static [CoreFunction],
    by_name: &'static HashMap<&'static str, HostId, FxBuildHasher>,
}

impl CoreLib {
    pub fn get() -> Self {
        CoreLib {
            functions: &CORE_LIB.functions,
            by_name: &CORE_LIB.by_name,
        }
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
                let name = Str::alloc(heap, function.name);

                HostFunction::alloc(heap, name, function.arity, function.f)
            })
            .collect();

        Self { functions }
    }

    pub(crate) fn trace(&self, tracer: &Tracer) {
        for f in &self.functions {
            tracer.visit(*f);
        }
    }
}

mod convert;
mod fmt;
mod panic;

static CORE_LIB: CoreLibData = functions! {
    fmt::print,
    panic::panic,
    convert::to_str,
    convert::to_int,
    convert::to_float,
    convert::parse_int,
    convert::parse_float,
    convert::type_name,
};
