use std::{rc::Rc, sync::LazyLock};

use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

use crate::{
    codegen::opcodes::HostId,
    gc::Tracer,
    value::host_function::HostFunctionCallbackRaw,
    vm::{
        gc::{GcPtr, Heap},
        value::{host_function::HostFunction, string::Str},
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
                    f: {
                        fn _shim(
                            cx: $crate::vm::value::host_function::Context<'_>,
                        ) -> $crate::error::Result<$crate::vm::value::ValueRaw> {
                            let f = $module::$name;
                            unsafe {
                                $crate::module::native::NativeFunctionCallback::call(&f, cx)
                            }
                        }

                        _shim
                    },
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
    pub fn callback_for(&self, id: HostId) -> HostFunctionCallbackRaw {
        unsafe { self.functions.get_unchecked(id.zx()).f }
    }
}

pub struct CoreFunction {
    pub name: &'static str,
    pub arity: u8,
    f: HostFunctionCallbackRaw,
}

pub(crate) struct RuntimeCoreLib {
    pub functions: Box<[GcPtr<HostFunction>]>,
}

impl RuntimeCoreLib {
    pub(crate) fn init(heap: &Heap) -> Self {
        #[cfg(debug_assertions)]
        if option_env!("NO_CORE").is_some() {
            return Self {
                functions: Box::new([]),
            };
        }

        let functions = CoreLib::get()
            .functions
            .iter()
            .map(|function| {
                // TODO: string interning
                let name = Str::alloc(heap, function.name);

                HostFunction::alloc(heap, name, function.arity, Rc::new(function.f))
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
mod list;
mod math;
mod panic;
mod str;

static CORE_LIB: CoreLibData = functions! {
    fmt::print,
    fmt::printf,
    fmt::format,
    list::map,
    list::append,
    list::list_len,
    // math::powi,
    // math::powf,
    convert::to_str,
    convert::to_int,
    convert::to_float,
    convert::parse_int,
    convert::parse_float,
    convert::type_name,
    panic::panic,
    panic::assert,
    panic::assert_eq,
    str::strip_prefix,
    str::starts_with,
    str::split_at,
    str::split,
    str::lines,
};
