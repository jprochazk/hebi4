use std::any::Any;

use crate::{
    error::{Result, error},
    gc::{GcPtr, GcRefMut, GcRoot, GcUninitRoot, Heap},
    vm::gc::{GcRef, Trace, Tracer},
};

pub trait ExternData: std::fmt::Debug + 'static {
    fn type_name(&self) -> &'static str;
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

#[doc(hidden)]
#[macro_export]
macro_rules! __impl_extern_data {
    ($T:ty) => {
        impl $crate::vm::value::external::ExternData for $T {
            fn type_name(&self) -> &'static str {
                ::std::any::type_name::<Self>()
            }

            fn as_any(&self) -> &dyn ::std::any::Any {
                self
            }

            fn as_any_mut(&mut self) -> &mut dyn ::std::any::Any {
                self
            }
        }
    };
}

/// Used to mark a Rust type as something which can be passed to `Extern`.
pub use crate::__impl_extern_data as extern_data;

pub struct Extern {
    data: Box<dyn ExternData>,
}

impl Extern {
    pub fn alloc(heap: &Heap, data: impl ExternData) -> GcPtr<Self> {
        let data = Box::new(data);
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self { data });
        })
    }

    pub fn new<'a>(heap: &Heap, root: GcUninitRoot<'a>, data: impl ExternData) -> GcRoot<'a, Self> {
        let ptr = Self::alloc(heap, data);
        unsafe { root.init_raw(ptr) }
    }
}

impl<'a> GcRef<'a, Extern> {
    pub fn cast_ref<T: ExternData>(&self) -> Result<&T> {
        self.data.as_any().downcast_ref().ok_or_else(|| {
            error(format!(
                "type mismatch: expected Extern<{}>, got Extern<{}>",
                std::any::type_name::<T>(),
                self.data.type_name(),
            ))
        })
    }
}

impl<'a> GcRefMut<'a, Extern> {
    pub fn cast_mut<T: ExternData>(&mut self) -> Result<&mut T> {
        // NOTE: polonius issue.
        if self.data.as_any().is::<T>() {
            return Ok(unsafe { self.data.as_any_mut().downcast_mut().unwrap_unchecked() });
        }

        let type_name = self.data.type_name();
        Err(error(format!(
            "type mismatch: expected Extern<{}>, got Extern<{}>",
            std::any::type_name::<T>(),
            type_name
        )))
    }
}

unsafe impl Trace for Extern {
    vtable!(Extern);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        _ = tracer;
    }
}

impl std::fmt::Debug for GcRef<'_, Extern> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Extern").field(&self.data).finish()
    }
}
