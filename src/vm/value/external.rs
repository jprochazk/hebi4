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
                $crate::vm::value::external::short_type_name::<Self>()
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

pub fn short_type_name<T>() -> &'static str {
    let s = std::any::type_name::<T>();

    // iterators optimize poorly
    //   s.rpslit("::").next().unwrap_or(s)
    let bytes = s.as_bytes();
    let mut i = bytes.len();
    while i >= 2 {
        i -= 1;
        if bytes[i] == b':' && bytes[i - 1] == b':' {
            return &s[i + 1..];
        }
    }

    s
}

/// Used to mark a Rust type as something which can be passed to `Extern`.
pub use crate::__impl_extern_data as extern_data;

pub struct ExternAny {
    data: Box<dyn ExternData>,
}

impl ExternAny {
    #[inline]
    pub fn alloc(heap: &Heap, data: impl ExternData + Sized) -> GcPtr<Self> {
        let data = Box::new(data);
        heap.alloc_no_gc(|ptr| unsafe {
            (*ptr).write(Self { data });
        })
    }

    #[inline]
    pub fn new<'a>(
        heap: &Heap,
        root: GcUninitRoot<'a>,
        data: impl ExternData + Sized,
    ) -> GcRoot<'a, Self> {
        let ptr = Self::alloc(heap, data);
        unsafe { root.init_raw(ptr) }
    }

    #[inline]
    pub(crate) unsafe fn cast_ref_raw<T: ExternData + Sized>(
        this: GcPtr<Self>,
    ) -> Result<&'static T> {
        let data = this.into_raw().as_ref().data.as_ref();

        data.as_any().downcast_ref().ok_or_else(|| {
            error(format!(
                "type mismatch: expected Extern<{}>, got Extern<{}>",
                short_type_name::<T>(),
                data.type_name(),
            ))
        })
    }

    #[inline]
    pub(crate) unsafe fn cast_mut_raw<T: ExternData + Sized>(
        this: GcPtr<Self>,
    ) -> Result<&'static mut T> {
        let data = this.into_raw().as_mut().data.as_mut();
        if data.as_any().is::<T>() {
            return Ok(unsafe { data.as_any_mut().downcast_mut().unwrap_unchecked() });
        }

        Err(error(format!(
            "type mismatch: expected Extern<{}>, got Extern<{}>",
            short_type_name::<T>(),
            data.type_name(),
        )))
    }
}

impl<'a> GcRef<'a, ExternAny> {
    #[inline]
    pub fn is<T: ExternData + Sized>(&self) -> Result<()> {
        if self.data.as_any().is::<T>() {
            return Ok(());
        } else {
            Err(error(format!(
                "type mismatch: expected Extern<{}>, got Extern<{}>",
                short_type_name::<T>(),
                self.data.type_name(),
            )))
        }
    }

    #[inline]
    pub fn cast_ref<T: ExternData + Sized>(&self) -> Result<&T> {
        self.data.as_any().downcast_ref().ok_or_else(|| {
            error(format!(
                "type mismatch: expected Extern<{}>, got Extern<{}>",
                short_type_name::<T>(),
                self.data.type_name(),
            ))
        })
    }
}

impl<'a> GcRefMut<'a, ExternAny> {
    #[inline]
    pub fn is<T: ExternData + Sized>(&self) -> bool {
        self.data.as_any().is::<T>()
    }

    #[inline]
    pub fn cast_ref<T: ExternData + Sized>(&self) -> Result<&T> {
        self.data.as_any().downcast_ref().ok_or_else(|| {
            error(format!(
                "type mismatch: expected Extern<{}>, got Extern<{}>",
                short_type_name::<T>(),
                self.data.type_name(),
            ))
        })
    }

    #[inline]
    pub fn cast_mut<T: ExternData + Sized>(&mut self) -> Result<&mut T> {
        // NOTE: polonius issue.
        if self.data.as_any().is::<T>() {
            return Ok(unsafe { self.data.as_any_mut().downcast_mut().unwrap_unchecked() });
        }

        let type_name = self.data.type_name();
        Err(error(format!(
            "type mismatch: expected Extern<{}>, got Extern<{}>",
            short_type_name::<T>(),
            type_name
        )))
    }
}

unsafe impl Trace for ExternAny {
    vtable!(ExternAny);

    #[inline]
    unsafe fn trace(&self, tracer: &Tracer) {
        _ = tracer;
    }
}

impl std::fmt::Debug for GcRef<'_, ExternAny> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Extern").field(&self.data).finish()
    }
}
