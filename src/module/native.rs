use std::{marker::PhantomData, sync::Arc};

use beef::lean::Cow;
use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

use crate::{
    error::{Error, Result, error},
    gc::{GcAnyPtr, GcAnyRef, GcAnyRefMut, GcPtr, GcRefMut, Heap, Trace},
    vm::{
        gc::{GcRef, ValueRef},
        value::{
            ValueRaw,
            host_function::{Context, HostFunctionCallback},
        },
    },
};

#[derive(Clone)]
pub struct NativeModule(Arc<NativeModuleInner>);

impl NativeModule {
    pub fn name(&self) -> &str {
        &self.0.name
    }

    pub fn num_functions(&self) -> usize {
        self.0.functions.len()
    }

    pub fn functions(&self) -> impl Iterator<Item = (&str, &NativeFunction)> {
        self.0
            .functions_by_name
            .iter()
            .map(|(k, v)| (k.as_ref(), unsafe { self.0.functions.get_unchecked(*v) }))
    }
}

struct NativeModuleInner {
    name: Cow<'static, str>,
    functions: Vec<NativeFunction>,
    functions_by_name: HashMap<Cow<'static, str>, usize, FxBuildHasher>,
}

impl NativeModule {
    pub fn builder(name: impl Into<Cow<'static, str>>) -> NativeModuleBuilder {
        NativeModuleBuilder {
            inner: NativeModuleInner {
                name: name.into(),
                functions: Vec::new(),
                functions_by_name: Default::default(),
            },
        }
    }
}

pub struct NativeModuleBuilder {
    inner: NativeModuleInner,
}

impl NativeModuleBuilder {
    pub fn function(mut self, f: NativeFunction) -> Self {
        let id = self.inner.functions.len();
        let name = f.name.clone();
        self.inner.functions.push(f);
        self.inner.functions_by_name.insert(name, id);
        self
    }

    pub fn finish(self) -> NativeModule {
        NativeModule(Arc::new(self.inner))
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __f {
    ($name:path) => {
        unsafe {
            $crate::module::native::NativeFunction::new(
                $crate::module::native::__function_name($name),
                $crate::module::native::arity_of(&$name),
                ::std::rc::Rc::new({
                    move |cx: $crate::value::host_function::Context| -> $crate::error::Result<$crate::value::ValueRaw> {
                        $crate::module::native::NativeFunctionCallback::call(&$name, cx)
                    }
                }),
            )
        }
    };

    ($name:expr, $callback:expr) => {
        unsafe {
            let callback = $callback;
            $crate::module::native::NativeFunction::new(
                $name,
                $crate::module::native::arity_of(&callback),
                ::std::rc::Rc::new({
                    move |cx: $crate::value::host_function::Context| -> $crate::error::Result<$crate::value::ValueRaw> {
                        $crate::module::native::NativeFunctionCallback::call(&callback, cx)
                    }
                })
            )
        }
    };
}

#[doc(hidden)]
pub fn __function_name<T>(_: T) -> &'static str {
    std::any::type_name::<T>()
        .rsplit("::")
        .find(|&part| part != "f" && part != "{{closure}}")
        .expect("Short function name")
}

pub use crate::__f as f;

pub struct NativeFunction {
    name: Cow<'static, str>,
    arity: u8,
    callback: HostFunctionCallback,
}

impl NativeFunction {
    pub unsafe fn new(
        name: impl Into<Cow<'static, str>>,
        arity: u8,
        callback: HostFunctionCallback,
    ) -> Self {
        Self {
            name: name.into(),
            arity,
            callback,
        }
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn arity(&self) -> u8 {
        self.arity
    }

    pub fn callback(&self) -> &HostFunctionCallback {
        &self.callback
    }
}

pub fn arity_of<'a, F: NativeFunctionCallback<'a, T>, T>(f: &F) -> u8 {
    F::ARITY
}

pub unsafe trait NativeFunctionCallback<'cx, T> {
    const ARITY: u8;

    unsafe fn call(&self, cx: Context<'cx>) -> Result<ValueRaw>;
}

pub trait IntoHebiResultRaw: Sized {
    unsafe fn into_hebi_result_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw>;
}

pub trait TryIntoHebiValueRaw: Sized {
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw>;
}

pub trait TryFromHebiValueRaw<'a>: Sized {
    unsafe fn try_from_hebi_value_raw(cx: &Context<'a>, value: ValueRaw) -> Result<Self>;
}

macro_rules! all_the_tuples {
    ($macro:ident) => {
        $macro!(0,);
        $macro!(1, A);
        $macro!(2, A, B);
        $macro!(3, A, B, C);
        $macro!(4, A, B, C, D);
        $macro!(5, A, B, C, D, E);
        $macro!(6, A, B, C, D, E, F);
        $macro!(7, A, B, C, D, E, F, G);
        $macro!(8, A, B, C, D, E, F, G, H);
        $macro!(9, A, B, C, D, E, F, G, H, I);
        $macro!(10, A, B, C, D, E, F, G, H, I, J);
    };
}

pub struct WithContext;

macro_rules! impl_native_function_callback {
    ($count:literal, $($T:ident),*) => {
        #[allow(non_snake_case)]
        unsafe impl<'cx, Func, R, $($T,)*> NativeFunctionCallback<'cx, (WithContext, R, $($T,)*)> for Func
        where
            Func: Fn(Context<'cx>, $($T,)*) -> R,
            R: IntoHebiResultRaw + 'cx,
            $($T: TryFromHebiValueRaw<'cx> + 'cx,)*
        {
            const ARITY: u8 = $count;

            unsafe fn call(&self, mut cx: Context<'cx>) -> Result<ValueRaw> {
                let [$($T,)*] = cx.args()?;
                // SAFETY: rooted on the stack
                let ($($T,)*) = (
                    $(<$T as TryFromHebiValueRaw>::try_from_hebi_value_raw(&cx, $T)?,)*
                );

                let result = {
                    let cx = cx.private_clone();
                    (self)(cx, $($T,)*)
                };

                <R as IntoHebiResultRaw>::into_hebi_result_raw(result, &mut cx)
            }
        }

        #[allow(non_snake_case)]
        unsafe impl<'cx, Func, R, $($T,)*> NativeFunctionCallback<'cx, (R, $($T,)*)> for Func
        where
            Func: Fn($($T,)*) -> R,
            R: IntoHebiResultRaw + 'cx,
            $($T: TryFromHebiValueRaw<'cx> + 'cx,)*
        {
            const ARITY: u8 = $count;

            unsafe fn call(&self, mut cx: Context<'cx>) -> Result<ValueRaw> {
                let [$($T,)*] = cx.args()?;
                // SAFETY: rooted on the stack
                let ($($T,)*) = (
                    $(<$T as TryFromHebiValueRaw>::try_from_hebi_value_raw(&cx, $T)?,)*
                );

                let result = (self)($($T,)*);

                <R as IntoHebiResultRaw>::into_hebi_result_raw(result, &mut cx)
            }
        }
    };
}

all_the_tuples!(impl_native_function_callback);

////////////////////////////////// impls //////////////////////////////////

impl<T: TryIntoHebiValueRaw> IntoHebiResultRaw for T {
    #[inline]
    unsafe fn into_hebi_result_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        <T as TryIntoHebiValueRaw>::try_into_hebi_value_raw(self, cx)
    }
}

impl<T: TryIntoHebiValueRaw> IntoHebiResultRaw for Result<T> {
    #[inline]
    unsafe fn into_hebi_result_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        self.and_then(|v| <T as TryIntoHebiValueRaw>::try_into_hebi_value_raw(v, cx))
    }
}

impl TryIntoHebiValueRaw for () {
    #[inline]
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Nil)
    }
}

impl TryIntoHebiValueRaw for bool {
    #[inline]
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Bool(self))
    }
}

impl TryIntoHebiValueRaw for i64 {
    #[inline]
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Int(self))
    }
}

impl TryIntoHebiValueRaw for f64 {
    #[inline]
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Float(self))
    }
}

impl TryIntoHebiValueRaw for &'static str {
    #[inline]
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        // TODO: string interning

        let ptr = crate::value::Str::alloc(cx.heap_mut(), self);
        Ok(ValueRaw::Object(ptr.as_any()))
    }
}

impl TryIntoHebiValueRaw for String {
    #[inline]
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        // TODO: string interning

        let ptr = crate::value::Str::alloc(cx.heap_mut(), self.as_str());
        Ok(ValueRaw::Object(ptr.as_any()))
    }
}

type Invariant<T> = PhantomData<fn(T) -> T>;
type InvariantLifetime<'a> = Invariant<&'a ()>;

/// A direct reference to a parameter.
///
/// Used in place of types like `GcRoot` and `GcRef` in
/// native function parameters. Behaves exactly like a
/// `GcRoot`
pub struct Param<'a, T: Sized + 'static> {
    ptr: GcPtr<T>,

    #[cfg(debug_assertions)]
    heap_id: crate::gc::HeapId,
    _lifetime: InvariantLifetime<'a>,
}

impl<'a, T: Trace> Param<'a, T> {
    #[inline]
    pub fn as_ptr(&self) -> GcPtr<T> {
        self.ptr
    }

    #[inline]
    pub fn as_ref<'v>(&'v self, heap: &'v Heap) -> GcRef<'v, T> {
        #[cfg(debug_assertions)]
        debug_assert!(heap.id() == self.heap_id);

        unsafe { self.ptr.as_ref() }
    }

    #[inline]
    pub fn as_mut<'v>(&'v self, heap: &'v mut Heap) -> GcRefMut<'v, T> {
        #[cfg(debug_assertions)]
        debug_assert!(heap.id() == self.heap_id);

        unsafe { self.ptr.as_mut() }
    }
}

pub struct Any<'a> {
    ptr: GcAnyPtr,

    #[cfg(debug_assertions)]
    heap_id: crate::gc::HeapId,
    _lifetime: InvariantLifetime<'a>,
}

impl<'a> Any<'a> {
    #[inline]
    pub(crate) unsafe fn from_ptr(ptr: GcAnyPtr, heap: &Heap) -> Self {
        Self {
            ptr,

            #[cfg(debug_assertions)]
            heap_id: heap.id(),
            _lifetime: PhantomData,
        }
    }

    #[inline]
    pub fn as_ptr(&self) -> GcAnyPtr {
        self.ptr
    }

    #[inline]
    pub fn as_ref<'v>(&'v self, heap: &'v Heap) -> GcAnyRef<'v> {
        #[cfg(debug_assertions)]
        debug_assert!(heap.id() == self.heap_id);

        // SAFETY: rooted by stack
        unsafe { self.ptr.as_ref() }
    }

    #[inline]
    pub fn as_mut<'v>(&'v self, heap: &'v mut Heap) -> GcAnyRefMut<'v> {
        #[cfg(debug_assertions)]
        debug_assert!(heap.id() == self.heap_id);

        // SAFETY: rooted by stack
        unsafe { self.ptr.as_mut() }
    }

    #[inline]
    pub fn is<T: Trace>(&self) -> bool {
        // SAFETY: rooted by stack
        unsafe { self.ptr.is::<T>() }
    }

    #[inline]
    pub fn cast<T: Trace>(&self) -> Option<Param<'a, T>> {
        // SAFETY: rooted by stack
        if !self.is::<T>() {
            return None;
        }

        Some(unsafe { self.cast_unchecked() })
    }

    #[inline]
    pub unsafe fn cast_unchecked<T: Trace>(&self) -> Param<'a, T> {
        Param {
            ptr: self.ptr.cast_unchecked(),

            #[cfg(debug_assertions)]
            heap_id: self.heap_id,
            _lifetime: PhantomData,
        }
    }
}

#[derive(Default)]
#[repr(C, u64)]
pub enum Value<'a> {
    #[default]
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
    Object(Any<'a>),
}

impl<'a> Value<'a> {
    #[inline]
    pub(crate) unsafe fn from_raw(v: ValueRaw, heap: &Heap) -> Self {
        match v {
            ValueRaw::Nil => Value::Nil,
            ValueRaw::Bool(v) => Value::Bool(v),
            ValueRaw::Int(v) => Value::Int(v),
            ValueRaw::Float(v) => Value::Float(v),
            ValueRaw::Object(v) => Value::Object(Any::from_ptr(v, heap)),
        }
    }

    #[inline]
    pub fn raw(&self) -> ValueRaw {
        match self {
            Value::Nil => ValueRaw::Nil,
            Value::Bool(v) => ValueRaw::Bool(*v),
            Value::Int(v) => ValueRaw::Int(*v),
            Value::Float(v) => ValueRaw::Float(*v),
            Value::Object(v) => ValueRaw::Object(v.as_ptr()),
        }
    }

    #[inline]
    pub fn as_ref<'v>(&'v self, heap: &'v Heap) -> ValueRef<'v> {
        // SAFETY: rooted by stack, shared access gated by `heap`
        unsafe { self.raw().as_ref() }
    }

    #[inline]
    pub fn coerce_bool(&self) -> bool {
        self.raw().coerce_bool()
    }

    #[inline]
    pub fn type_name(&self) -> &'static str {
        // SAFETY: rooted by stack
        unsafe { self.raw().type_name() }
    }

    #[inline]
    pub fn into_object<T: Trace>(self) -> Option<Param<'a, T>> {
        match self {
            Value::Object(gc) => gc.cast(),
            _ => None,
        }
    }
}

// TODO: passing `GcRef` into host functions is unsound.
// even though they _are_ rooted, it's possible to create
// aliased unique references.
// there needs to be some type which is just a `GcRef`,
// but requires `heap` access to actually dereference.
// something "rooted by the stack", essentially.

impl<'a, T: Trace> TryFromHebiValueRaw<'a> for Param<'a, T> {
    #[inline]
    unsafe fn try_from_hebi_value_raw(cx: &Context<'a>, value: ValueRaw) -> Result<Self> {
        let value = Any::try_from_hebi_value_raw(cx, value)?;
        let Some(value) = value.cast::<T>() else {
            return Err(mismatched_type_error(
                T::vtable().type_name,
                &ValueRaw::Object(value.as_ptr()),
            ));
        };
        Ok(value)
    }
}

impl<'a> TryFromHebiValueRaw<'a> for Any<'a> {
    #[inline]
    unsafe fn try_from_hebi_value_raw(cx: &Context<'a>, value: ValueRaw) -> Result<Self> {
        let value = Value::try_from_hebi_value_raw(cx, value)?;
        let Value::Object(value) = value else {
            return Err(mismatched_type_error("Object", &value.raw()));
        };
        Ok(value)
    }
}

impl<'a> TryFromHebiValueRaw<'a> for Value<'a> {
    #[inline]
    unsafe fn try_from_hebi_value_raw(cx: &Context<'a>, value: ValueRaw) -> Result<Self> {
        Ok(Value::from_raw(value, cx.heap()))
    }
}

#[cold]
unsafe fn mismatched_type_error(expected: &'static str, actual: &ValueRaw) -> Error {
    error(format!(
        "mismatched type, expected {expected}  got {}",
        actual.type_name()
    ))
}

impl<'a> TryFromHebiValueRaw<'a> for () {
    #[inline]
    unsafe fn try_from_hebi_value_raw(cx: &Context<'a>, value: ValueRaw) -> Result<Self> {
        match value {
            ValueRaw::Nil => Ok(()),
            v => Err(mismatched_type_error("Nil", &v)),
        }
    }
}

impl<'a> TryFromHebiValueRaw<'a> for bool {
    #[inline]
    unsafe fn try_from_hebi_value_raw(cx: &Context<'a>, value: ValueRaw) -> Result<Self> {
        match value {
            ValueRaw::Bool(v) => Ok(v),
            v => Err(mismatched_type_error("Bool", &v)),
        }
    }
}

impl<'a> TryFromHebiValueRaw<'a> for i64 {
    #[inline]
    unsafe fn try_from_hebi_value_raw(cx: &Context<'a>, value: ValueRaw) -> Result<Self> {
        match value {
            ValueRaw::Int(v) => Ok(v),
            v => Err(mismatched_type_error("Int", &v)),
        }
    }
}

impl<'a> TryFromHebiValueRaw<'a> for f64 {
    #[inline]
    unsafe fn try_from_hebi_value_raw(cx: &Context<'a>, value: ValueRaw) -> Result<Self> {
        match value {
            ValueRaw::Float(v) => Ok(v),
            v => Err(mismatched_type_error("Float", &v)),
        }
    }
}

impl<'a> TryFromHebiValueRaw<'a> for String {
    #[inline]
    unsafe fn try_from_hebi_value_raw(cx: &Context<'a>, value: ValueRaw) -> Result<Self> {
        let Some(value) = value.into_object::<crate::value::Str>() else {
            return Err(mismatched_type_error("Str", &value));
        };

        Ok(value.as_ref().as_str().to_owned())
    }
}
