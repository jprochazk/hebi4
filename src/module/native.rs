use std::sync::Arc;

use beef::lean::Cow;
use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

use crate::{
    error::{Error, Result, error},
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
macro_rules! __function_shim {
    ($name:path) => {{
        unsafe fn _shim(
            cx: $crate::vm::value::host_function::Context<'_>,
        ) -> $crate::error::Result<$crate::vm::value::ValueRaw> {
            let f = $name;
            $crate::module::native::NativeFunctionCallback::call(&f, cx)
        }

        _shim
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __function {
    ($name:tt) => {{
        unsafe {
            $crate::module::native::NativeFunction::from_callback(
                $crate::module::native::function!(@stem $name),
                $crate::module::native::arity_of(&$name),
                $crate::__function_shim!($name),
            )
        }
    }};

    (@stem $tail:ident) => (stringify!($tail));
    (@stem $($asdf:ident ::)* $tail:ident) => (stringify!($tail));
}

pub use crate::__function as function;

pub struct NativeFunction {
    name: Cow<'static, str>,
    arity: u8,
    callback: HostFunctionCallback,
}

impl NativeFunction {
    pub unsafe fn from_callback(
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

    pub fn callback(&self) -> HostFunctionCallback {
        self.callback
    }
}

pub(crate) fn arity_of<'a, F: NativeFunctionCallback<'a, T>, T>(f: &F) -> u8 {
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

pub trait TryFromHebiValue<'a>: Sized {
    fn try_from_hebi_value(cx: &Context<'a>, value: ValueRef<'a>) -> Result<Self>;
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

macro_rules! impl_native_function_callback {
    ($count:literal, $($T:ident),*) => {
        #[allow(non_snake_case)]
        unsafe impl<'cx, Func, R, $($T,)*> NativeFunctionCallback<'cx, (R, $($T,)*)> for Func
        where
            Func: Fn(Context<'cx>, $($T,)*) -> R,
            R: IntoHebiResultRaw + 'cx,
            $($T: TryFromHebiValue<'cx> + 'cx,)*
        {
            const ARITY: u8 = $count;

            unsafe fn call(&self, mut cx: Context<'cx>) -> Result<ValueRaw> {
                let [$($T,)*] = cx.args()?;
                // SAFETY: rooted on the stack
                let ($($T,)*) = (
                    $($T.as_ref(),)*
                );
                let ($($T,)*) = (
                    $(<$T as TryFromHebiValue>::try_from_hebi_value(&cx, $T)?,)*
                );

                let result = {
                    let cx = cx.private_clone();
                    (self)(cx, $($T,)*)
                };

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
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Nil)
    }
}

impl TryIntoHebiValueRaw for bool {
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Bool(self))
    }
}

impl TryIntoHebiValueRaw for i64 {
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Int(self))
    }
}

impl TryIntoHebiValueRaw for f64 {
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Float(self))
    }
}

impl TryIntoHebiValueRaw for &'static str {
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        // TODO: string interning

        let ptr = crate::value::Str::alloc(cx.heap(), self);
        Ok(ValueRaw::Object(ptr.as_any()))
    }
}

impl TryIntoHebiValueRaw for String {
    unsafe fn try_into_hebi_value_raw(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        // TODO: string interning

        let ptr = crate::value::Str::alloc(cx.heap(), self.as_str());
        Ok(ValueRaw::Object(ptr.as_any()))
    }
}

impl<'a> TryFromHebiValue<'a> for ValueRef<'a> {
    fn try_from_hebi_value(cx: &Context<'a>, value: ValueRef<'a>) -> Result<Self> {
        Ok(value)
    }
}

fn mismatched_type_error(expected: &'static str, actual: &ValueRef<'_>) -> Error {
    error(format!(
        "mismatched type, expected {expected} but got {}",
        actual.type_name()
    ))
}

impl<'a> TryFromHebiValue<'a> for () {
    fn try_from_hebi_value(cx: &Context<'a>, value: ValueRef<'a>) -> Result<Self> {
        match value {
            ValueRef::Nil => Ok(()),
            v => Err(mismatched_type_error("Nil", &v)),
        }
    }
}

impl<'a> TryFromHebiValue<'a> for bool {
    fn try_from_hebi_value(cx: &Context<'a>, value: ValueRef<'a>) -> Result<Self> {
        match value {
            ValueRef::Bool(v) => Ok(v),
            v => Err(mismatched_type_error("Bool", &v)),
        }
    }
}

impl<'a> TryFromHebiValue<'a> for i64 {
    fn try_from_hebi_value(cx: &Context<'a>, value: ValueRef<'a>) -> Result<Self> {
        match value {
            ValueRef::Int(v) => Ok(v),
            v => Err(mismatched_type_error("Int", &v)),
        }
    }
}

impl<'a> TryFromHebiValue<'a> for f64 {
    fn try_from_hebi_value(cx: &Context<'a>, value: ValueRef<'a>) -> Result<Self> {
        match value {
            ValueRef::Float(v) => Ok(v),
            v => Err(mismatched_type_error("Float", &v)),
        }
    }
}

impl<'a> TryFromHebiValue<'a> for String {
    fn try_from_hebi_value(cx: &Context<'a>, value: ValueRef<'a>) -> Result<Self> {
        let Some(value) = value.into_object::<crate::value::Str>() else {
            return Err(mismatched_type_error("Str", &value));
        };

        Ok(value.as_str().to_owned())
    }
}

impl<'a> TryFromHebiValue<'a> for GcRef<'a, crate::value::Str> {
    fn try_from_hebi_value(cx: &Context<'a>, value: ValueRef<'a>) -> Result<Self> {
        let Some(value) = value.into_object::<crate::value::Str>() else {
            return Err(mismatched_type_error("Str", &value));
        };

        Ok(value)
    }
}
