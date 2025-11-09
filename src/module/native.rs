use std::sync::Arc;

use beef::lean::Cow;
use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

use crate::{
    error::Result,
    vm::{
        gc::ValueRef,
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

pub trait IntoHebiResult: Sized {
    unsafe fn into_hebi_result(self, cx: &mut Context<'_>) -> Result<ValueRaw>;
}

pub trait TryIntoHebiValue: Sized {
    unsafe fn try_into_hebi_value(self, cx: &mut Context<'_>) -> Result<ValueRaw>;
}

pub trait TryFromHebiValue: Sized {
    unsafe fn try_from_hebi_value(cx: &Context<'_>, value: ValueRaw) -> Result<Self>;
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
            R: IntoHebiResult + 'cx,
            $($T: TryFromHebiValue + 'cx,)*
        {
            const ARITY: u8 = $count;

            unsafe fn call(&self, mut cx: Context<'cx>) -> Result<ValueRaw> {
                let [$($T,)*] = cx.args()?;
                let ($($T,)*) = (
                    $(<$T as TryFromHebiValue>::try_from_hebi_value(&cx, $T)?,)*
                );

                let result = {
                    let cx = cx.private_clone();
                    (self)(cx, $($T,)*)
                };

                <R as IntoHebiResult>::into_hebi_result(result, &mut cx)
            }
        }
    };
}

all_the_tuples!(impl_native_function_callback);

////////////////////////////////// impls //////////////////////////////////

impl<T: TryIntoHebiValue> IntoHebiResult for T {
    #[inline]
    unsafe fn into_hebi_result(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        <T as TryIntoHebiValue>::try_into_hebi_value(self, cx)
    }
}

impl<T: TryIntoHebiValue> IntoHebiResult for Result<T> {
    #[inline]
    unsafe fn into_hebi_result(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        self.and_then(|v| <T as TryIntoHebiValue>::try_into_hebi_value(v, cx))
    }
}

impl TryIntoHebiValue for () {
    unsafe fn try_into_hebi_value(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Nil)
    }
}

impl TryIntoHebiValue for &'static str {
    unsafe fn try_into_hebi_value(self, cx: &mut Context<'_>) -> Result<ValueRaw> {
        // TODO: string interning

        let ptr = crate::value::Str::alloc(cx.heap(), self);
        Ok(ValueRaw::Object(ptr.as_any()))
    }
}

impl<'a> TryFromHebiValue for ValueRef<'a> {
    unsafe fn try_from_hebi_value(cx: &Context<'_>, value: ValueRaw) -> Result<Self> {
        // UNROOTED: must be rooted in `cx` stack or elsewhere
        Ok(value.as_ref())
    }
}
