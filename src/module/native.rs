use crate::error::Result;
use crate::value::ValueRaw;
use crate::value::host_function::{Context, HostFunctionCallback};
use hashbrown::HashMap;
use rustc_hash::FxBuildHasher;

pub struct NativeModule {}

impl NativeModule {
    pub fn builder(name: String) -> NativeModuleBuilder {
        NativeModuleBuilder {
            name,
            functions: Vec::new(),
            functions_by_name: Default::default(),
        }
    }
}

pub struct NativeModuleBuilder {
    name: String,
    functions: Vec<NativeFunction>,
    functions_by_name: HashMap<String, usize, FxBuildHasher>,
}

pub struct NativeFunction {
    name: String,
    arity: u8,
    callback: HostFunctionCallback,
}

pub unsafe trait NativeFunctionCallback<'cx, T> {
    const ARITY: u8;

    unsafe fn call(&self, cx: Context<'cx>) -> Result<ValueRaw>;
}

pub trait IntoHebiResult: Sized {
    unsafe fn into_hebi_result(self, cx: &Context<'_>) -> Result<ValueRaw>;
}

impl IntoHebiResult for () {
    #[inline]
    unsafe fn into_hebi_result(self, _cx: &Context<'_>) -> Result<ValueRaw> {
        Ok(ValueRaw::Nil)
    }
}

impl IntoHebiResult for Result<()> {
    #[inline]
    unsafe fn into_hebi_result(self, cx: &Context<'_>) -> Result<ValueRaw> {
        self.map(|_| ValueRaw::Nil)
    }
}

pub trait TryFromHebiValue: Sized {
    unsafe fn try_from_hebi_value(cx: &Context<'_>, value: ValueRaw) -> Result<Self>;
}

impl<'a> TryFromHebiValue for crate::gc::ValueRef<'a> {
    unsafe fn try_from_hebi_value(cx: &Context<'_>, value: ValueRaw) -> Result<Self> {
        // UNROOTED: must be rooted in `cx` stack or elsewhere
        Ok(value.as_ref())
    }
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

            unsafe fn call(&self, cx: Context<'cx>) -> Result<ValueRaw> {
                let [$($T,)*] = cx.args()?;
                let ($($T,)*) = (
                    $(<$T as TryFromHebiValue>::try_from_hebi_value(&cx, $T)?,)*
                );

                let result = {
                    let cx = cx.private_clone();
                    (self)(cx, $($T,)*)
                };

                <R as IntoHebiResult>::into_hebi_result(result, &cx)
            }
        }
    };
}

all_the_tuples!(impl_native_function_callback);
