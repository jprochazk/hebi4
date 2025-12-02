use crate::prelude::*;

pub fn panic() -> HebiResult<()> {
    Err(error("explicit panic"))
}

pub fn assert(v: bool) -> HebiResult<()> {
    if v {
        Ok(())
    } else {
        Err(error("assertion failed"))
    }
}

pub fn assert_eq(cx: Context, a: Value, b: Value) -> HebiResult<()> {
    macro_rules! test {
        ($a:expr, $b:expr) => {{
            if $a == $b {
                Ok(())
            } else {
                Err(error(format!(
                    "assertion failed: {a} != {b}",
                    a = $a,
                    b = $b,
                )))
            }
        }};
    }

    // TODO: dedup with vm
    match (a, b) {
        (Value::Nil, Value::Nil) => Ok(()),
        (Value::Bool(a), Value::Bool(b)) => test!(a, b),

        (Value::Int(a), Value::Int(b)) => test!(a, b),
        (Value::Int(a), Value::Float(b)) => test!(a as f64, b),
        (Value::Float(a), Value::Float(b)) => test!(a, b),
        (Value::Float(a), Value::Int(b)) => test!(a, b as f64),

        (Value::Object(a), Value::Object(b)) => {
            if let Some(a) = a.cast::<Str>()
                && let Some(b) = b.cast::<Str>()
            {
                test!(a.as_ref(&cx).as_str(), b.as_ref(&cx).as_str())
            } else {
                Err(error(format!(
                    "assertion failed: cannot compare types ({}, {})",
                    a.type_name(),
                    b.type_name()
                )))
            }
        }
        _ => Err(error("assertion failed: type mismatch")),
    }
}

// TODO: return an actual Error object
pub fn r#try<'a>(mut cx: Context<'a>, f: Value<'a>) -> HebiResult<Ret<'a>> {
    let_root!(in &cx; callee);
    let callee = f.root(&mut cx, callee);

    let_root!(in &cx; ret);
    match cx.call(&callee, (), ret) {
        Ok(v) => cx.ret(v),
        Err(err) => {
            let_root!(in &cx; error_msg);
            let error_msg = Str::new(&cx, error_msg, &err.to_string());
            cx.ret(error_msg)
        }
    }
}
