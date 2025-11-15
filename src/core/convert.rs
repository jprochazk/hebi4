use crate::prelude::*;

pub(crate) fn stringify(v: ValueRef) -> HebiResult<String> {
    let mut out = Vec::new();
    stringify_into(v, &mut out)?;
    Ok(unsafe { String::from_utf8_unchecked(out) })
}

pub(crate) fn stringify_into(v: ValueRef, out: &mut impl std::io::Write) -> HebiResult<()> {
    match v {
        ValueRef::Nil => write!(out, "nil"),
        ValueRef::Bool(v) => write!(out, "{v}"),
        ValueRef::Int(v) => write!(out, "{v}"),
        ValueRef::Float(v) => write!(out, "{v:?}"),
        ValueRef::Object(v) => {
            // specialize strings
            if let Some(v) = v.cast::<Str>() {
                write!(out, "{v}")
            } else {
                write!(out, "{v:?}")
            }
        }
    }
    .map_err(|err| error(err.to_string()))
}

pub fn to_str(cx: Context, v: Value) -> HebiResult<String> {
    stringify(v.as_ref(cx.heap()))
}

pub fn parse_int(cx: Context, v: Param<Str>) -> HebiResult<i64> {
    v.as_ref(cx.heap())
        .as_str()
        .parse::<i64>()
        .map_err(|err| error(err.to_string()))
}

pub fn parse_float(cx: Context, v: Param<Str>) -> HebiResult<f64> {
    v.as_ref(cx.heap())
        .as_str()
        .parse::<f64>()
        .map_err(|err| error(err.to_string()))
}

pub fn to_int(_cx: Context, v: f64) -> i64 {
    v as i64
}

pub fn to_float(_cx: Context, v: i64) -> f64 {
    v as f64
}

pub fn type_name<'a>(cx: Context<'a>, v: Value<'a>) -> &'static str {
    // TODO: string interning

    v.type_name()
}
