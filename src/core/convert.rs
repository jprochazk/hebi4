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

pub fn to_int(v: Value) -> HebiResult<i64> {
    match v {
        Value::Int(v) => Ok(v),
        Value::Float(v) => Ok(v as i64),
        _ => error("cannot convert value to int").into(),
    }
}

pub fn to_float(v: Value) -> HebiResult<f64> {
    match v {
        Value::Int(v) => Ok(v as f64),
        Value::Float(v) => Ok(v),
        _ => error("cannot convert value to int").into(),
    }
}

pub fn type_name<'a>(cx: Context<'a>, v: Value<'a>) -> &'static str {
    // TODO: string interning

    v.type_name()
}

/// Converts an ASCII character byte to a digit in the specified radix.
/// Similar to Rust's `char::to_digit`.
/// Radix must be in range 2-36.
pub fn to_digit(byte: i64, radix: i64) -> HebiResult<i64> {
    if radix < 2 || radix > 36 {
        return Err(error("radix must be in range 2-36"));
    }

    if byte < 0 || byte > 255 {
        return Err(error("byte out of range"));
    }

    let ch = byte as u8 as char;
    match ch.to_digit(radix as u32) {
        Some(digit) => Ok(digit as i64),
        None => Err(error("invalid digit for radix")),
    }
}
