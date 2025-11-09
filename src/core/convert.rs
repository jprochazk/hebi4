use crate::prelude::*;

pub fn to_str(_cx: Context, v: ValueRef<'_>) -> String {
    match v {
        ValueRef::Nil => "nil".to_owned(),
        ValueRef::Bool(v) => v.to_string(),
        ValueRef::Int(v) => v.to_string(),
        ValueRef::Float(v) => format!("{v:?}"),
        ValueRef::Object(v) => {
            format!("{v:?}")
        }
    }
}

pub fn parse_int(_cx: Context, v: GcRef<Str>) -> HebiResult<i64> {
    v.as_str()
        .parse::<i64>()
        .map_err(|err| error(err.to_string()))
}

pub fn parse_float(_cx: Context, v: GcRef<Str>) -> HebiResult<f64> {
    v.as_str()
        .parse::<f64>()
        .map_err(|err| error(err.to_string()))
}

pub fn to_int(_cx: Context, v: f64) -> i64 {
    v as i64
}

pub fn to_float(_cx: Context, v: i64) -> f64 {
    v as f64
}
