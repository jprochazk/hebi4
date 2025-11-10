use crate::prelude::*;

pub fn print<'a>(mut cx: Context<'a>, v: Value<'a>) -> HebiResult<()> {
    let v = match v.as_ref(cx.heap()) {
        ValueRef::Nil => format!("nil"),
        ValueRef::Bool(v) => format!("{v}"),
        ValueRef::Int(v) => format!("{v}"),
        ValueRef::Float(v) => format!("{v:?}"),
        ValueRef::Object(v) => format!("{v:?}"),
    };

    let o = &mut cx.stdio().stdout;
    writeln!(o, "{v}").map_err(|err| error(err.to_string()))
}
