use crate::error::{Result, error};
use crate::gc::ValueRef;
use crate::span::Span;
use crate::value::host_function::Context;

pub fn print<'a>(mut cx: Context<'a>, v: ValueRef<'a>) -> Result<()> {
    // let [value] = cx.args()?;

    let o = &mut cx.stdio().stdout;
    match v {
        ValueRef::Nil => writeln!(o, "nil"),
        ValueRef::Bool(v) => writeln!(o, "{v}"),
        ValueRef::Int(v) => writeln!(o, "{v}"),
        ValueRef::Float(v) => writeln!(o, "{v:?}"),
        ValueRef::Object(v) => writeln!(o, "{v:?}"), //unsafe { writeln!(o, "{:?}", v.as_ref()) },
    }
    // TODO: span here
    .map_err(|err| error(err.to_string(), Span::empty()))
}
