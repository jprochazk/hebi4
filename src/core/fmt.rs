use crate::{
    error::{Result, error},
    vm::{gc::ValueRef, value::host_function::Context},
};

pub fn print<'a>(mut cx: Context<'a>, v: ValueRef<'a>) -> Result<()> {
    let o = &mut cx.stdio().stdout;
    match v {
        ValueRef::Nil => writeln!(o, "nil"),
        ValueRef::Bool(v) => writeln!(o, "{v}"),
        ValueRef::Int(v) => writeln!(o, "{v}"),
        ValueRef::Float(v) => writeln!(o, "{v:?}"),
        ValueRef::Object(v) => writeln!(o, "{v:?}"),
    }
    .map_err(|err| error(err.to_string()))
}
