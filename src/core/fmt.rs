use crate::error::{Result, error};
use crate::span::Span;
use crate::value::{ValueRaw, host_function::Context};

pub fn print(mut ctx: Context) -> Result<ValueRaw> {
    let [value] = ctx.args()?;

    let o = &mut ctx.stdio().stdout;
    match value {
        crate::gc::ValueRef::Nil => writeln!(o, "nil"),
        crate::gc::ValueRef::Bool(v) => writeln!(o, "{v}"),
        crate::gc::ValueRef::Int(v) => writeln!(o, "{v}"),
        crate::gc::ValueRef::Float(v) => writeln!(o, "{v:?}"),
        crate::gc::ValueRef::Object(v) => writeln!(o, "{v:?}"),
    }
    // TODO: span here
    .map_err(|err| error(err.to_string(), Span::empty()))?;

    Ok(ValueRaw::Nil)
}
