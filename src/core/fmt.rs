use super::convert::stringify_into;
use crate::prelude::*;

pub fn print(mut cx: Context, v: Value) -> HebiResult<()> {
    let v = match v.as_ref(cx.heap()) {
        ValueRef::Nil => format!("nil"),
        ValueRef::Bool(v) => format!("{v}"),
        ValueRef::Int(v) => format!("{v}"),
        ValueRef::Float(v) => format!("{v:?}"),
        ValueRef::Object(v) => {
            if let Some(v) = v.cast::<Str>() {
                format!("{v}")
            } else {
                format!("{v:?}")
            }
        }
    };

    let o = &mut cx.stdio().stdout;
    writeln!(o, "{v}").map_err(|err| error(err.to_string()))
}

pub fn printf(mut cx: Context, template: Param<Str>, args: Param<Table>) -> HebiResult<()> {
    let out = format_into_string(template.as_ref(cx.heap()), args.as_ref(cx.heap()))?;

    writeln!(&mut cx.stdio().stdout, "{out}").map_err(|err| error(err.to_string()))
}

fn is_identifier_char(b: u8) -> bool {
    matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'_')
}

pub fn format(cx: Context, template: Param<Str>, args: Param<Table>) -> HebiResult<String> {
    format_into_string(template.as_ref(cx.heap()), args.as_ref(cx.heap()))
}

pub(crate) fn format_into_string(template: GcRef<Str>, args: GcRef<Table>) -> HebiResult<String> {
    let mut out = Vec::new();
    format_into(template, args, &mut out)?;
    // SAFETY: The original `template` is guaranteed valid `utf-8`,
    // and `format_into` only produces valid `utf-8` as well.
    let out = unsafe { String::from_utf8_unchecked(out) };
    Ok(out)
}

pub(crate) fn format_into(
    template: GcRef<Str>,
    args: GcRef<Table>,
    out: &mut impl std::io::Write,
) -> HebiResult<()> {
    let str = template.as_str();
    let bytes = str.as_bytes();

    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if c == b'{' {
            i += 1;
            let start = i;
            loop {
                if i >= bytes.len() {
                    return Err(error("unclosed argument name"));
                } else if bytes[i] == b'}' {
                    break;
                } else if !is_identifier_char(bytes[i]) {
                    // TODO: better error?
                    return Err(error("argument names must be valid identifier"));
                } else {
                    i += 1;
                }
            }
            let name = &str[start..i];
            i += 1; // skip the `}`

            let Some(value) = args.get(name) else {
                // TODO: better error?
                return Err(error(format!("missing argument {name}")));
            };

            stringify_into(value, out)?;
        } else {
            i += 1;
            out.write(&[c]).map_err(|err| error(err.to_string()))?;
        }
    }

    Ok(())
}
