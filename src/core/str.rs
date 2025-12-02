use crate::{prelude::*, value::ValueRaw};

pub fn starts_with<'a>(cx: Context<'a>, str: Param<'a, Str>, with: Param<'a, Str>) -> bool {
    str.as_ref(&cx)
        .as_str()
        .starts_with(with.as_ref(&cx).as_str())
}

pub fn strip_prefix<'a>(
    cx: Context<'a>,
    str: Param<'a, Str>,
    with: Param<'a, Str>,
) -> HebiResult<Ret<'a>> {
    match str.as_ref(&cx).as_str().find(with.as_ref(&cx).as_str()) {
        Some(prefix_start) => {
            let prefix_len = with.as_ref(&cx).len();

            let_root!(in &cx; out);
            let out = Str::new(
                &cx,
                out,
                &str.as_ref(&cx).as_str()[prefix_start + prefix_len..],
            );
            cx.ret(out)
        }
        None => cx.ret(str),
    }
}

pub fn split_at<'a>(mut cx: Context<'a>, str: Param<'a, Str>, at: i64) -> HebiResult<Ret<'a>> {
    let str = str.as_ref(&cx);
    let (first, last) = match str.as_str().split_at_checked(at as usize) {
        Some(v) => v,
        None => return Err(error("index out of bounds")),
    };

    let_root!(in &cx; root);
    let first = Str::new(&cx, root, first);

    let_root!(in &cx; root);
    let last = Str::new(&cx, root, last);

    let_root!(in &cx; out);
    let out = List::new_zeroed(&cx, out, 2);

    unsafe {
        out.as_mut(&mut cx)
            .set_raw_unchecked(0, ValueRaw::Object(first.as_any().as_ptr()));
        out.as_mut(&mut cx)
            .set_raw_unchecked(1, ValueRaw::Object(last.as_any().as_ptr()));
    }

    cx.ret(out)
}

// pub fn substring<'a>(
//     cx: Context<'a>,
//     str: Param<'a, Str>,
//     at: i64,
//     len: i64,
// ) -> HebiResult<Ret<'a>> {
//     todo!()
// }

pub fn split<'a>(
    mut cx: Context<'a>,
    str: Param<'a, Str>,
    by: Param<'a, Str>,
) -> HebiResult<Ret<'a>> {
    let_root!(in &cx; out);
    let out = List::new(&cx, out, 0);

    let len = str.as_ref(&cx).len();
    let by_len = by.as_ref(&cx).len();

    if by_len == 0 {
        unsafe {
            out.as_mut(&mut cx)
                .push_raw(ValueRaw::Object(str.as_ptr().as_any()));
        }
        return cx.ret(out);
    }

    let mut start = 0;
    let mut i = 0;

    while i < len {
        let mut matches = true;
        if i + by_len > len {
            matches = false;
        } else {
            for j in 0..by_len {
                if str.as_ref(&cx).as_str().as_bytes()[i + j]
                    != by.as_ref(&cx).as_str().as_bytes()[j]
                {
                    matches = false;
                    break;
                }
            }
        }

        if matches {
            let segment = str.as_ref(&cx);
            let segment = &segment.as_str()[start..i];
            let_root!(in &cx; seg);
            let seg = Str::new(&cx, seg, segment);
            out.as_mut(&mut cx).push(ValueRoot::Object(seg.as_any()));

            start = i + by_len;
            i = start;
        } else {
            i += 1;
        }
    }

    let segment = str.as_ref(&cx);
    let segment = &segment.as_str()[start..len];
    let_root!(in &cx; seg);
    let seg = Str::new(&cx, seg, segment);
    out.as_mut(&mut cx).push(ValueRoot::Object(seg.as_any()));

    cx.ret(out)
}

pub fn lines<'a>(mut cx: Context<'a>, str: Param<'a, Str>) -> HebiResult<Ret<'a>> {
    let_root!(in &cx; out);
    let out = List::new(&cx, out, 0);

    let mut start = 0;
    let mut i = 0;
    let len = str.as_ref(&cx).len();

    macro_rules! push_segment {
        () => {{
            let segment = str.as_ref(&cx);
            let segment = &segment.as_str()[start..i];
            let remainder = (len as isize) - (i as isize);
            if segment.is_empty() && remainder <= 0 {
                // don't output final segment
                false
            } else {
                let_root!(in &cx; line);
                let line = Str::new(&cx, line, segment);

                out.as_mut(&mut cx).push(ValueRoot::Object(line.as_any()));

                start = i + 1;
                i += 1;

                true
            }
        }};
    }

    while i < len {
        match str.as_ref(&cx).as_str().as_bytes()[i] {
            // `\r\n`
            b'\r' if str.as_ref(&cx).as_str().as_bytes().get(i + 1) == Some(&b'\n') => {
                // this only skips the `\r`:
                if !push_segment!() {
                    break;
                }

                // also skip the `\n`
                start += 1;
                i += 1;
            }

            // `\n`
            b'\n' => {
                if !push_segment!() {
                    break;
                }
            }

            // other
            _ => {
                i += 1;
            }
        }
    }

    push_segment!();

    let _ = (start, i);

    cx.ret(out)
}

pub fn str_len(cx: Context, str: Param<Str>) -> i64 {
    str.as_ref(&cx).len() as i64
}
