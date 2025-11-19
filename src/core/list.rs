use crate::prelude::*;

pub fn map<'a>(
    mut cx: Context<'a>,
    list: Param<'a, List>,
    callee: Value<'a>,
) -> HebiResult<Ret<'a>> {
    let_root!(in cx; root);
    let callee = callee.root(&mut cx, root);

    reroot!(in cx; list = list);
    let len = list.as_ref(&cx).len();

    let_root!(in cx; new_list);
    let new_list = List::new_zeroed(&cx, new_list, len);

    for i in 0..len {
        // NOTE: the old list's size can change because the
        // user may have a reference to it. New list's size
        // can't, because the only reference we have is the
        // one above.

        let Some(old_value) = list.as_ref(&cx).get(i) else {
            return error("list size changed during `map`").into();
        };
        let_root!(in cx; value);
        let value = old_value.root(&cx, value);

        let_root!(in cx; ret);
        let ret = cx.call(&callee, (&value,), ret)?;
        unsafe {
            new_list.as_mut(&mut cx).set_raw_unchecked(i, ret.raw());
        }
    }

    Ok(cx.ret(ValueRoot::Object(new_list.as_any())))
}
