use crate::prelude::*;

pub fn map<'a>(
    mut cx: Context<'a>,
    list: Param<'a, List>,
    callee: Value<'a>,
) -> HebiResult<Ret<'a>> {
    reroot!(in cx; list = list);

    let_root!(in cx; root);
    let callee = callee.root(&mut cx, root);

    let len = list.as_ref(&cx).len();

    let_root!(in cx; new_list);
    let new_list = List::new_zeroed(&cx, new_list, len);

    for i in 0..len {
        // TODO: reusable roots?
        let_root!(in cx; value);
        // SAFETY: `i` is in bounds for `list`
        let value = unsafe { list.as_ref(&cx).get_unchecked(i) }.root(&cx, value);

        let_root!(in cx; ret);
        let ret = cx.call(&callee, (&value,), ret)?;

        // SAFETY: `i` is in bounds for `new_list`
        unsafe {
            new_list.as_mut(&mut cx).set_raw_unchecked(i, ret.raw());
        }

        // NOTE: the old list's size can change because the
        // user may have a reference to it. New list's size
        // can't, because the only reference we have is the
        // one above.
        if list.as_ref(&cx).len() != len {
            return error("list size changed during `map`").into();
        }
    }

    cx.ret(new_list)
}

pub fn append<'a>(
    mut cx: Context<'a>,
    mut list: Param<'a, List>,
    value: Value<'a>,
) -> HebiResult<Ret<'a>> {
    let_root!(in cx; value_root);
    let value = value.root(&mut cx, value_root);
    list.as_mut(&mut cx).push(value);

    cx.ret(list)
}
