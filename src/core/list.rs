use crate::{
    gc::GcPtr,
    prelude::*,
    value::{ValueRaw, host_function::HostFunction},
};

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

pub fn list_len(cx: Context, list: Param<List>) -> i64 {
    list.as_ref(&cx).len() as i64
}

pub fn list_iter<'a>(cx: Context<'a>, list: Param<'a, List>) -> HebiResult<Ret<'a>> {
    let ptr = unsafe { make_list_iter_raw(&cx, list.as_ptr()) };
    unsafe { cx.ret_raw(ValueRaw::Object(ptr.as_any())) }
}

// exposed like this so it can be directly dispatched by VM
pub(crate) unsafe fn make_list_iter_raw(heap: &Heap, list: GcPtr<List>) -> GcPtr<HostFunction> {
    // TODO: string interning
    let name = Str::alloc(heap, "@list_iter");
    let arity = 0;
    let num_upvalues = 3; // (list, initial_len, index)
    let func = HostFunction::alloc_closure(
        heap,
        name,
        arity,
        num_upvalues,
        crate::__f!(list_iter_step).callback().clone(),
    );

    func.as_mut().upvalues[0] = ValueRaw::Object(list.as_any());
    func.as_mut().upvalues[1] = ValueRaw::Int(list.as_ref().len() as i64);
    func.as_mut().upvalues[2] = ValueRaw::Int(0);

    func
}

fn list_iter_step<'a>(cx: Context<'a>) -> HebiResult<Ret<'a>> {
    // We care greatly about the performance of this function, so we avoid using roots.
    // SAFETY:
    // - `list` is reachable from `cx.callee.upvalues`, which is reachable from call stack.
    // - We have exactly 3 upvalues with types [List, Int, Int] - see `make_list_iter_raw`.
    unsafe {
        let callee = cx.callee();

        let [list, initial_len, index]: [ValueRaw; 3] =
            callee.as_ref().upvalues[..].try_into().unwrap_unchecked();

        let list = list.into_object::<List>().unwrap_unchecked();
        let initial_len = initial_len.int_unchecked() as usize;
        let index = index.int_unchecked() as usize;

        // List can be mutated between calls to step, which is illegal.
        // We check this _before_ bounds checking.
        if list.as_ref().len() != initial_len {
            return error("list size changed during iteration").into();
        }

        // Done, signal `nil`.
        if index >= list.as_ref().len() {
            return cx.ret(ValueRoot::Nil);
        }

        // Not done, increment index and return current item.
        callee.as_mut().upvalues[2] = ValueRaw::Int((index + 1) as i64);

        let item = list.as_ref().get_unchecked(index).raw();
        cx.ret_raw(item)
    }
}
