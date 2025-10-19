/// ```rust,ignore
/// string!(in heap; string = "test");
/// println!("{string}");
/// ```
#[macro_export]
macro_rules! string {
    (in $heap:ident; $string:ident = $str:expr) => {
        // SAFETY: the allocated object is immediately rooted
        let_root_unchecked!(
            unsafe in $heap;
            $string = $crate::vm::value::String::alloc($heap, $str)
        );
    };
}

/// ```rust,ignore
/// list!(in heap; list = 0);
/// ```
#[macro_export]
macro_rules! list {
    (in $heap:ident; $list:ident = $capacity:expr) => {
        // SAFETY: the allocated object is immediately rooted
        let_root_unchecked!(
            unsafe in $heap;
            $list = $crate::vm::value::List::alloc($heap, $capacity)
        );
    };
}

/// ```rust,ignore
/// table!(in heap; table = 0);
/// ```
#[macro_export]
macro_rules! table {
    (in $heap:ident; $table:ident = $capacity:expr) => {
        // SAFETY: the allocated object is immediately rooted
        let_root_unchecked!(
            unsafe in $heap;
            $table = $crate::vm::value::Table::alloc($heap, $capacity)
        );
    };
}
