#[macro_export]
#[doc(hidden)]
macro_rules! __string {
    (in $heap:ident; mut $string:ident = $str:expr) => {
        // SAFETY: the allocated object is immediately rooted
        $crate::gc::let_root_unchecked!(
            unsafe in $heap;
            mut $string = $crate::value::String::alloc($heap, $str)
        );
    };
    (in $heap:ident; $string:ident = $str:expr) => {
        // SAFETY: the allocated object is immediately rooted
        $crate::gc::let_root_unchecked!(
            unsafe in $heap;
            $string = $crate::value::String::alloc($heap, $str)
        );
    };
}

/// Allocate a managed string, and immediately root it.
///
/// The rhs of the assignment is the string to copy.
///
/// ```rust
/// # use hebi4::{gc::Heap, value::string};
/// # let mut heap = unsafe { &mut Heap::__testing() };
/// string!(in heap; string = "test");
/// ```
pub use crate::__string as string;

#[macro_export]
#[doc(hidden)]
macro_rules! __list {
    (in $heap:ident; mut $list:ident = $capacity:expr) => {
        // SAFETY: the allocated object is immediately rooted
        $crate::gc::let_root_unchecked!(
            unsafe in $heap;
            mut $list = $crate::value::List::alloc($heap, $capacity)
        );
    };
    (in $heap:ident; $list:ident = $capacity:expr) => {
        // SAFETY: the allocated object is immediately rooted
        $crate::gc::let_root_unchecked!(
            unsafe in $heap;
            $list = $crate::value::List::alloc($heap, $capacity)
        );
    };
}

/// Allocate a managed list, and immediately root it.
///
/// The rhs of the assignment is the initial capacity of the list.
///
/// ```rust
/// # use hebi4::{gc::Heap, value::list};
/// # let mut heap = unsafe { &mut Heap::__testing() };
/// list!(in heap; list = 0);
/// ```
pub use crate::__list as list;

#[macro_export]
#[doc(hidden)]
macro_rules! __table {
    (in $heap:ident; mut $table:ident = $capacity:expr) => {
        // SAFETY: the allocated object is immediately rooted
        $crate::gc::let_root_unchecked!(
            unsafe in $heap;
            mut $table = $crate::value::Table::alloc($heap, $capacity)
        );
    };
    (in $heap:ident; $table:ident = $capacity:expr) => {
        // SAFETY: the allocated object is immediately rooted
        $crate::gc::let_root_unchecked!(
            unsafe in $heap;
            $table = $crate::value::Table::alloc($heap, $capacity)
        );
    };
}

/// Allocate a managed table, and immediately root it.
///
/// The rhs of the assignment is the initial capacity of the table.
///
/// ```rust
/// # use hebi4::{gc::Heap, value::table};
/// # let mut heap = unsafe { &mut Heap::__testing() };
/// table!(in heap; table = 0);
/// ```
pub use crate::__table as table;
