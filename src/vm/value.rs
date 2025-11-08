use crate::gc::{GcAnyPtr, GcPtr, Trace};

// TODO: string interning

// TODO: carry object type tag directly in value's discriminant.
// This can be done via pointer tagging, there are enough bits
// to tag `Nil`, `Bool`, `Int`, `Float`.

// TODO: impl `Display` for value refs
//       -> each type can customize its own `display`
//       -> for userdata, we just print the type name

// TODO: value marshalling
//       -> to support user data, needs GcRef<T>.

// pub trait FromValueRaw: Sized {
//     fn from_value(value: ValueRaw) -> Option<Self>;
// }

// pub trait IntoValueRaw {
//     fn into_value(self, heap: &Heap) -> ValueRaw;
// }

#[derive(Default, Clone, Copy)]
#[repr(C, u64)]
pub enum ValueRaw {
    #[default]
    Nil = 0,
    Bool(bool) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
    Object(GcAnyPtr),
}

impl ValueRaw {
    /// Coerce `self` to a boolean:
    ///
    /// - `nil` is always `false`
    /// - bools are returned as-is
    /// - `int` is false if `== 0`
    /// - `float` is false if `== 0.0`
    /// - objects are always `true`
    #[inline]
    pub fn coerce_bool(self) -> bool {
        match self {
            ValueRaw::Nil => false,
            ValueRaw::Bool(v) => v,
            ValueRaw::Int(v) => v != 0,
            ValueRaw::Float(v) => v != 0.0,
            ValueRaw::Object(gc) => true,
        }
    }

    /// ## Safety
    ///
    /// - If `self` is an `Object`, the object must be live.
    #[inline]
    pub unsafe fn type_name(self) -> &'static str {
        match self {
            ValueRaw::Nil => "nil",
            ValueRaw::Bool(_) => "bool",
            ValueRaw::Int(_) => "int",
            ValueRaw::Float(_) => "float",
            ValueRaw::Object(gc) => gc.type_name(),
        }
    }

    /// ## Safety
    ///
    /// - If `self` is an `Object`, the object must be live.
    #[inline]
    pub unsafe fn into_object<T: Trace>(self) -> Option<GcPtr<T>> {
        match self {
            ValueRaw::Object(gc) => gc.cast(),
            _ => None,
        }
    }
}

pub mod closure;
pub mod function;
pub mod host_function;
pub mod list;
pub mod module;
pub mod string;
pub mod table;
pub mod userdata;

pub(crate) use self::string::StringHasher;
#[allow(unused_imports)] // re-exports
pub use self::{
    closure::Closure,
    function::FunctionProto,
    list::{List, ListIter},
    module::ModuleProto,
    string::Str,
    table::{Table, TableEntries},
    userdata::UserData,
};

#[cfg(test)]
mod tests {
    use crate::{
        value::List,
        vm::gc::{Heap, let_root},
    };

    #[test]
    fn heap_collect_on_drop() {
        // `heap` frees all managed objects on drop

        let heap = &mut Heap::new();
        let_root!(in heap; v);
        let v = List::new(heap, v, 0);
    }
}
