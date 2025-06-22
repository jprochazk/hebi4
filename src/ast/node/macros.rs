// macro_rules! count {
//     ($thing:tt $($tail:tt)*) => (1 + count!($($tail)*));
//     ($thing:tt) => (1);
//     () => (0);
// }

pub(super) fn _node_slice_into_packed<Src: crate::ast::Tagged + crate::ast::PackedAbi>(
    unpacked: &[Src],
) -> &[crate::ast::Packed] {
    unsafe { std::mem::transmute(unpacked) }
}

pub(super) unsafe fn _node_slice_from_packed_unchecked<
    Dst: crate::ast::Tagged + crate::ast::PackedAbi,
>(
    packed: &[crate::ast::Packed],
) -> &[Dst] {
    debug_assert!(
        packed
            .iter()
            .all(|v| Dst::tag_range().contains(&(v.tag() as u8)))
    );
    unsafe { std::mem::transmute(packed) }
}

macro_rules! declare_node {
    // matches a struct with:
    // - "fake" kind/tag annotations
    // - optional lifetime
    // - any number of component fields (for fixed-arity nodes)
    // - an optional "tail" field (for variable-arity nodes)
    // components + tail may both be present, resulting in a mixed-arity node
    //
    // Generates the struct, and a `Node` implementation for it
    (
        #[kind($NodeKind:ident), tag($TagName:ident)]
        $vis:vis struct $name:ident $(<$lifetime:lifetime>)? {
            $($field_vis:vis $field:ident : $field_ty:ty,)*
            $(#[tail] $tail_vis:vis $tail:ident : $tail_ty:ty,)?
        }
    ) => {
        $vis struct $name $(<$lifetime>)? {
            $($field_vis $field : $field_ty,)*
            $($tail_vis $tail : $tail_ty,)?
        }

        impl $(<$lifetime>)? private::Sealed for declare_node!(@with_lifetime $name $($lifetime)?) {}
        impl $(<$lifetime>)? Node for declare_node!(@with_lifetime $name $($lifetime)?) {
            type NodeKind = $NodeKind;

            type Output<'ast> = declare_node!(@replace_lifetime $name 'ast $($lifetime)?);

            const TAG: Tag = Tag::$TagName;

            #[inline]
            unsafe fn unpack_unchecked<'ast>(node: Self::NodeKind, ast: &'ast $crate::ast::Ast) -> Self::Output<'ast> {
                debug_assert!(node.base_tag() == Tag::$TagName);

                declare_node!(@unpack(ast, node) $($field : $field_ty)* $(; $tail)?);

                $name { $($field,)* $($tail)? }
            }

            #[inline]
            fn pack_into(&self, ast: &mut AstBuilder) -> Self::NodeKind {
                let tag = Tag::$TagName;

                unsafe {
                    $NodeKind::from_packed_unchecked(
                        declare_node!(@pack(ast, self, tag) $($field)* $(; $tail)?)
                    )
                }
            }
        }
    };

    // yields `Type<'lifetime>` if $lifetime is provided, and just `Type` otherwise
    (@with_lifetime $name:ident $lifetime:lifetime) => ($name<$lifetime>);
    (@with_lifetime $name:ident) => ($name);

    // yields `Type<'to>` if $from is provided, and just `Type` otherwise
    (@replace_lifetime $name:ident $to:lifetime $from:lifetime) => ($name<$to>);
    (@replace_lifetime $name:ident $to:lifetime) => ($name);

    // tail only
    (@unpack($ast:ident, $node:ident) ; $tail:ident) => {
        let $tail = unsafe {
            $ast
                .slice($node.into_packed().index(), $node.into_packed().length())
                .unwrap_unchecked()
        };
        let $tail = unsafe { $crate::ast::node::macros::_node_slice_from_packed_unchecked($tail) };
    };
    // components + tail
    (@unpack($ast:ident, $node:ident) $($field:ident : $field_ty:ty)+ ; $tail:ident) => {
        let ([$($field),*], $tail) = unsafe {
            let packed = $node.into_packed();
            $ast
                .components_with_tail(packed.index(), packed.length())
                .unwrap_unchecked()
        };
        $(
            let $field = unsafe { <$field_ty>::from_packed_unchecked($field) };
        )*
        let $tail = unsafe { $crate::ast::node::macros::_node_slice_from_packed_unchecked($tail) };
    };
    // components only
    (@unpack($ast:ident, $node:ident) $($field:ident : $field_ty:ty)+) => {
        let [$($field),*] = unsafe {
            let packed = $node.into_packed();
            $ast
                .components(packed.index())
                .unwrap_unchecked()
        };
        $(
            let $field = unsafe { $crate::ast::PackedAbi::from_packed_unchecked($field) };
        )*
    };
    // empty
    (@unpack($ast:ident, $node:ident)) => {
        let _ = $ast;
    };

    // tail only
    (@pack($ast:ident, $self:ident, $tag:ident) ; $tail:ident) => {{
        let tail = $crate::ast::node::macros::_node_slice_into_packed($self.$tail);
        let (index, length) = $ast
            .insert_packed_contiguous(tail)
            .unwrap_or_default();

        Packed::with_length_index($tag, length, index)
    }};
    // components + tail
    (@pack($ast:ident, $self:ident, $tag:ident) $($field:ident)+ ; $tail:ident) => {{
        let index = declare_node!(@pack_components($ast, $self) $($field)*);
        let tail = $crate::ast::node::macros::_node_slice_into_packed($self.$tail);
        let (_, length) = $ast
            .insert_packed_contiguous(tail)
            .unwrap_or_default();

        Packed::with_length_index($tag, length, index)
    }};
    // components only
    (@pack($ast:ident, $self:ident, $tag:ident) $($field:ident)+) => {{
        let index = declare_node!(@pack_components($ast, $self) $($field)*);

        Packed::with_index($tag, index)
    }};
    // empty
    (@pack($ast:ident, $self:ident, $tag:ident)) => {{
        let _ = $ast;
        Packed::tag_only($tag)
    }};

    // we store the index of the first field, so we need to separate the
    // list of fields into a head + tail
    (@pack_components($ast:ident, $self:ident) $first:ident $($rest:ident)*) => {{
        let index = $ast.insert_packed($self.$first.into_packed());
        $(
            $ast.insert_packed($self.$rest.into_packed());
        )*
        index
    }};
}
