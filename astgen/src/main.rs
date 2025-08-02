//! Node categories:
//! - Stmt
//! - Expr
//!
//! Types of nodes:
//! - Empty nodes
//! - Fixed-arity nodes
//! - Variable-arity nodes
//! - Mixed-arity nodes
//! - Inline-value nodes
//!   - Ident/String/Float stores intern table id as u32
//!   - Int stores value up to u56
//!
//! NOTE: Previously also had `Misc`, which was just `Ident`,
//! but there was no difference between `Ident` and `ExprUse`.
//! Can make a 3rd node category for these kinds of things
//! which don't need to be "traversed", and are only stored
//! directly in other nodes.
//! It stings a bit that `ExprUse` is "wasteful" though, because
//! it won't directly store an ident intern id...
//!
//! Need to generate:
//! - Kind enum
//! - Category-specific kind enums
//! - `Packed` wrapper structs for:
//!   - Categories (Stmt, Expr)
//!   - Nodes (Return, If, Do, Fn, Int, etc.)
//! - "Packing" function (store subnodes in AST)
//!   - Must include spans
//! - "Unpacked" structures for nodes
//!   - Spans are stored externally, so need getters for
//!     individual fields' spans
//! - Visitor trait
//! - Debug implementation
//!
//!

// TODO: statically prevent one AST's nodes from being used with another AST
// - each node gains an invariant lifetime
// - retrieving a node will brand it with its AST's lifetime
// - many places have to be updated to deal with this

use heck::AsPascalCase;
use std::path::Path;
use std::{collections::BTreeMap, str::FromStr};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).take(2).collect();
    let [infile, outfile] = &args[..] else {
        panic!("invalid args: {args:?}\nexpected: infile outfile");
    };
    let infile = Path::new(&infile);
    let infile = std::fs::read_to_string(infile).expect("infile should exist");

    let mut nodes = parse_root(&infile);

    assign_tag_numbers(&mut nodes);
    let out = emit_nodes(nodes);

    if outfile == "-" {
        use std::io::Write as _;
        std::io::stdout().write_all(out.as_bytes()).unwrap();
    } else {
        let outfile = Path::new(&outfile);
        std::fs::create_dir_all(outfile.parent().unwrap_or(outfile))
            .expect("failed to create parent directories");
        std::fs::write(&outfile, out).expect("failed to write file");
        cmd(format!("rustfmt {}", outfile.display()));
    }
}

fn cmd(cmd: impl AsRef<str>) {
    let cmd = cmd.as_ref();
    let mut parts = cmd.split_ascii_whitespace();
    let Some(cmd) = parts.next() else {
        return;
    };
    let args = parts;

    let mut p = std::process::Command::new(cmd)
        .args(args)
        .stdout(std::io::stdout())
        .stderr(std::io::stderr())
        .spawn()
        .unwrap();
    let status = p.wait().unwrap();
    if !status.success() {
        eprintln!("command exited with non-zero exit code");
        std::process::exit(1);
    }
}

#[derive(Debug)]
struct Node<'a> {
    name: &'a str,
    category: Category,
    kind: NodeKind<'a>,
    tag_n: u8,
}

impl Node<'_> {
    #[inline]
    fn has_tail(&self) -> bool {
        match &self.kind {
            NodeKind::Empty => false,
            NodeKind::FixedArity { .. } => false,
            NodeKind::VariableArity { .. } => true,
            NodeKind::MixedArity { .. } => true,
            NodeKind::InlineValue { .. } => false,
        }
    }

    #[inline]
    fn tail(&self) -> Option<&Field<'_>> {
        match &self.kind {
            NodeKind::VariableArity { tail } | NodeKind::MixedArity { tail, .. } => Some(tail),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum NodeKind<'a> {
    Empty,
    FixedArity {
        fields: Vec<Field<'a>>,
        inline: Option<Inline<'a>>, // only u24
    },
    VariableArity {
        tail: Field<'a>,
    },
    MixedArity {
        fields: Vec<Field<'a>>,
        tail: Field<'a>,
    },
    InlineValue {
        inline: Inline<'a>, // u24-u56
    },
}

#[derive(Debug, Clone, Copy)]
enum Category {
    Root,
    Stmt,
    Expr,
    Data, // only used to store data for other nodes, not as a node itself
}

impl Category {
    /// Returns `true` if the category is [`Root`].
    ///
    /// [`Root`]: Category::Root
    #[inline]
    fn is_root(&self) -> bool {
        matches!(self, Self::Root)
    }

    /// Returns `true` if the category is [`Stmt`].
    ///
    /// [`Stmt`]: Category::Stmt
    #[inline]
    fn is_stmt(&self) -> bool {
        matches!(self, Self::Stmt)
    }

    /// Returns `true` if the category is [`Expr`].
    ///
    /// [`Expr`]: Category::Expr
    #[inline]
    fn is_expr(&self) -> bool {
        matches!(self, Self::Expr)
    }

    /// Returns `true` if the category is [`Data`].
    ///
    /// [`Data`]: Category::Data
    #[inline]
    fn is_data(&self) -> bool {
        matches!(self, Self::Data)
    }
}

#[derive(Debug)]
struct Field<'a> {
    name: &'a str,
    ty: &'a str,
    opt: bool,
    tail: bool,
}

struct WrapTy<T>(&'static str, T);

impl<T: std::fmt::Display> std::fmt::Display for WrapTy<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}<{}>", self.0, self.1)
    }
}

trait WrapTyExt<T>: Sized {
    fn wrap_ty(self, ty: &'static str) -> WrapTy<Self>;
}

impl<T: std::fmt::Display> WrapTyExt<T> for T {
    fn wrap_ty(self, ty: &'static str) -> WrapTy<Self> {
        WrapTy(ty, self)
    }
}

struct PrefixTy<T>(&'static str, T);

impl<T: std::fmt::Display> std::fmt::Display for PrefixTy<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

trait PrefixTyExt<T>: Sized {
    fn prefix_ty(self, ty: &'static str) -> PrefixTy<Self>;
}

impl<T: std::fmt::Display> PrefixTyExt<T> for T {
    fn prefix_ty(self, ty: &'static str) -> PrefixTy<Self> {
        PrefixTy(ty, self)
    }
}

struct WrapTail<T>(T);

impl<T: std::fmt::Display> std::fmt::Display for WrapTail<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

struct MaybePascalCase<T>(T, bool);

impl<T: AsRef<str>> std::fmt::Display for MaybePascalCase<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.1 {
            write!(f, "{}", AsPascalCase(&self.0))
        } else {
            write!(f, "{}", self.0.as_ref())
        }
    }
}

pub struct Join<Iter, Sep>
where
    Iter: Iterator,
{
    iter: Iter,
    sep: Sep,
}

impl<Iter, Sep> std::fmt::Display for Join<Iter, Sep>
where
    Iter: Iterator + Clone,
    <Iter as Iterator>::Item: std::fmt::Display,
    Sep: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.iter.clone().peekable();
        while let Some(item) = iter.next() {
            write!(f, "{item}")?;
            if iter.peek().is_some() {
                write!(f, "{}", self.sep)?;
            }
        }
        Ok(())
    }
}

pub trait JoinIter: Sized + Iterator {
    fn join<Sep>(self, sep: Sep) -> Join<Self, Sep>;
}

impl<Iter> JoinIter for Iter
where
    Iter: Sized + Iterator + Clone,
{
    fn join<Sep>(self, sep: Sep) -> Join<Self, Sep> {
        Join { iter: self, sep }
    }
}

enum Either<A, B> {
    A(A),
    B(B),
}

impl<A: std::fmt::Display, B: std::fmt::Display> std::fmt::Display for Either<A, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Either::A(a) => std::fmt::Display::fmt(a, f),
            Either::B(b) => std::fmt::Display::fmt(b, f),
        }
    }
}

fn is_lowercase_ty(v: &str) -> bool {
    match v {
        "u56" => true,
        "u32" => true,
        "u24" => true,
        "f32" => true,
        "bool" => true,
        _ => false,
    }
}

fn is_builtin_ty(v: &str) -> bool {
    match v {
        "u32" => true,
        "f32" => true,
        "bool" => true,
        _ => false,
    }
}

impl<'a> Field<'a> {
    #[inline]
    fn resolved_ty<'d>(&'d self, super_: bool) -> impl std::fmt::Display + 'd {
        if self.opt && self.tail {
            panic!("a type cannot be optional and tail at the same time");
        }

        let ty = MaybePascalCase(self.ty, !is_lowercase_ty(self.ty));
        let ty = if super_ && !is_builtin_ty(self.ty) {
            Either::A(ty.prefix_ty("super::"))
        } else {
            Either::B(ty)
        };
        if self.opt {
            Either::A(ty.wrap_ty("Opt"))
        } else if self.tail {
            Either::B(Either::A(WrapTail(ty)))
        } else {
            Either::B(Either::B(ty))
        }
    }

    #[inline]
    fn resolved_ty_spanned<'d>(&'d self, super_: bool) -> impl std::fmt::Display + 'd {
        if self.opt && self.tail {
            panic!("a type cannot be optional and tail at the same time");
        }

        let ty = MaybePascalCase(self.ty, !is_lowercase_ty(self.ty));
        let ty = if super_ {
            Either::A(ty.prefix_ty("super::"))
        } else {
            Either::B(ty)
        };
        if self.opt {
            Either::A(ty.wrap_ty("Opt").wrap_ty("Spanned"))
        } else if self.tail {
            Either::B(Either::A(WrapTail(ty.wrap_ty("Spanned"))))
        } else {
            Either::B(Either::B(ty.wrap_ty("Spanned")))
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Inline<'a> {
    name: &'a str,
    ty: &'a str,
    bits: Bits,
}

impl Inline<'_> {
    #[inline]
    fn resolved_ty<'d>(&'d self, super_: bool) -> impl std::fmt::Display + 'd {
        let ty = MaybePascalCase(self.ty, !is_lowercase_ty(self.ty));
        if super_ && !is_builtin_ty(self.ty) {
            Either::A(ty.prefix_ty("super::"))
        } else {
            Either::B(ty)
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Bits {
    U24,
    U32,
    U56,
}

#[derive(Debug)]
struct BitsParseError;
impl std::error::Error for BitsParseError {}
impl std::fmt::Display for BitsParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "failed to parse bits")
    }
}

impl FromStr for Bits {
    type Err = BitsParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let v = match s {
            "u24" => Bits::U24,
            "u32" => Bits::U32,
            "u56" => Bits::U56,
            _ => return Err(BitsParseError),
        };
        Ok(v)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct NodeId(usize);

#[derive(Debug)]
struct Nodes<'a> {
    flat: Vec<Node<'a>>,
    by_name: BTreeMap<&'a str, NodeId>,

    stmts: Vec<NodeId>,
    exprs: Vec<NodeId>,
    data: Vec<NodeId>,
}

impl<'a> Nodes<'a> {
    fn new() -> Self {
        let mut this = Self {
            flat: Default::default(),
            by_name: Default::default(),

            stmts: Default::default(),
            exprs: Default::default(),
            data: Default::default(),
        };

        let root = Node {
            name: "root",
            category: Category::Root,
            kind: NodeKind::VariableArity {
                tail: Field {
                    name: "body",
                    ty: "stmt",
                    opt: false,
                    tail: true,
                },
            },
            tag_n: 1,
        };
        this.add(root.name, root);

        this
    }

    #[inline]
    fn add(&mut self, name: &'a str, desc: Node<'a>) {
        let node_id = NodeId(self.flat.len());

        match desc.category {
            Category::Root => {}
            Category::Stmt => self.stmts.push(node_id),
            Category::Expr => self.exprs.push(node_id),
            Category::Data => self.data.push(node_id),
        }
        if self.by_name.insert(name, node_id).is_some() {
            panic!("duplicate node: {name}");
        }
        self.flat.push(desc);
    }

    #[inline]
    fn root(&self) -> &Node<'_> {
        &self.flat[0]
    }

    #[inline]
    fn in_order(&self) -> impl Iterator<Item = &Node<'_>> + '_ {
        std::iter::once(self.root()).chain(
            self.stmts
                .iter()
                .copied()
                .chain(self.exprs.iter().copied())
                .chain(self.data.iter().copied())
                .map(|node| &self.flat[node.0]),
        )
    }

    #[inline]
    fn stmts(&self) -> impl Iterator<Item = &Node<'_>> + '_ {
        self.stmts.iter().copied().map(|node| &self.flat[node.0])
    }

    #[inline]
    fn exprs(&self) -> impl Iterator<Item = &Node<'_>> + '_ {
        self.exprs.iter().copied().map(|node| &self.flat[node.0])
    }

    #[inline]
    fn category(&self, ty: &str) -> Category {
        match ty {
            "expr" => Category::Expr,
            "stmt" => Category::Stmt,
            _ => {
                let id = self.by_name[ty];
                self.flat[id.0].category
            }
        }
    }
}

fn parse_root<'a>(s: &'a str) -> Nodes<'a> {
    let mut nodes = Nodes::new();

    for node_desc in s.split(';') {
        let node_desc = node_desc.trim();
        if node_desc.is_empty() {
            continue;
        }

        parse_node_desc(&mut nodes, node_desc);
    }

    nodes
}

fn parse_node_desc<'a>(nodes: &mut Nodes<'a>, node_desc: &'a str) {
    let Some((kind, rest)) = node_desc.split_once(' ') else {
        panic!("failed to parse node:\n{node_desc:?}");
    };
    let kind = kind.trim_end();
    let rest = rest.trim_start();

    let category = match kind {
        "stmt" => Category::Stmt,
        "expr" => Category::Expr,
        "data" => Category::Data,
        _ => panic!("invalid category: {kind}\n{node_desc}"),
    };

    let name_end_idx = rest
        .find('(')
        .or_else(|| rest.find('{'))
        .unwrap_or(rest.len());
    let name = rest[..name_end_idx].trim();
    let rest = rest[name_end_idx..].trim();

    let inline = if let Some(inline_info) = rest.strip_prefix('(') {
        let Some(end_idx) = inline_info.find(')') else {
            panic!("missing closing ) in node:\n{node_desc}");
        };
        let inline_info = &inline_info[..end_idx];
        let Some((inline_name, inline_info)) = inline_info.split_once(':') else {
            panic!("missing : in inline info:\n{node_desc}");
        };
        let name = inline_name.trim();
        let inline_info = inline_info.trim();

        let Some((ty, bits)) = inline_info.split_once(" as ") else {
            panic!("missing type and/or bits in inline info:\n{node_desc}");
        };
        let ty = ty.trim();
        let bits = bits.trim().parse().unwrap();

        Some(Inline { name, ty, bits })
    } else {
        None
    };

    let mut fields = Vec::new();
    let mut tail = None;

    if let Some(body_idx) = rest.find('{') {
        let Some(body_end_idx) = rest.rfind('}') else {
            panic!("failed to find body end:\n{node_desc}");
        };

        let body = rest[body_idx + 1..body_end_idx].trim();
        for field_desc in body.split(',') {
            let field_desc = field_desc.trim();
            if field_desc.is_empty() {
                continue;
            }

            let Some((name, ty)) = field_desc.split_once(':') else {
                panic!("invalid field: {field_desc:?}\n{node_desc}");
            };
            let name = name.trim();
            let ty = ty.trim();
            let (ty, opt) = match ty.strip_suffix('?') {
                Some(ty) => (ty, true),
                None => (ty, false),
            };

            let is_tail = ty.starts_with('[');
            if is_tail {
                let ty = ty.strip_prefix('[').unwrap();
                let ty = ty.trim();
                let ty = match ty.strip_suffix(']') {
                    Some(v) => v,
                    None => panic!("invalid field ty, missing closing ]:\n{node_desc}"),
                };
                let ty = ty.trim();
                tail = Some(Field {
                    name,
                    ty,
                    opt: false,
                    tail: true,
                });
            } else {
                fields.push(Field {
                    name,
                    ty,
                    opt,
                    tail: false,
                });
            }
        }
    };

    let kind = match (&fields[..], inline, tail) {
        ([], None, None) => NodeKind::Empty,
        ([], None, Some(tail)) => NodeKind::VariableArity { tail },
        ([], Some(inline), None) => NodeKind::InlineValue { inline },
        ([_, ..], None, None) => NodeKind::FixedArity {
            fields,
            inline: None,
        },
        ([_, ..], Some(inline), None) => {
            assert!(
                matches!(inline.bits, Bits::U24),
                "invalid node {name}, fixed-arity nodes may use at most 24 bits of inline storage:\n{node_desc}"
            );
            NodeKind::FixedArity {
                fields,
                inline: Some(inline),
            }
        }
        ([_, ..], None, Some(tail)) => NodeKind::MixedArity { fields, tail },
        (_, Some(_), Some(_)) => {
            panic!(
                "invalid node {name}, mixed-arity nodes may not use inline storage:\n{node_desc}"
            );
        }
    };

    let node = Node {
        name,
        category,
        kind,
        tag_n: u8::MAX,
    };
    nodes.add(name, node);
}

fn assign_tag_numbers(nodes: &mut Nodes<'_>) {
    let mut n = 2; // 0 is invalid, 1 = root, 255 = none

    // order is stmt, expr, data
    for stmt in nodes.stmts.iter().copied() {
        nodes.flat[stmt.0].tag_n = n;
        n += 1;
    }

    for expr in nodes.exprs.iter().copied() {
        nodes.flat[expr.0].tag_n = n;
        n += 1;
    }

    for data in nodes.data.iter().copied() {
        nodes.flat[data.0].tag_n = n;
        n += 1;
    }
}

use std::fmt::Write as _;

/// Emit a line
macro_rules! ln {
    ($f:ident, $($tt:tt)*) => (writeln!($f, $($tt)*).unwrap());
    ($f:ident) => (writeln!($f).unwrap());
}

/// Emit multi-line string
macro_rules! ml {
    ($f:ident, $($tt:tt)*) => (indoc::writedoc!($f, $($tt)*).unwrap());
}

fn emit_nodes(nodes: Nodes<'_>) -> String {
    let mut out = String::new();

    emit_prelude(&mut out);
    emit_runtime(&mut out);
    emit_nodekind_enum(&nodes, &mut out);
    emit_category_kind_enums(&nodes, &mut out);
    emit_packed_wrappers(&nodes, &mut out);
    emit_node_parts(&nodes, &mut out);
    emit_pack_impls(&nodes, &mut out);
    emit_field_getters(&nodes, &mut out);
    // emit_unpack_impls(&nodes, &mut out);
    emit_span_getters(&nodes, &mut out);
    // emit_visitor(&nodes, &mut out);
    emit_debug_impls(&nodes, &mut out);

    out
}

fn emit_runtime(out: &mut String) {
    let runtime = include_str!("./runtime.rs");

    let file_start_marker = "//file-start";
    let runtime = runtime
        .find(file_start_marker)
        .map(|i| &runtime[i + file_start_marker.len()..])
        .unwrap_or(runtime);

    out.push_str(runtime);
}

fn emit_prelude(out: &mut String) {
    ml!(
        out,
        "
        // Generated by astgen at {now}
        //
        // See `astgen/src/main.rs` and `astgen/src/runtime.rs`

        use super::{{
            Ast,
            IdentId, StrId, IntId, FloatId,
            AssignOp, InfixOp, PrefixOp,
        }};
        use crate::span::{{Spanned, Span}};
        ",
        now = jiff::Zoned::now().in_tz("UTC").unwrap(),
    );
}

fn emit_nodekind_enum(nodes: &Nodes, out: &mut String) {
    ln!(
        out,
        "#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]"
    );
    ln!(out, "#[repr(u8)]");
    ln!(out, "pub enum NodeKind {{");
    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);
        let n = node.tag_n;
        ln!(out, "    {name} = {n},");
    }
    ln!(out, "    None = 255,");
    ln!(out, "}}\n");
    ln!(out, "impl NodeKind {{");
    ln!(
        out,
        "    const fn all() -> &'static [(&'static str, u8)] {{"
    );
    ln!(out, "        &[");
    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);
        let n = node.tag_n;
        ln!(out, "    (\"{name}\", {n}),");
    }
    ln!(out, "    (\"None\", 255),");
    ln!(out, "        ]");
    ln!(out, "    }}");
    ln!(out, "}}\n");
}

fn emit_category_kind_enums(nodes: &Nodes, out: &mut String) {
    // note: intentionally leaving out `data`
    for (category_name, members) in [("stmt", &nodes.stmts), ("expr", &nodes.exprs)] {
        let category_name = AsPascalCase(category_name);
        ln!(out, "#[repr(u8)]");
        ln!(out, "pub enum {category_name}Kind<'a> {{");
        for node in members.iter().copied() {
            let node = &nodes.flat[node.0];
            let name = AsPascalCase(node.name);
            ln!(
                out,
                "    {name}(Node<'a, {name}>) = NodeKind::{name} as u8,"
            );
        }
        ln!(out, "}}\n");
        ml!(
            out,
            "
            impl<'a> Node<'a, {category_name}> {{
                #[inline]
                pub fn kind(&self) -> {category_name}Kind<'a> {{
                    let node: &'a {category_name} = &*self.node;
                    match node.0.kind() {{
                        {kinds}
                        // SAFETY: guaranteed to be a valid `Stmt`
                        _ => unsafe {{ std::hint::unreachable_unchecked() }}
                    }}
                }}
            }}
            ",
            kinds = members
                .iter()
                .map(|&id| &nodes.flat[id.0])
                .map(|node| {
                    let name = AsPascalCase(node.name);
                    format!(
                        "NodeKind::{name} => {category_name}Kind::{name}(Node {{ ast: self.ast, node: unsafe {{ {name}::from_packed(&node.0) }} }}),"
                    )
                })
                .join('\n')
        );
    }
}

fn emit_packed_wrappers(nodes: &Nodes, out: &mut String) {
    // note: intentionally leaving out `data`
    let things = ["stmt", "expr"]
        .iter()
        .copied()
        .chain(nodes.in_order().map(|node| node.name))
        .map(AsPascalCase);
    for name in things {
        ml!(
            out,
            "
            #[derive(Clone, Copy)]
            #[repr(transparent)]
            pub struct {name}(Packed);

            impl Sealed for {name} {{}}

            impl From<{name}> for Packed {{
                #[inline]
                fn from(v: {name}) -> Self {{
                    v.0
                }}
            }}\n
        "
        );
    }

    for (category, ids) in [("stmt", &nodes.stmts), ("expr", &nodes.exprs)] {
        let name = AsPascalCase(category);
        let range_start = AsPascalCase(nodes.flat[ids[0].0].name);
        let range_end = AsPascalCase(nodes.flat[ids[ids.len() - 1].0].name);
        ml!(
            out,
            "
            /// SAFETY: `self` is a transparent wrapper over `Packed`.
            unsafe impl PackedAbi for {name} {{
                #[inline]
                fn check_kind(kind: NodeKind) -> bool {{
                    kind >= NodeKind::{range_start} && kind <= NodeKind::{range_end}
                }}
            }}

            impl TryFrom<Packed> for {name} {{
                type Error = ();

                #[inline]
                fn try_from(v: Packed) -> Result<Self, Self::Error> {{
                    if <Self as PackedAbi>::check_kind(v.kind()) {{
                        Ok(Self(v))
                    }} else {{
                        Err(())
                    }}
                }}
            }}
            "
        );
    }

    ml!(
        out,
        "
        impl TryFrom<Packed> for Root {{
            type Error = ();

            #[inline]
            fn try_from(v: Packed) -> Result<Self, Self::Error> {{
                if <Self as PackedAbi>::check_kind(v.kind()) {{
                    Ok(Self(v))
                }} else {{
                    Err(())
                }}
            }}
        }}
        "
    );

    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);
        ml!(
            out,
            "
            /// SAFETY: `self` is a transparent wrapper over `Packed`.
            unsafe impl PackedAbi for {name} {{
                #[inline]
                fn check_kind(kind: NodeKind) -> bool {{
                    kind == NodeKind::{name}
                }}
            }}\n
            "
        );
    }

    for node in nodes.stmts() {
        let name = AsPascalCase(node.name);
        ml!(
            out,
            "
            impl From<{name}> for Stmt {{
                #[inline]
                fn from(v: {name}) -> Self {{
                    Stmt(v.0)
                }}
            }}

            impl TryFrom<Stmt> for {name} {{
                type Error = NodeCastError;
                #[inline]
                fn try_from(v: Stmt) -> Result<Self, Self::Error> {{
                    if !matches!(v.0.kind(), NodeKind::{name}) {{
                        return Err(NodeCastError);
                    }}

                    Ok(Self(v.0))
                }}
            }}\n
            "
        );
    }

    for node in nodes.exprs() {
        let name = AsPascalCase(node.name);
        ml!(
            out,
            "
            impl From<{name}> for Expr {{
                #[inline]
                fn from(v: {name}) -> Self {{
                    Expr(v.0)
                }}
            }}

            impl TryFrom<Expr> for {name} {{
                type Error = NodeCastError;
                #[inline]
                fn try_from(v: Expr) -> Result<Self, Self::Error> {{
                    if !matches!(v.0.kind(), NodeKind::{name}) {{
                        return Err(NodeCastError);
                    }}

                    Ok(Self(v.0))
                }}
            }}\n
            "
        );
    }
}

fn emit_node_parts(nodes: &Nodes, out: &mut String) {
    // ln!(out, "pub mod parts {{ use super::Opt;");

    // for (category, members) in [("stmt", &nodes.stmts), ("expr", &nodes.exprs)] {
    //     let name = AsPascalCase(category);

    //     ml!(
    //         out,
    //         "
    //         pub enum {name}<'a> {{
    //             {variants}
    //         }}
    //         ",
    //         variants = members
    //             .iter()
    //             .map(|&id| &nodes.flat[id.0])
    //             .map(|node| {
    //                 let kind = AsPascalCase(node.name);
    //                 format!("{kind}(&'a super::{kind}),")
    //             })
    //             .join('\n')
    //     );
    // }

    // for node in nodes.in_order() {
    //     let name = AsPascalCase(node.name);
    //     ln!(out, "pub struct {name} {{");
    //     match &node.kind {
    //         NodeKind::Empty => {}
    //         NodeKind::FixedArity { fields, inline } => {
    //             for field in fields {
    //                 ln!(out, "    pub {}: {},", field.name, field.resolved_ty(true));
    //             }
    //             if let Some(inline) = inline {
    //                 ln!(
    //                     out,
    //                     "    pub {}: {},",
    //                     inline.name,
    //                     inline.resolved_ty(true)
    //                 );
    //             }
    //         }
    //         NodeKind::VariableArity { tail } => {
    //             ln!(out, "    pub {}: {},", tail.name, tail.resolved_ty(true));
    //         }
    //         NodeKind::MixedArity { fields, tail } => {
    //             for field in fields {
    //                 ln!(out, "    pub {}: {},", field.name, field.resolved_ty(true));
    //             }
    //             ln!(out, "    pub {}: {},", tail.name, tail.resolved_ty(true));
    //         }
    //         NodeKind::InlineValue { inline } => {
    //             ln!(
    //                 out,
    //                 "    pub {}: {},",
    //                 inline.name,
    //                 inline.resolved_ty(true)
    //             );
    //         }
    //     }
    //     ln!(out, "}}");
    // }
    // ln!(out, "}}\n");

    ln!(out, "pub mod spanned {{ use super::{{Spanned, Opt}};");
    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);
        let lifetime = if node.has_tail() { "<'a>" } else { "" };
        ln!(out, "pub struct {name}{lifetime} {{");
        match &node.kind {
            NodeKind::Empty => {}
            NodeKind::FixedArity { fields, inline } => {
                for field in fields {
                    ln!(
                        out,
                        "    pub {}: {},",
                        field.name,
                        field.resolved_ty_spanned(true)
                    );
                }
                if let Some(inline) = inline {
                    ln!(
                        out,
                        "    pub {}: {},",
                        inline.name,
                        inline.resolved_ty(true)
                    );
                }
            }
            NodeKind::VariableArity { tail } => {
                ln!(
                    out,
                    "    pub {}: &'a {},",
                    tail.name,
                    tail.resolved_ty_spanned(true),
                );
            }
            NodeKind::MixedArity { fields, tail } => {
                for field in fields {
                    ln!(
                        out,
                        "    pub {}: {},",
                        field.name,
                        field.resolved_ty_spanned(true),
                    );
                }
                ln!(
                    out,
                    "    pub {}: &'a {},",
                    tail.name,
                    tail.resolved_ty_spanned(true),
                );
            }
            NodeKind::InlineValue { inline } => {
                ln!(
                    out,
                    "    pub {}: {},",
                    inline.name,
                    inline.resolved_ty(true)
                );
            }
        }
        ln!(out, "}}");
    }
    ln!(out, "}}\n");
}

fn emit_pack_impls(nodes: &Nodes, out: &mut String) {
    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);
        let lifetime = if node.has_tail() { "<'a>" } else { "" };
        ml!(
            out,
            "
            impl{lifetime} Pack for spanned::{name}{lifetime} {{        
                type Node = {name};

                #[inline]
                fn pack(self, ast: &mut Ast) -> Self::Node {{
            "
        );
        match &node.kind {
            NodeKind::Empty => {
                ml!(
                    out,
                    "
                    let _ = (self, ast);
                    let node = Packed::kind_only(NodeKind::{name});
                    {name}(node)
                    "
                );
            }
            NodeKind::FixedArity { fields, inline } => {
                if let Some(inline) = inline {
                    ml!(
                        out,
                        "
                        let Self {{ {inline}, {fields} }} = self;
                        {fields_into};
                        let index = ast.append(&[{fields}]);
                        let {inline}: u24 = {inline}.into_u24();
                        let node = Packed::fixed_arity_inline(
                            NodeKind::{name},
                            {inline},
                            index
                        );
                        {name}(node)
                        ",
                        inline = inline.name,
                        fields = fields.iter().map(|f| f.name).join(','),
                        fields_into = fields
                            .iter()
                            .map(|Field { name, .. }| format!("let {name} = {name}.map(|v| v.0);"))
                            .join('\n'),
                    );
                } else {
                    ml!(
                        out,
                        "
                        let Self {{ {fields} }} = self;
                        {fields_into};
                        let index = ast.append(&[{fields}]);
                        let node = Packed::fixed_arity(NodeKind::{name}, index);
                        {name}(node)
                        ",
                        fields = fields.iter().map(|f| f.name).join(','),
                        fields_into = fields
                            .iter()
                            .map(|Field { name, .. }| format!("let {name} = {name}.map(|v| v.0);"))
                            .join('\n'),
                    );
                }
            }
            NodeKind::VariableArity { tail } => {
                ml!(
                    out,
                    "
                    let Self {{ {tail} }} = self;
                    let length = {tail}.len();
                    if length > u24::MAX.get() as usize {{
                        panic!(\"length is out of bounds for u24\");
                    }}
                    let length = u24::new(length as u32);
                    let {tail} = <_>::into_spanned_packed_slice({tail});
                    let index = ast.append({tail});
                    let node = Packed::variable_arity(
                        NodeKind::{name},
                        length,
                        index
                    );
                    {name}(node)
                    ",
                    tail = tail.name,
                );
            }
            NodeKind::MixedArity { fields, tail } => {
                ml!(
                    out,
                    "
                    let Self {{ {fields}, {tail} }} = self;
                    {fields_into};
                    let index = ast.append(&[{fields}]);

                    let length = {tail}.len();
                    if length > u24::MAX.get() as usize {{
                        panic!(\"length is out of bounds for u24\");
                    }}
                    let length = u24::new(length as u32);
                    let {tail} = <_>::into_spanned_packed_slice({tail});
                    let _ = ast.append({tail});
                    let node = Packed::mixed_arity(
                        NodeKind::{name},
                        length,
                        index
                    );
                    {name}(node)
                    ",
                    tail = tail.name,
                    fields = fields.iter().map(|f| f.name).join(','),
                    fields_into = fields
                        .iter()
                        .map(|Field { name, .. }| format!("let {name} = {name}.map(|v| v.0);"))
                        .join('\n'),
                );
            }
            NodeKind::InlineValue { inline } => {
                ml!(
                    out,
                    "
                        let _ = ast;
                        let Self {{ {inline} }} = self;
                        let {inline}: u56 = {inline}.into_u56();
                        let node = Packed::inline(NodeKind::{name}, {inline});
                        {name}(node)
                    ",
                    inline = inline.name,
                );
            }
        }
        ln!(out, "    }}");
        ln!(out, "}}\n");
    }
}

fn emit_unpack_impls(nodes: &Nodes, out: &mut String) {
    for (category, members) in [("stmt", &nodes.stmts), ("expr", &nodes.exprs)] {
        let name = AsPascalCase(category);
        ml!(
            out,
            "
            impl Unpack for {name} {{
                type Node<'a> = parts::{name}<'a>;

                #[inline]
                fn unpack<'a>(&'a self, ast: &'a Ast) -> Self::Node<'a> {{
                    match self.kind() {{
                        {kinds}
                    }}
                }}
            }}
            ",
            kinds = members.iter()
                .map(|&id| &nodes.flat[id.0])
                .map(|node| {
                    let kind = AsPascalCase(node.name);
                    format!("{name}Kind::{kind} => parts::{name}::{kind}(unsafe {{ PackedNode::from_packed(&self.0) }}),")
                })
                .join('\n')
        );
    }

    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);
        ml!(
            out,
            "
            impl Unpack for {name} {{        
                type Node<'a> = &'a parts::{name};
            
                #[inline]
                fn unpack<'a>(&'a self, ast: &'a Ast) -> Self::Node<'a> {{
            "
        );
        match &node.kind {
            NodeKind::Empty => {
                ml!(
                    out,
                    "
                    let _ = unsafe {{ self.0.as_kind_only() }};
                    let nodes: *const Packed = unsafe {{ ast.nodes.get_unchecked(0..0).as_ptr() }};
                    unsafe {{ core::mem::transmute(nodes) }}
                    ",
                );
            }
            NodeKind::FixedArity {
                fields,
                inline: Some(inline),
            } => {
                let num_fields = fields.len();
                ml!(
                    out,
                    "
                    const N: usize = {num_fields};
                    let repr = unsafe {{ self.0.as_fixed_arity_inline() }};
                    let index = repr.index as usize;
                    let node: *const Packed = unsafe {{ ast.nodes.get_unchecked(index..index+N).as_ptr() }};
                    {fields_check}
                    let {inline} = <{inline_ty}>::from_u24(repr.value);
                    Self::Node {{ {fields}, {inline} }}
                    ",
                    fields_check = fields
                        .iter()
                        .enumerate()
                        .map(|(i, Field { name, .. })| format!(
                            "let {name} = unsafe {{ <_>::from_packed(*raw.get_unchecked({i})) }};"
                        ))
                        .join('\n'),
                    fields = fields.iter().map(|f| f.name).join(','),
                    inline = inline.name,
                    inline_ty = inline.resolved_ty(false),
                );
            }
            NodeKind::FixedArity {
                fields,
                inline: None,
            } => {
                let num_fields = fields.len();
                ml!(
                    out,
                    "
                    const N: usize = {num_fields};
                    let repr = unsafe {{ self.0.as_fixed_arity() }};
                    let index = repr.index as usize;
                    let raw: &[Packed] = unsafe {{ ast.nodes.get_unchecked(index..index+N) }};
                    {fields_from}
                    Self::Node {{ {fields} }}
                    ",
                    fields_from = fields
                        .iter()
                        .enumerate()
                        .map(|(i, Field { name, .. })| format!(
                            "let {name} = unsafe {{ <_>::from_packed(*raw.get_unchecked({i})) }};"
                        ))
                        .join('\n'),
                    fields = fields.iter().map(|f| f.name).join(','),
                );
            }
            NodeKind::VariableArity { tail } => {
                ml!(
                    out,
                    "
                    let repr = unsafe {{ self.0.as_variable_arity() }};
                    let index = repr.index as usize;
                    let length = repr.length.get() as usize;
                    let raw: &[Packed] = unsafe {{ ast.nodes.get_unchecked(index..index+length) }};
                    let {tail}: &[_] = unsafe {{ <_>::from_packed_slice(raw) }};
                    Self::Node {{ {tail} }}
                    ",
                    tail = tail.name,
                );
            }
            NodeKind::MixedArity { fields, tail } => {
                let num_fields = fields.len();
                ml!(
                    out,
                    "
                    const N: usize = {num_fields};
                    let repr = unsafe {{ self.0.as_mixed_arity() }};
                    let index = repr.index as usize;
                    let tail_length = repr.tail_length.get() as usize;
                    let raw: &[Packed] = unsafe {{ ast.nodes.get_unchecked(index..index+N+tail_length) }};
                    {fields_from}
                    let {tail}: &[_] = unsafe {{ <_>::from_packed_slice(raw.get_unchecked(N..)) }};
                    Self::Node {{ {fields}, {tail} }}
                    ",
                    fields_from = fields
                        .iter()
                        .enumerate()
                        .map(|(i, Field { name, .. })| format!(
                            "let {name} = unsafe {{ <_>::from_packed(*raw.get_unchecked({i})) }};"
                        ))
                        .join('\n'),
                    fields = fields.iter().map(|f| f.name).join(','),
                    tail = tail.name,
                );
            }
            NodeKind::InlineValue { inline } => {
                ml!(
                    out,
                    "
                    let _ = ast;
                    let repr = unsafe {{ self.0.as_inline() }};
                    let {inline} = <{inline_ty}>::from_u56(repr.value);
                    Self::Node {{ {inline} }}
                    ",
                    inline = inline.name,
                    inline_ty = inline.resolved_ty(false),
                );
            }
        }
        ln!(out, "  }}");
        ln!(out, "}}\n");
    }
}

fn emit_field_getters(nodes: &Nodes, out: &mut String) {
    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);

        // no fields
        if matches!(node.kind, NodeKind::Empty) {
            continue;
        }

        ln!(out, "impl<'a> Node<'a, {name}> {{");
        match &node.kind {
            NodeKind::Empty => {
                // no fields
            }
            NodeKind::FixedArity { fields, inline } => {
                let repr = match inline {
                    Some(_) => "fixed_arity_inline",
                    None => "fixed_arity",
                };
                for (i, field) in fields.iter().enumerate() {
                    ml!(
                        out,
                        "
                        #[inline(always)]
                        pub fn {field}(&self) -> Node<'a, {ty}> {{
                            const OFFSET: usize = {i};
                            let repr = unsafe {{ self.node.0.as_{repr}() }};
                            let index = repr.index as usize;
                            let node = unsafe {{ self.ast.nodes.get_unchecked(index + OFFSET) }};
                            Node {{ ast: self.ast, node: unsafe {{ <{ty}>::from_packed(node) }} }}
                        }}\n
                        ",
                        field = field.name,
                        ty = field.resolved_ty(false),
                    );
                }

                if let Some(inline) = inline {
                    ml!(
                        out,
                        "
                        #[inline(always)]
                        pub fn {inline}(&self) -> ValueNode<'a, {ty}> {{
                            let repr = unsafe {{ self.node.0.as_fixed_arity_inline() }};
                            ValueNode {{ ast: self.ast, value: <{ty}>::from_u24(repr.value) }}
                        }}
                        ",
                        inline = inline.name,
                        ty = inline.resolved_ty(false),
                    );
                }
            }
            NodeKind::VariableArity { tail } => {
                let i = 0;
                ml!(
                    out,
                    "
                    #[inline]
                    pub fn {tail}(&self) -> NodeList<'a, {ty}> {{
                        const OFFSET: usize = {i};
                        let repr = unsafe {{ self.node.0.as_variable_arity() }};
                        let index = repr.index as usize;
                        let length = repr.length.get() as usize;
                        let nodes = unsafe {{ self.ast.nodes.get_unchecked(index + OFFSET..index + OFFSET + length) }};
                        NodeList {{ ast: self.ast, nodes: unsafe {{ {ty}::from_packed_slice(nodes) }} }}
                    }}\n
                    ",
                    tail = tail.name,
                    ty = AsPascalCase(tail.ty),
                );
            }
            NodeKind::MixedArity { fields, tail } => {
                for (i, field) in fields.iter().enumerate() {
                    ml!(
                        out,
                        "
                        #[inline(always)]
                        pub fn {field}(&self) -> Node<'a, {ty}> {{
                            const OFFSET: usize = {i};
                            let repr = unsafe {{ self.node.0.as_mixed_arity() }};
                            let index = repr.index as usize;
                            let node = unsafe {{ self.ast.nodes.get_unchecked(index + OFFSET) }};
                            Node {{ ast: self.ast, node: unsafe {{ <{ty}>::from_packed(node) }} }}
                        }}\n
                        ",
                        field = field.name,
                        ty = field.resolved_ty(false),
                    );
                }

                let i = fields.len();

                ml!(
                    out,
                    "
                    #[inline]
                    pub fn {tail}(&self) -> NodeList<'a, {ty}> {{
                        const OFFSET: usize = {i};
                        let repr = unsafe {{ self.node.0.as_mixed_arity() }};
                        let index = repr.index as usize;
                        let length = repr.tail_length.get() as usize;
                        let nodes = unsafe {{ self.ast.nodes.get_unchecked(index + OFFSET..index + OFFSET + length) }};
                        NodeList {{ ast: self.ast, nodes: unsafe {{ {ty}::from_packed_slice(nodes) }} }}
                    }}\n
                    ",
                    tail = tail.name,
                    ty = AsPascalCase(tail.ty),
                );
            }
            NodeKind::InlineValue { inline } => {
                ml!(
                    out,
                    "
                    #[inline(always)]
                    pub fn {inline}(&self) -> ValueNode<'a, {ty}> {{
                        let repr = unsafe {{ self.node.0.as_inline() }};
                        ValueNode {{ ast: self.ast, value: <{ty}>::from_u56(repr.value) }}
                    }}
                    ",
                    inline = inline.name,
                    ty = inline.resolved_ty(false),
                );
            }
        }
        ln!(out, "}}\n");
    }
}

fn emit_span_getters(nodes: &Nodes, out: &mut String) {
    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);

        // these have no internal spans, so early-out
        if matches!(node.kind, NodeKind::Empty | NodeKind::InlineValue { .. }) {
            continue;
        }

        // span getters are not perf sensitive, so we keep bounds checks

        ln!(out, "impl<'a> Node<'a, {name}> {{");
        match &node.kind {
            NodeKind::Empty => {
                // no internal spans
            }
            NodeKind::FixedArity { fields, inline } => {
                let repr = match inline {
                    Some(_) => "fixed_arity_inline",
                    None => "fixed_arity",
                };
                for (i, field) in fields.iter().enumerate() {
                    ml!(
                        out,
                        "
                        #[inline]
                        pub fn {field}_span(&self) -> Span {{
                            let repr = unsafe {{ self.0.as_{repr}() }};
                            let index = repr.index as usize;
                            self.ast.spans[index + {i}]
                        }}\n
                        ",
                        field = field.name.strip_suffix('_').unwrap_or(field.name),
                    );
                }

                let _ = inline; // no spans for inline fields
            }
            NodeKind::VariableArity { tail } => {
                ml!(
                    out,
                    "
                    #[inline]
                    pub fn {tail}_spans(&self) -> &'a [Span] {{
                        let repr = unsafe {{ self.0.as_variable_arity() }};
                        let index = repr.index as usize;
                        let length = repr.length.get() as usize;
                        &self.ast.spans[index..index+length]
                    }}\n
                    ",
                    tail = tail.name.strip_suffix('_').unwrap_or(tail.name),
                );
            }
            NodeKind::MixedArity { fields, tail } => {
                for (i, field) in fields.iter().enumerate() {
                    ml!(
                        out,
                        "
                        #[inline]
                        pub fn {field}_span(&self) -> Span {{
                            let repr = unsafe {{ self.0.as_mixed_arity() }};
                            let index = repr.index as usize;
                            self.ast.spans[index + {i}]
                        }}\n
                        ",
                        field = field.name.strip_suffix('_').unwrap_or(field.name),
                    );
                }

                let tail_start = fields.len();

                ml!(
                    out,
                    "
                    #[inline]
                    pub fn {tail}_spans(&self) -> &'a [Span] {{
                        let repr = unsafe {{ self.0.as_mixed_arity() }};
                        let index = {tail_start} + repr.index as usize;
                        let tail_length = repr.tail_length.get() as usize;
                        &self.ast.spans[index..index + tail_length]
                    }}\n
                    ",
                    tail = tail.name.strip_suffix('_').unwrap_or(tail.name),
                );
            }
            NodeKind::InlineValue { inline } => {
                let _ = inline; // no spans for inline fields
            }
        }
        ln!(out, "}}\n");
    }
}

/*

pub trait Visitor {
    type Output = ();

    fn visit_stmt(&mut self, stmt: Stmt) -> Output {
        match stmt.kind() {
            StmtKind::
        }
    }
}

*/

// fn emit_visitor(nodes: &Nodes, out: &mut String) {
//     for (category, members) in [("stmt", &nodes.stmts), ("expr", &nodes.exprs)] {
//         let category_pascal = AsPascalCase(category);
//         ml!(
//             out,
//             "
//             pub fn walk_{category}<V: ?Sized + Visitor>(
//                 visitor: &mut V,
//                 ast: &Ast,
//                 node: {category_pascal},
//             ) -> Result<(), V::Error> {{
//                 match node.kind() {{
//                     {kinds}
//                 }}
//             }}\n
//             ",
//             kinds = members
//                 .iter()
//                 .map(|&v| &nodes.flat[v.0])
//                 .map(|node| {
//                     let name = node.name;
//                     let name_pascal = AsPascalCase(name);
//                     indoc::formatdoc!(
//                         "
//                         {category_pascal}Kind::{name_pascal} => {{
//                             let node = unsafe {{ <_>::from_packed(node.0) }};
//                             visitor.visit_{name}(
//                                 ast,
//                                 node,
//                                 unsafe {{ node.unpack(ast) }},
//                             )
//                         }}
//                         "
//                     )
//                 })
//                 .join('\n'),
//         );
//     }

//     ln!(out, "pub trait Visitor {{");
//     ln!(out, "type Error;");
//     ln!(out);

//     fn visit_field(field: &Field<'_>) -> String {
//         let unpack = if !["expr", "stmt"].contains(&field.ty) {
//             format!("unsafe {{ node.unpack(ast) }}")
//         } else {
//             String::new()
//         };

//         if field.opt {
//             format!(
//                 "if let Some(node) = parts.{name}.into() {{ self.visit_{ty}(ast, node, {unpack})?; }}",
//                 ty = field.ty,
//                 name = field.name,
//             )
//         } else if field.tail {
//             format!(
//                 "for node in parts.{name} {{ self.visit_{ty}(ast, *node, {unpack})?; }}",
//                 ty = field.ty,
//                 name = field.name,
//             )
//         } else {
//             format!(
//                 "let node = parts.{name}; self.visit_{ty}(ast, node, {unpack})?;",
//                 ty = field.ty,
//                 name = field.name,
//             )
//         }
//     }

//     ml!(
//         out,
//         "
//         fn visit_root(&mut self, ast: &Ast, node: Root) -> std::result::Result<(), Self::Error> {{
//             let parts = unsafe {{ node.unpack(ast) }};
//             {body}

//             Ok(())
//         }}\n
//         ",
//         body = visit_field(nodes.root().tail().unwrap())
//     );

//     for (category, members) in [("stmt", &nodes.stmts), ("expr", &nodes.exprs)] {
//         let category_pascal = AsPascalCase(category);
//         ml!(
//             out,
//             "
//             fn visit_{category}(&mut self, ast: &Ast, node: {category_pascal}) -> std::result::Result<(), Self::Error> {{
//                 walk_{category}(self, ast, node)
//             }}\n
//             "
//         );

//         for node in members.iter().map(|&id| &nodes.flat[id.0]) {
//             let name = node.name;
//             let name_pascal = AsPascalCase(name);
//             let lifetime = if node.has_tail() { "<'_>" } else { "" };
//             let parts = match &node.kind {
//                 NodeKind::Empty => String::new(),
//                 NodeKind::FixedArity { fields, inline: _ } => fields
//                     .iter()
//                     .filter(|field| !nodes.category(field.ty).is_data())
//                     .map(visit_field)
//                     .join('\n')
//                     .to_string(),
//                 NodeKind::VariableArity { tail } => {
//                     if !nodes.category(tail.ty).is_data() {
//                         visit_field(tail)
//                     } else {
//                         String::new()
//                     }
//                 }
//                 NodeKind::MixedArity { fields, tail } => {
//                     let fields = fields
//                         .iter()
//                         .filter(|field| !nodes.category(field.ty).is_data())
//                         .map(visit_field)
//                         .join('\n');
//                     let tail = if !nodes.category(tail.ty).is_data() {
//                         visit_field(tail)
//                     } else {
//                         String::new()
//                     };
//                     format!("{fields}\n{tail}")
//                 }
//                 NodeKind::InlineValue { inline: _ } => String::new(),
//             };

//             ml!(
//                 out,
//                 "
//                 #[allow(unused_variables)]
//                 fn visit_{name}(
//                     &mut self,
//                     ast: &Ast,
//                     node: {name_pascal},
//                     parts: parts::{name_pascal}{lifetime},
//                 ) -> std::result::Result<(), Self::Error> {{
//                     {parts}

//                     Ok(())
//                 }}\n
//                 "
//             );
//         }
//     }

//     ln!(out, "}}\n");
// }

fn emit_debug_impls(nodes: &Nodes, out: &mut String) {
    for (category, members) in [("stmt", &nodes.stmts), ("expr", &nodes.exprs)] {
        let category_pascal = AsPascalCase(category);
        let kinds = members
            .iter()
            .map(|&id| &nodes.flat[id.0])
            .map(|Node { name, .. }| {
                let name_pascal = AsPascalCase(name);
                format!(
                    "{category_pascal}Kind::{name_pascal}(node) => std::fmt::Debug::fmt(&node, f),"
                )
            })
            .join('\n');
        ml!(
            out,
            "
            impl<'a> std::fmt::Debug for Node<'a, {category_pascal}> {{
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
                    match self.kind() {{
                        {kinds}
                    }}
                }}
            }}\n
            "
        );
    }

    for node in nodes.in_order() {
        let name = node.name;
        let name_pascal = AsPascalCase(name);
        let parts = match &node.kind {
            NodeKind::Empty => format!("write!(f, \"{name_pascal}\")?;"),
            NodeKind::FixedArity { fields, inline } => {
                format!(
                    "
                    f.debug_tuple(\"{name_pascal}\")
                        {inline}
                        {fields}
                        .finish()?;
                    ",
                    fields = fields
                        .iter()
                        .map(|Field { name, opt, .. }| {
                            if *opt {
                                format!(".field(&self.{name}().as_option())")
                            } else {
                                format!(".field(&self.{name}())")
                            }
                        })
                        .join('\n'),
                    inline = inline
                        .as_ref()
                        .map(|Inline { name, .. }| format!(".field(&self.{name}())"))
                        .unwrap_or_default()
                )
            }
            NodeKind::VariableArity {
                tail: Field {
                    name: tail_name, ..
                },
            } => {
                format!(
                    "
                    f.debug_tuple(\"{name_pascal}\")
                        {tail}
                        .finish()?;
                    ",
                    tail = format!(".field(&DebugIter(self.{tail_name}().iter()))"),
                )
            }
            NodeKind::MixedArity {
                fields,
                tail: Field {
                    name: tail_name, ..
                },
            } => {
                format!(
                    "
                    f.debug_tuple(\"{name_pascal}\")
                        {fields}
                        {tail}
                        .finish()?;
                    ",
                    fields = fields
                        .iter()
                        .map(|Field { name, opt, .. }| {
                            if *opt {
                                format!(".field(&self.{name}().as_option())")
                            } else {
                                format!(".field(&self.{name}())")
                            }
                        })
                        .join('\n'),
                    tail = format!(".field(&DebugIter(self.{tail_name}().iter()))"),
                )
            }
            NodeKind::InlineValue {
                inline: Inline {
                    name: inline_name, ..
                },
            } => {
                format!(
                    "
                    f.debug_tuple(\"{name_pascal}\")
                        {inline}
                        .finish()?;
                    ",
                    inline = format!(".field(&self.{inline_name}())")
                )
            }
        };
        ml!(
            out,
            "
            impl<'a> std::fmt::Debug for Node<'a, {name_pascal}> {{
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
                    {parts}

                    Ok(())
                }}
            }}\n
            "
        );
    }
}
