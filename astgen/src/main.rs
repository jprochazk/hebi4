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
        run(format!("rustfmt {}", outfile.display()));
    }
}

fn run(cmd: impl AsRef<str>) {
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
        panic!("command exited with non-zero exit code");
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
    Stmt,
    Expr,
    Data, // only used to store data for other nodes, not as a node itself
}

#[derive(Debug)]
struct Field<'a> {
    name: &'a str,
    ty: &'a str,
    opt: bool,
    tail: bool,
}

struct WrapOpt<T>(T);

impl<T: std::fmt::Display> std::fmt::Display for WrapOpt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Opt<{}>", self.0)
    }
}

struct WrapTail<T>(T);

impl<T: std::fmt::Display> std::fmt::Display for WrapTail<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&'a [{}]", self.0)
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

impl<'a> Field<'a> {
    #[inline]
    fn resolved_ty<'d>(&'d self) -> impl std::fmt::Display + 'd {
        if self.opt && self.tail {
            panic!("a type cannot be optional and tail at the same time");
        }

        let ty = MaybePascalCase(self.ty, !is_lowercase_ty(self.ty));
        if self.opt {
            Either::A(WrapOpt(ty))
        } else if self.tail {
            Either::B(Either::A(WrapTail(ty)))
        } else {
            Either::B(Either::B(ty))
        }
    }
}

#[derive(Debug)]
struct Inline<'a> {
    name: &'a str,
    ty: &'a str,
    bits: Bits,
}

impl Inline<'_> {
    #[inline]
    fn resolved_ty<'d>(&'d self) -> impl std::fmt::Display + 'd {
        MaybePascalCase(self.ty, !is_lowercase_ty(self.ty))
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

#[derive(Debug, Default)]
struct Nodes<'a> {
    by_name: BTreeMap<&'a str, NodeId>,
    stmts: Vec<NodeId>,
    exprs: Vec<NodeId>,
    data: Vec<NodeId>,
    flat: Vec<Node<'a>>,
}

impl<'a> Nodes<'a> {
    fn add(&mut self, name: &'a str, desc: Node<'a>) {
        let node_id = NodeId(self.flat.len());

        match desc.category {
            Category::Stmt => self.stmts.push(node_id),
            Category::Expr => self.exprs.push(node_id),
            Category::Data => self.data.push(node_id),
        }
        if self.by_name.insert(name, node_id).is_some() {
            panic!("duplicate node: {name}");
        }
        self.flat.push(desc);
    }

    fn in_order(&self) -> impl Iterator<Item = &Node<'_>> + '_ {
        self.stmts
            .iter()
            .copied()
            .chain(self.exprs.iter().copied())
            .chain(self.data.iter().copied())
            .map(|node| &self.flat[node.0])
    }
}

fn parse_root<'a>(s: &'a str) -> Nodes<'a> {
    let mut nodes = Nodes::default();

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

macro_rules! ln {
    ($f:ident, $($tt:tt)*) => (writeln!($f, $($tt)*).unwrap());
    ($f:ident) => (writeln!($f).unwrap());
}

macro_rules! w {
    ($f:ident, $($tt:tt)*) => (write!($f, $($tt)*).unwrap());
    ($f:ident) => (write!($f).unwrap());
}

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
    emit_unpack_impls(&nodes, &mut out);

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
        use super::{{Ast, AstBuilder, IdentId, StrId, FloatId, AssignOp, InfixOp, PrefixOp}};
        "
    );
}

fn emit_nodekind_enum(nodes: &Nodes, out: &mut String) {
    ln!(
        out,
        "#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]"
    );
    ln!(out, "#[repr(u8)]");
    ln!(out, "pub enum NodeKind {{");
    ln!(out, "    Root = 1,");
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
    ln!(out, "        ]");
    ln!(out, "    }}");
    ln!(out, "}}\n");
}

fn emit_category_kind_enums(nodes: &Nodes, out: &mut String) {
    // note: intentionally leaving out `data`
    for (category_name, node_list) in [("stmt", &nodes.stmts), ("expr", &nodes.exprs)] {
        let category_name = AsPascalCase(category_name);
        ln!(
            out,
            "#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]"
        );
        ln!(out, "#[repr(u8)]");
        ln!(out, "pub enum {category_name}Kind {{");
        for node in node_list.iter().copied() {
            let node = &nodes.flat[node.0];
            let name = AsPascalCase(node.name);
            ln!(out, "    {name} = NodeKind::{name} as u8,");
        }
        ln!(out, "}}\n");
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
            /// SAFETY: `self` is a transparent wrapper over `Packed`.
            unsafe impl PackedAbi for {name} {{}}

        "
        );
    }
}

fn emit_node_parts(nodes: &Nodes, out: &mut String) {
    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);
        let lifetime = if node.has_tail() { "<'a>" } else { "" };
        ln!(out, "pub struct {name}Parts{lifetime} {{");
        match &node.kind {
            NodeKind::Empty => {}
            NodeKind::FixedArity { fields, inline } => {
                for field in fields {
                    ln!(out, "    pub {}: {},", field.name, field.resolved_ty());
                }
                if let Some(inline) = inline {
                    ln!(out, "    pub {}: {},", inline.name, inline.resolved_ty());
                }
            }
            NodeKind::VariableArity { tail } => {
                ln!(out, "    pub {}: {},", tail.name, tail.resolved_ty());
            }
            NodeKind::MixedArity { fields, tail } => {
                for field in fields {
                    ln!(out, "    pub {}: {},", field.name, field.resolved_ty());
                }
                ln!(out, "    pub {}: {},", tail.name, tail.resolved_ty());
            }
            NodeKind::InlineValue { inline } => {
                ln!(out, "    pub {}: {},", inline.name, inline.resolved_ty());
            }
        }
        ln!(out, "}}\n");
    }
}

fn emit_pack_impls(nodes: &Nodes, out: &mut String) {
    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);
        let lifetime = if node.has_tail() { "<'a>" } else { "" };
        ml!(
            out,
            "
            impl Pack for {name} {{        
                type Parts<'a> = {name}Parts{lifetime};

                fn pack<'a>(parts: Self::Parts<'a>, ast: &mut AstBuilder) -> Self {{
            "
        );
        match &node.kind {
            NodeKind::Empty => {
                ml!(
                    out,
                    "
                    let _ = (parts, ast);
                    Self::from_packed(Packed::kind_only(NodeKind::{name}))
                    "
                );
            }
            NodeKind::FixedArity { fields, inline } => {
                if let Some(inline) = inline {
                    ml!(
                        out,
                        "
                        let {name}Parts {{ {inline}, {fields} }} = parts;
                        {fields_into};
                        let index = ast.append(&[{fields}]);
                        let {inline}: u24 = {inline}.into_u24();
                        Self::from_packed(
                            Packed::fixed_arity_inline(NodeKind::{name}, {inline}, index)
                        )
                        ",
                        inline = inline.name,
                        fields = fields.iter().map(|f| f.name).join(','),
                        fields_into = fields
                            .iter()
                            .map(|Field { name, .. }| format!(
                                "let {name} = <_>::into_packed({name});"
                            ))
                            .join('\n'),
                    );
                } else {
                    ml!(
                        out,
                        "
                        let {name}Parts {{ {fields} }} = parts;
                        {fields_into};
                        let index = ast.append(&[{fields}]);
                        Self::from_packed(
                            Packed::fixed_arity(NodeKind::{name}, index)
                        )
                        ",
                        fields = fields.iter().map(|f| f.name).join(','),
                        fields_into = fields
                            .iter()
                            .map(|Field { name, .. }| format!(
                                "let {name} = <_>::into_packed({name});"
                            ))
                            .join('\n'),
                    );
                }
            }
            NodeKind::VariableArity { tail } => {
                ml!(
                    out,
                    "
                    let {name}Parts {{ {tail} }} = parts;
                    let length = {tail}.len();
                    if length > u24::MAX.get() as usize {{
                        panic!(\"length is out of bounds for u24\");
                    }}
                    let length = u24::new(length as u32);
                    let {tail} = <_>::into_packed_slice({tail});
                    let index = ast.append({tail});
                    Self::from_packed(
                        Packed::variable_arity(NodeKind::{name}, length, index)
                    )
                    ",
                    tail = tail.name,
                );
            }
            NodeKind::MixedArity { fields, tail } => {
                ml!(
                    out,
                    "
                    let {name}Parts {{ {fields}, {tail} }} = parts;
                    {fields_into};
                    let index = ast.append(&[{fields}]);

                    let length = {tail}.len();
                    if length > u24::MAX.get() as usize {{
                        panic!(\"length is out of bounds for u24\");
                    }}
                    let length = u24::new(length as u32);
                    let {tail} = <_>::into_packed_slice({tail});
                    let _ = ast.append({tail});

                    Self::from_packed(
                        Packed::mixed_arity(NodeKind::{name}, length, index)
                    )
                    ",
                    tail = tail.name,
                    fields = fields.iter().map(|f| f.name).join(','),
                    fields_into = fields
                        .iter()
                        .map(|Field { name, .. }| format!("let {name} = <_>::into_packed({name});"))
                        .join('\n'),
                );
            }
            NodeKind::InlineValue { inline } => {
                ml!(
                    out,
                    "
                        let _ = ast;
                        let {name}Parts {{ {inline} }} = parts;
                        let {inline}: u56 = {inline}.into_u56();
                        Self::from_packed(
                            Packed::inline(NodeKind::{name}, {inline})
                        )
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
    /*
    impl Unpack for {name} {{
        unsafe fn unpack<'a>(self, ast: &'a Ast) -> Self::Parts<'a> {{
            {name}Parts {{
                $($fields,*)
                $($tail,)?
                $($inline,)?
            }}
        }}
    }}
    */
    for node in nodes.in_order() {
        let name = AsPascalCase(node.name);
        ml!(
            out,
            "
            impl Unpack for {name} {{        
                unsafe fn unpack<'a>(self, ast: &'a Ast) -> Self::Parts<'a> {{
            "
        );
        match &node.kind {
            NodeKind::Empty => {
                ml!(
                    out,
                    "
                    let _ = unsafe {{ self.0.as_kind_only() }};
                    let _ = ast;
                    Self::Parts {{}}
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
                    let raw: &[Packed] = unsafe {{ ast.nodes.get_unchecked(index..index+N) }};
                    {fields_from}
                    let {inline} = <{inline_ty}>::from_u24(repr.value);
                    Self::Parts {{ {fields}, {inline} }}
                    ",
                    fields_from = fields
                        .iter()
                        .enumerate()
                        .map(|(i, Field { name, .. })| format!(
                            "let {name} = <_>::from_packed(unsafe {{ *raw.get_unchecked({i}) }});"
                        ))
                        .join('\n'),
                    fields = fields.iter().map(|f| f.name).join(','),
                    inline = inline.name,
                    inline_ty = inline.resolved_ty(),
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
                    Self::Parts {{ {fields} }}
                    ",
                    fields_from = fields
                        .iter()
                        .enumerate()
                        .map(|(i, Field { name, .. })| format!(
                            "let {name} = <_>::from_packed(unsafe {{ *raw.get_unchecked({i}) }});"
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
                    let {tail}: &[_] = <_>::from_packed_slice(raw);
                    Self::Parts {{ {tail} }}
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
                    let {tail}: &[_] = <_>::from_packed_slice(unsafe {{ raw.get_unchecked(N..) }});
                    Self::Parts {{ {fields}, {tail} }}
                    ",
                    fields_from = fields
                        .iter()
                        .enumerate()
                        .map(|(i, Field { name, .. })| format!(
                            "let {name} = <_>::from_packed(unsafe {{ *raw.get_unchecked({i}) }});"
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
                    Self::Parts {{ {inline} }}
                    ",
                    inline = inline.name,
                    inline_ty = inline.resolved_ty(),
                );
            }
        }
        ln!(out, "  }}");
        ln!(out, "}}\n");
    }
}

/*
TODO: spans


impl Var {
    pub fn name_span(&self, ast: &Ast) -> Span {
        ast.spans[self.index + FIELD_NUM]
    }
}
*/
