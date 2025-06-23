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
        std::fs::write(outfile, out).expect("failed to write file");
    }
}

#[derive(Debug)]
struct Node<'a> {
    name: &'a str,
    category: Category,
    kind: NodeKind<'a>,
    tag_n: u8,
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
}

#[derive(Debug)]
struct Inline<'a> {
    name: &'a str,
    ty: &'a str,
    bits: Bits,
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

            let is_tail = ty.starts_with('[');
            if is_tail {
                let ty = ty.strip_prefix('[').unwrap();
                let ty = ty.trim();
                let ty = match ty.strip_suffix(']') {
                    Some(v) => v,
                    None => panic!("invalid field ty, missing closing ]:\n{node_desc}"),
                };
                let ty = ty.trim();
                tail = Some(Field { name, ty });
            } else {
                fields.push(Field { name, ty });
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
    emit_packing_fns(&nodes, &mut out);
    emit_unpacked_structs(&nodes, &mut out);

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
        use super::{{Ast, AstBuilder, IdentId, StrId}};
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
    for category in [&nodes.stmts, &nodes.exprs, &nodes.data] {
        for node in category.iter().copied() {
            let node = &nodes.flat[node.0];
            let name = AsPascalCase(node.name);
            let n = node.tag_n;
            ln!(out, "    {name} = {n},");
        }
    }
    ln!(out, "    None = 255,");
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
            impl PackedAbi for {name} {{}}

        "
        );
    }
}

fn emit_packing_fns(nodes: &Nodes, out: &mut String) {}
fn emit_unpacked_structs(nodes: &Nodes, out: &mut String) {}
