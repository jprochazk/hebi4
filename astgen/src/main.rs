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

fn main() {
    let mut nodes = parse_root(include_str!("./nodes.ast"));

    assign_tag_numbers(&mut nodes);
    println!("{nodes:#?}");
    emit_nodes(nodes);
}

use std::{collections::BTreeMap, str::FromStr};

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
    Fixed {
        fields: Vec<Field<'a>>,
    },
    FixedInline {
        fields: Vec<Field<'a>>,
        inline: Inline<'a>, // only u24
    },
    Variable {
        tail: Field<'a>,
    },
    Mixed {
        fields: Vec<Field<'a>>,
        tail: Field<'a>,
    },
    Inline {
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
        ([], None, Some(tail)) => NodeKind::Variable { tail },
        ([], Some(inline), None) => NodeKind::Inline { inline },
        ([_, ..], None, None) => NodeKind::Fixed { fields },
        ([_, ..], Some(inline), None) => {
            assert!(
                matches!(inline.bits, Bits::U24),
                "invalid node {name}, fixed-arity nodes may use at most 24 bits of inline storage:\n{node_desc}"
            );
            NodeKind::FixedInline { fields, inline }
        }
        ([_, ..], None, Some(tail)) => NodeKind::Mixed { fields, tail },
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
fn emit_nodes(nodes: Nodes<'_>) -> String {
    let mut out = String::new();

    emit_kinds(&nodes, &mut out);

    out
}

fn emit_kinds(nodes: &Nodes, out: &mut String) {}
