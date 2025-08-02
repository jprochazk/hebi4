use std::{collections::HashMap, path::Path};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).take(2).collect();
    let [infile, outfile] = &args[..] else {
        panic!("invalid args: {args:?}\nexpected: infile outfile");
    };
    let infile = Path::new(&infile);
    let infile = std::fs::read_to_string(infile).expect("infile should exist");
    let infile = strip_single_line_comments(&infile);

    let instructions = parse(&infile);

    let out = emit(instructions);

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

#[derive(Debug, Default)]
struct Instructions<'a> {
    /// Used to panic if an opcode with the same name already exists
    by_name: HashMap<&'a str, usize>,
    /// Used to panic if an opcode with the same opcode value already
    /// exists. Only set after `auto` opcode is resolved.
    by_opcode: HashMap<u8, usize>,
    /// List of opcodes in the order they appear in input file.
    flat: Vec<Instruction<'a>>,
}

impl<'a> Instructions<'a> {
    fn add(&mut self, instruction: Instruction<'a>) {
        if self.by_name.contains_key(&instruction.name) {
            panic!("duplicate instruction name {:?}", instruction.name);
        }

        if let Some(existing) = self.by_opcode.get(&instruction.opcode) {
            let existing = &self.flat[*existing];
            panic!(
                "duplicate opcode {:?} used in {} and {}",
                instruction.opcode, instruction.name, existing.name,
            );
        }

        let i = self.flat.len();
        self.by_name.insert(instruction.name, i);
        self.by_opcode.insert(instruction.opcode, i);
        self.flat.push(instruction);
    }
}

#[derive(Debug, Clone)]
struct Instruction<'a> {
    /// Short name of the opcode.
    name: &'a str,

    /// Block comment placed before the opcode
    docs: String,

    /// Opcode value
    ///
    /// Can either be specified explicitly in hexadecimal (`0xAF`),
    /// or as `auto` in which case it is incremented by 1 from
    /// opcode placed before it.
    opcode: u8,

    operands: Operands<'a>,
}

impl Instruction<'_> {
    fn type_name(&self) -> impl std::fmt::Display + '_ {
        struct VariantName<'a>(&'a str);
        impl std::fmt::Display for VariantName<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use std::fmt::Write as _;
                let first_char = self.0.chars().nth(0).unwrap().to_ascii_uppercase();
                let first_char_byte_len = first_char.len_utf8();
                f.write_char(first_char)?;
                f.write_str(&self.0[first_char_byte_len..])
            }
        }

        VariantName(self.name)
    }
}

// An operand may use at most 24 bits, here are different valid
// configurations of operand sets:
#[derive(Debug, Clone, Copy)]
enum Operands<'a> {
    None,
    A8 {
        a: Operand<'a>,
    },
    A24 {
        a: Operand<'a>,
    },
    A8B8 {
        a: Operand<'a>,
        b: Operand<'a>,
    },
    A8B16 {
        a: Operand<'a>,
        b: Operand<'a>,
    },
    A8B8C8 {
        a: Operand<'a>,
        b: Operand<'a>,
        c: Operand<'a>,
    },
}

impl Operands<'_> {
    fn fields(&self, r#pub: bool) -> String {
        let vis = if r#pub { "pub " } else { "" };
        match self {
            Operands::None => format!("_a: u8, _b: u8, _c: u8"),
            Operands::A8 { a } => format!("{vis}{}: {}, _b: u8, _c: u8", a.name, a.ty),
            Operands::A24 { a } => format!("{vis}{}: {}", a.name, a.ty),
            Operands::A8B8 { a, b } => {
                format!(
                    "{vis}{}: {}, {vis}{}: {}, _c: u8",
                    a.name, a.ty, b.name, b.ty
                )
            }
            Operands::A8B16 { a, b } => {
                format!("{vis}{}: {}, {vis}{}: {}", a.name, a.ty, b.name, b.ty)
            }
            Operands::A8B8C8 { a, b, c } => format!(
                "{vis}{}: {}, {vis}{}: {}, {vis}{}: {}",
                a.name, a.ty, b.name, b.ty, c.name, c.ty
            ),
        }
    }

    fn count(&self) -> u8 {
        match self {
            Operands::None => 0,
            Operands::A8 { a: _ } => 1,
            Operands::A24 { a: _ } => 1,
            Operands::A8B8 { a: _, b: _ } => 2,
            Operands::A8B16 { a: _, b: _ } => 2,
            Operands::A8B8C8 { a: _, b: _, c: _ } => 3,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Operand<'a> {
    /// Operand name
    name: &'a str,

    /// Operand type
    ty: OperandType,
}

#[derive(Debug, Clone, Copy)]
enum OperandType {
    /// 8-bit register
    Reg,

    /// 16-bit literal ID
    Lit,

    /// 8-bit literal ID
    Lit8,

    /// 8-bit value
    Imm8,

    /// 16-bit value
    Imm16,

    /// 24-bit value
    Imm24,
}

impl OperandType {
    fn bits(&self) -> u8 {
        match self {
            OperandType::Reg => 8,
            OperandType::Lit => 16,
            OperandType::Lit8 => 8,
            OperandType::Imm8 => 8,
            OperandType::Imm16 => 16,
            OperandType::Imm24 => 24,
        }
    }

    fn str(&self) -> &'static str {
        match self {
            OperandType::Reg => "Reg",
            OperandType::Lit => "Lit",
            OperandType::Lit8 => "Lit8",
            OperandType::Imm8 => "Imm8",
            OperandType::Imm16 => "Imm16",
            OperandType::Imm24 => "Imm24",
        }
    }
}

impl std::fmt::Display for OperandType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.str())
    }
}

fn strip_single_line_comments(s: &str) -> String {
    s.lines()
        .map(|line| line.trim())
        .filter(|line| !line.starts_with("#") && !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

// parse opcode descriptions
fn parse(s: &str) -> Instructions {
    let mut instructions = Instructions::default();

    let mut previous_opcode = 0;
    for description in s.split(';') {
        let mut description = description.trim();
        if description.is_empty() {
            continue;
        }

        // 1. find `/*` and `*/`, convert contents into docs

        let mut docs = String::new();

        if let Some(docs_start) = description.find("/*")
            && let Some(docs_end) = description[docs_start + 2..].find("*/")
        {
            // map index into original string
            let docs_end = docs_start + 2 + docs_end;
            let docs_content = &description[docs_start + 2..docs_end];

            // doc comment is possibly multi-line
            for line in docs_content.lines() {
                let line = line.trim().strip_prefix("*").unwrap_or(line).trim();
                if !line.is_empty() {
                    docs.push_str(line);
                }
                docs.push('\n');
            }

            description = &description[docs_end + 2..];
        }

        // 2. remainder is an instruction

        let instruction = parse_instruction(description, previous_opcode, docs);
        previous_opcode = instruction.opcode;
        instructions.add(instruction);
    }

    instructions
}

// <name> <...operands> = <opcode>
//
// where <opcode> is either:
// - hexcode (`0xFF`)
// - `auto`, meaning "one plus the previous opcode"
fn parse_instruction(s: &str, previous_opcode: u8, docs: String) -> Instruction {
    let mut parts = s.split_ascii_whitespace();

    let Ok(name) = parts
        .next()
        .ok_or_else(|| panic!("missing instruction name:\n{s}"));

    let mut operands = Vec::new();
    while let Some(next) = parts.next()
        && next != "="
    {
        let Ok((name, ty)) = next
            .split_once(':')
            .ok_or_else(|| panic!("invalid operand {next:?} in instruction:\n{s}"));

        let ty = match ty {
            "reg" => OperandType::Reg,
            "lit" => OperandType::Lit,
            "lit8" => OperandType::Lit8,
            "imm8" => OperandType::Imm8,
            "imm16" => OperandType::Imm16,
            "imm24" => OperandType::Imm24,
            other => panic!("invalid operand type {other:?} in instruction:\n{s}"),
        };

        operands.push((Operand { name, ty }, ty.bits()));
    }

    let operands = match &operands[..] {
        [] => Operands::None,
        [(a, 8)] => Operands::A8 { a: *a },
        [(a, 24)] => Operands::A24 { a: *a },
        [(a, 8), (b, 8)] => Operands::A8B8 { a: *a, b: *b },
        [(a, 8), (b, 16)] => Operands::A8B16 { a: *a, b: *b },
        [(a, 8), (b, 8), (c, 8)] => Operands::A8B8C8 {
            a: *a,
            b: *b,
            c: *c,
        },

        _ => panic!("invalid operand combination {operands:?} in instruction:\n{s}"),
    };

    let Ok(opcode) = parts
        .next()
        .ok_or_else(|| panic!("missing opcode value:\n{s}"));
    let opcode = if opcode == "auto" {
        previous_opcode + 1
    } else {
        match parse_hex_u8(opcode) {
            Some(v) => v,
            None => panic!("invalid hex opcode {opcode:?} in instruction:\n{s}"),
        }
    };

    Instruction {
        name,
        docs,
        opcode,
        operands,
    }
}

// `0xFF`
fn parse_hex_u8(s: &str) -> Option<u8> {
    let s = s.strip_prefix("0x")?;

    u8::from_str_radix(s, 16).ok()
}

use std::fmt::{Display, Write as _};

/// Emit a line
macro_rules! ln {
    ($f:ident, $($tt:tt)*) => (writeln!($f, $($tt)*).unwrap());
    ($f:ident) => (writeln!($f).unwrap());
}

/// Emit multi-line string
macro_rules! ml {
    ($f:ident, $($tt:tt)*) => (indoc::writedoc!($f, $($tt)*).unwrap());
}

fn emit(is: Instructions) -> String {
    // ...
    //
    // - generate assembly and disassembly
    //   - assembly = function to encode the instruction
    //     - note that certain instructions will need help, such as jumps
    //     - which have to be patched after-the-fact
    //
    //   - disassembly = function to decode the instruction and "pretty-print" it
    //     - should constants be printed inline?
    //     - should registers be transformed into variable names where appropriate?
    //     - it's possible, because "bytecode" isn't just the instruction encoding,
    //       but also constants and debug info.
    //     - debug info _could_ be optional if you don't plan to serialize.
    //     - printing should NOT require source code,
    //       but it could be optional to provide line-by-line disassembly
    //
    //   - each instruction needs a stable serialization for bytecode caching
    //     - stored as little-endian (no byte swaps on most CPUs)
    //     - NOT stable across "breaking" versions.
    //
    // jump table and dispatch will be written manually
    //
    //

    let mut o = String::new();

    emit_prelude(&mut o);
    emit_runtime(&mut o);
    emit_instruction_enum(&mut o, &is);
    emit_operand_structs(&mut o, &is);
    emit_instruction_asm(&mut o, &is);
    emit_jump_table(&mut o, &is);

    o
}

fn emit_prelude(o: &mut String) {
    ml!(
        o,
        "
        // Generated by asmgen at {now}
        //
        // See `asmgen/src/main.rs` and `asmgen/src/runtime.rs`
        ",
        now = jiff::Zoned::now().in_tz("UTC").unwrap(),
    );
}

fn emit_runtime(out: &mut String) {
    let runtime = include_str!("./runtime.rs");

    let file_start_marker = "//file-start";
    let runtime = runtime
        .find(file_start_marker)
        .map(|i| &runtime[i + file_start_marker.len()..])
        .unwrap_or(runtime);

    out.push_str(runtime);
    out.push('\n');
}

fn emit_instruction_enum(o: &mut String, is: &Instructions) {
    ml!(
        o,
        "
        #[derive(Clone, Copy)]
        #[repr(u8, align(4))]
        pub enum Instruction {{
        {instructions}
        }}

        // assert compatibility with `u32`
        const _: () = assert!(std::mem::size_of::<Instruction>() == std::mem::size_of::<u32>());
        const _: () = assert!(std::mem::align_of::<Instruction>() == std::mem::align_of::<u32>());

        ",
        instructions = is
            .flat
            .iter()
            .map(|i| {
                let name = i.type_name();
                let opcode = i.opcode;
                let fields = i.operands.fields(false);
                format!("    {name} {{ {fields} }} = {opcode},")
            })
            .join("\n")
    );
}

fn emit_operand_structs(o: &mut String, is: &Instructions) {
    for i in &is.flat {
        ml!(
            o,
            "
            #[derive(Clone, Copy)]
            #[repr(C, align(4))]
            pub struct {name} {{
            _op: u8,
            {fields}
            }}

            ",
            name = i.type_name(),
            fields = i.operands.fields(true),
        );

        // assert on field stability
        ml!(
            o,
            "
            const _: () = assert!(std::mem::size_of::<{name}>() == std::mem::size_of::<Instruction>());
            const _: () = assert!(std::mem::align_of::<{name}>() == std::mem::align_of::<Instruction>());
            const _: () = assert_bit_equal(
                &{name} {{ _op: {opcode}, {fields} }},
                &Instruction::{name} {{ {fields} }},
            );

            ",
            name = i.type_name(),
            opcode = i.opcode,
            fields = match i.operands {
                Operands::None => format!("_a: 0x7A, _b: 0x7B, _c: 0x7C"),
                Operands::A8 { a } => format!("{}: {}(0x7A), _b: 0x7B, _c: 0x7C", a.name, a.ty),
                Operands::A24 { a } => format!("{}: {}(u24::new(0x7A7B7C))", a.name, a.ty),
                Operands::A8B8 { a, b } => format!(
                    "{}: {}(0x7A), {}: {}(0x7B), _c: 0x7C",
                    a.name, a.ty, b.name, b.ty
                ),
                Operands::A8B16 { a, b } =>
                    format!("{}: {}(0x7A), {}: {}(0x7B7C)", a.name, a.ty, b.name, b.ty),
                Operands::A8B8C8 { a, b, c } => format!(
                    "{}: {}(0x7A), {}: {}(0x7B), {}: {}(0x7C)",
                    a.name, a.ty, b.name, b.ty, c.name, c.ty
                ),
            },
        );
    }
}

fn emit_instruction_asm(o: &mut String, is: &Instructions) {
    let instructions = is
        .flat
        .iter()
        .map(|i| {
            let name = i.name;
            let cname = i.type_name();
            let args = match i.operands {
                Operands::None => String::new(),
                Operands::A8 { a } => format!("{}: {}", a.name, a.ty),
                Operands::A24 { a } => format!("{}: {}", a.name, a.ty),
                Operands::A8B8 { a, b } => format!("{}: {}, {}: {}", a.name, a.ty, b.name, b.ty),
                Operands::A8B16 { a, b } => format!("{}: {}, {}: {}", a.name, a.ty, b.name, b.ty),
                Operands::A8B8C8 { a, b, c } => format!(
                    "{}: {}, {}: {}, {}: {},",
                    a.name, a.ty, b.name, b.ty, c.name, c.ty
                ),
            };
            let fields = match i.operands {
                Operands::None => format!("{{ _a: 0x7F, _b: 0x7F, _c: 0x7F }}"),
                Operands::A8 { a } => format!("{{ {}, _b: 0x7F, _c: 0x7F }}", a.name),
                Operands::A24 { a } => format!("{{ {} }}", a.name),
                Operands::A8B8 { a, b } => format!("{{ {}, {}, _c: 0x7F }}", a.name, b.name),
                Operands::A8B16 { a, b } => format!("{{ {}, {} }}", a.name, b.name),
                Operands::A8B8C8 { a, b, c } => format!("{{ {}, {}, {} }}", a.name, b.name, c.name),
            };
            format!("pub fn {name}({args}) -> Instruction {{ Instruction::{cname} {fields} }}")
        })
        .join('\n');
    ml!(
        o,
        "
        pub mod asm {{
            use super::*;
        {instructions}
        }}
        "
    );
}

fn emit_jump_table(o: &mut String, is: &Instructions) {
    ml!(
        o,
        "
        #[repr(C)]
        pub struct JumpTable {{
        {instructions}
        }}

        impl JumpTable {{
            #[inline]
            fn as_ptr(&self) -> JumpTablePtr {{
                let ptr = &raw const self.{first_instruction};
                JumpTablePtr(ptr.cast())
            }}
        }}


        ",
        first_instruction = is.flat.iter().next().unwrap().name,
        instructions = is
            .flat
            .iter()
            .map(|i| {
                let name = i.name;
                let type_name = i.type_name();
                format!("    pub {name}: Op<{type_name}>,")
            })
            .join("\n")
    );

    // ...
}

/*

// for inspection

```
#[repr(u8)]
enum Instruction {
    Nop,
    Add { dst: R, lhs: R, rhs: R },
    // etc
}
```

# assembly



# reading operands

```
use operands::*;

let i = (i);
```

this feels brittle, it should be something that's
kept up-to-date with assembly/disassembly. changing/adding/removing
an instruction should light up compiler output with errors that
point out places which must be updated. no virtue in doing this manually


idea: generate JumpTable _struct_, which holds a `fn` for each instruction:

```
enum Value { ... }
struct Instruction(u32);
struct Context { ... }

struct StackPtr(*mut Value);
struct InstructionPtr(*const Instruction);
struct JumpTablePtr(*const ());
struct ContextPtr(*mut Context);

// need 5 registers: either `extern "C"` on linux/mac, or `extern "sysv64"` on windows
/// for function types
macro_rules! platform_abi { ... }

/// for function declarations
macro_rules! platform_abi_decl { ... }

type Op<Operands: Sized> = platform_abi!(unsafe fn(Operands, JumpTablePtr, StackPtr, InstructionPtr));

struct Operands(u32);

type OpaqueOp = platform_abi!(unsafe fn(Operands, JumpTablePtr, StackPtr, InstructionPtr));

#[repr(C)]
struct JumpTable {
    nop: Op<Nop>,
    add: Op<Add>,
    // etc.
}
```

it _must_ be filled in in its entirety, so the instructions and operands are kept up-to-date!
the `JumpTable` struct is bit-compatible with a `[nop, add, ...]` array, so it
can be transmuted to an "opaque" jump table, which may be used to dispatch with just a `u32`
as the operand type.


*/

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
