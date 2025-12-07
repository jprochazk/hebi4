use std::collections::BTreeSet;

use super::*;

const JUMP_TABLE_PREFIX: &str = "hebi4::vm::JT::";
const TCO_EXCEPTIONS: &[&str] = &[
    // `stop` mean stop, so it should return, not tail-call
    "::stop",
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Arch {
    X86_64,
    Aarch64,
}

fn detect_arch() -> Arch {
    if cfg!(target_arch = "x86_64") {
        Arch::X86_64
    } else if cfg!(target_arch = "aarch64") {
        Arch::Aarch64
    } else {
        panic!("Unsupported architecture")
    }
}

pub fn disasm() {
    let c = cargo("build").with_args(["--release", "--package=hebi4-cli", "--bin=hebi4"]);
    c.run();

    let arch = detect_arch();
    let objdump_cmd = match arch {
        Arch::X86_64 => {
            "objdump -C --no-addresses --no-show-raw-insn -j .text -M intel -d target/release/hebi4"
        }
        Arch::Aarch64 => "objdump -C --no-addresses --no-show-raw-insn -d target/release/hebi4",
    };
    let c = cmd(objdump_cmd);
    c.run();
}

pub fn check_tco() {
    let arch = detect_arch();

    let o = {
        let c = cargo("build").with_args(["--release", "--package=hebi4-cli", "--bin=hebi4"]);
        println!("> {c:?}");
        c.run();

        let objdump_cmd = match arch {
            Arch::X86_64 => {
                "objdump -C --no-addresses --no-show-raw-insn -j .text -M intel -d target/release/hebi4"
            }
            Arch::Aarch64 => "objdump -C --no-addresses --no-show-raw-insn -d target/release/hebi4",
        };
        let c = cmd(objdump_cmd);
        println!("> {c:?}");

        c.output_utf8()
    };

    // find all jump table entries, and check if TCO kicked in
    println!("Parsing sections");
    let mut jt = BTreeSet::new();
    let mut missing_tco = Vec::new();
    for section in parse_sections(&o, JUMP_TABLE_PREFIX) {
        let name_without_prefix = section.name.strip_prefix(JUMP_TABLE_PREFIX).unwrap();
        // on arm these are additionally hashed
        let opcode_name = name_without_prefix
            .split_once("::")
            .map(|(name, _)| name)
            .unwrap_or(name_without_prefix);
        jt.insert(opcode_name);

        if !has_tail_call(&section, arch) {
            // this one is allowed to have no tail-call jump
            if TCO_EXCEPTIONS
                .iter()
                .any(|ex| ex.starts_with("::") && opcode_name == &ex[2..])
            {
                continue;
            }
            missing_tco.push(section.name.to_string());
        }
    }

    let mut failed = false;

    // every opcode must have a corresponding JT entry
    let opcodes_s =
        std::fs::read_to_string("src/codegen/opcodes.s").expect("failed to read opcodes.s");
    let mut missing = Vec::new();
    for op in opcodes(&opcodes_s) {
        if !jt.contains(op) {
            missing.push(op);
        }
    }

    if !missing.is_empty() {
        eprintln!("Missing JT entries for opcodes:");
        for m in missing {
            eprintln!("  {m:?}");
        }
        failed = true;
    } else {
        println!("All instructions have a jump table entry");
    }

    if !missing_tco.is_empty() {
        println!("Instructions without tail-call jump to next handler:");
        for n in &missing_tco {
            println!("  {n:?}");
        }
        failed = true;
    } else {
        println!("All instructions are tail-call optimized");
    }

    if failed {
        println!("Some checks failed");
        std::process::exit(1);
    } else {
        println!("All checks passed");
    }
}

fn is_register(s: &str, arch: Arch) -> bool {
    match arch {
        Arch::X86_64 => matches!(
            s,
            "rax"
                | "rbx"
                | "rcx"
                | "rdx"
                | "rsi"
                | "rdi"
                | "rbp"
                | "rsp"
                | "r8"
                | "r9"
                | "r10"
                | "r11"
                | "r12"
                | "r13"
                | "r14"
                | "r15"
        ),
        Arch::Aarch64 => {
            // ARM64 registers: x0-x30, w0-w30, sp, lr, etc.
            matches!(
                s,
                "x0" | "x1"
                    | "x2"
                    | "x3"
                    | "x4"
                    | "x5"
                    | "x6"
                    | "x7"
                    | "x8"
                    | "x9"
                    | "x10"
                    | "x11"
                    | "x12"
                    | "x13"
                    | "x14"
                    | "x15"
                    | "x16"
                    | "x17"
                    | "x18"
                    | "x19"
                    | "x20"
                    | "x21"
                    | "x22"
                    | "x23"
                    | "x24"
                    | "x25"
                    | "x26"
                    | "x27"
                    | "x28"
                    | "x29"
                    | "x30"
                    | "w0"
                    | "w1"
                    | "w2"
                    | "w3"
                    | "w4"
                    | "w5"
                    | "w6"
                    | "w7"
                    | "w8"
                    | "w9"
                    | "w10"
                    | "w11"
                    | "w12"
                    | "w13"
                    | "w14"
                    | "w15"
                    | "w16"
                    | "w17"
                    | "w18"
                    | "w19"
                    | "w20"
                    | "w21"
                    | "w22"
                    | "w23"
                    | "w24"
                    | "w25"
                    | "w26"
                    | "w27"
                    | "w28"
                    | "w29"
                    | "w30"
                    | "sp"
                    | "lr"
            )
        }
    }
}

fn has_tail_call(section: &Section, arch: Arch) -> bool {
    match arch {
        Arch::X86_64 => {
            for ins in &section.code {
                if ins.op == "jmp" {
                    let a = ins.args.trim();
                    if a.is_empty() {
                        continue;
                    }

                    // Check for direct register jump: `jmp rax`
                    if !a.contains('[') && !a.contains(']') {
                        let first_arg = a.split_whitespace().next().unwrap_or("");
                        if is_register(first_arg, arch) {
                            return true;
                        }
                    }

                    // Check for indirect memory jump with register: `jmp QWORD PTR [rsi+rax*8]`
                    // Must contain a register to be a computed jump, not a static address
                    if let Some(start) = a.find('[') {
                        if let Some(end) = a.find(']') {
                            let addr_expr = &a[start + 1..end];
                            // Check if any register is used in the address expression
                            if addr_expr
                                .split(|c: char| !c.is_alphanumeric())
                                .any(|part| is_register(part, arch))
                            {
                                return true;
                            }
                        }
                    }
                }
            }
            false
        }
        Arch::Aarch64 => {
            for ins in &section.code {
                // ARM64 uses `br` for indirect branch (tail call)
                if ins.op == "br" {
                    let a = ins.args.trim();
                    if a.is_empty() {
                        continue;
                    }

                    // Check for register branch: `br x4`
                    let first_arg = a.split_whitespace().next().unwrap_or("");
                    if is_register(first_arg, arch) {
                        return true;
                    }
                }
            }
            false
        }
    }
}

fn opcodes(src: &str) -> impl Iterator<Item = &str> {
    let mut lines = src.lines();

    std::iter::from_fn(move || {
        while let Some(line) = lines.next() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            let Some(name) = line.split_ascii_whitespace().next() else {
                continue;
            };

            if name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
                return Some(name);
            }
        }

        None
    })
}

#[derive(Debug, PartialEq, Eq)]
struct Section<'a> {
    /// section name
    name: &'a str,

    /// list of instructions
    code: Vec<Instruction<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
struct Instruction<'a> {
    /// opcode, such as `jmp`
    op: &'a str,

    /// args, everything until the end of the line, excluding any comments
    args: &'a str,
}

// addr <name>:
//   addr opcode args  # optional comment
//   addr opcode args
//   ...
//
// addr <next_name>:
//   ...
//
fn parse_sections<'a>(o: &'a str, filter_prefix: &str) -> impl Iterator<Item = Section<'a>> {
    // On x86_64 Linux/ELF: ".text:"
    // On ARM64 macOS/Mach-O: "__TEXT,__text:"
    let text_section = if let Some((_, text)) = o.split_once(".text:") {
        text
    } else if let Some((_, text)) = o.split_once("__TEXT,__text:") {
        text
    } else {
        panic!("Could not find .text or __TEXT,__text section in objdump output");
    };
    let mut c = text_section.as_bytes();

    core::iter::from_fn(move || {
        let c = &mut c;

        loop {
            skip_ws(c);
            let Some(name) = parse_header(c) else {
                // done
                break None;
            };

            if !name.starts_with(filter_prefix) {
                skip_until_next_header(c);
                continue;
            }

            let mut code = Vec::new();
            while let Some(op) = parse_op(c) {
                code.push(op);
            }

            break Some(Section { name, code });
        }
    })
}

fn utf8(c: &[u8]) -> &str {
    core::str::from_utf8(c).expect("valid utf-8")
}

fn parse_op<'a>(c: &mut &'a [u8]) -> Option<Instruction<'a>> {
    let line = line(c);
    if line.is_empty() {
        return None;
    }
    let line = line.trim();

    // Split on any whitespace (tab or space), not just space
    let mut parts = line.splitn(2, |c: char| c.is_whitespace());
    let op = parts.next().unwrap_or("");
    let rest = parts.next().unwrap_or("");
    let (args, _) = rest.split_once('#').unwrap_or((rest, ""));

    Some(Instruction {
        op: op.trim(),
        args: args.trim(),
    })
}

fn skip_until_next_header(c: &mut &[u8]) {
    while !line(c).is_empty() {}
}

fn line<'a>(c: &mut &'a [u8]) -> &'a str {
    let mut i = 0;
    while i < c.len() && c[i] != b'\n' {
        i += 1;
    }

    let line = utf8(&c[..i]);

    *c = c.get(i + 1..).unwrap_or(&[]);

    line
}

fn parse_header<'a>(c: &mut &'a [u8]) -> Option<&'a str> {
    if c.get(0) != Some(&b'<') {
        return None;
    }
    let name = parse_symbol(c);

    if c.get(0) == Some(&b'\n') {
        *c = &c[1..];
    }

    Some(name)
}

// `<foo::<bar>>:` -> `foo::<bar>`
fn parse_symbol<'a>(c: &mut &'a [u8]) -> &'a str {
    let mut i = 0;
    while i < c.len() && c[i] != b'\n' {
        i += 1;
    }

    let name = utf8(&c[1..i - 2]);

    *c = &c[i..];

    name
}

fn skip_ws(c: &mut &[u8]) {
    let mut i = 0;
    while i < c.len() && c[i].is_ascii_whitespace() {
        i += 1;
    }
    *c = &c[i..];
}

#[test]
fn asdf() {
    let src = r#"
target/release/hebi4:     file format elf64-x86-64


Disassembly of section .text:

<_start>:
	endbr64
	xor    ebp,ebp
	and    rsp,0xfffffffffffffff0
	push   rax
	call   QWORD PTR [rip+0x6b50b]        # <__libc_start_main@GLIBC_2.34>
	int3

<deregister_tm_clones::<foo>>:
	jmp    rax
	ret
	nop    DWORD PTR [rax+0x0]

<hebi::vm::JT::asdf>:
	jmp    rax

"#;
    assert_eq!(
        parse_sections(src, "").collect::<Vec<_>>(),
        [
            Section {
                name: "_start",
                code: vec![
                    Instruction {
                        op: "endbr64",
                        args: "",
                    },
                    Instruction {
                        op: "xor",
                        args: "ebp,ebp",
                    },
                    Instruction {
                        op: "and",
                        args: "rsp,0xfffffffffffffff0",
                    },
                    Instruction {
                        op: "push",
                        args: "rax",
                    },
                    Instruction {
                        op: "call",
                        args: "QWORD PTR [rip+0x6b50b]",
                    },
                    Instruction {
                        op: "int3",
                        args: "",
                    },
                ],
            },
            Section {
                name: "deregister_tm_clones::<foo>",
                code: vec![
                    Instruction {
                        op: "jmp",
                        args: "rax",
                    },
                    Instruction {
                        op: "ret",
                        args: "",
                    },
                    Instruction {
                        op: "nop",
                        args: "DWORD PTR [rax+0x0]",
                    },
                ],
            },
            Section {
                name: "hebi::vm::JT::asdf",
                code: vec![Instruction {
                    op: "jmp",
                    args: "rax",
                }]
            }
        ]
    );
}
