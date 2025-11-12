use std::collections::BTreeSet;

use super::*;

const JUMP_TABLE_PREFIX: &str = "hebi4::vm::JT::";
const TCO_EXCEPTIONS: &[&str] = &[
    // `stop` mean stop, so it shouldn't tail call anything else.
    "::stop",
    // host calls use the machine stack, which is fine.
    "::hostcall",
];

pub fn check_tco() {
    let o = {
        let c = cargo("build").with_args(["--release", "--package=hebi4-cli", "--bin=hebi4"]);
        println!("> {c:?}");
        c.run();

        let c = cmd(
            "objdump -C --no-addresses --no-show-raw-insn -j .text -M intel -d target/release/hebi4",
        );
        println!("> {c:?}");

        c.output_utf8()
    };

    // find all jump table entries, and check if TCO kicked in
    println!("Parsing sections");
    let mut jt = BTreeSet::new();
    let mut missing_tco = Vec::new();
    for section in parse_sections(&o, JUMP_TABLE_PREFIX) {
        jt.insert(section.name.strip_prefix(JUMP_TABLE_PREFIX).unwrap());

        if !has_jump_to_reg(&section) {
            // this one is allowed to have no tail-call jump
            if TCO_EXCEPTIONS.iter().any(|ex| section.name.ends_with(ex)) {
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

fn is_register(s: &str) -> bool {
    match s {
        "rax" | "rbx" | "rcx" | "rdx" | "rsi" | "rdi" | "rbp" | "rsp" | "r8" | "r9" | "r10"
        | "r11" | "r12" | "r13" | "r14" | "r15" => true,
        _ => false,
    }
}

fn has_jump_to_reg(section: &Section) -> bool {
    for ins in &section.code {
        if ins.op == "jmp" {
            let a = ins.args.trim();
            // require a direct register, not memory
            if !a.is_empty() && !a.contains('[') && !a.contains(']') {
                let first_arg = a.split_whitespace().next().unwrap_or("");
                if is_register(first_arg) {
                    return true;
                }
            }
        }
    }

    false
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
    let (_, text) = o.split_once(".text:").unwrap();
    let mut c = text.as_bytes();

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

    let (op, rest) = line.split_once(' ').unwrap_or((line, ""));
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
