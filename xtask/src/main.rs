fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    match args[0].as_str() {
        "codegen" => codegen(),
        _ => help(),
    }
}

fn codegen() {
    cargo("run --release -p astgen -- src/ast/nodes.ast src/ast/nodes.rs");
    cargo("run --release -p asmgen -- src/codegen/opcodes.s src/codegen/opcodes.rs");
}

fn help() {
    let s = "
usage: cargo x <command>

commands:
    codegen    generate AST and bytecode
";
    eprint!("{s}");

    std::process::exit(1);
}

fn cargo(s: &str) {
    cmd(&format!("cargo {s}"))
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
        .expect("failed to spawn cmd");
    let status = p.wait().expect("faield to run command");
    if !status.success() {
        eprintln!("command exited with non-zero exit code");
        std::process::exit(1);
    }
}
