use std::ffi::OsStr;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    match args[0].as_str() {
        "codegen" => codegen(),
        "miri" => miri(&args[1..]),
        _ => help(),
    }
}

fn codegen() {
    cargo("run --release -p astgen -- src/ast/nodes.ast src/ast/nodes.rs").run();
    cargo("run --release -p asmgen -- src/codegen/opcodes.s src/codegen/opcodes.rs").run();
}

fn miri(args: &[String]) {
    let miriflags = "-Zmiri-tree-borrows -Zmiri-disable-isolation";
    cargo("miri test")
        .with_args(args)
        .with_envs([("MIRIFLAGS", miriflags)])
        .run();
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

fn cargo(s: &str) -> std::process::Command {
    cmd(&format!("cargo {s}"))
}

fn cmd(cmd: impl AsRef<str>) -> std::process::Command {
    let cmd = cmd.as_ref();
    let mut parts = cmd.split_ascii_whitespace();
    let Some(cmd) = parts.next() else {
        unreachable!("empty command");
    };
    let args = parts;

    let mut cmd = std::process::Command::new(cmd);
    cmd.args(args);
    cmd.stdout(std::io::stdout());
    cmd.stderr(std::io::stderr());
    cmd
}

trait CommandExt {
    fn run(self);
    fn with_envs<I, K, V>(self, envs: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: AsRef<OsStr>,
        V: AsRef<OsStr>;
    fn with_args<I, V>(self, args: I) -> Self
    where
        I: IntoIterator<Item = V>,
        V: AsRef<OsStr>;
}

impl CommandExt for std::process::Command {
    fn run(mut self) {
        let status = self
            .spawn()
            .expect("failed to spawn cmd")
            .wait()
            .expect("faield to run command");
        if !status.success() {
            eprintln!("command exited with non-zero exit code");
            std::process::exit(1);
        }
    }

    fn with_envs<I, K, V>(mut self, envs: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        self.envs(envs);
        self
    }

    fn with_args<I, V>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = V>,
        V: AsRef<OsStr>,
    {
        self.args(args);
        self
    }
}
