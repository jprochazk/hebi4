use std::ffi::OsStr;

mod check_tco;
use check_tco::check_tco;

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    match args[0].as_str() {
        "codegen" => codegen(),
        "miri" => miri(&args[1..]),
        "check-tco" => check_tco(),
        "sort-imports" => sort_imports(),
        "playground" => playground(),
        _ => help(),
    }
}

fn codegen() {
    cargo("run --release -p astgen -- src/ast/nodes.ast src/ast/nodes.rs").run();
    cargo("run --release -p asmgen -- src/codegen/opcodes.s src/codegen/opcodes.rs").run();
}

fn miri(args: &[String]) {
    // let miriflags = "-Zmiri-tree-borrows -Zmiri-disable-isolation";
    let miriflags = "-Zmiri-disable-isolation";
    cargo("+nightly miri nextest run")
        .with_args(args)
        .with_envs([("MIRIFLAGS", miriflags)])
        .run();
}

fn sort_imports() {
    cargo("+nightly fmt -- --unstable-features --config imports_granularity=Crate,group_imports=StdExternalCrate")
        .run();
}

fn playground() {
    cmd("npm --prefix playground run dev").exec();
}

fn help() {
    let s = "
usage: cargo x <command>

commands:
    codegen       generate AST and bytecode
    miri          run tests under miri
    check-tco     check if tail-call optimization kicked in (linux-x64 only)
    sort-imports  run nightly rustfmt with import config
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
    cmd
}

trait CommandExt {
    fn run(self);
    fn exec(self);
    fn output(self) -> Vec<u8>;
    fn output_utf8(self) -> String;
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
        self.stdout(std::io::stdout());
        self.stderr(std::io::stderr());
        let status = self
            .spawn()
            .expect("failed to spawn cmd")
            .wait()
            .expect("failed to run command");
        if !status.success() {
            eprintln!("command exited with non-zero exit code");
            std::process::exit(1);
        }
    }

    fn exec(self) {
        imp::exec_replace(self);
    }

    fn output(mut self) -> Vec<u8> {
        let output = std::process::Command::output(&mut self).expect("failed to run command");
        if !output.status.success() {
            eprintln!("command exited with non-zero exit code");
            if let Ok(err) = String::from_utf8(output.stderr) {
                eprintln!("{err}");
            } else {
                eprintln!("(stderr was not valid utf-8)");
            }
            std::process::exit(1);
        }
        output.stdout
    }

    fn output_utf8(self) -> String {
        String::from_utf8(self.output()).expect("invalid utf-8 output")
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

#[cfg(unix)]
mod imp {
    use std::{os::unix::process::CommandExt, process::Command};

    pub fn exec_replace(mut command: Command) {
        eprintln!("{}", command.exec());
    }
}

#[cfg(windows)]
mod imp {
    pub fn exec_replace(command: Command) {
        // Just execute the process as normal.
        process_builder.run()
    }
}
