use std::ffi::OsStr;

mod check_tco;
use check_tco::check_tco;

mod ci;
use ci::{ci_jobs, output_matrix};

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    match args[0].as_str() {
        "playground" => playground(),

        "miri" => miri(&args[1..]),

        "check-tco" => check_tco(),

        "test" => test(&args[1..]),

        "clippy" => clippy(),

        "fmt" => fmt(),
        "fmt-check" => fmt_check(),

        "codegen" => codegen(),
        "codegen-check" => codegen_check(),

        "deny" => deny(),
        "shear" => shear(),

        "ci" => ci(),
        "ci-jobs" => output_matrix(),
        _ => help(),
    }
}

fn miri(args: &[String]) {
    // let miriflags = "-Zmiri-tree-borrows -Zmiri-disable-isolation";
    let miriflags = "-Zmiri-disable-isolation";
    cargo("+nightly miri nextest run")
        .with_args(args)
        .with_envs([("MIRIFLAGS", miriflags)])
        .run();
}

fn playground() {
    cmd("npm --prefix playground run dev").exec();
}

fn test(args: &[String]) {
    cargo("nextest run --all-features").with_args(args).run();
    cargo("nextest run --no-default-features")
        .with_args(args)
        .run();
}

fn clippy() {
    cargo("clippy --all-targets --all-features -- -D warnings").run();
}

fn fmt() {
    cargo("+nightly fmt --all -- --unstable-features --config imports_granularity=Crate,group_imports=StdExternalCrate")
        .run();
}

fn fmt_check() {
    cargo("+nightly fmt --all -- --unstable-features --config imports_granularity=Crate,group_imports=StdExternalCrate --check").run();
}

fn codegen() {
    cargo("run --release -p astgen -- src/ast/nodes.ast src/ast/nodes.rs").run();
    cargo("run --release -p asmgen -- src/codegen/opcodes.s src/codegen/opcodes.rs").run();
    fmt();
}

fn codegen_check() {
    codegen();
    cmd("git diff --exit-code").run();
}

fn deny() {
    cargo("deny check").run();
}

fn shear() {
    cargo("shear").run();
}

fn ci() {
    let jobs = ci_jobs();
    for (i, job) in jobs.iter().enumerate() {
        if i > 0 {
            eprintln!();
        }
        eprintln!("> {:?}", job.command);
        cargo(&format!("x {}", job.command)).run();
    }
    eprintln!("\n✓ All CI checks passed!");
}

fn help() {
    let s = "
usage: cargo x <command>

commands:
    codegen       generate AST and bytecode
    miri          run tests under miri
    check-tco     check if tail-call optimization kicked in (linux-x64 only)
    test          run test suite with nextest
    clippy        run clippy with all features
    fmt           run nightly rustfmt with import config
    fmt-check     check formatting with nightly rustfmt
    codegen-check check if codegen is up to date
    deny          run cargo deny checks
    shear         check for unused dependencies
    ci            run all CI checks locally
    ci-jobs       output CI job matrix as JSON (for GitHub Actions)
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
