use std::{
    fs::read_to_string,
    io::{BufWriter, StdoutLock, Write, stdout},
    path::Path,
};

use argh::FromArgValue;
use hebi4::prelude::*;

fn main() {
    let args: Args = argh::from_env();

    let opts = args.optimize.unwrap_or_default().0;
    if args.print_ast {
        let code = &args.code_or_path;
        print_ast(code);
    } else if args.disassemble {
        let code = &args.code_or_path;
        disassemble(code, opts);
    } else if args.eval {
        let code = &args.code_or_path;
        eval_string(code, opts);
    } else {
        let path = Path::new(&args.code_or_path);
        eval_file(path, opts)
    }
}

#[derive(argh::FromArgs)]
/// Hebi4 CLI.
struct Args {
    /// enable an optimization
    #[argh(option, short = 'O')]
    optimize: Option<Optimize>,

    /// parse and pretty-print AST
    #[argh(switch, short = 'p', long = "print-ast")]
    print_ast: bool,

    /// compile and print disassembly
    #[argh(switch, short = 'd')]
    disassemble: bool,

    /// eval a string
    #[argh(switch, short = 'e')]
    eval: bool,

    /// code to evaluate if `-e` is set, or path to a file
    #[argh(positional)]
    code_or_path: String,
}

#[derive(Default)]
struct Optimize(EmitOptions);

impl FromArgValue for Optimize {
    fn from_arg_value(value: &str) -> Result<Self, String> {
        let mut o = EmitOptions::empty();
        for opt in value.split(',') {
            match opt {
                "dce" => o.dead_code_elimination = true,
                _ => return Err(format!("unrecognized optimization option: {opt}")),
            }
        }
        Ok(Optimize(o))
    }
}

macro_rules! bail {
    ($($tt:tt)*) => {
        eprintln!($($tt)*);
        ::std::process::exit(1);
    };
}

struct BufferedStdout<'a>(BufWriter<StdoutLock<'a>>);

impl BufferedStdout<'_> {
    fn with<R>(f: impl FnOnce(&mut Self) -> R) -> R {
        f(&mut Self(BufWriter::new(stdout().lock())))
    }
}

impl Write for BufferedStdout<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

impl Drop for BufferedStdout<'_> {
    fn drop(&mut self) {
        self.0.flush().unwrap();
    }
}

fn disassemble(code: &str, opts: EmitOptions) {
    let module = compile_string(code, opts);
    BufferedStdout::with(|o| {
        writeln!(o, "{}", module.disasm(code)).unwrap();
    });
}

fn print_ast(code: &str) {
    match hebi4::parse(code) {
        Ok(ast) => BufferedStdout::with(|o| {
            for node in ast.root().body() {
                writeln!(o, "{node:#?}").unwrap();
            }
        }),
        Err(err) => {
            bail!("{}", err.render(&code).to_string());
        }
    }
}

fn compile_string(code: &str, opts: EmitOptions) -> Module {
    match Module::compile_with(None, &code, opts) {
        Ok(m) => m,
        Err(err) => {
            eprintln!("{}", err.render(&code));
            std::process::exit(1);
        }
    }
}

fn eval_string(code: &str, opts: EmitOptions) {
    let module = compile_string(code, opts);

    Hebi::new().with(|mut vm| {
        let loaded_module = vm.load(&module);
        match vm.run(&loaded_module) {
            Ok(v) => BufferedStdout::with(|o| {
                writeln!(o, "{:?}", unsafe { v.as_ref() }).unwrap();
            }),
            Err(err) => {
                bail!("{}", err.render(&code).to_string());
            }
        }
    });
}

fn eval_file(path: &Path, opts: EmitOptions) {
    let code = read_to_string(path).expect("failed to read file");
    eval_string(&code, opts)
}
