use std::{fs::read_to_string, path::Path};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let args: Vec<&str> = args.iter().map(|s| s.as_str()).collect();

    match &args[..] {
        ["-p" | "--print-ast", expr] => print_string(expr),
        ["-e" | "--eval", expr] => eval_string(expr),
        [path] => eval_file(Path::new(path)),
        _ => help(),
    }
}

fn print_string(code: &str) {
    let result = match hebi4::parse(code) {
        Ok(ast) => {
            let mut out = String::new();
            for node in ast.root().body() {
                use std::fmt::Write as _;
                writeln!(&mut out, "{node:#?}").unwrap();
            }
            out
        }
        Err(err) => err.render(&code).to_string(),
    };
    println!("{result}");
}

fn eval_string(code: &str) {
    let module = match hebi4::Module::compile(None, &code) {
        Ok(m) => m,
        Err(err) => {
            eprintln!("{}", err.render(&code));
            std::process::exit(1);
        }
    };

    hebi4::Hebi::new().with(|mut vm| {
        let loaded_module = vm.load(&module);
        let result = match vm.run(&loaded_module) {
            Ok(v) => format!("{:?}", unsafe { v.as_ref() }),
            Err(err) => err.render(&code).to_string(),
        };
        println!("{result}");
    });
}

fn eval_file(path: &Path) {
    let code = read_to_string(path).expect("failed to read file");
    eval_string(&code)
}

fn help() {
    eprintln!(
        "\
Hebi4

Usage: hebi4 <file|options>

Options:
  -p, --print-ast <string>  Parse `string` and pretty-print its AST
  -e, --eval <string>       Evaluate `string`
"
    );
}
