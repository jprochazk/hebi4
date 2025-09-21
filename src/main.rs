use std::{fs::read_to_string, path::Path};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.len() != 1 {
        return help();
    }

    let path = Path::new(args[0].as_str());
    run(&path);
}

fn run(path: &Path) {
    let code = read_to_string(path).expect("failed to read file");

    let tokens = hebi4::tokenize(&code);
    let ast = match hebi4::parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("{}", err.render(&code));
            std::process::exit(1);
        }
    };

    let chunk = match hebi4::emit(&ast) {
        Ok(chunk) => chunk,
        Err(err) => {
            eprintln!("{}", err.render(&code));
            std::process::exit(1);
        }
    };

    hebi4::Vm::new().with(|mut vm| {
        let result = match vm.run(chunk) {
            Ok(v) => vm.fmt(v).to_string(),
            Err(err) => err.render(&code).to_string(),
        };
        println!("{result}");
    });
}

fn help() {
    eprintln!("Usage: hebi4 <file.hi>");
}
