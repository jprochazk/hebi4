use std::{fs::read_to_string, path::Path};

use super::emit;
use crate::{codegen::EmitOptions, parser::parse, token::tokenize};

struct Options {
    emit: EmitOptions,
    module_test: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            emit: EmitOptions::default(),
            module_test: false,
        }
    }
}

fn parse_options(options: &str) -> Options {
    let mut o = Options {
        emit: EmitOptions::empty(),
        module_test: false,
    };

    for opt in options.split(',') {
        match opt {
            "dce" => o.emit.dead_code_elimination = true,
            "module" => o.module_test = true,
            _ => {}
        }
    }

    o
}

#[glob_test::glob("../../tests/inputs/codegen/*.hi")]
fn emitter(path: &Path) {
    let input = read_to_string(path).unwrap();
    let input = input.trim();
    let options = if input.starts_with("//") {
        let first_line = input.lines().next().unwrap().trim_start_matches("//");
        parse_options(first_line)
    } else {
        Default::default()
    };
    let input = if options.module_test {
        input
    } else {
        &format!("do {{\n{input}\n}}")
    };

    let tokens = tokenize(input);
    let ast = match parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            panic!("{}", err.render(input));
        }
    };
    let (snapshot, failure) = match emit("test".into(), &ast, options.emit) {
        Ok(m) => (format!("SOURCE\n{input}\n\nOK\n{}", m.disasm(input)), false),
        Err(err) => (
            format!("SOURCE\n{input}\n\nERROR\n{}", err.render(input)),
            true,
        ),
    };

    #[cfg(not(miri))]
    {
        insta::assert_snapshot!(snapshot);
        let _ = failure;
    }

    #[cfg(miri)]
    {
        eprintln!("{snapshot}");
    }
}

// NOTE: leave this here, it's handy for debugging:
// 1. uncomment this test
// 2. filter for it (`cargo test -- _temp`),
// 3. set a breakpoint somewhere in codegen

// #[test]
// fn _temp() {
//     emitter(std::path::Path::new("tests/inputs/run/arithmetic_add.hi"));
//     panic!();
// }
