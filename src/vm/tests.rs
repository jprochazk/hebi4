use std::fs::read_to_string;

use super::Vm;
use crate::{codegen::emit, parser::parse, token::tokenize};

uitest::uitest!("tests/inputs/run/*.hi", "run", |path| {
    let input = read_to_string(path).unwrap();
    let tokens = tokenize(&input);
    let ast = match parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            panic!("{}", err.render(&input));
        }
    };
    let chunk = match emit(&ast) {
        Ok(chunk) => chunk,
        Err(err) => panic!("{}", err.render(&input)),
    };
    let disasm = chunk.disasm(&input).to_string();

    let mut vm = Vm::new();
    let (snapshot, failure) = vm.with(|mut r| match r.run_once(chunk) {
        Ok(()) => (format!("OK\n"), false),
        Err(err) => (
            format!("ERROR\n{}\n\nDISASM:\n\n{disasm}", err.render(&input)),
            true,
        ),
    });

    #[cfg(not(miri))]
    {
        insta::assert_snapshot!(snapshot);
        let _ = failure;
    }

    #[cfg(miri)]
    {
        if failure {
            panic!("{snapshot}");
        } else {
            eprintln!("{snapshot}");
        }
    }
});
