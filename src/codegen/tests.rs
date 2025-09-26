use std::{fs::read_to_string, path::Path};

use crate::{parser::parse, token::tokenize};

use super::emit;

#[glob_test::glob("../../tests/inputs/codegen/*.hi")]
fn emitter(path: &Path) {
    let input = read_to_string(path).unwrap();
    let tokens = tokenize(&input);
    let ast = match parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            panic!("{}", err.render(&input));
        }
    };
    let (snapshot, failure) = match emit(&ast) {
        Ok(m) => (format!("OK\n{}", m.disasm(&input)), false),
        Err(err) => (format!("ERROR\n{}", err.render(&input)), true),
    };

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
}
