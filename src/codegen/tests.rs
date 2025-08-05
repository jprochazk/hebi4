use std::fs::read_to_string;

use super::emit;
use crate::{parser::parse, token::tokenize};

uitest::uitest!("tests/inputs/codegen/*.hi", "codegen", |path| {
    let input = read_to_string(path).unwrap();
    let tokens = tokenize(&input);
    let ast = match parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            panic!("{}", err.render(&input));
        }
    };
    let (snapshot, failure) = match emit(&ast) {
        Ok(chunk) => (format!("OK\n{}", chunk.disasm(&input)), false),
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
});
