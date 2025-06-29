use std::fs::read_to_string;

use crate::{parser::parse, token::tokenize};

globdir::globdir!("tests/inputs/*.hi", |path| {
    let input = read_to_string(path).unwrap();
    let tokens = tokenize(&input);
    let snapshot = match parse(&tokens) {
        Ok(ast) => format!("OK\n{ast:#?}"),
        Err(err) => format!("ERROR\n{}", err.render(&input)),
    };
    insta::assert_snapshot!(snapshot)
});
