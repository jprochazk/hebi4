use std::fs::read_to_string;

use crate::{parser::parse, token::tokenize};

globdir::globdir!("tests/inputs/*.hi", |path| {
    let input = read_to_string(path).unwrap();
    let tokens = tokenize(&input);
    insta::assert_debug_snapshot!(parse(&tokens).unwrap());
});
