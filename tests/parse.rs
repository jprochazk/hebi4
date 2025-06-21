use hebi4::{parser::parse, token::tokenize};
use std::fs::read_to_string;

globdir::globdir!("tests/inputs/*.hi", |path| {
    let input = read_to_string(path).unwrap();
    let tokens = tokenize(&input);
    insta::assert_debug_snapshot!(parse(&tokens).unwrap());
});
