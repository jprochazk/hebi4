use crate::token::tokenize;
use std::fs::read_to_string;

globdir::globdir!("tests/inputs/*.hi", |path| {
    let input = read_to_string(path).unwrap();
    insta::assert_debug_snapshot!(tokenize(&input));
});
