use crate::token::tokenize;
use std::fs::read_to_string;

uitest::uitest!("tests/inputs/*.hi", "tokenize", |path| {
    let input = read_to_string(path).unwrap();
    insta::assert_debug_snapshot!(tokenize(&input));
});
