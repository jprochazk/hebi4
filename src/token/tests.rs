use std::{fs::read_to_string, path::Path};

use crate::token::tokenize;

#[glob_test::glob("../../tests/inputs/syntax/*.hi")]
fn tokenizer(path: &Path) {
    let input = read_to_string(path).unwrap();
    let input = input.trim();
    let emit_snapshot =
        input.starts_with("//") && input.lines().next().unwrap().contains("tokenize");

    let snapshot = format!("SOURCE\n{input}\n\nTOKENS\n{:#?}", tokenize(&input));

    #[cfg(not(miri))]
    if emit_snapshot {
        insta::assert_snapshot!(snapshot);
    }

    #[cfg(miri)]
    {
        let _ = emit_snapshot;
        eprintln!("{snapshot}");
    }
}
