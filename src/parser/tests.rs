use std::{fs::read_to_string, path::Path};

use crate::token::tokenize;

use super::parse;

#[glob_test::glob("../../tests/inputs/syntax/*.hi")]
fn parser(path: &Path) {
    let input = read_to_string(path).unwrap();
    let emit_snapshot = input.starts_with("##") && input.lines().next().unwrap().contains("parse");

    let tokens = tokenize(&input);
    let (snapshot, failure) = match parse(&tokens) {
        Ok(ast) => (format!("OK\n{ast:#?}"), false),
        Err(err) => (format!("ERROR\n{}", err.render(&input)), true),
    };

    #[cfg(not(miri))]
    if emit_snapshot {
        insta::assert_snapshot!(snapshot);
        let _ = failure;
    }

    #[cfg(miri)]
    {
        let _ = emit_snapshot;
        if failure {
            panic!("{snapshot}");
        } else {
            eprintln!("{snapshot}");
        }
    }
}
