use std::{fs::read_to_string, path::Path};

use super::parse;
use crate::token::tokenize;

#[glob_test::glob("../../tests/inputs/syntax/*.hi")]
fn parser(path: &Path) {
    let input = read_to_string(path).unwrap();
    let emit_snapshot = input.starts_with("//") && input.lines().next().unwrap().contains("parse");

    let tokens = tokenize(&input);
    let (snapshot, failure) = match parse(&tokens) {
        Ok(ast) => (format!("SOURCE\n{input}\n\nOK\n{ast:#?}"), false),
        Err(err) => (
            format!("SOURCE\n{input}\n\nERROR\n{}", err.render(&input)),
            true,
        ),
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
