use std::{fs::read_to_string, path::Path};

#[glob_test::glob("../../tests/inputs/run/*.hi")]
fn run(path: &Path) {
    let input = read_to_string(path).unwrap();
    let tokens = crate::token::tokenize(&input);
    let ast = match crate::parser::parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            panic!("{}", err.render(&input));
        }
    };
    let chunk = match crate::codegen::emit(&ast) {
        Ok(chunk) => chunk,
        Err(err) => panic!("{}", err.render(&input)),
    };
    let disasm = chunk.disasm(&input).to_string();

    let mut vm = crate::vm::Vm::new();
    vm.with(|mut r| {
        let (snapshot, failure) = match r.run(chunk) {
            Ok(value) => (format!("OK\n{}", r.fmt(value)), false),
            Err(err) => (
                format!("ERROR\n{}\n\nDISASM:\n\n{disasm}", err.render(&input)),
                true,
            ),
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
}
