use super::{Module, Vm};
use std::{fs::read_to_string, path::Path};

fn module(input: &str) -> Module {
    let tokens = crate::token::tokenize(&input);
    let ast = match crate::parser::parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            panic!("{}", err.render(&input));
        }
    };
    let module = match crate::codegen::emit(&ast) {
        Ok(m) => m,
        Err(err) => panic!("{}", err.render(&input)),
    };

    module
}

#[glob_test::glob("../../tests/inputs/run/*.hi")]
fn run(path: &Path) {
    let input = read_to_string(path).unwrap();
    let module = module(&input);

    let mut vm = Vm::new();
    vm.with(|mut r| {
        // run each code snippet twice using the same VM,
        // ensuring it has the same result.
        let (snapshot, failure) = match r.run(&module) {
            Ok(value) => (format!("OK\n{}", r.fmt(value)), false),
            Err(err) => (
                format!(
                    "ERROR\n{}\n\nDISASM:\n\n{}",
                    err.render(&input),
                    module.disasm(&input)
                ),
                true,
            ),
        };

        let (snapshot2, failure2) = match r.run(&module) {
            Ok(value) => (format!("OK\n{}", r.fmt(value)), false),
            Err(err) => (
                format!(
                    "ERROR\n{}\n\nDISASM:\n\n{}",
                    err.render(&input),
                    module.disasm(&input)
                ),
                true,
            ),
        };

        assert_eq!(snapshot, snapshot2);
        assert_eq!(failure, failure2);

        #[cfg(not(miri))]
        {
            insta::assert_snapshot!(snapshot);
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

// It should be possible to run two or more different modules
// using the same VM.
#[test]
fn separate_modules() {
    let a = module(r#"fn f(a) do a end f(10)"#);
    let b = module(r#"100 + 200"#);

    Vm::new().with(|mut r| {
        let a = r.run(&a).unwrap();
        let a = r.fmt(a).to_string();
        let b = r.run(&b).unwrap();
        let b = r.fmt(b).to_string();

        assert_eq!(a, "10");
        assert_eq!(b, "300");
    })
}
