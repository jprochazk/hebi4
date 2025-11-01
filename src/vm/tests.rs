use std::{fs::read_to_string, path::Path};

use super::Hebi;
use crate::Module;

fn module(input: &str) -> Module {
    let tokens = crate::token::tokenize(&input);
    let ast = match crate::parser::parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            panic!("{}", err.render(&input));
        }
    };
    let module = match crate::codegen::emit("test".into(), &ast, Default::default()) {
        Ok(m) => m,
        Err(err) => panic!("{}", err.render(&input)),
    };

    module
}

#[glob_test::glob("../../tests/inputs/run/*.hi")]
fn run(path: &Path) {
    let input = read_to_string(path).unwrap();
    let input = input.trim();
    let module = module(input);

    let mut vm = Hebi::new();
    vm.with(|mut r| {
        let loaded_module = r.load(&module);

        // run each code snippet twice using the same VM,
        // ensuring it has the same result.
        let (snapshot, failure) = match r.run(&loaded_module) {
            Ok(value) => (
                format!("SOURCE\n{input}\n\nOK\n{:?}", unsafe { value.as_ref() }),
                false,
            ),
            Err(err) => (
                format!(
                    "SOURCE\n{input}\n\nERROR\n{}\n\nDISASM:\n\n{}",
                    err.render(input),
                    module.disasm(input)
                ),
                true,
            ),
        };

        let (snapshot2, failure2) = match r.run(&loaded_module) {
            Ok(value) => (
                format!("SOURCE\n{input}\n\nOK\n{:?}", unsafe { value.as_ref() }),
                false,
            ),
            Err(err) => (
                format!(
                    "SOURCE\n{input}\n\nERROR\n{}\n\nDISASM:\n\n{}",
                    err.render(input),
                    module.disasm(input)
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
    let a = module(r#"fn f(a) { a } f(10)"#);
    let b = module(r#"100 + 200"#);

    Hebi::new().with(|mut r| {
        let a = r.load(&a);
        let a = r.run(&a).unwrap();
        let a = unsafe { a.as_ref() };
        let a = format!("{a:?}");
        let b = r.load(&b);
        let b = r.run(&b).unwrap();
        let b = unsafe { b.as_ref() };
        let b = format!("{b:?}");

        assert_eq!(a, "Int(10)");
        assert_eq!(b, "Int(300)");
    })
}
