use std::{fmt::Write as _, fs::read_to_string, path::Path};

use super::Hebi;
use crate::{
    module::{NativeModule, function},
    vm::{self, Context, Runtime, Stdio},
};

fn module(input: &str) -> crate::module::Module {
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

fn take_stdio(r: &mut Runtime<'_>) -> (String, String) {
    let stdout = r
        .stdio()
        .stdout
        .as_any_mut()
        .downcast_mut::<Vec<u8>>()
        .unwrap();
    let stdout_s = String::from_utf8(stdout.clone()).unwrap();
    stdout.clear();
    let stderr = r
        .stdio()
        .stderr
        .as_any_mut()
        .downcast_mut::<Vec<u8>>()
        .unwrap();
    let stderr_s = String::from_utf8(stderr.clone()).unwrap();
    stderr.clear();

    (stdout_s, stderr_s)
}

fn snapshot<'vm>(
    input: &str,
    module: &crate::module::Module,
    r: &mut Runtime<'vm>,
    loaded_module: &vm::Module<'vm>,
) -> (String, bool) {
    match r.run(&loaded_module) {
        Ok(value) => {
            let (stdout, stderr) = take_stdio(r);
            let mut snapshot = format!("SOURCE\n{input}\n\nOK\n{:?}", unsafe { value.as_ref() });
            if !stdout.is_empty() {
                snapshot.write_str("\n\nSTDOUT\n").unwrap();
                snapshot.write_str(&stdout).unwrap();
            }
            if !stderr.is_empty() {
                snapshot.write_str("\n\nSTDERR\n").unwrap();
                snapshot.write_str(&stderr).unwrap();
            }

            (snapshot, false)
        }

        Err(err) => {
            let (stdout, stderr) = take_stdio(r);
            let mut snapshot = format!(
                "SOURCE\n{input}\n\nERROR\n{}\n\nDISASM:\n\n{}",
                err.render(input),
                module.disasm(input)
            );
            if !stdout.is_empty() {
                snapshot.write_str("\n\nSTDOUT\n").unwrap();
                snapshot.write_str(&stdout).unwrap();
            }
            if !stderr.is_empty() {
                snapshot.write_str("\n\nSTDERR\n").unwrap();
                snapshot.write_str(&stderr).unwrap();
            }

            (snapshot, true)
        }
    }
}

fn native_module() -> NativeModule {
    fn foo(cx: Context<'_>) -> &'static str {
        "hello"
    }

    NativeModule::builder("test")
        .function(function!(foo))
        .finish()
}

fn vm() -> Hebi {
    Hebi::new().with_stdio(Stdio {
        stdout: Box::new(Vec::new()),
        stderr: Box::new(Vec::new()),
    })
}

#[glob_test::glob("../../tests/inputs/run/*.hi")]
fn run(path: &Path) {
    let input = read_to_string(path).unwrap();
    let input = input.trim();
    let module = module(input);
    let test = native_module();

    let mut vm = vm();
    vm.with(|mut r| {
        r.register(&test);

        let loaded_module = r.load(&module);

        // run each code snippet twice using the same VM,
        // ensuring it has the same result.
        let (snapshot1, failure1) = snapshot(input, &module, &mut r, &loaded_module);
        let (snapshot2, failure2) = snapshot(input, &module, &mut r, &loaded_module);

        assert_eq!(snapshot1, snapshot2);
        assert_eq!(failure1, failure2);

        #[cfg(not(miri))]
        {
            let snapshot = snapshot1;
            insta::assert_snapshot!(snapshot);
        }

        #[cfg(miri)]
        {
            // on miri all we care about is if there's any undefined behavior.
            eprintln!("{snapshot1}");
        }
    });
}

// It should be possible to run two or more different modules
// using the same VM.
#[test]
fn separate_modules() {
    let a = module(r#"fn f(a) { a } f(10)"#);
    let b = module(r#"100 + 200"#);

    vm().with(|mut r| {
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
