use std::{fmt::Write as _, fs::read_to_string, path::Path};

use super::Hebi;
use crate::{
    error::Result,
    module::{NativeModule, f},
    value::ValueRaw,
    vm::{self, Runtime, Stdio},
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
    result: Result<ValueRaw>,
    r: &mut Runtime<'vm>,
    module: &crate::module::Module,
    loaded_module: &vm::Module<'vm>,
) -> (String, bool) {
    match result {
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

fn native_modules() -> impl IntoIterator<Item = NativeModule> {
    use crate::prelude::*;

    [
        {
            fn foo(cx: Context<'_>) -> &'static str {
                "hello"
            }

            NativeModule::builder("test")
                .function(self::f!(foo))
                .finish()
        },
        {
            use std::sync::{Arc, Mutex};

            struct State {
                value: i64,
            }

            let state = Arc::new(Mutex::new(State { value: 0 }));

            NativeModule::builder("test2")
                .function(self::f!("reset", {
                    let state = state.clone();
                    move |cx: Context| {
                        let mut state = state.lock().expect("poisoned");
                        state.value = 0;
                    }
                }))
                .function(self::f!("inc", {
                    let state = state.clone();
                    move |cx: Context, n: i64| {
                        let mut state = state.lock().expect("poisoned");
                        state.value += n;
                    }
                }))
                .function(self::f!("dec", {
                    let state = state.clone();
                    move |cx: Context, n: i64| {
                        let mut state = state.lock().expect("poisoned");
                        state.value -= n;
                    }
                }))
                .function(self::f!("get", {
                    let state = state.clone();
                    move |cx: Context| {
                        let state = state.lock().expect("poisoned");
                        state.value
                    }
                }))
                .finish()
        },
        {
            #[derive(Debug)]
            struct Foo {
                value: i64,
            }

            extern_data!(Foo);

            fn foo_new<'a>(cx: Context<'a>, value: i64) -> HebiResult<Ret<'a>> {
                crate::prelude::let_root!(in &cx; buf);
                let buf = Extern::new(&cx, buf, Foo { value });

                cx.ret(buf)
            }

            fn foo_value(cx: Context, foo: Param<Extern>) -> HebiResult<i64> {
                let foo = foo.as_ref(&cx);
                let foo: &Foo = foo.cast_ref()?;
                Ok(foo.value)
            }

            NativeModule::builder("test3")
                .function(self::f!(foo_new))
                .function(self::f!(foo_value))
                .finish()
        },
    ]
}

fn vm() -> Hebi {
    Hebi::new().with_stdio(Stdio {
        stdout: Box::new(Vec::new()),
        stderr: Box::new(Vec::new()),
    })
}

struct Options {
    module_test: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self { module_test: false }
    }
}

fn parse_options(s: &str) -> Options {
    let mut o = Options::default();
    if s.contains("module") {
        o.module_test = true;
    }
    o
}

#[glob_test::glob("../../tests/inputs/run/*.hi")]
fn run(path: &Path) {
    let input = read_to_string(path).unwrap();
    let input = input.trim();
    let options = if input.starts_with("//") {
        let first_line = input.lines().next().unwrap().trim_start_matches("//");
        parse_options(first_line)
    } else {
        Default::default()
    };
    let input = if options.module_test {
        input
    } else {
        &format!("do {{\n{input}\n}}")
    };
    let module = module(input);
    let native_modules = native_modules();

    let mut vm = vm();
    let mut vm = vm.enter();
    for module in native_modules {
        vm.register(&module);
    }

    let loaded_module = vm.load(&module);

    // run each code snippet twice using the same VM,
    // ensuring it has the same result.
    let (snapshot1, failure1) = snapshot(
        input,
        vm.run(&loaded_module),
        &mut vm,
        &module,
        &loaded_module,
    );
    let (snapshot2, failure2) = snapshot(
        input,
        vm.run(&loaded_module),
        &mut vm,
        &module,
        &loaded_module,
    );

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
}

// It should be possible to run two or more different modules
// using the same VM.
#[test]
fn separate_modules() {
    let a = module(r#"fn f(a) { a } f(10)"#);
    let b = module(r#"100 + 200"#);

    let mut vm = vm();
    let mut vm = vm.enter();
    let a = vm.load(&a);
    let a = vm.run(&a).unwrap();
    let a = unsafe { a.as_ref() };
    let a = format!("{a:?}");
    let b = vm.load(&b);
    let b = vm.run(&b).unwrap();
    let b = unsafe { b.as_ref() };
    let b = format!("{b:?}");

    assert_eq!(a, "Int(10)");
    assert_eq!(b, "Int(300)");
}

#[test]
fn stack_unwinding() {
    let src = "panic()";
    let m = module(src);

    let mut vm = vm();
    let mut vm = vm.enter();
    let l = vm.load(&m);
    let (s0, f0) = snapshot(src, vm.run(&l), &mut vm, &m, &l);
    let (s1, f1) = snapshot(src, vm.run(&l), &mut vm, &m, &l);

    assert_eq!(s0, s1);
    assert_eq!(f0, f1);

    unsafe {
        assert!(vm.vm.as_mut().frames.is_empty());
    }
}

#[test]
fn nested_enter() {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut a = vm();
        let a = a.enter();
        let mut b = vm();
        let b = b.enter();
    }));

    assert!(result.is_err());
}

// NOTE: cannot run this under miri due to `mmap` call
#[cfg(not(miri))]
#[cfg(feature = "async")]
#[test]
fn async_run() {
    let src = r#"
import {sleep} from "eepy"
print("before")
sleep(1)
print("after")
"#;
    let m = module(src);

    let n = NativeModule::builder("eepy")
        .function(self::f!(async "sleep", async |n: i64| {
            smol::Timer::after(std::time::Duration::from_millis(n as u64)).await;
        }))
        .finish();

    let mut vm = vm();
    let mut vm = vm.enter();
    vm.register(&n);
    let l = vm.load(&m);

    let result = smol::block_on(async { vm.run_async(&l).await });

    let (s, _) = snapshot(src, result, &mut vm, &m, &l);

    #[cfg(not(miri))]
    {
        insta::assert_snapshot!(s);
    }
}
