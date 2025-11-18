use std::io::{self, Write};

use hebi4::prelude::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

/// A writer that calls into JavaScript for each write
struct JsWriter {
    callback: js_sys::Function,
}

impl JsWriter {
    fn new(callback: js_sys::Function) -> Self {
        Self { callback }
    }
}

impl Write for JsWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let s =
            std::str::from_utf8(buf).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        let js_string = JsValue::from_str(s);
        self.callback
            .call1(&JsValue::NULL, &js_string)
            .map_err(|e| {
                io::Error::new(io::ErrorKind::Other, format!("JS callback error: {:?}", e))
            })?;

        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[wasm_bindgen]
pub fn get_version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[wasm_bindgen]
pub struct ParseResult {
    success: bool,
    output: String,
}

#[wasm_bindgen]
impl ParseResult {
    #[wasm_bindgen(getter)]
    pub fn success(&self) -> bool {
        self.success
    }

    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.output.clone()
    }
}

#[wasm_bindgen]
pub fn parse(code: &str) -> ParseResult {
    match hebi4::parse(code) {
        Ok(ast) => {
            let mut output = String::new();
            for (i, node) in ast.root().body().into_iter().enumerate() {
                if i > 0 {
                    output.push_str("\n\n");
                }
                output.push_str(&format!("{:#?}", node));
            }

            if output.is_empty() {
                output = "(empty program)".to_string();
            }

            ParseResult {
                success: true,
                output,
            }
        }
        Err(err) => ParseResult {
            success: false,
            output: err.render(code).to_string(),
        },
    }
}

#[wasm_bindgen]
pub struct DisasmResult {
    success: bool,
    output: String,
}

#[wasm_bindgen]
impl DisasmResult {
    #[wasm_bindgen(getter)]
    pub fn success(&self) -> bool {
        self.success
    }

    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.output.clone()
    }
}

#[wasm_bindgen]
pub fn disassemble(code: &str, enable_dce: bool) -> DisasmResult {
    let opts = if enable_dce {
        EmitOptions {
            dead_code_elimination: true,
        }
    } else {
        EmitOptions::default()
    };

    match Module::compile_with(None, code, opts) {
        Ok(module) => DisasmResult {
            success: true,
            output: format!("{}", module.disasm(code)),
        },
        Err(err) => DisasmResult {
            success: false,
            output: err.render(code).to_string(),
        },
    }
}

#[wasm_bindgen]
pub struct RunResult {
    success: bool,
    output: String,
}

#[wasm_bindgen]
impl RunResult {
    #[wasm_bindgen(getter)]
    pub fn success(&self) -> bool {
        self.success
    }

    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.output.clone()
    }
}

#[wasm_bindgen]
pub fn run(code: &str, enable_dce: bool, output_callback: js_sys::Function) -> RunResult {
    let opts = if enable_dce {
        EmitOptions {
            dead_code_elimination: true,
        }
    } else {
        EmitOptions::default()
    };

    match Module::compile_with(None, code, opts) {
        Ok(module) => {
            let mut result = RunResult {
                success: false,
                output: String::new(),
            };

            // Create stdio with JS callback
            let stdio = Stdio {
                stdout: Box::new(JsWriter::new(output_callback.clone())),
                stderr: Box::new(JsWriter::new(output_callback)),
            };

            let mut vm = Hebi::new().with_stdio(stdio);
            let mut vm = vm.enter();
            let loaded_module = vm.load(&module);
            match vm.run(&loaded_module) {
                Ok(v) => {
                    result.success = true;
                    result.output = format!("{:?}", unsafe { v.as_ref() });
                }
                Err(err) => {
                    result.success = false;
                    result.output = err.render(code).to_string();
                }
            }

            result
        }
        Err(err) => RunResult {
            success: false,
            output: err.render(code).to_string(),
        },
    }
}
