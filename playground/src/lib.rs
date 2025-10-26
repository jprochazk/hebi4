use hebi4::{EmitOptions, Hebi, Module};
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
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
pub fn run(code: &str, enable_dce: bool) -> RunResult {
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

            Hebi::new().with(|mut vm| {
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
            });

            result
        }
        Err(err) => RunResult {
            success: false,
            output: err.render(code).to_string(),
        },
    }
}
