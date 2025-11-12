use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Job {
    pub name: String,
    pub command: String,
    pub toolchain: Toolchain,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub components: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub install: Vec<String>,
    pub cache_key: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Toolchain {
    Stable,
    Nightly,
}

impl Job {
    pub fn new(name: impl Into<String>, command: impl Into<String>) -> Self {
        let name = name.into();
        let command = command.into();
        let cache_key = command.clone();

        Self {
            name,
            command,
            toolchain: Toolchain::Stable,
            components: vec![],
            install: vec![],
            cache_key,
        }
    }

    pub fn nightly(mut self) -> Self {
        self.toolchain = Toolchain::Nightly;
        self
    }

    pub fn component(mut self, component: impl Into<String>) -> Self {
        self.components.push(component.into());
        self
    }

    pub fn install(mut self, tool: impl Into<String>) -> Self {
        self.install.push(tool.into());
        self
    }

    pub fn cache_key(mut self, key: impl Into<String>) -> Self {
        self.cache_key = key.into();
        self
    }
}

pub fn ci_jobs() -> Vec<Job> {
    vec![
        Job::new("Test Suite", "test").install("nextest"),
        Job::new("Miri", "miri").nightly().component("miri"),
        // TODO: re-enable at some point
        // Job::new("Clippy", "clippy").component("clippy"),
        Job::new("Rustfmt", "fmt-check")
            .nightly()
            .component("rustfmt")
            .cache_key("fmt"),
        Job::new("Codegen Check", "codegen-check").cache_key("codegen"),
        Job::new("Cargo Deny", "deny").install("cargo-deny"),
    ]
}

#[derive(Serialize)]
struct MatrixOutput {
    include: Vec<Job>,
}

pub fn output_matrix() {
    let matrix = MatrixOutput { include: ci_jobs() };
    println!("{}", serde_json::to_string(&matrix).unwrap());
}
