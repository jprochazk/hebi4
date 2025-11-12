use std::collections::HashSet;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Job {
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

    #[allow(dead_code)]
    pub fn cache_key(mut self, key: impl Into<String>) -> Self {
        self.cache_key = key.into();
        self
    }
}

pub fn job(command: impl Into<String>) -> Job {
    let command = command.into();
    let cache_key = command
        .split_ascii_whitespace()
        .next()
        .unwrap_or(command.as_str())
        .to_owned();

    Job {
        command,
        toolchain: Toolchain::Stable,
        components: vec![],
        install: vec![],
        cache_key,
    }
}

pub fn ci_jobs() -> Vec<Job> {
    vec![
        job("test").install("nextest"),
        // TODO: re-enable at some point
        // job("clippy").component("clippy"),
        job("fmt-check").nightly().component("rustfmt"),
        job("codegen-check").nightly().component("rustfmt"),
        job("deny").install("cargo-deny"),
        job("shear").install("cargo-shear"),
        job("check-tco"),
        job("miri").nightly().component("miri").install("nextest"),
    ]
}

fn validate(jobs: &[Job]) {
    let mut cache_keys = HashSet::new();
    for job in jobs {
        if !cache_keys.insert(job.cache_key.as_str()) {
            panic!(
                "duplicate cache key {}; set a unique key explicity for that job",
                job.cache_key
            );
        }
    }
}

pub fn output_matrix() {
    let jobs = ci_jobs();
    validate(&jobs);
    println!("{}", serde_json::to_string(&jobs).unwrap());
}
