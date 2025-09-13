use std::path::Path;

fn main() {
    rerun_if_changed_recursive(Path::new("tests/inputs"));
}

fn rerun_if_changed_recursive(dir: &Path) {
    assert!(dir.is_dir());

    println!("cargo::rerun-if-changed={}", dir.display());

    for entry in dir.read_dir().unwrap() {
        let entry = entry.unwrap();
        let meta = entry.metadata().unwrap();
        if meta.is_dir() {
            rerun_if_changed_recursive(&entry.path());
        }
    }
}
