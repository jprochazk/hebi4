
### Development

Run tests:
```
cargo test
```

Update snapshot tests (requires [`cargo-insta`](https://crates.io/crates/cargo-insta)):
```
cargo review
```

AST can be modified by changing:
- [`nodes.ast`](./src/ast/nodes.ast)
- [`runtime.rs`](./astgen/src/runtime.rs)

then running:
```
cargo codegen
```

