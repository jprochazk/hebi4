
### Development

Run tests:
```
cargo test
```

Update snapshot tests (requires [`cargo-insta`](https://crates.io/crates/cargo-insta)):
```
cargo review
```

New tests can be created by adding a `.hi` file to [`tests/inputs`](./tests/inputs).
These files are passed through all stages of the compiler: lexer, parser, bytecode emitter.

The file may begin with a comment to filter the stages it will be tested against:
- `tokenize` for [`src/token.rs`](./src/token.rs)
- `parse` for [`src/parser.rs`](./src/parser.rs)

Multiple different stages may be combined, e.g. `tokenize,parse` will run only through those
two stages, but not through the bytecode emit.

Try to keep tests minimal and to the point as much as possible.

AST can be modified by changing:
- [`nodes.ast`](./src/ast/nodes.ast) for AST nodes
- [`runtime.rs`](./astgen/src/runtime.rs) for non-generated code which is
  copied to the top of the generated file

then running:
```
cargo codegen
```

