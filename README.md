Work-in-progress programming language.

Heavily inspired by other languages in the same category: Lua, Python, and JavaScript.

## Development

Working in this repository only requires a recent version of the Rust compiler and Cargo.

### Testing

Run tests:
```
cargo test
```

Update snapshot tests (requires [`cargo-insta`](https://crates.io/crates/cargo-insta)):
```
cargo review
```

New tests can be created by adding a `.hi` file to [`tests/inputs`](./tests/inputs).

Files in `tests/inputs` are passed through the tokenize and parse stages.

They may begin with a comment (`##`) to filter the stages it will be tested against:
- `tokenize` for [`src/token.rs`](./src/token.rs)
- `parse` for [`src/parser.rs`](./src/parser.rs)

This can also be used to completely disable test files. Adding an empty comment to the top of
the file will cause it to not be tested against _any_ stage.

Multiple different stages may be combined, e.g. `tokenize,parse` will run only through those
two stages, but not through the bytecode emit.

Files in `tests/inputs/codegen` are passed through codegen. These tests always fail if they
fail to parse. The resulting bytecode is disassembled for the snapshot.

Files in `tests/inputs/run` are passed through the runtime, end-to-end.

Try to keep tests minimal and to the point as much as possible. Separate tests should be written
for each stage where the behavior may have changed. If a change in codegen may result in different
runtime behavior, it should have both a `codegen` test and a `run` test.

### Codegen

The implementation heavily relies on code generated from custom DSLs:
- AST is generated from [`nodes.ast`](./src/ast/nodes.ast), using [`astgen`](./astgen)
- Instruction set is generated from [`opcodes.s`](./src/codegen/opcodes.s), using [`asmgen`](./asmgen)

Both code generators have a `runtime.rs` file, which is the place for any helpers and types used by the
generated code. It is concatenated together with the generated code to produce the final result.

<!-- TODO: move most of `runtime` out of astgen/asmgen (why is it there again?)
<!-- TODO: move astgen and asmgen into xtask -->

After changing anything, run

```
cargo codegen
```

To re-generate code.

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
