## Compiler

The implementation uses a two-pass compiler:

- Parser
- Code generator

## Parser

A relatively simple lexer and LL(1) recursive-descent parser.
The lexer is generated using [Logos](https://crates.io/crates/logos), with a small wrapper around it.
The parser is handwritten, using a few helper functions, and outputs an AST.

## AST

See [ast.md](./ast.md).

## Errors

Currently, the entire compilation pipeline exits immediately upon encountering the first error.
Errors may be rendered into an error message:

```text,ignore
invalid number of arguments, expected 3 but got 1
12 |  f(0)
   |   ^^^
```

