# Introduction

Hebi is a work-in-progress dynamically-typed programming language, heavily inspired by other languages in the same category: Lua, Python, and JavaScript.

## Design Goals

- **Performance**: Hebi aims to be a fast dynamically-typed language with a focus on efficient bytecode compilation and VM execution
- **Simplicity**: The language design prioritizes simplicity and ease of use
- **Memory Efficiency**: The implementation uses novel techniques to minimize memory overhead (see [AST Representation](./ast.md))

## Project Status

Hebi is currently in active development. The core language features are being implemented, including:

- Lexer and parser
- AST representation with optimized memory layout
- Bytecode compiler with register allocation
- Stack-based virtual machine
- Garbage collection

## Repository

The source code is available at [github.com/jprochazk/hebi4](https://github.com/jprochazk/hebi4).
