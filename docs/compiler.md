## Compiler

We use a two-pass compiler:

- Source code -> AST
- AST -> Bytecode

## Parser

A relatively simple lexer and LL(1) recursive-descent parser.

The lexer is generated using [Logos](https://crates.io/crates/logos), with a small wrapper around it.
The parser is handwritten, using a few helper functions, and outputs an AST. The AST representation
is more interesting: see [ast.md](./ast.md).

## Code generation

This pass behaves like a tree-walking interpreter, mixing what would traditionally be separate passes:

- Variable resolution
- Constant folding
- Register allocation
- Dead code elimination

The resulting bytecode is fixed-width at 32 bits per instruction, and entirely register-based.
Some instructions emit an additional _data_ slot, which is used to store extra operands, such
as the inline cache for table operations.

### Variable resolution

Whenever we come across a declaration, we add it to the list of variables. There are no "globals",
so if a name can't be resolved to a symbol later down the line, we know it doesn't exist, and produce
an error message.

TODO: This is not entirely true, there are (will be) globals in REPL mode.

For functions to be able to recursively call each other, we do a pre-traversal of all statement lists,
where we collect all function declarations.

### Constant folding and register allocation

The central type for this is `Value`, which can hold a few different types, including a special
`dynamic` type, which holds a register.

All expression evaluation yields `Value`s. If an expression can't be evaluated immediately, the
code generator instead emits bytecode for evaluating that expression at runtime, into some register,
which it then passes back up to its caller as a `dynamic`.

If an evaluation _requires_ that a constant value exists in a register, then the value may be
_materialized_ into a register. For example, the expression `return 1` will initially produce a
constant value `1`, and the `return` expression will materialize it into an `lint r0, 1; ret`.

Expressions may also be _partially_ evaluated. For example, in `v == 0`, we can immediately evaluate
the `0`, and emit a type-specialized instruction which takes a numeric constant for its `rhs`.
That instruction then skips the type check for `rhs`.

Register allocation essentially just allocates fixed stack slots for instruction operands ahead of time.
There are at most 256 registers (`u8`), one of which (`r0`) is always used as the return value slot.

When a register is needed, the allocator increments the current register by 1, and returns it.
When a register is no longer needed, it is freed, and the allocator decrements the current register by 1.

Some care is taken to ensure register re-use as much as possible. For example, when assigning to a
variable, we want the destination operand to re-use the variable's register, instead of allocating a
temporary one, and issuing a `mov` to the variable later.

Function calls are a bit special. We don't want the VM to have to copy arguments to a new call frame,
instead, the arguments should already be in the right place at the time of the call. To do this,
the code generator allocates contiguous registers for the return slot and call arguments.

### Dead code elimination

NOTE: This is not implemented yet.

We keep track of basic block boundaries during compilation. A basic block entry is either the beginning
of a function, or a jump target. A basic block exit is any control flow instruction, such as `jmp`
or `ret`.

When inside a basic block, we don't do anything special. But when encountering a BB exit, we mark any
subsequent generated code as "unreachable". Unreachable code is never even emitted, though we still
traverse the AST as usual.

## Errors

Currently, the entire compilation pipeline exits immediately upon encountering the first error.
Errors may be rendered into an error message:

```text,ignore
invalid number of arguments, expected 3 but got 1
12 |  f(0)
   |   ^^^
```

