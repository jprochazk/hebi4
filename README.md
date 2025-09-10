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

<!-- TODO: move astgen and asmgen into xtask -->

After changing anything, run

```
cargo codegen
```

To re-generate code.

## Unsafe code

This project uses unsafe code liberally. The reasoning for this is that it is _much_ harder
to claw back performance gains when every part of the code is full of small inefficiencies.
This kind of death by a thousand cuts is really difficult to come back from, often requiring
massive rewrites and complex refactors.

Safe code heavily relies on LLVM recognizing certain patterns and optimizing them appropriately.
Compilers are _really_ good nowadays! They do an excellent job optimizing code. But it can be
unreliable, and small changes can result in huge regressions in generated code.

Here I try to take matters into my own hands, in an attempt to give less confusing IR to LLVM
for optimization. A typical example is auto-vectorization and removal of bounds checks in code
using `Iterator` combinators. For VM code, I prefer numeric `for` loops and `get_unchecked`. 

That being said, care is taken to ensure soundness and most importantly _correctness_ of the
compiler and VM. Every corner of the implementation is tested. Every test must pass `miri`.
Unsafe code is paired with SAFETY comments as appropriate. It is not currently enforced, as
implementation details are heavily in flux. Only _safe code_ may be exposed to users.

The code contains no _lies_ about unsafe parts being safe, even in the compiler and VM internals.
Unsafe code is not wrapped in safe functions to make the code look prettier or simpler, as that
only obscures the reality of what the code is doing.

Certain kinds of unsafe operations _are_ wrapped in unsafe functions when it can help prevent mistakes.
For example, creating intermediate references while working with raw pointers is a surefire way to
undefined behavior. In the VM, raw pointers are wrapepd in newtyped structs, which have methods
operating on `self`. These methods do the Right Thing on the inner pointers without creating
intermediate references, or only creating them in the smallest region of code possible.

While unsafe code is used liberally, it still used _with intent_, and not just to avoid "dealing
with the borrow checker". Safe code is preferred when it doesn't harm the goals of this project.
Contributions which only change unsafe code into safe code are likely to be rejected, unless they
are accompanied by benchmarks showing why it doesn't result in lower performance and less reliable
optimizer behavior.

### VM

The VM is pretty much entirely unsafe code. It relies on invariants from the compiler and semantics
of the language to maintain a high standard of quality for the generated assembly. We aim to avoid:

- panic branches, even if they never run
- calls to drop glue
- bounds checks
- stack spills of VM state
- etc.

and similar. There are definitely ways to reduce the surface area of unsafe code, but doing so
often requires introducing complex abstractions.

A concrete example of something the VM does is it assumes that register operands are
always in bounds. Constructing operands like `Reg` is actually _unsafe_ via the `new_unchecked`
constructor. This adds unsafe surface area to the compiler. The VM then relies on these `Reg`
operands being in bounds for the current call frame. There are other approaches to this problem,
but any of the other ones I've managed to come up with always come with _some_ downside, like a
practically unreachable panic path which just sits there, staring menacingly.

### Garbage collection

The language implemented here is garbage collected.

The GC implementation landscape in Rust is pretty barren. Which makes sense, not many people are working on
garbage-collected languages, and those who are typically just use `Rc`.

Reference counting schemes are not viable for us. VM values must remain trivially copyable, and must not
have `Drop` impls. _Especially_ not `drop`s which contain branches! A dynamically-typed `Value` with
an `Rc` in one of its variants must first type-check before it can decrement the reference count.

This naturally excludes not only `std::rc::Rc`, but also:
- the [`gc`](https://docs.rs/gc/latest/gc/) crate, where `Gc` pointers track the number of roots,
  similar to a reference count.
- the [`bacon_rajan_cc`](https://docs.rs/bacon_rajan_cc/latest/bacon_rajan_cc/) crate

among others.

The mutator must be able to yield to the collector at _any_ point, not just well-defined safe points.
This way, we avoid the problem of not being able to trigger a collection in nested `vm->rust->vm` calls,
long loops, and similar scenarios, unless every code path is configured to be able to yield at any point.
This means crates like [`gc-arena`](https://docs.rs/gc-arena/latest/gc_arena/), though very cool, are also
unsuitable.

It is possible to implement certain kinds of garbage collectors entirely within safe code
(see [safe-gc](https://docs.rs/safe-gc/latest/safe_gc/)), but we don't want to accept the overheads associated
with a fully safe implementation.

#### GC implementation

The approach used by the VM is not novel at all. It is inspired by:

- The rooting API from [SpiderMonkey](https://spidermonkey.dev/)
- The _gatekeeper_ `Heap` type from [safe-gc](https://docs.rs/safe-gc/latest/safe_gc/)

To reduce implementation complexity, the GC is _not_ extensible by users. This means it is impossible for
users to add a type into the GC's type hierarchy of traceable objects. Instead:
- User types are wrapped in a "GC box", which manages the object's lifetime similar to a regular Rust `Box`
- User types which refer to garbage collected objects must do so through a reference-counted handle type.
  - The reference counted handles are only used by the userland API, not by the VM internals or builtin functions.

It is likely possible to extend this API to be a generic one, and release it as a library. Some of the changes
required may be:

- Using a manually constructed VTable per object instead of a type tag.
  - This VTable would hold impls for trace, drop, etc., and be used by the GC instead of having it match on the type tag,
    and casting the type.
- Adding a way to safely and atomically initialize an entire object in the presence of allocations, which may trigger a collection.
  - An API similar to Rust for Linux Kernel's [`pin_init`](https://docs.rs/pin-init/latest/pin_init/) may be necessary here.
- Extending the `Ref` and `RefMut` types to userland, somehow.
  - This may require the nightly `arbitrary_self_types` feature.

The GC API outlined below supports precise, incremental, and generational garbage collection.
Currently, it is only precise, and stop-the-world.

Before being accessed, objects must be allocated in a `Heap`, and then _rooted_ on the stack. `StackRoot`s are linked into a
per-heap stack-allocated list. They are constructed in a macro which doesn't leak the root's identifier.
The stack root is then wrapped in a `Root`, which holds a pinned mutable reference to the `StackRoot`. As a result,
stack roots are pinned to the stack and cannot be moved or referred to directly.

A `Root` allows for safe access to the underlying object. It serves as proof that the object is rooted and is guaranteed to
survive the next collection cycle, which makes it safe to dereference. To actually access the object, one of two methods must be called:

- `as_ref`, which requires a `&Heap` reference
- `as_mut`, which requires a `&mut Heap` reference

This is where the "gatekeeper" `Heap` comes in. It acts as a kind of token, of which there is only a single one active
at a time in any given program. This token is passed around by reference, and must be used in order to _dereference_ roots.

- If you have a shared reference to the heap, you can dereference as many `Root`s as you want to obtain shared access to them.
- If you have a _unique_ reference to the heap, you can dereference exactly one `Root` to obtain a unique mutable reference to it.

Additionally, to trigger a collection, you need a `&mut Heap`. This means collections may not happen while Rust `&T` or `&mut T` are
held on the stack - they must not exist across a call to `collect`, and the borrow checker enforces this for us.

The consequence is that Rust's guarantees for shared xor mutable access are never violated, and we can have `&mut T` to GC'd objects,
which means we don't need any kind of interior mutability scheme! But we may only have one mutable reference at a time.
That's actually a problem, which we have a workaround for:

- `as_ref` returns `Ref<'heap, T>` instead of `&'heap T`
- `as_mut` returns `RefMut<'heap, T>` instead of `&'heap mut T`

There is no other way to construct a `Ref` or `RefMut`. This guarantees that if you have a `Ref` or a `RefMut`, then the underlying
object is rooted. Any value stored in a rooted object becomes _transitively_ rooted, because it's reachabable through its rooted parent.

Methods which read the object are implemented on `Ref<'heap, T>`, and methods which mutate it are implemented on `RefMut<'heap, T>`.
We can do this because `Ref`, `RefMut`, and all the object types are declared within the same crate, so we aren't stopped by orphan rules.
This is one place where the GC not being generic is important.

And finally: Mutating methods which accept an object and store it within the receiver always accept `Root`s instead of `Ref`s.
Internally, they may unwrap the `Root` and store the raw `Gc<T>`. This is always safe, because the container is guaranteed to be rooted.

That's it! The resulting GC and object API is actually pretty nice, and it is _fully_ safe to use. Compiler-enforced safety is vital here.
Not only because GC bugs can be some of the worst heisenbugs in existence, and I wouldn't wish them upon anybody, but also because
the API is exposed as the embedding API as well. If it's exposed to users, it _must_ be safe code.

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
