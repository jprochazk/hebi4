## Unsafe code

This project uses a lot of unsafe code.

Care is taken to ensure soundness and most importantly _correctness_ of the compiler and VM.
Every corner of the implementation is tested. Every test must pass `miri`.
Unsafe code is paired with SAFETY comments as appropriate.
Code exposed to users must not be able to cause UB unless it is marked `unsafe`.

The ultimate goal here is to match or exceed the performance of programming languages in the
same category. That's difficult to do: Most of those languages are written in C, or C++,
which are inherently unsafe languages, which rely on a lot of UB for their performance
characteristics.

To give a concrete example, the "traditional" way to write a bytecode interpreter in Rust is
to use a loop with a large `match` statement in it:

```rust
fn interpreter(bytecode: &[Instruction], stack: &mut [Value]) {
  let pc = 0;
  loop {
    match bytecode[pc] {
      Instruction::Nop => {}
      Instruction::Mov { src, dst } => {
        stack[dst] = stack[src];
      }
      Instruction::Jmp { offset } => {
        pc += offset;
        continue;
      }
    }
    pc += 1;
  }
}
```

This is perfectly functional, and it's fully safe. Unfortunately, it's also hopelessly slow:

- Stack accesses require bounds checks. Our bytecode compiler knows how large `stack`
  must be at the time it generates bytecode. So why are we doing any bounds checks?
- Reading an instruction also requires a bounds check.

The worst part is that the `loop+match` pattern (probably) won't be recognized by LLVM, and
optimized to "threaded" dispatch, where each instruction handler directly jumps to the next one.
Instead, they'll jump to the start of the loop, and _then_ jump to the next instruction handler.

In C, you can do something like this:

```c
void interpreter(Instruction* bytecode, Value* stack) {
  static const void* jump_table[] = {&&nop, &&mov, &&jmp, &&halt};

  Instruction insn = *bytecode;
  goto *jump_table[opcode(insn)];

  nop: {
    insn = *bytecode++;
    goto *jump_table[opcode(insn)];
  };

  mov: {
    Reg src = operand_a8(insn);
    Reg dst = operand_b8(insn);
    stack[dst] = stack[src];

    insn = *bytecode++;
    goto *jump_table[opcode(insn)];
  };

  jmp: {
    Offset offset = operand_a24(insn);

    bytecode += offset;
    insn = *bytecode;
    goto *jump_table[opcode(insn)];
  };

  halt: {
    return;
  };
}
```

There are a few ways in which this is faster:

1. Each instruction handler directly jumps to the next one using a _computed goto_.
2. There are no bounds checks, not when reading an instruction nor when accessing the stack.

Now, the problem with writing all your code in C is that all your code is in C! I like Rust. It has
a lot of nice features, and most of the compiler _can_ use safe code. Even a lot of the VM implementation
can use safe code. It feels bad to have to give that up, just so that the instruction dispatch method
used can be "optimal".

We can mostly have our cake and eat it too, using tail calls.

```rust
enum Control {
  Stop,
  Error,
}

const JUMP_TABLE: [fn(Ip, Sp, Instruction) -> Control; 256] = [
  nop,
  mov,
  jmp,
  halt,
  // ...
];

fn dispatch_current(ip: Ip, sp: Sp) -> Control {
  let insn = ip.read();
  JUMP_TABLE[opcode(insn)](ip, sp)
}

fn dispatch_next(ip: Ip, sp: Sp) -> Control {
  let ip = ip.next();
  dispatch_current(ip, sp)
}

fn nop(ip: Ip, sp: Sp, insn: Instruction) -> Control {
  dispatch_next(ip, sp)
}

fn mov(ip: Ip, sp: Sp, insn: Instruction) -> Control {
  let src = operand_a8(insn);
  let dst = operand_b8(insn);

  let tmp = sp.read(src);
  sp.write(dst, tmp);

  dispatch_next(ip, sp)
}

fn jmp(ip: Ip, sp: Sp, insn: Instruction) -> Control {
  let offset = operand_a24(insn);

  let ip = ip.add(offset);

  dispatch_current(ip, sp)
}

fn halt(ip: Ip, sp: Sp, insn: Instruction) -> Control {
  Control::Stop
}
```

The above code optimizes to something _very_ close to computed goto. Every instruction handler tail-calls
the next one. The downside here is that each function is still a full function, prelude and all. It needs to
save/restore volatile registers, and sometimes allocate stack space for additional intermediates.

With this, we still can't hope to match the performance of an interpeter in hand-written assembly,
but we can get pretty close to or even beat one which is written in C!

### VM

In release mode, the VM is written to take advantage of tail call optimization.
It falls back to loop+match in debug mode.

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

Another thing is that the _instruction pointer_ is just a pointer. We don't carry around its
length, and we don't perform a bounds check when reading from it. This means we assume that the compiler
will:

- Properly terminate all branches (with a `ret` or `stop` instruction)
- Generate all `jmp` instruction with in-bounds targets

Once again, big assumptions! But we need to make those assumptions in order to reach our performance targets.

### Garbage collection

The language implemented here is garbage collected.

It is possible to implement certain kinds of garbage collectors entirely within safe code
(see [safe-gc](https://docs.rs/safe-gc/latest/safe_gc/)), but we don't want to accept the overheads associated
with a fully safe implementation.

The approach used by the VM is not novel at all. It is inspired by:

- The rooting API from [SpiderMonkey](https://spidermonkey.dev/)
- [safe-gc](https://docs.rs/safe-gc/latest/safe_gc/)

The GC API outlined below supports precise, incremental, and generational garbage collection.
Currently, it is only precise, and stop-the-world.

Before being accessed, objects must be allocated in a `Heap`, and then _rooted_ on the stack. `StackRoot`s are linked into a
per-heap stack-allocated list. They are constructed in a macro which doesn't leak the root's identifier.
The stack root is then wrapped in a `GcRoot`, which holds a pinned mutable reference to the `StackRoot`. As a result,
stack roots are pinned to the stack and cannot be moved or referred to directly.

A `GcRoot` allows for safe access to the underlying object. It serves as proof that the object is rooted and is guaranteed to
survive the next collection cycle, which makes it safe to dereference. To actually access the object, one of two methods must be called:

- `as_ref`, which requires a `&Heap` reference
- `as_mut`, which requires a `&mut Heap` reference

This is where the `Heap` comes in. It acts as a kind of token, of which there is only a single one active
at a time in any given thread. This token is passed around by reference, and must be used in order to _dereference_ roots.

- If you have a shared reference to the heap, you can dereference as many `GcRoot`s as you want to obtain shared access to them.
- If you have a _unique_ reference to the heap, you can dereference exactly one `GcRoot` to obtain a unique mutable reference to it.

Additionally, to trigger a collection, you need a `&mut Heap`. This means collections may not happen while Rust `&T` or `&mut T` are
held on the stack - they must not exist across a call to `collect`, and the borrow checker enforces this for us.

The consequence is that Rust's guarantees for shared xor mutable access are never violated, and we can have `&mut T` to GC'd objects,
which means we don't need any kind of interior mutability scheme! But we may only have one mutable reference at a time.
That's actually a problem, which we have a workaround for:

- `as_ref` returns `GcRef<'heap, T>` instead of `&'heap T`
- `as_mut` returns `GcRefMut<'heap, T>` instead of `&'heap mut T`

There is no other way to construct a `GcRef` or `GcRefMut`. This guarantees that if you have a `GcRef` or a `GcRefMut`, then the underlying
object is rooted. Any value stored in a rooted object becomes _transitively_ rooted, because it's reachabable through its rooted parent.

Methods which read the object are implemented on `GcRef<'heap, T>`, and methods which mutate it are implemented on `GcRefMut<'heap, T>`.
We can do this because `GcRef`, `GcRefMut`, and all the object types are declared within the same crate, so we aren't stopped by orphan rules.
This is one place where the GC not being generic is important.

And finally: Mutating methods which accept an object and store it within the receiver always accept `GcRoot`s instead of `GcRef`s.
Internally, they may unwrap the `GcRoot` and store the raw `GcPtr<T>`. This is always safe, because the container is guaranteed to be rooted.

That's it! The resulting GC and object API is actually pretty nice, and it is _fully_ safe to use. Compiler-enforced safety is vital here.
Not only because GC bugs can be some of the worst heisenbugs in existence, and I wouldn't wish them upon anybody, but also because
the API is exposed as the embedding API as well. If it's exposed to users, it _must_ be safe code.
