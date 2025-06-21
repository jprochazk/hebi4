The AST is stored as a homogenous list of nodes, which we'll call the "flat AST".

There are two types of nodes, statements and expressions.
Both types of nodes are stored in one contiguous array.
We place constraints on the relative ordering of syntax nodes in this array.

Each syntax node is only present in the tree once, and has exactly one supernode.
A given node's subnodes are stored contiguously in the flat AST.
Subnodes are additionally ordered by their source-order,
so for example an `if` expression's `cond_expr` comes before its `then_expr`.

All these properties combined mean we only need two possible representations for nodes:
  - tag + index
  - tag + index + length

Given an index and a known number of subnodes, we can reconstruct the node from our flat AST.

Given the following integer size limits:
- `u32` index (~4 billion total nodes per kind)
- `u24` length (~16 million subnodes)
- `u8` tag (255 unique tags)

We can easily pack all this information into a single `u64`:

```rust
struct PackedNode {
    tag_and_length: u32,
    first_child_index: u32,
}
```

Many syntax nodes have a statically known number of subnodes, in which case we don't
actually need the `length`, so we leave it at 0.

Given the above `PackedNode` definition, we can retrieve subnodes as:
```rust
&ast[node.first_child_index..][..node.tag_and_length & LENGTH_MASK]
```

This however isn't enough, we've only addressed the _structure_ of the AST, but the AST contains
data like identifiers and strings. We store those in side-tables, with these values being interned.
Nodes can point into these side-tables by re-purposing their `first_child_index`.

For integers specifically, we store them inline in a `PackedNode`, spanning both `tag_and_length`
and `first_child_index`. The maximum value of an int is `u52`, so with just a tag, we have
`u56` left, which is more than enough.

### Simple example

```rust
struct Loop {
    body: Vec<Stmt>,
}
```

This is one of the simplest cases. A loop holds a list of statements to execute in each iteration.

Here's what that looks like in our flat AST:

```rust
struct Loop<'ast> {
    // `Stmt` is really just a `PackedNode` that restricts
    // the possible space of `Tag`s
    body: &'ast [Stmt<'ast>],
}

impl<'ast> Unpack for Loop<'ast> {
    fn unpack(ast: &Ast, node: PackedNode) -> Loop<'_> {
        assert_eq!(node.tag(), Tag::Loop);

        let body = ast.stmt_span(node.index()..node.index()+node.length());

        Loop { body }
    }
}
```

We can tie this together with the Visitor pattern, which will make traversing this kind of flat AST
convenient:

```rust
impl AstVisitor for BytecodeEmitter {
    fn visit_loop_stmt<'ast>(&mut self, visitor: &Visitor<'ast>, loop_: Loop<'ast>) {
        self.enter_loop(); // Allocates break/continue labels
        for stmt in &loop_.body {
            // Recurse into each statement.
            // The visitor will call our `visit_<node>` methods after unpacking `<node>`.
            visitor.visit(stmt, self);
        }
        self.leave_loop(); // Pops one pair of labels off the loop stack
    }

    // Every `visit_<node>` method has a default implementation that does nothing but visit any subnodes.
}
```

The visitor does the heavy lifting of recognizing nodes based on their tag, and reconstructing the node,
and dispatching the correct `visit_<node>` method.

### Advanced example

Something much more complex is a function. It has:
- A name
- A variable number of parameters
- A body block

This is a kind of mix between a statically-sized node, and a variably-sized one. In order to make this work,
we can enforce that:
- The name is an `IdentifierNode` which always comes 1st, and
- The body block is a `BlockExpr` which always comes 2nd,
- On top of which there are `N=node.length()` extra nodes which represent parameters,
  each of which being an `IdentifierNode`.

```rust
struct Function<'ast> {
    name: Ident<'ast>,
    body: BlockExpr<'ast>,
    params: &'ast [Ident<'ast>],
}

impl<'ast> Unpack for Function<'ast> {
    fn unpack(ast: &Ast, node: PackedNode) -> Function<'_> {
        assert_eq!(node.tag(), Tag::Function);

        let name = ast.ident(node.index());
        let body = ast.expr(node.index() + 1);
        let params = ast.ident_span(node.index()+2..node.index()+2+node.length());

        Function { name, params, body }
    }
}
```

This implies that a mixed-arity node can only have one variable-arity component. And that's true,
but it's not a problem, because you can always add an indirection to resolve it. That adds a bit of
overhead, but it's still not that bad.

### Why go through all the trouble?

It's a universally known fact that modern CPUs are fast, and memory access is slow.
CPUs attempt to mask memory latency with complex, heavily cached, pipelined execution of instructions.

By representing the AST this way, we're substantially reducing the memory required to store it. Reducing memory
usage is a worthy goal in itself, but what we're really after is reducing both the frequency of memmove instructions,
and the volume of data transferred between RAM and the CPU caches when traversing our AST.

- The data we're fetching has a consistent size, and is nicely aligned with CPU cache lines
- Each packed node fits within a single 64-bit register, instead of e.g. 3 registers for the 3 pointers required to
  represent an `if` with two branches
- We always fetch all subnodes of a given node at once, instead of dereferencing multiple pointers for
  each of the node's separate fields
- Nodes are contiguous in memory, and supernodes are placed very close to subnodes.
- Even a relatively deep subtree is practically guaranteed to be fetched all at once and likely held in L1 dcache,
  and _definitely_ held in L2 cache the entire time that we're traversing it
  - A 32 KiB L1 cache can fit 128 `PackedNode`s.
  - A 24 MiB L2 cache can fit a whopping _3 million_ `PackedNode`s.

- We're doing AST traversal, which means _many_ branches:
  - More relevant nodes being in cache implies lower latency to evaluate a branch condition.
    - If the CPU runs out of other independent (out-of-order) work to do while waiting for the data used
      in a branch condition, it will stall.

I could go on, but the benefits are numerous. CPUs love this kind of stuff!
