## AST

A traditional AST would use a "tree of pointers" representation.

For example, let's consider a simple binary expression:

```rust
1 + (1 * 2)
```

The AST for it would look like:

```rust
Binary {
    op: Add,
    lhs: Int(1),
    rhs: Binary {
        op: Mul,
        lhs: Int(1),
        rhs: Int(2)
    }
}
```

Using a basic tree structure like:
```rust
// size = 16 bytes, alignment = 8 bytes
enum Expr {
    Binary(Box<Binary>),
    Int(Box<Int>),
    // other nodes
}

// size = 40 bytes, alignment = 8 bytes
struct Binary {
    op: BinaryOp,
    lhs: Expr,
    rhs: Expr,
}

// size = 1 byte, alignment = 1 byte
enum BinaryOp { Add, Sub, Mul, Div }

// size = 8 bytes, alignment = 8 bytes
struct Int {
    value: u64,
}
```

The total memory footprint for our example AST would be:

```rust
Expr box Binary { // 16 bytes
    op: Add, // 8 bytes (due to alignment)
    lhs: Expr::Int(box), // 16 bytes + 8 bytes
    rhs: Expr::Int(box Binary { // 16 bytes
        op: Mul, // 8 bytes (due to alignment)
        lhs: Expr::Int(box Int(1)), // 16 bytes + 8 bytes
        rhs: Expr::Int(box Int(2)), // 16 bytes + 8 bytes
    }),
}

// 16*5 + 8*4 = 112 bytes!
```

### Flat layout

A common method to reduce the size of ASTs is to store nodes in an array, and use 32-bit indices
instead of "pointers". Some people call this a "flat" layout:

```rust
type ExprArray = Vec<Expr>;
struct ExprId(u32);

// size = 32 bytes, alignment = 8 bytes
enum Expr {
    Binary(Binary), // size = 12 bytes, alignment = 4 bytes
    Int(Int), // size = 8 bytes, alignment = 8 bytes
    // other nodes, one of which brings size to 32 bytes
}

// size = 12 bytes, alignment = 4 bytes
struct Binary {
    op: BinaryOp, // 4 bytes (rounded to 4)
    lhs: ExprId, // 4 bytes
    rhs: ExprId, // 4 bytes
}

// size = 1 byte, alignment = 1 byte
enum BinaryOp { Add, Sub, Mul, Div }

// size = 8 bytes, alignment = 8 bytes
struct Int {
    value: u64,
}
```

Note that `Expr` above would grow/shrink depending on what's stored inside it, so for the sake of
this argument let's imagine we add some nodes, and it is exactly 32 bytes.

The memory footprint of our AST is now:
```rust
[
  0: Expr::Int(1) // 32 bytes
  1: Expr::Int(1) // 32 bytes
  2: Expr::Int(2) // 32 bytes
  3: Expr::Binary(Mul, ExprId(1), ExprId(2)) // 32 bytes
  4: Expr::Binary(Add, ExprId(0), ExprId(3)) // 32 bytes
]

// 32*5 = 160 bytes
```

Looks like that made things worse... Storing all variants inline in the enum means the enum is as large
as its largest variant, which is not great. The alternative is to `Box` all variants, as was done with
the basic AST:

```rust 
// size = 16 bytes, alignment = 8 bytes
enum Expr {
    Binary(Box<Binary>), // size = 8 bytes, alignment = 8 bytes
    Int(Int), // size = 8 bytes, alignment = 8 bytes
    // other nodes
}
```

Let's avoid boxing the `Int` variant, since it's already small enough. The memory footprint is now:

The memory footprint of our AST is now:
```rust
[
  0: Expr::Int(1) // 16 bytes
  1: Expr::Int(1) // 16 bytes
  2: Expr::Int(2) // 16 bytes
  3: Expr::Binary(box {Mul, ExprId(1), ExprId(2)}) // 16 bytes + 12 bytes
  4: Expr::Binary(box {Add, ExprId(0), ExprId(3)}) // 16 bytes + 12 bytes
]

// 16*5 + 12*2 = 104 bytes
```

Okay, that's _slightly_ better. The downside is that now each variant takes up space in the array,
but also separately somewhere on the heap. We've destroyed memory locality at the cost of a lower
memory footprint. 

### Super-flat layout

A few things bother me about the above layout:

- For a `Binary` expression, we need to store two "pointers" of some kind to the two sub-nodes.
- If we want to reduce the size of `Expr`, we must wrap every large node in `Box`.

Surely we can do better than that. We can take advantage of the fact that our syntax tree nodes
have two distinct "shapes":

- A node with a fixed number of sub-nodes, and optionally some small inline data
- A node with no sub-nodes, and only some inline data

If we force the sub-nodes of a node to be contiguous, we only need to store the index of its
_first sub-node_. The index of the 2nd (and beyond) can be inferred by knowing their relative order,
e.g. for `Binary`, `lhs` comes before `rhs`, so `lhs == nodes[index+0]` and `rhs == nodes[index+1]`.

To store nodes with a dynamic number of sub-nodes, we'll also need a length field. We can combine the
two in three different ways:

- Only fixed number of sub-nodes
- Only dynamic number of sub-nodes
- A "prefix" of some fixed number of sub-nodes, and a "tail" of a dynamic number of sub-nodes,
  with the tail starting after the prefix.

For now, we'll only support `u32` indices to nodes, and `u24` lengths. To know what kind of node we have,
we'll still need a tag. There aren't many kinds of nodes, so a `u8` tag will be enough.

Under this rule set, almost all the structural information is _implicit_, and still have plenty of flexibility
to store any kind of syntax node, not just those with a fixed number of sub-nodes. As a result, each syntax node
is packed into just 64 bits.

A compiler needs more than just structural information. To store additional data, we'll use secondary storage,
and store indices in the packed nodes.

Spans are stored separately, with each node getting its own span. The index of a node is also its index into
the span array.

Here's the full in-memory layout of our syntax `Node` (in pseudo-Rust):

```rust
struct Node {
    tag: Tag,
    repr: Repr,
}

#[repr(u8)]
enum Tag {
    Int,
    Binary,
    // other nodes
}

union Repr {
    empty: { _padding: u56 },
    fixed: { next: u32, val: u24 },
    variable: { next: u32, len: u24 },
    mixed: { next: u32, tail_len: u24, },
    inline: { val: u56 },
}
```

Okay, now that we have our layout, what is the total memory footprint for `1 + (1 * 2)`?

```rust
[
  0: Node(Int, 1) // 8 bytes
  1: Node(Int, 2) // 8 bytes
  2: Node(Int, 1) // 8 bytes
  3: Node(Binary, NodeId(0), Mul) // 8 bytes
  4: Node(Binary, NodeId(2), Add) // 8 bytes
]

// 5*8 = 40 bytes
```

Wow! That made a _huge_ difference. The memory footprint of this "super-flat" AST is:
- 35% of a tree of pointers AST (112 bytes -> 40 bytes)
- 38% of a flat AST (104 bytes -> 40 bytes)

And that's not all of it. We've cut our memory usage in half, but we've also completely removed
all boxing, so the entire AST is in one contiguous buffer. This buffer can double its size every
time it needs to grow, reducing the number of allocations done during parsing to nearly zero
comparsed to a tree of pointers.

Finally, this kind of AST is _great_ for memory locality. Every node is in the same array, and
all related nodes are very close together, due to them being contiguous!

One major downside is that using this kind of AST introduces a _lot_ of complexity.

In Hebi, I've resorted to generating all the code required to construct and traverse the AST.
For the few nodes I have, it's generating nearly 3000 lines of code, which is a _massive_ increase
over the simple "tree of pointers" AST.

