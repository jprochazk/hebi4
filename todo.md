### Spans

Given a `Packed` node, need to retrieve its span in O(1)

They are retrieved infrequently, only when producing errors.
Append should ideally be O(1) and not require any wasted space,
each span is already 8 bytes.

Idea:

- Store `Vec<Span>`
- Push `Span` into it during `insert_packed`/`insert_packed_contiguous`
- In `Visitor`, each `visit_*` fn also gets a `SpanIndex`, which is just
  a rebranding of `NodeIndex`.
  - Before the visitor calls `visit_*`, it has access to the packed node,
    which stores the index of the first child. Given the index of the first
    child, we can retrieve the index of any child. Those indices can then
    be used to retrieve the span.

The problem with the above is that given just a `node::NodeType`, you can't
retrieve its span. You must have its _parent node_ to retrieve its span.
This seems like a severe limitation.

The alternative would be to somehow hash `Packed`, but not all `Packed` are
unique!
Can we make them unique? Yes, but that would cost us. Some `Packed` store
inline values.
These inline values may be unique (such as for `Ident` and `Str`,
where they represent an ID into an intern table), but others may not (such as
`Int`, which stores its integer value inline). That integer is not unique, you
can have the same integer in multiple different places across the source text.

### Attempt #1

I tried to store the first child index in each unpacked node type.
This didn't really work, because the node does NOT yet have its children packed
in the AST at the time it's constructed. Those child nodes have not yet been
assigned indices, so there is no way to retrieve their spans, either.

### Findings

Spans must survive with each node until the node is packed into the AST,
which seems doable: return `Result<Spanned<Node>>` when parsing.

```rust
let node = p.open();
let name = parse_ident(p, buf)?;
// etc...
Ok(p.close(node, node::Something::new(name)));
```
In the code above `name` will be `Spanned<node::Ident>`. When the node is closed,
its span will be inserted into the `spans` array in the AST. The parent node will
be wrapped in its own span that we also finalize inside `close`.

When constructing errors in the parser, we can always reference the span of some
node thanks to the `Spanned` wrapper.

In `declare_node!`, we can generate getters for spans of child nodes.
Each component has a unique name and a statically known offset,
so we generate `node.<component>_span(&Ast)`.
For tail, we can either generate `node.<tail>_spanned(&Ast)`, or add a getter
which yields a span given an index into the tail, e.g. `node.<tail>_span(usize, &Ast)`.
Users can call `node.<tail>()` which yields an iterator, then `.enumerate()` it and
pass the index into `<tail>_span` to retrieve that.

Note that all this extra effort is motivated by not wanting to fetch the spans
everywhere all the time. They should only be fetched if we're constructing an error
message.

The unpacked node must store the index of its children. This index would not have
any meaning unless the node has been packed into the AST first. That means we need
to statically ensure the node is packed into the AST before its children may be
accessed in any way.

One way to accomplish this is to separate the structures into "before packing" and
"after packing". The former would implement `pack`, which would transform it into
the latter. We don't actually need to be able to carry around the structure
"before packing", they are only ever produced as an argument to `p.pack`.

Instead of having two different `struct`s for this, we can have a `New` wrapper,
which can only have `pack` called on it. That will be passed to `p.close`, which
will `pack` it and produce the packed node, wrapping it in a span.

This is a bit overkill, but it is technically public API, so we have to be careful.

### more thoughts

13:24 mosscode: ok i think that will work
13:24 mosscode: spans can be retrieved when you have access to the parent node
13:25 mosscode: here we have a `Stmt`, which will be unpacked into some specific stmt type
13:25 mosscode: e.g. var
13:25 mosscode: `Var` will internally store the index of its first child, and because all child node offsets are statically known, we can calculate the rest of the indices from that
13:26 mosscode: so to retrieve the span of `value`, we'll do `var.value_span(ast)`
13:27 mosscode: for something like `Loop` which is a variable-arity node (dynamically sized body), it needs a slightly different handling
13:27 mosscode: we still know where the child node array begins, so all we need to do is add the current index to it
13:28 mosscode: that again gives us the NodeIndex for each child node, which can be used to retrieve its span
13:28 mosscode: it's... a bit annoying, but i think this is enough to make everything work
13:28 mosscode: specifically, you can't access the span of a random node. but when traversing the AST, you _always_ have access to the parent node, so it should be fine
13:30 mosscode: we're not reading any spans unless we need to, which is the ultimate goal
13:31 mosscode: in theory we don't need full spans, we could re-parse from the start of a given node to find its end like we do for tokens with lexemes which are only known at runtime, but that's an unbounded amount of work, so the tradeoff is likely not worth it, and we should just store the full 8 byte (start,end) spans
