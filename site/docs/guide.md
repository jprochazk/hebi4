# Language guide

Hebi's syntax is inspired by, and borrows from:

- Rust
- JavaScript
- Lua
- Python
- Gleam

It's in the same family of _bracketed_ programming languages as Rust (the [BCPL](https://en.wikipedia.org/wiki/BCPL) family)
with some [ML](https://en.wikipedia.org/wiki/Standard_ML) influences, such as [_implicit returns_](#implicit-returns).

> Note: Hebi's implementation is still _heavily_ in flux, and syntax may still change at any point!

```js
// disclaimer: this module doesn't actually exist :)
import {service, response, header, body} from "std/http/server"

fn fib(n) {
  if n < 2 { n }
  else { fib(n-1) + fib(n-2) }
}

service({
  "/fib/:num": fn(req) {
    let num = parse_int(req.params.num)
    if num > 40 {
      return response(400)
    }

    let result = fib(num)

    response(200)
      |> header("Content-Type", "text/plain")
      |> body(to_str(result))
  },
})
```

## Comments

It's often useful and important to annotate your source code with some extra information
for yourself and other people. The only form of a comment you can write in Hebi is:

```rust
// A line comment.
```

There are no block comments or special doc comments.

## Statements

The base unit of syntax in Hebi is a _statement_. Every program consists of statements.
Each statement does something, and produces no value.

### Variables

```rust
let v = nil
```

Variables must have both a name and a value. `let` without value is a syntax error:

```
$ hebi4 -e 'let v'

expected '=', found '<eof>'
1 |  let v
  |       ^
```

All variables in Hebi are mutable, meaning you can change their value by _assigning_ to them:

```js
let v = nil
print(v) // prints "nil"
v = 10
print(v) // prints "10"
```

There are two kinds of variables:

- Module variables (module-scoped globals)
- Local variables (block-scoped)

Variables scoped to a module can be accessed by other modules.
They are also shared by all functions in the same module.

Block-scoped variables are only visible to other statements in the same scope.
They can also be _captured_ by functions, which is discussed in the next section.

### Functions

```rust
fn f() {

}
fn f(a, b, c) {}
```

Functions may also be nested.

```rust
fn outer() {
  fn inner() {}
}
```

All functions are also _closures_. Functions in the module scope close over
the entire module, while functions in inner blocks close over only local
variables and functions visible to them.

```rust
fn f() {
  print(v)
}

let v = "hi"
```

You can of course _call_ functions, if they're in scope:

```rust
f() // prints "hi"
```

```rust
fn is_even(n) {
  if n == 1 { false }
  else { is_odd(n-1) }
}

fn is_odd(n) {
  if n == 1 { true }
  else { is_even(n-1) }
}

print(is_even(10)) // prints "true"
```

When a _local variable_ is _captured_, its value is copied into the function's closure environment.
This is different from how closures work in JavaScript, Lua, and Python.

```rust
fn counter() {
  let v = 0

  return {
    get: fn() { v }
    set: fn(n) { v = n }
  }
}

let c = counter()

print(c.get()) // 0
c.set(10)
print(c.get()) // still 0
```

If you want a shared mutable closure environment, you can wrap your data in a [table](#tables):

```rust
fn counter() {
  let s = { v: 0 }

  return {
    get: fn() { s.v }
    set: fn(n) { s.v = n }
  }
}

let c = counter()

print(c.get()) // 0
c.set(10)
print(c.get()) // 10
```

### Imports

Modules may import other modules.

```js
import {a, b, c} from "foo"
```

They are evaluated dynamically, wherever the import appears.
To import something lazily, you can wrap it in a function:

```js
fn f() {
  import {a, b, c} from "foo"

  foo()
}
```

You can either import parts of a module, or the entire module:

```js
import {a,b,c} from "foo" // import only parts of the module
import {a,b,c} from foo // same thing, but the module specifier is an identifier

import foo from "foo" // import entire module
import foo from foo // same as above
import foo // shorthand for `foo from foo`
```

## Expressions

Statements operate on _values_, which are produced by _expressions_.

### Literals

Literals (also known as constants) are values which are taken _literally_ from the source code.

#### Nil

The "nothing" value:
```lua
v = nil
```

#### Booleans

A binary _true_ or _false_ value.

```js
v = true
v = false
```

Used in [control flow](#control-flow).

#### Numbers

There are two kinds of numbers:

- _Floating point_ numbers, and
- _Integers_

They are distinct types, but are generally interchangeable:

```js
let int = 2
let float = 3.14

print(int * float)
```

If you want to convert between them explicitly, you can use the built-in `to_int` and `to_float`.

Calling `to_int` on a floating point has the same effect as _rounding down_ the float.

#### Strings

A string is a sequence of characters. Strings in Hebi use UTF-8 encoding. They are also immutable.

```js
let v = "hi!"
```

Strings also support _escape sequences_, which have special meaning:

|      | meaning |
|------|---------|
| `\a` | alert   |
| `\b` | non-destructive backspace |
| `\v` | vertical tab |
| `\f` | form feed |
| `\n` | new line |
| `\r` | carriage return |
| `\t` | horizontal tab |
| `\'` | single quote |
| `\"` | double quote |
| `\\` | backslash |
| `\e` | escape |
| `\x` | hex code |
| `\u` | unicode byte sequence |

`\x` and `\u` can be used to directly embed _byte sequences_ in strings:

- `\x0A` is equivalent to `\n`
- `\u{1F602}` is an emoji (😂)

Note that they must still result in valid utf-8.

Many of these escapes are pretty archaic and aren't really useful anymore.
An example of that is `\a` (alert), which was used to literally produce some kind of sound when
the computer detected it in standard output.

Use these wisely!

### Containers

Hebi has two built-in container types: arrays and tables.

#### Array

An array is a dynamically-sized sequence of values.

```js
let v = [0,1,2]
```

You can access the values in an array using an _index_:

```js
v[0] + v[1]
```

Only _integers_ are valid indices.

#### Tables

A table is a sequence of _key-value pairs_. They are sometimes called _hash maps_ or _associative arrays_.

```js
let v = {a:0, b:1, c:2}
```

Tables are indexed by _strings_.

```js
print(v["a"]) // 0
```

Other types must be explicitly converted to strings before being used as keys:

```js
let k = to_str(0)
v[k] = "hi"!

print(v[k])
```

You can also use the _field_ syntax to index tables:

```js
print(v.a) // 0
```

### Blocks

```js
let v = do { 1 + 1 }
```

You can use these to limit the scope of things. The final expression in the block is its value.
This is known as an [implicit return](#implicit-returns).

### Implicit returns

In Hebi, the final expression in any block of code is its return value, just like in Rust.

```rust
fn foo() {
  "hi"
}

print(foo()) // prints "hi"
```

```rust
print(do { "hi" }) // prints "hi"
```

### Control flow

Hebi is a Turing-complete language, offering loops and conditional expressions:

```rust
let i = 0
loop {
  print(i)
  if i == 10 {
    print("end")
    break
  }
  i += 1
}
```

`if` expressions also support [implicit returns](#implicit-returns):

```rust
fn test(number) {
  if number > 10 {
    "That's more than I can count."
  } else {
    to_str(number)
  }
}

print(test(1)) // prints "1"
print(test(100)) // prints "That's more than I can count."
```

## That's it!

As you can tell, some stuff is missing. Hebi is quite minimal right now, and while it will
stay minimal, it shouldn't stay _this_ minimal.

For example, we definitely want more kinds of loops:

- `for i in 0..n`
- `for v in array`
- `while i < 10`

We'd also like _pipeline_ syntax:

```js
let number = 10

print(number |> double())
```
