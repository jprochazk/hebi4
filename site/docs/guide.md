# Syntax

Hebi's syntax is inspired by, and borrows from:

- Rust
- JavaScript
- Lua
- Python
- Gleam

It's in the same family of _bracketed_ programming languages as Rust (the [BCPL](https://en.wikipedia.org/wiki/BCPL) family)
with some [ML](https://en.wikipedia.org/wiki/Standard_ML) influences, such as [_implicit returns_](#implicit-returns).

```js
import service, response, header, body from "std/http/server"

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

The unit of syntax in Hebi is a _statement_. Every program consists of statements.
Each statement does something, and produces no value.

### Variables

```rust
let v = nil
```

Each variable must have both a name and a value. `let` without value is a syntax error:

```
$ hebi4 -e 'let v'

expected '=', found '<eof>'
1 |  let v
  |       ^
```

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

Any function can refer to variables outside of its scope, as long as those
variables are declared _before_ the function:

```rust
let v = "hi"
fn f() {
  print(v)
}

f() // prints "hi"
```

```rust
// f.hi
fn f() {
  print(v)
}
let v = "hi"
f()
```

```
$ hebi4 f.hi

could not resolve name
2 |    print(v)
  |          ^
```

There's a special exception for functions calling other functions. If you're
at the top-level of a module, then any function may call any other function,
and they'll be resolved correctly:

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

This is not the case for an inner scope:

```rust
// bad.hi
fn main() {
  fn is_even(n) {
    if n == 1 { false }
    else { is_odd(n-1) }
  }

  fn is_odd(n) {
    if n == 1 { true }
    else { is_even(n-1) }
  }

  print(is_even(10)) // prints "true"
}
```

```
could not resolve name
4 |      else { is_odd(n-1) }
  |             ^^^^^^
```

When a variable is _captured_, its value is copied into the function's
closure environment:

```rust
fn counter() {
  let v = 0
  return (fn() {
    let tmp = v
    v += 1
    v
  })
}

let c = counter()
print(c()) // 0
print(c()) // 1
```

Key word being _copy_ - the _variable_ isn't shared, unlike in languages
like Lua and JavaScript, only the _value_ is.

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

If you want shared mutable state, you can wrap your data in a [table](#tables):

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

...and again, there is an exception for top-level code.
Top-level functions _do_ share variables, not just values:

```rust
let v = 0

fn get() { v }
fn set(n) { v = n }

print(get()) // 0
set(10)
print(get()) // 10
```

### Imports

Modules may import other modules.

```js
import a, b, c from "foo"
import "bar" as bar
```

They are evaluated dynamically, wherever the import appears.
To import something lazily, you can wrap it in a function:

```js
fn f() {
  import foo from "bar"

  foo()
}
```

## Expressions

### Literals

#### Nil

#### Booleans

#### Numbers

#### Strings

#### Arrays

#### Tables

### Blocks

### Implicit returns

### Control flow

### Assignment

### Function calls
