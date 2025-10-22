
grammar syntax:
  `<term>` = `<definition>`

where `<term>` is an identifier consisting of ascii characters and underscores,
and `<definition>` is a _newline delimited_ list of possible definitions for the term.

there are some special pre-defined terms, which are fully uppercase:
  - EOF, denoting end of file
  - IDENT, denoting an identifier defined using the following regex:
       `[a-zA-Z_][a-zA-Z_0-9]*`
  - INTEGER, denoting an integer defined using the following regex:
       `[1-9](_[0-9]|[0-9])*`
    meaning a non-zero decimal digit, followed by any number of digits
    underscores may be used as visual separators, but they may not be present
    at the end
  - FLOAT, denoting a floating-point number, defined using the following regex:
       `[1-9][0-9]*\.[0-9]+(e[0-9]+)`
    specifically, the period and a number on both sides of it are not optional.
    floats do not allow underscores as visual separators
  - STRING, denoting a UTF8-encoded string, defined as:
       1. An opening double-quote
       2. A sequence of UTF8-encoded text, optionally containing special escapes
       3. A closing double-quote
    single quote strings are NOT allowed - the language only supports double quotes.
    the parser takes care to support unicode properly, so e.g. emojis may appear in strings.
    the list of allowed escapes is as follows:
       - `\a` => byte 0x07
       - `\b` => byte 0x08
       - `\v` => byte 0x0B
       - `\f` => byte 0x0C
       - `\n` => newline
       - `\r` => carriage return
       - `\t` => tab
       - `\\` => backslash
       - `\e` => byte 0x1B
       - `\E` => byte 0x1B
       - `\x` => 7-bit character code (exactly 2 digits, up to 0x7F), e.g. \x7F
       - `\u` => 24-bit Unicode character code (up to 6 digits), e.g. \u{7FFF}
       - `\"` => an escaped double quote character

symbols in quotes are tokens, and bare identifiers denote the presence of some other term.
terms may be recursive, either directly or indirectly.
a term may appear in a definition with a suffix to denote sequences of terms:
  - `*` means zero or more
  - `,*` means zero or more, delimited by comma, and allowing a trailing comma
  - `+` means one or more
  - `,+` means one or more, delimited by comma, and allowing a trailing comma
  - `?` means exactly zero or exactly one

additionally, "PRECEDENCE" is used to denote a precedence table for expression parsing.
it lists operators and their names in order of decreasing precedence, with a terminator at the end
which is used as the lhs/rhs of the expression with the least precedence.

the grammar is NOT whitespace-sensitive, but ASCII SPACE is the only allowed whitespace character.
using tabs produces a syntax error. for tabs in strings, the `\t` escape may be used.


a program is a sequence of statements. an empty program is also valid.
```
program =
    stmt* EOF
```

the language supports variables, functions, infinite loops, and various expressions
```
stmt =
    stmt_var
    stmt_fn
    stmt_loop
    stmt_expr
```

a variable must always be initialized to some value
```
stmt_var =
    "var" IDENT "=" expr
```

a function has list of parameters, its body is a list of statements,
and its body is a list of statements terminated by the "}" character
because this is a dynamically typed language, there are no parameter type annotations
the param list is a list of identifiers surrounded by parentheses

note that a function may also appear as an expression, in which case it isn't required
to have a name. (see `expr_fn`)
```
stmt_fn =
    "fn" IDENT "(" IDENT,* ")" "{" stmt* "}"
```

loops are infinite by default, and must be manually broken out of with `break` to become non-infinite
```
stmt_loop =
    "loop" "{" stmt* "}"
```

an expression statement is an expression executed purely for its side-effects
```
stmt_expr =
    expr_top
```

these expressions have maximum precedence
```
expr_top =
    expr_return
    expr_break
    expr_continue
    expr_if
    expr_do
    expr_fn
    expr_assign
```

return is an expression. it may be tricky to parse a return without a value, but it is unambigous:
a `return` appears either at the top-level, or in a function.
if it appears at the top-level, then it must either contain a subexpression, or we must find
something that can't possibly begin an expression, or the end of the file.
similarly, for `return` in a function, we must find either the "}" character,
an expression, or something that can't possibly begin an expression.
for both of these, the parser must maintain a function that determines if the next token may begin an expression.
```
expr_return =
    "return"
    "return" expr?
```

break and continue are also expressions, but they are only defined by their associated keywords.
note that while `return` may appear in any place that expects an expression, `break` and `continue` may only appear
within a loop body. the parser must maintain state to determine whether it is currently in a loop,
and reject `break` and `continue` expressions outside of loops.
```
expr_break =
    "break"

expr_continue =
    "continue"
```

`if` is also an expression. it yields the last value of the last stmt_expr within each of its branches.
in case no `stmt_expr` appears within a given branch, then the value of that branch is `nil`.

this allows it to be used as a kind of ternary operator.

in case the `expr_if` appears as the direct child of a `stmt_expr` node, it does not need to have an `else` branch.
if it appears as a subexpression anywhere, it MUST have an `else` branch.
for example:
  var x = if true { 0 }
would be a syntax error; the `if` expr is used as the rhs of a `stmt_var`, which means it MUST have an `else` branch.
```
expr_if =
    "if" expr "{" stmt* "}"
    "if" expr "{" stmt* "}" "else" "{" stmt* "}"
```

`do` expressions are similar to `if`: a sequence of statements, its value being the last `stmt_expr` in its body.
in case no `stmt_expr` appears within its body, then its value is `nil`.
```
expr_do =
    "do" "{" stmt* "}"
```

same as `stmt_fn`, but may be anonymous
```
expr_fn =
    "fn" IDENT? "(" IDENT,* ")" "{" stmt* "}"
```

an assignment is the highest precedence expression.
due to how the grammar is written out, it may only appear as the direct descendant of a `stmt_expr`,
and may not appear as a subexpression anywhere.
that means syntax like:
  x = y = 0
is explicitly NOT allowed.
```
expr_assign =
    assign_target "=" expr
    assign_target "+=" expr
    assign_target "-=" expr
    assign_target "*=" expr
    assign_target "/=" expr
    expr
```

in the parser, the easiest way to handle this is to parse a full `expr`,
but then perform a check for `assign_target` before producing the resulting syntax tree node.
```
assign_target =
    IDENT suffix_except_call*

suffix_except_call =
    suffix_index
    suffix_field
```

an `expr` term may appear as a subexpression.
note that here we repeat `return`, `break`, and `continue`
all of which may appear as subexpressions, unlike their counterparts in `expr_top`.
```
expr =
    expr_return
    expr_break
    expr_continue
    expr_infix
```

a binary expression of some sort, defined by a precedence table.
```
expr_infix =
    PRECEDENCE {
        expr_or:  "or"
        expr_and: "and"
        expr_eq:  ==, !=
        expr_cmp: >, >=, <, <=
        expr_add: +, -
        expr_mul: *, /

        _: expr_prefix
    }
```

a unary expression.
```
expr_prefix =
    "-" expr_suffix
    "not" expr_suffix
    expr_suffix
```

a postfix expression.
this includes calls, indexing, and field access, all of which may be chained arbitrarily.
```
expr_suffix =
    suffix_target suffix+
    expr_primary

suffix =
    suffix_call
    suffix_call_table
    suffix_index
    suffix_field

suffix_call =
    "(" expr,* ")"

suffix_call_table =
    "{" table_entry,* "}"

suffix_index =
    "[" expr "]"

suffix_field =
    "." ident

suffix_target =
    expr_use
    expr_group

expr_use =
    IDENT
```

a grouping expression is used to manually specify precedence.
```
expr_group =
    "(" expr ")"

expr_primary =
    expr_list
    expr_table
    expr_number
    expr_bool
    expr_string
    expr_nil
    expr_use
    expr_group

expr_list =
    "[" expr,* "]"

expr_table =
    "{" table_entry,* "}"
```

table entries are key-value pairs separated by an equals sign.
a key may be either an identifier or a string. numeric keys are not allowed.
one entry in an table may be a standalone identifier,
which is shorthand for `IDENT = IDENT`.
```
table_entry =
    IDENT "=" expr
    expr_use
    expr_string "=" expr
    expr_string
```

numbers are either integers or floats; they are not just syntactically different,
but actually different types.
```
expr_number =
    expr_int
    expr_float
```

an integer is a signed value up to ~52 bits.
```
expr_int =
    INTEGER
```

a float is a IEEE754 floating-point number, except that due to the use of NaN-boxing
in the VM, it may not be one special kind of NaN.
```
expr_float =
    FLOAT

expr_bool =
    "true"
    "false"

expr_string =
    STRING

expr_nil =
    "nil"
```



## Examples

```
fn fib_rec(n) {
    if n < 2 { n }
    else { fib(n-1) + fib(n-2) }
}

fn fib_iter(n) {
    if n < 2 { return n }
    var prev = 0
    var curr = 1

    var i = 2
    loop {
        if i > n { break }
        i += 1

        var next = prev + curr
        prev = curr
        curr = next
    }

    return curr
}

fn factorial(n) {
  if n <= 1 {
    return 1
  }
  return n * factorial(n - 1)
}

fn greatest_common_divisor(a, b) {
  loop
    if a == b {
      return a
    }

    if a > b {
      a = a - b
    } else {
      b = b - a
    }
  }
}

fn is_prime(n) {
  if n < 2 {
    return false
  }

  var i = 2
  loop
    if i * i > n {
      break
    }

    if n / i * i == n {
      return false
    }

    i = i + 1
  }

  return true
}

fn selection_sort(arr) {
  var n = len(arr)
  var i = 0
  loop
    if i >= n - 1 {
      break
    }

    var min_idx = i
    var j = i + 1
    loop
      if j >= n {
        break
      }
      if arr[j] < arr[min_idx] {
        min_idx = j
      }
      j = j + 1
    }

    if min_idx != i {
      var tmp = arr[i]
      arr[i] = arr[min_idx]
      arr[min_idx] = tmp
    }

    i = i + 1
  }

  return arr
}

fn binary_search(arr, target) {
  var low  = 0
  var high = len(arr) - 1

  loop
    if low > high {
      break
    }

    var mid = (low + high) / 2
    var v   = arr[mid]

    if v == target {
      return mid
    }

    if v < target {
      low = mid + 1
    } else {
      high = mid - 1
    }
  }

  return -1
}
```

```
svc = http.service()

svc.GET("/hello/{name}", fn(req, res) {
  res.status(200)
  res.header("Content-Type", "text/plain")
  res.body("Hello, {req.params.name}!")
})

http.serve(svc)
```

