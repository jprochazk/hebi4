export const examples = {
  "welcome": {
    name: "Welcome to Hebi4",
    code: `// Welcome to the Hebi4 playground
// The examples selector on the top right contains some code samples.
// Try selecting different output modes: Run, Disassembly, AST

fn greet() {
  print("Hello, chat")
}

greet()`
  },

  "fibonacci": {
    name: "Fibonacci (Recursive)",
    code: `fn fib(n) {
  if n < 2 {
    n
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

fib(10)`
  },

  "conditionals": {
    name: "Conditional Evaluation",
    code: `let a = 1
let b = 2
let result = 0

if (a < b) and (a < b) {
  result += 1
}

if not ((a < b) and (a < b)) {
  // won't execute
} else {
  result += 1
}

if (a < b) or (a < b) {
  result += 1
} else {
  // won't execute
}

if not ((a < b) or (a < b)) {
  // won't execute
} else {
  result += 1
}

result`
  },

  "arithmetic": {
    name: "Arithmetic Operations",
    code: `let x = 10
let y = 3

let sum = x + y
let diff = x - y
let prod = x * y
let quot = x / y

quot`
  },

  "tables": {
    name: "Tables (Objects)",
    code: `let person = {
  name: "Alice",
  age: 30,
}

person.name`
  },

  "lists": {
    name: "Lists (Arrays)",
    code: `let numbers = [1, 2, 3, 4, 5]

numbers[0] + numbers[4]`
  },

  "factorial": {
    name: "Factorial",
    code: `fn factorial(n) {
  if n <= 1 {
    1
  } else {
    n * factorial(n - 1)
  }
}

factorial(5)`
  },

  "fake_methods": {
    name: "Fake methods",
    code: `fn Counter() {
  let state = {value: 0}

  let inc = fn() {
    state.value += 1
    state.value
  }

  let dec = fn() {
    state.value -= 1
    state.value
  }

  state.inc = inc
  state.dec = dec

  return state
}

let c = Counter()
print(c.value)
print(c.inc())
print(c.dec())
`
  }
};
