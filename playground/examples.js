export const examples = {
  "welcome": {
    name: "Welcome to Hebi4",
    code: `// Welcome to the Hebi4 Playground!
// Click "Run" or press Ctrl+Enter to execute code

fn greet(name) {
  "Hello, " + name + "!"
}

greet("World")`
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
  greet: fn() {
    "Hello, I'm " + person.name
  }
}

person.greet()`
  },

  "lists": {
    name: "Lists (Arrays)",
    code: `let numbers = [1, 2, 3, 4, 5]
let sum = 0

// Note: This is a simple example
// Full iteration features may vary
numbers[0] + numbers[4]`
  },

  "closure": {
    name: "Closures",
    code: `fn makeCounter() {
  let count = 0
  fn() {
    count = count + 1
    count
  }
}

let counter = makeCounter()
counter()
counter()
counter()`
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
  }
};
