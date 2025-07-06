/* Do nothing. */
nop = 0x00;

/* Move value from register `src` to register `dst`. */
mov dst:reg src:reg = 0x01;

// Value instructions

/* Load `nil` value into register `dst`. */
lnil dst:reg = 0x02;

/* Load 16-bit integer `v` into register `dst`. */
linti dst:reg v:i16 = 0x03;

/*
 * Load literal by `id` into register `dst`.
 *
 * `id` stores a 64-bit integer.
 */
lintc dst:reg id:lit = 0x04;

/* Load literal `true` into register `dst`. */
ltrue dst:reg = 0x05;

/* Load literal `false` into register `dst`. */
lfalse dst:reg = 0x06;

/*
 * Load closure function by `id` into register `dst`.
 *
 * `id` stores a ClosureInfo.
 */
lfncl dst:reg id:lit = 0x10;

/*
 * Load function by `id` into register `dst`.
 *
 * `id` stores a FuncInfo.
 *
 * This implies no captures, but the resulting object
 * is still a `Closure`.
 */
lfunc dst:reg id:lit = 0x11;

/*
 * Load array with values in range `dst..dst+len`.
 */
larr dst:reg len:u8 = 0x12;

/*
 * Load object with key-value pairs in range `dst..dst+len`.
 *
 * There are total `len*2` values in the range.
 */
lobj dst:reg len:u8 = 0x13;

// In hebi4's VM, there is only one `jmp` instruction with
// a signed offset (stored as u24 with a bias).
//
// To make this instruction conditional, it may be prefixed with
// various other instructions which perform value comparisons.
// If a given comparison yields `true`, they skip the `jmp`.

/* Adjust instruction pointer by `rel`. */
jmp rel:s24 = 0x30;

/* If `v` coerced to bool is `true`, skip next instruction. */
test v:reg = auto;

/* If `lhs == rhs`, skip next instruction. */
eq lhs:reg rhs:reg = auto;
/* If `lhs == rhs`, skip next instruction. */
eqi lhs:reg rhs:i16 = auto;
/* If `lhs == rhs`, skip next instruction. */
eqc lhs:reg rhs:lit = auto;

/* If `lhs < rhs`, skip next instruction. */
lt lhs:reg rhs:reg = auto;
/* If `lhs < rhs`, skip next instruction. */
lti lhs:reg rhs:i16 = auto;
/* If `lhs < rhs`, skip next instruction. */
ltc lhs:reg rhs:lit = auto;

/* If `lhs <= rhs`, skip next instruction. */
le lhs:reg rhs:reg = auto;
/* If `lhs <= rhs`, skip next instruction. */
lei lhs:reg rhs:i16 = auto;
/* If `lhs <= rhs`, skip next instruction. */
lec lhs:reg rhs:lit = auto;

// Binary instructions

/* `dst = lhs + rhs`, `lhs`/`rhs` must be numeric. */
add dst:reg lhs:reg rhs:reg = 0x50;
/* `dst = lhs + rhs`, `lhs` must be numeric. */
addi dst:reg lhs:reg rhs:i8 = auto;

/* `dst = lhs - rhs`, `lhs`/`rhs` must be numeric. */
sub dst:reg lhs:reg rhs:reg = auto;
/* `dst = lhs - rhs`, `lhs` must be numeric. */
subi dst:reg lhs:reg rhs:i8 = auto;

/* `dst = lhs * rhs`, `lhs`/`rhs` must be numeric. */
mul dst:reg lhs:reg rhs:reg = auto;
/* `dst = lhs * rhs`, `lhs` must be numeric. */
muli dst:reg lhs:reg rhs:i8 = auto;

/* `dst = lhs / rhs`, `lhs`/`rhs` must be numeric. */
div dst:reg lhs:reg rhs:reg = auto;
/* `dst = lhs / rhs`, `lhs` must be numeric. */
divi dst:reg lhs:reg rhs:i8 = auto;

// Unary instructions

/* `dst = -rhs`, `rhs` must be numeric. */
unm dst:reg rhs:reg = auto;
/* `dst = not rhs`, `rhs` must be bool. */
not dst:reg rhs:reg = auto;

// Function calls
//
// Hebi4's VM uses overlapping stacks for function calls.
// That means the closure and arguments passed to it during
// a call are required to be not only contiguous, but also
// top of the stack at the time of the call.
//
// This isn't the same as needing them at the end of the
// current stack frame, there just need to be no live values
// above the arguments.
//
// The layout for a stack frame is:
//
//   [ret, arg0, arg1, .., argN, local0, local1, .., localN]
//    ^
//   enum Value { tag, union value }
//
// Information about call frames is stored in a separate array:
//
//   [frame, ..]
//    ^
//   struct CallFrame { func_id:16, stack_size:8, <padding:16>, return_addr:32 }
// 
// The return value is always at r0, followed by the arguments, then
// the function's locals/intermediates.
//
// To perform a call, the arguments are evaluated and placed into their
// corresponding registers:
//
//   [.., callee, arg0, arg1, .., argN, .., <dead intermediates>]
//
// When the `call` instruction is dispatched, it performs various checks,
// then constructs a new stack frame with its based at `callee`:
//
//   old: [.., callee, arg0, arg1, .., argN]
//   new:     [ret,    arg0, arg1, .., argN, local0, local1, .., localN]
//
// Information about the previous stack frame is pushed into the
// call frame array, and the interpreter dispatches the next instruction
// at the start of the callee's code.
//
// This kind of layout is possible due to two invariants:
// - Variables are below intermediate values on the stack
// - Only the most recent intermediate is considered live
//
// This means values after and including `callee` can be safely discarded,
// so the new stack can re-use those slots. This greatly reduces the total
// number of stack slots needed for function calls.

/*
 * `dst = func(dst+1..dst+1+args)`
 */
call dst:reg func:reg args:u8 = 0x70;

// Due to (intentional) limitations of Hebi's syntax as semantics, it is
// possible to statically know that a given variable may only ever contain
// a function, and also exactly which function. The latter means we can
// retrieve the required number of arguments whenever emitting this kind of
// function call.
//
// The resulting call is named "fastcall", because it's much faster than
// a regular call, as it requires no type or arity checking.

/*
 * `dst = funcs[id](dst..dst+funcs[id].args)`
 */
fastcall dst:reg id:lit = auto;

/* Return from current call */
ret = auto;
