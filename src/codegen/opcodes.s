/* Do nothing. */
nop;

/* Move value from register `src` to register `dst`. */
mov dst:reg src:reg;

# TODO: module variables, closure captures, field loads

# Value instructions

/* Load `nil` value into register `dst`. */
lnil dst:reg;

/* Load 16-bit integer `v` into register `dst`. */
lsmi dst:reg v:imm16;

/* Load literal `true` into register `dst`. */
ltrue dst:reg;

/* Load literal `false` into register `dst`. */
lfalse dst:reg;

/*
 * Load literal by `id` into register `dst`.
 *
 * `id` holds a 64-bit integer.
 */
lint dst:reg id:lit;

/*
 * Load literal by `id` into register `dst`.
 *
 * `id` holds a 64-bit float.
 */
lnum dst:reg id:lit;

/*
 * Load literal by `id` into register `dst`.
 *
 * `id` holds a string literal.
 */
lstr dst:reg id:lit;

/*
 * Load closure function by `id` into register `dst`.
 *
 * `id` holds a ClosureInfo.
 */
lcli dst:reg id:lit;

/*
 * Load function by `id` into register `dst`.
 *
 * `id` holds a FuncInfo.
 *
 * This implies no captures, but the resulting object
 * is still a `Closure`.
 */
lfni dst:reg id:lit;

# TODO: constant arrays/objects don't need to use stack space at all
/*
 * Load array with values in range `dst..dst+len`.
 */
larr dst:reg len:imm8;

/*
 * Load object with key-value pairs in range `dst..dst+len`.
 *
 * There are total `len*2` values in the range.
 */
lobj dst:reg len:imm8;

# In hebi4's VM, there is only one `jmp` instruction with
# a signed offset (stored as u24 with a bias).
#
# To make this instruction conditional, it may be prefixed with
# various other instructions which perform value comparisons.
# If a given comparison yields `true`, they skip the `jmp`.

/* Adjust instruction pointer by `rel`. */
jmp rel:imm24;

/* Skip `jmp` if `v` coerced to bool is `true` */
istrue v:reg;
/**
 * If `v` coerced to bool is `true`:
 * - Set `dst` to original `v`
 * - Skip `jmp`
 */
istruec dst:reg v:reg;

/* Skip `jmp` if `v` coerced to bool is `false` */
isfalse v:reg;
/**
 * If `v` coerced to bool is `false`:
 * - Set `dst` to original `v`
 * - Skip `jmp`
 */
isfalsec dst:reg v:reg;

/* Skip `jmp` if `lhs < rhs` (register, register) */
islt lhs:reg rhs:reg;
/* Skip `jmp` if `lhs <= rhs` (register, register) */
isle lhs:reg rhs:reg;
/* Skip `jmp` if `lhs > rhs` (register, register) */
isgt lhs:reg rhs:reg;
/* Skip `jmp` if `lhs >= rhs` (register, register) */
isge lhs:reg rhs:reg;

/* Skip `jmp` if `lhs == rhs` (register, register) */
iseq lhs:reg rhs:reg;
/* Skip `jmp` if `lhs != rhs` (register, register) */
isne lhs:reg rhs:reg;

# Specialized for certain kinds of constants:

/* Skip `jmp` if `lhs == rhs` (register, literal string) */
iseqs lhs:reg rhs:lit;
/* Skip `jmp` if `lhs != rhs` (register, literal string) */
isnes lhs:reg rhs:lit;

/* Skip `jmp` if `lhs == rhs` (register, literal number) */
iseqn lhs:reg rhs:lit;
/* Skip `jmp` if `lhs != rhs` (register, literal number) */
isnen lhs:reg rhs:lit;

/* Skip `jmp` if `lhs == rhs` (register, primitive) */
iseqp lhs:reg rhs:imm8;
/* Skip `jmp` if `lhs != rhs` (register, primitive) */
isnep lhs:reg rhs:imm8;

# Binary instructions

/* `dst = lhs + rhs` (register, register) */
addvv dst:reg lhs:reg rhs:reg;
/* `dst = lhs + rhs` (register, literal) */
addvn dst:reg lhs:reg rhs:lit8;
/* `dst = lhs + rhs` (literal, register) */
addnv dst:reg lhs:lit8 rhs:reg;

/* `dst = lhs - rhs` (register, register) */
subvv dst:reg lhs:reg rhs:reg;
/* `dst = lhs - rhs` (register, literal) */
subvn dst:reg lhs:reg rhs:lit8;
/* `dst = lhs - rhs` (literal, register) */
subnv dst:reg lhs:lit8 rhs:reg;

/* `dst = lhs * rhs` (register, register) */
mulvv dst:reg lhs:reg rhs:reg;
/* `dst = lhs * rhs` (register, literal) */
mulvn dst:reg lhs:reg rhs:lit8;
/* `dst = lhs * rhs` (literal, register) */
mulnv dst:reg lhs:lit8 rhs:reg;

/* `dst = lhs / rhs` (register, register) */
divvv dst:reg lhs:reg rhs:reg;
/* `dst = lhs / rhs` (register, literal) */
divvn dst:reg lhs:reg rhs:lit8;
/* `dst = lhs / rhs` (literal, register) */
divnv dst:reg lhs:lit8 rhs:reg;

# Unary instructions

/* `dst = -rhs` */
unm dst:reg rhs:reg;
/* `dst = not rhs` */
not dst:reg rhs:reg;

# Function calls
#
# Hebi4's VM uses overlapping stacks for function calls.
# That means the closure and arguments passed to it during
# a call are required to be not only contiguous, but also
# top of the stack at the time of the call.
#
# This isn't the same as needing them at the end of the
# current stack frame, there just need to be no live values
# above the arguments.
#
# The layout for a stack frame is:
#
#   [ret, arg0, arg1, .., argN, local0, local1, .., localN]
#    ^
#   enum Value { tag, union value }
#
# Information about call frames is stored in a separate array:
#
#   [frame, ..]
#    ^
#   struct CallFrame { func_id:16, stack_size:8, <padding:16>, return_addr:32 }
# 
# The return value is always at r0, followed by the arguments, then
# the function's locals/intermediates.
#
# To perform a call, the arguments are evaluated and placed into their
# corresponding registers:
#
#   [.., callee, arg0, arg1, .., argN, .., <dead intermediates>]
#
# When the `call` instruction is dispatched, it performs various checks,
# then constructs a new stack frame with its base at `callee`:
#
#   old: [.., callee, arg0, arg1, .., argN]
#   new:     [ret,    arg0, arg1, .., argN, local0, local1, .., localN]
#
# Information about the previous stack frame is pushed into the
# call frame array, and the interpreter dispatches the next instruction
# at the start of the callee's code.
#
# This kind of layout is possible due to two invariants:
# - Variables are below intermediate values on the stack
# - Only the most recent intermediate is considered live
#
# This means values after and including `callee` can be safely discarded,
# so the new stack can re-use those slots. This greatly reduces the total
# number of stack slots needed for function calls.

# TODO: specialize for differing number of arguments

/*
 * `dst = func(dst+1..dst+1+args)`
 */
call dst:reg func:reg args:imm8;

# Due to (intentional) limitations of Hebi's syntax and semantics, it is
# possible to statically know that a given variable may only ever contain
# a function, and also exactly which function. The latter means we can
# retrieve the required number of arguments whenever emitting this kind of
# function call.
#
# The resulting call is named "fastcall", because it's much faster than
# a regular call, as it requires no type or arity checking.

/*
 * `dst = funcs[id](dst..dst+funcs[id].args)`
 */
fastcall dst:reg id:fnid;

/* Return from current call */
ret;

/* Stop execution. */
stop;

/* Triggers a panic. Never generated by the compiler. */
trap;
