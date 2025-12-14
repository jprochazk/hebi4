/* Do nothing. */
nop;

/* Move value from register `src` to register `dst`. */
mov dst:reg src:reg;

# Module variables, upvalues, array indices and table keys.

/* Load module variable `src` to register `dst`. */
lmvar dst:reg src:mvar;

/* Store register `dst` into module variable `mvar`. */
smvar dst:mvar src:reg;

/* Load current closure's upvalue `src` to register `dst`. */
luv dst:reg src:uv;

/* Store register `src` to current closure's upvalue `dst`. */
suv dst:uv src:reg;

/* Load index `idx` (register) from `target` (array or table) to register `dst`. */
lidx dst:reg target:reg idx:reg;

/* Load index `idx` (literal) from `target` (array) to register `dst`. */
lidxn dst:reg target:reg idx:lit8;

/* Store value from `src` into `idx` (register) in `target` (array or table). */
sidx target:reg idx:reg src:reg;

/* Store value from `src` into `idx` (register) in `target` (array). */
sidxn target:reg idx:lit8 src:reg;

/* Load `key` (register) from `target` (object) to register `dst` (object). */
lkey dst:reg target:reg key:reg;

/* Load `key` (literal) from `target` (object) to register `dst` (object). */
lkeyc dst:reg target:reg key:lit8;

# `sidx` is emitted for `a[k] = v`, `skey` only exists for cases where
# a constant field key can't be emitted as a literal
/* Store value from `src` into `key` (register) in `target` (object). */
skey target:reg key:reg src:reg;

/* Store value from `src` into `key` (literal) in `target` (object). */
skeyc target:reg key:lit8 src:reg;

# Value instructions

/* Load `nil` value into register `dst`. */
lnil dst:reg;

/* Load 16-bit integer `v` into register `dst`. */
lsmi dst:reg v:imm16s;

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
 * `id` holds a `ClosureProto`.
 */
lclosure dst:reg id:lit;

/* Load function by `id` into register `dst`. */
lfunc dst:reg id:fnid;

/* Load host function by `id` into register `dst`. */
lhost dst:reg id:hostid;

# TODO: constant lists/tables don't need to use stack space at all
/*
 * Allocate a list with `capacity` into register `dst`.
 */
llist dst:reg cap:imm16;

/*
 * Allocate an object with `capacity` into register `dst`.
 */
ltable dst:reg cap:imm16;

# In hebi4's VM, there is only one `jmp` instruction with
# a signed offset (stored as u24 with a bias).
#
# To make this instruction conditional, it may be prefixed with
# various other instructions which perform value comparisons.
# If a given comparison yields `true`, they skip the `jmp`.

/* Adjust instruction pointer by `rel`. */
jmp rel:imm24s;

/* `dst = dst + 1` (register) and adjust IP by `rel` */
forloop dst:reg rel:imm16s;

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

# Specialized for certain kinds of constants: strings, numbers, "primitives"
#
# This specialization reduces the number of type checks we have to do for
# comparisons against constant values.

/* Skip `jmp` if `lhs == nil` (register, primitive) */
isnil v:reg;
/* Skip `jmp` if `lhs != nil` (register, primitive) */
isnotnil v:reg;
/* Skip `jmp` if `v` coerced to bool is `true` */
istrue v:reg;
/* Skip `jmp` if `v` coerced to bool is `false` */
isfalse v:reg;
/* Skip `jmp` if `lhs == rhs` (register, literal string) */
iseqs lhs:reg rhs:lit;
/* Skip `jmp` if `lhs != rhs` (register, literal string) */
isnes lhs:reg rhs:lit;
/* Skip `jmp` if `lhs == rhs` (register, literal int) */
iseqi lhs:reg rhs:lit;
/* Skip `jmp` if `lhs != rhs` (register, literal int) */
isnei lhs:reg rhs:lit;
/* Skip `jmp` if `lhs == rhs` (register, literal float) */
iseqf lhs:reg rhs:lit;
/* Skip `jmp` if `lhs != rhs` (register, literal float) */
isnef lhs:reg rhs:lit;

/* `dst = lhs < rhs` (register, register) */
isltv dst:reg lhs:reg rhs:reg;
/* `dst = lhs <= rhs` (register, register) */
islev dst:reg lhs:reg rhs:reg;
/* `dst = lhs > rhs` (register, register) */
isgtv dst:reg lhs:reg rhs:reg;
/* `dst = lhs >= rhs` (register, register) */
isgev dst:reg lhs:reg rhs:reg;
/* `dst = lhs == rhs` (register, register) */
iseqv dst:reg lhs:reg rhs:reg;
/* `dst = lhs != rhs` (register, register) */
isnev dst:reg lhs:reg rhs:reg;

# Arithmetic instructions
#
# LHS and RHS may be either in a register, or a constant.
# 
# Using a constant typically avoids having to execute a few
# instructions to materialize the value at runtime.
#
# When the compiler runs out of 8-bit literal slots, it falls back
# to using `vv` variants by emitting a load of the literal first.

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

/* `dst = lhs % rhs` (register, register) */
remvv dst:reg lhs:reg rhs:reg;
/* `dst = lhs % rhs` (register, literal) */
remvn dst:reg lhs:reg rhs:lit8;
/* `dst = lhs % rhs` (literal, register) */
remnv dst:reg lhs:lit8 rhs:reg;

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
# number of stack slots needed for function calls, and avoids a memcpy of
# call arguments before a call.

# Due to (intentional) limitations of Hebi's syntax and semantics, it is
# possible to statically know that a given variable may only ever contain
# a function, and also exactly which function. The latter means we can
# retrieve the required number of arguments whenever emitting this kind of
# function call.
#
# The resulting call is named "fastcall", because it's much faster than
# a regular call, as it requires no type or arity checking.
# Regular `call` instructions are only generated for calling functions
# stored in variables. 

# TODO: specialize for differing number of arguments?
/*
 * `dst = callee(callee+1..callee+1+args)`
 */
call dst:reg callee:reg args:imm8;

/*
 * `dst = funcs[id](dst..dst+funcs[id].args)`
 */
fastcall dst:reg id:fnid;

/*
 * `dst = host[id](dst..dst+host[id].args)`
 */
hostcall dst:reg id:hostid;

/*
 * Import a module.
 *
 * `id` holds an `ImportInfo` literal that contains the module spec
 * and the mapping of module names to local registers.
 */
import _unused:reg id:lit;

/*
 * Retrieve the iterator for `target`.
 *
 * `dst = builtin_get_iterator(target)`
 */
iter dst:reg target:reg;

# Note that `ret` and `stop` are separate, to avoid a branch in `ret` which
# would otherwise be required to check if there are any call frames left to
# return to. There is always at least one call frame on the call frame stack.
#
# The VM initiates execution by first stepping into a "trampoline".
# That's a bit of handwritten bytecode, which calls the module's main entrypoint
# with zero arguments. When the main entrypoint returns to the trampoline, it
# executes a `stop` which tells the VM to break out of the dispatch loop.

/*
 * Return from the current call.
 */
ret;

/*
 * Return from the current call, using value from `src`.
 */
retv src:reg;

/*
 * Stop execution, and yield to the VM's caller.
 * Never generated by the compiler.
 */
stop;

