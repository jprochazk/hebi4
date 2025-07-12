#![allow(unsafe_op_in_unsafe_fn)]

#[inline]
fn op(i: u32) -> u8 {
    (i >> 24) as u8
}

#[inline]
fn abc(i: u32) -> (u8, u8, u8) {
    //   op:8     a:8      b:8      c:8
    // [ 00000000 00000000 00000000 00000000 ]
    let a = ((i >> 16) & 0xFF) as u8;
    let b = ((i >> 8) & 0xFF) as u8;
    let c = (i & 0xFF) as u8;
    (a, b, c)
}

#[inline]
fn rv(i: u32) -> (u8, i16) {
    //   op:8     r:8      v:16
    // [ 00000000 ________ 00000000 00000000 ]
    let r = ((i >> 16) & 0xFF) as u8;
    let v = ((i & 0xFFFF) as u16) as i16;
    (r, v)
}

macro_rules! at {
    ($s:expr, $i:expr) => {
        $s[$i as usize]
    };
}

type Instruction = u32;

type Op = unsafe extern "C" fn(u32, *mut i16, *const u32, *const ());

#[inline(never)]
pub unsafe fn run(ops: &[u32]) -> i16 {
    static JT: [Op; 7] = [nop, add, sub, mul, div, int, end];

    /// Dispatch at current `$ip`.
    macro_rules! dispatch_at {
        ($stack:expr, $ip:expr, $jt:expr) => {
            let i = *$ip;
            let op = *$jt.add(i as usize).cast::<Op>();
            op(i, $stack, $ip, $jt)
        };
    }

    /// Dispatch at `$ip + 1`.
    macro_rules! dispatch_next {
        ($stack:expr, $ip:expr, $jt:expr) => {
            let ip = $ip.add(1);
            dispatch_at!($stack, ip, $jt);
        };
    }

    macro_rules! at {
        ($stack:expr, $i:expr) => {
            *$stack.add($i as usize)
        };
    }

    unsafe extern "C" fn nop(_: u32, stack: *mut i16, ip: *const u32, jt: *const ()) {
        dispatch_next!(stack, ip, jt);
    }

    unsafe extern "C" fn add(i: u32, stack: *mut i16, ip: *const u32, jt: *const ()) {
        let (dst, lhs, rhs) = abc(i);
        at!(stack, dst) = at!(stack, lhs) + at!(stack, rhs);
        dispatch_next!(stack, ip, jt);
    }

    unsafe extern "C" fn sub(i: u32, stack: *mut i16, ip: *const u32, jt: *const ()) {
        let (dst, lhs, rhs) = abc(i);
        at!(stack, dst) = at!(stack, lhs) - at!(stack, rhs);
        dispatch_next!(stack, ip, jt);
    }

    unsafe extern "C" fn mul(i: u32, stack: *mut i16, ip: *const u32, jt: *const ()) {
        let (dst, lhs, rhs) = abc(i);
        at!(stack, dst) = at!(stack, lhs) * at!(stack, rhs);
        dispatch_next!(stack, ip, jt);
    }

    unsafe extern "C" fn div(i: u32, stack: *mut i16, ip: *const u32, jt: *const ()) {
        let (dst, lhs, rhs) = abc(i);
        at!(stack, dst) = at!(stack, lhs) / at!(stack, rhs);
        dispatch_next!(stack, ip, jt);
    }

    unsafe extern "C" fn int(i: u32, stack: *mut i16, ip: *const u32, jt: *const ()) {
        let (dst, v) = rv(i);
        at!(stack, dst) = v;
        dispatch_next!(stack, ip, jt);
    }

    unsafe extern "C" fn end(_: u32, _: *mut i16, _: *const u32, _: *const ()) {
        // no dispatch
    }

    let mut stack = [0i16; 256];
    let stack = stack.as_mut_ptr();
    let ip = ops.as_ptr();
    let jt = JT.as_ptr().cast::<()>();
    dispatch_next!(stack, ip, jt);

    *stack
}
