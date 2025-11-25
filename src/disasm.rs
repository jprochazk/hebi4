// use crate::{error::Location, span::Span};

use crate::{
    codegen::opcodes::{DecodedInsn, FnId, HostId},
    core::CoreLib,
    module::{Local, Upvalue},
};

struct DisasmModuleWithSrc<'a>(&'a crate::module::Module, &'a str);

impl<'a> std::fmt::Display for DisasmModuleWithSrc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let DisasmModuleWithSrc(module, src) = self;

        for func in module.functions() {
            std::fmt::Display::fmt(&DisasmFuncWithSrc(*module, func, *src), f)?;
        }
        Ok(())
    }
}

struct DisasmFuncWithSrc<'a>(
    &'a crate::module::Module,
    &'a crate::module::FuncInfo,
    &'a str,
);

impl<'a> std::fmt::Display for DisasmFuncWithSrc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let DisasmFuncWithSrc(module, func, src) = self;

        writeln!(f, ".fn {name}", name = func.name())?;
        writeln!(f, "  params {arity}", arity = func.arity())?;
        writeln!(f, "  stack {stack_size}", stack_size = func.stack_size())?;
        if func.has_debug_info() {
            if func.num_locals() > 0 {
                writeln!(f, "  .locals")?;
                for i in 0..func.num_locals() {
                    let Local { span, reg } = func.local_at(i);
                    let name = &src[span];
                    writeln!(f, "    {name} = {reg}")?;
                }
            }

            if func.num_upvalues() > 0 {
                writeln!(f, "  .upvalues")?;
                for i in 0..func.num_upvalues() {
                    let Upvalue { span, info } = func.upvalue_at(i);
                    let name = &src[span];
                    writeln!(f, "    {name} = {info}")?;
                }
            }
        }

        let code = func.code();
        writeln!(f, "  .code")?;
        let width = num_digits(code.len());
        // let mut prev_span = None;
        for (i, insn) in code.iter().enumerate() {
            // let span = Location::from_source_span(src, dbg.spans[i]).line_span();
            // if prev_span != Some(span) {
            //     prev_span = Some(span);

            //     writeln!(f, "  # {}", &src[span])?;
            // }

            write!(
                f,
                "  {i:width$}  {}",
                DisasmInsnWithSrc(module, func, src, i, insn.into_enum())
            )?;
        }
        writeln!(f)?;

        Ok(())
    }
}

struct DisasmInsnWithSrc<'a>(
    &'a crate::module::Module,
    &'a crate::module::FuncInfo,
    &'a str,
    usize,
    DecodedInsn,
);

impl std::fmt::Display for DisasmInsnWithSrc<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let DisasmInsnWithSrc(module, func, src, i, insn) = self;

        let i = *i;
        type I = DecodedInsn;
        match *insn {
            I::Nop {} => writeln!(f, "nop")?,
            I::Mov { dst, src } => writeln!(f, "mov {dst}, {src}")?,

            I::Lmvar { dst, src } => writeln!(f, "lmvar {dst}, {src}")?,
            I::Smvar { src, dst } => writeln!(f, "smvar {dst}, {src}")?,
            I::Luv { dst, src } => writeln!(f, "luv {dst}, {src}")?,
            I::Suv { dst, src } => writeln!(f, "suv {dst}, {src}")?,
            I::Lidx { dst, target, idx } => writeln!(f, "lidx {dst}, {target}, {idx}")?,
            I::Lidxn { dst, target, idx } => writeln!(
                f,
                "lidxn {dst}, {target}, {idx}",
                idx = func.literal(idx.zx()),
            )?,
            I::Sidx { target, idx, src } => writeln!(f, "sidx {target}, {idx}, {src}")?,
            I::Sidxn { target, idx, src } => writeln!(
                f,
                "sidxn {target}, {idx}, {src}",
                idx = func.literal(idx.zx()),
            )?,
            I::Lkey { dst, target, key } => writeln!(f, "lkey {dst}, {target}, {key}")?,
            I::Lkeyc { dst, target, key } => writeln!(
                f,
                "lkeyc {dst}, {target}, {key}",
                key = func.literal(key.zx()),
            )?,
            I::Skey { target, key, src } => writeln!(f, "skey {target}, {key}, {src}")?,
            I::Skeyc { target, key, src } => writeln!(
                f,
                "skeyc {target}, {key}, {src}",
                key = func.literal(key.zx()),
            )?,

            I::Lnil { dst } => writeln!(f, "lnil {dst}")?,
            I::Lsmi { dst, v } => writeln!(f, "lsmi {dst}, {v}")?,
            I::Ltrue { dst } => writeln!(f, "ltrue {dst}")?,
            I::Lfalse { dst } => writeln!(f, "lfalse {dst}")?,
            I::Lint { dst, id } => writeln!(f, "lint {dst}, {v}", v = func.literal(id.zx()))?,
            I::Lnum { dst, id } => writeln!(f, "lnum {dst}, {v}", v = func.literal(id.zx()))?,
            I::Lstr { dst, id } => writeln!(f, "lstr {dst}, {v}", v = func.literal(id.zx()))?,
            I::Lclosure { dst, id } => {
                let closure = func.literal(id.zx()).closure().unwrap();
                writeln!(
                    f,
                    "lclosure {dst}, {id}   ; {name}, uv={n}",
                    name = module.function_at(closure.func).name(),
                    n = closure.upvalues.len(),
                )?
            }
            I::Lfunc { dst, id } => writeln!(
                f,
                "lfunc {dst}, {id}   ; {id}={name}",
                name = module.function_at(id).name()
            )?,
            I::Lhost { dst, id } => writeln!(
                f,
                "lhost {dst}, {id}   ; {id}={name}",
                name = module.host_function_at(id).name
            )?,
            I::Llist { dst, cap } => writeln!(f, "llist {dst}, {cap}")?,
            I::Ltable { dst, cap } => writeln!(f, "ltable {dst}, {cap}")?,
            I::Jmp { rel } => writeln!(f, "jmp {rel}   ; to {}", (i as isize) + rel.sz())?,
            I::Islt { lhs, rhs } => writeln!(f, "islt {lhs}, {rhs}")?,
            I::Isle { lhs, rhs } => writeln!(f, "isle {lhs}, {rhs}")?,
            I::Isgt { lhs, rhs } => writeln!(f, "isgt {lhs}, {rhs}")?,
            I::Isge { lhs, rhs } => writeln!(f, "isge {lhs}, {rhs}")?,
            I::Iseq { lhs, rhs } => writeln!(f, "iseq {lhs}, {rhs}")?,
            I::Isne { lhs, rhs } => writeln!(f, "isne {lhs}, {rhs}")?,
            I::Istrue { v } => writeln!(f, "istrue {v}")?,
            I::Isfalse { v } => writeln!(f, "isfalse {v}")?,
            I::Isnil { v } => writeln!(f, "isnil {v}")?,
            I::Isnotnil { v } => writeln!(f, "isnotnil {v}")?,
            I::Iseqs { lhs, rhs } => {
                writeln!(f, "iseqs {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Isnes { lhs, rhs } => {
                writeln!(f, "isnes {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Iseqi { lhs, rhs } => {
                writeln!(f, "iseqi {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Isnei { lhs, rhs } => {
                writeln!(f, "isnei {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Iseqf { lhs, rhs } => {
                writeln!(f, "iseqf {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Isnef { lhs, rhs } => {
                writeln!(f, "isnef {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Isltv { dst, lhs, rhs } => writeln!(f, "isltv {dst}, {lhs}, {rhs}")?,
            I::Islev { dst, lhs, rhs } => writeln!(f, "islev {dst}, {lhs}, {rhs}")?,
            I::Isgtv { dst, lhs, rhs } => writeln!(f, "isgtv {dst}, {lhs}, {rhs}")?,
            I::Isgev { dst, lhs, rhs } => writeln!(f, "isgev {dst}, {lhs}, {rhs}")?,
            I::Iseqv { dst, lhs, rhs } => writeln!(f, "iseqv {dst}, {lhs}, {rhs}")?,
            I::Isnev { dst, lhs, rhs } => writeln!(f, "isnev {dst}, {lhs}, {rhs}")?,
            I::Addvv { dst, lhs, rhs } => writeln!(f, "add {dst}, {lhs}, {rhs}")?,
            I::Addvn { dst, lhs, rhs } => {
                writeln!(f, "add {dst}, {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Addnv { dst, lhs, rhs } => {
                writeln!(f, "add {dst}, {lhs}, {rhs}", lhs = func.literal(lhs.zx()))?
            }
            I::Subvv { dst, lhs, rhs } => writeln!(f, "sub {dst}, {lhs}, {rhs}")?,
            I::Subvn { dst, lhs, rhs } => {
                writeln!(f, "sub {dst}, {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Subnv { dst, lhs, rhs } => {
                writeln!(f, "sub {dst}, {lhs}, {rhs}", lhs = func.literal(lhs.zx()))?
            }
            I::Mulvv { dst, lhs, rhs } => writeln!(f, "mul {dst}, {lhs}, {rhs}")?,
            I::Mulvn { dst, lhs, rhs } => {
                writeln!(f, "mul {dst}, {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Mulnv { dst, lhs, rhs } => {
                writeln!(f, "mul {dst}, {lhs}, {rhs}", lhs = func.literal(lhs.zx()))?
            }
            I::Divvv { dst, lhs, rhs } => writeln!(f, "div {dst}, {lhs}, {rhs}")?,
            I::Divvn { dst, lhs, rhs } => {
                writeln!(f, "div {dst}, {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Divnv { dst, lhs, rhs } => {
                writeln!(f, "div {dst}, {lhs}, {rhs}", lhs = func.literal(lhs.zx()))?
            }
            I::Remvv { dst, lhs, rhs } => writeln!(f, "rem {dst}, {lhs}, {rhs}")?,
            I::Remvn { dst, lhs, rhs } => {
                writeln!(f, "rem {dst}, {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
            }
            I::Remnv { dst, lhs, rhs } => {
                writeln!(f, "rem {dst}, {lhs}, {rhs}", lhs = func.literal(lhs.zx()))?
            }
            I::Unm { dst, rhs } => writeln!(f, "unm {dst}, {rhs}")?,
            I::Not { dst, rhs } => writeln!(f, "not {dst}, {rhs}")?,
            I::Call { dst, callee, args } => writeln!(f, "call {dst}, {callee}, {args}   ; dyn")?,
            I::Fastcall { dst, id } => writeln!(
                f,
                "call {dst}, {id}   ; {id}={name}",
                name = module.function_at(id).name(),
            )?,
            I::Hostcall { dst, id } => writeln!(
                f,
                "call {dst}, {name}",
                name = module.host_function_at(id).name,
            )?,
            I::Import { _unused, id } => {
                writeln!(f, "import {id}   ; {v}", v = func.literal(id.zx()))?
            }
            I::Ret {} => writeln!(f, "ret")?,
            I::Retv { src } => writeln!(f, "retv {src}")?,
            I::Stop {} => writeln!(f, "stop")?,
        }

        Ok(())
    }
}

impl std::fmt::Display for DecodedInsn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        type I = DecodedInsn;
        match *self {
            I::Nop {} => write!(f, "nop"),
            I::Mov { dst, src } => write!(f, "mov {dst}, {src}"),
            I::Lmvar { dst, src } => write!(f, "lmvar {dst}, {src}"),
            I::Smvar { src, dst } => write!(f, "smvar {dst}, {src}"),
            I::Luv { dst, src } => write!(f, "luv {dst}, {src}"),
            I::Suv { dst, src } => write!(f, "suv {dst}, {src}"),
            I::Lidx { dst, target, idx } => write!(f, "lidx {dst}, {target}, {idx}"),
            I::Lidxn { dst, target, idx } => write!(f, "lidxn {dst}, {target}, {idx}"),
            I::Sidx { target, idx, src } => write!(f, "sidx {target}, {idx}, {src}"),
            I::Sidxn { target, idx, src } => write!(f, "sidxn {target}, {idx}, {src}"),
            I::Lkey { dst, target, key } => write!(f, "lkey {dst}, {target}, {key}"),
            I::Lkeyc { dst, target, key } => write!(f, "lkeyc {dst}, {target}, {key}"),
            I::Skey { target, key, src } => write!(f, "skey {target}, {key}, {src}"),
            I::Skeyc { target, key, src } => write!(f, "skeyc {target}, {key}, {src}"),
            I::Lnil { dst } => write!(f, "lnil {dst}"),
            I::Lsmi { dst, v } => write!(f, "lsmi {dst}, {v}"),
            I::Ltrue { dst } => write!(f, "ltrue {dst}"),
            I::Lfalse { dst } => write!(f, "lfalse {dst}"),
            I::Lint { dst, id } => write!(f, "lint {dst}, {id}"),
            I::Lnum { dst, id } => write!(f, "lnum {dst}, {id}"),
            I::Lstr { dst, id } => write!(f, "lstr {dst}, {id}"),
            I::Lclosure { dst, id } => write!(f, "lclosure {dst}, {id}"),
            I::Lfunc { dst, id } => write!(f, "lfunc {dst}, {id}"),
            I::Lhost { dst, id } => write!(f, "lhost {dst}, {id}"),
            I::Llist { dst, cap } => write!(f, "llist {dst}, {cap}"),
            I::Ltable { dst, cap } => write!(f, "ltable {dst}, {cap}"),
            I::Jmp { rel } => write!(f, "jmp {rel}"),
            I::Islt { lhs, rhs } => write!(f, "islt {lhs}, {rhs}"),
            I::Isle { lhs, rhs } => write!(f, "isle {lhs}, {rhs}"),
            I::Isgt { lhs, rhs } => write!(f, "isgt {lhs}, {rhs}"),
            I::Isge { lhs, rhs } => write!(f, "isge {lhs}, {rhs}"),
            I::Iseq { lhs, rhs } => write!(f, "iseq {lhs}, {rhs}"),
            I::Isne { lhs, rhs } => write!(f, "isne {lhs}, {rhs}"),
            I::Istrue { v } => write!(f, "istrue {v}"),
            I::Isfalse { v } => write!(f, "isfalse {v}"),
            I::Isnil { v } => write!(f, "isnil {v}"),
            I::Isnotnil { v } => write!(f, "isnotnil {v}"),
            I::Iseqs { lhs, rhs } => write!(f, "iseqs {lhs}, {rhs}"),
            I::Isnes { lhs, rhs } => write!(f, "isnes {lhs}, {rhs}"),
            I::Iseqi { lhs, rhs } => write!(f, "iseqi {lhs}, {rhs}"),
            I::Isnei { lhs, rhs } => write!(f, "isnei {lhs}, {rhs}"),
            I::Iseqf { lhs, rhs } => write!(f, "iseqf {lhs}, {rhs}"),
            I::Isnef { lhs, rhs } => write!(f, "isnef {lhs}, {rhs}"),
            I::Isltv { dst, lhs, rhs } => write!(f, "isltv {dst}, {lhs}, {rhs}"),
            I::Islev { dst, lhs, rhs } => write!(f, "islev {dst}, {lhs}, {rhs}"),
            I::Isgtv { dst, lhs, rhs } => write!(f, "isgtv {dst}, {lhs}, {rhs}"),
            I::Isgev { dst, lhs, rhs } => write!(f, "isgev {dst}, {lhs}, {rhs}"),
            I::Iseqv { dst, lhs, rhs } => write!(f, "iseqv {dst}, {lhs}, {rhs}"),
            I::Isnev { dst, lhs, rhs } => write!(f, "isnev {dst}, {lhs}, {rhs}"),
            I::Addvv { dst, lhs, rhs } => write!(f, "add {dst}, {lhs}, {rhs}"),
            I::Addvn { dst, lhs, rhs } => write!(f, "add {dst}, {lhs}, {rhs}"),
            I::Addnv { dst, lhs, rhs } => write!(f, "add {dst}, {lhs}, {rhs}"),
            I::Subvv { dst, lhs, rhs } => write!(f, "sub {dst}, {lhs}, {rhs}"),
            I::Subvn { dst, lhs, rhs } => write!(f, "sub {dst}, {lhs}, {rhs}"),
            I::Subnv { dst, lhs, rhs } => write!(f, "sub {dst}, {lhs}, {rhs}"),
            I::Mulvv { dst, lhs, rhs } => write!(f, "mul {dst}, {lhs}, {rhs}"),
            I::Mulvn { dst, lhs, rhs } => write!(f, "mul {dst}, {lhs}, {rhs}"),
            I::Mulnv { dst, lhs, rhs } => write!(f, "mul {dst}, {lhs}, {rhs}"),
            I::Divvv { dst, lhs, rhs } => write!(f, "div {dst}, {lhs}, {rhs}"),
            I::Divvn { dst, lhs, rhs } => write!(f, "div {dst}, {lhs}, {rhs}"),
            I::Divnv { dst, lhs, rhs } => write!(f, "div {dst}, {lhs}, {rhs}"),
            I::Remvv { dst, lhs, rhs } => write!(f, "rem {dst}, {lhs}, {rhs}"),
            I::Remvn { dst, lhs, rhs } => write!(f, "rem {dst}, {lhs}, {rhs}"),
            I::Remnv { dst, lhs, rhs } => write!(f, "rem {dst}, {lhs}, {rhs}"),
            I::Unm { dst, rhs } => write!(f, "unm {dst}, {rhs}"),
            I::Not { dst, rhs } => write!(f, "not {dst}, {rhs}"),
            I::Call { dst, callee, args } => write!(f, "call {dst}, {callee}, {args}"),
            I::Fastcall { dst, id } => write!(f, "call {dst}, {id}"),
            I::Hostcall { dst, id } => write!(f, "call {dst}, {id}"),
            I::Import { _unused, id } => write!(f, "import {id}"),
            I::Ret {} => write!(f, "ret"),
            I::Retv { src } => write!(f, "retv {src}"),
            I::Stop {} => write!(f, "stop"),
        }
    }
}

fn num_digits(v: usize) -> usize {
    use core::iter::successors;

    successors(Some(v), |&n| (n >= 10).then_some(n / 10)).count()
}

impl crate::module::Module {
    pub fn disasm<'a>(&'a self, src: &'a str) -> impl std::fmt::Display + 'a {
        DisasmModuleWithSrc(self, src)
    }
}

impl crate::module::Module {
    fn function_at(&self, id: FnId) -> &crate::module::FuncInfo {
        crate::module::Module::functions(self).get(id.zx()).unwrap()
    }

    fn host_function_at(&self, id: HostId) -> &crate::core::CoreFunction {
        CoreLib::get().entry(id)
    }
}

impl crate::module::FuncInfo {
    fn has_debug_info(&self) -> bool {
        self.dbg().is_some()
    }

    fn num_locals(&self) -> usize {
        self.dbg().map(|dbg| dbg.locals.len()).unwrap_or_default()
    }

    fn local_at(&self, i: usize) -> Local {
        self.dbg().map(|dbg| dbg.locals[i]).unwrap()
    }

    fn num_upvalues(&self) -> usize {
        self.dbg().map(|dbg| dbg.upvalues.len()).unwrap_or_default()
    }

    fn upvalue_at(&self, i: usize) -> Upvalue {
        self.dbg().map(|dbg| dbg.upvalues[i]).unwrap()
    }

    fn closure_fn_id_at(&self, i: usize) -> FnId {
        let crate::module::Literal::ClosureInfo(info) = &self.literals()[i] else {
            unreachable!();
        };
        info.func
    }

    fn literal(&self, at: usize) -> &crate::module::Literal {
        &self.literals()[at]
    }
}
