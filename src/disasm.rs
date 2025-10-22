// use crate::{error::Location, span::Span};

use crate::{
    codegen::opcodes::{FnId, Insn},
    gc::ValueRef,
    module::Local,
    vm::{
        gc::GcRef,
        value::{FunctionProto, ModuleProto},
    },
};

pub trait DisasmModule {
    type Func: DisasmFunc;

    fn functions(&self) -> impl Iterator<Item = Self::Func> + '_;
    fn function_at(&self, id: FnId) -> Self::Func;
}

struct DisasmModuleWithSrc<'a, Mod: DisasmModule>(Mod, &'a str);

impl<'a, Mod: Copy + DisasmModule> std::fmt::Display for DisasmModuleWithSrc<'a, Mod>
where
    Mod::Func: Copy + DisasmFunc,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let DisasmModuleWithSrc(module, src) = self;

        for func in module.functions() {
            std::fmt::Display::fmt(&DisasmFuncWithSrc(*module, func, *src), f)?;
        }
        Ok(())
    }
}

pub trait DisasmFunc {
    fn name(&self) -> impl std::fmt::Display + '_;
    fn arity(&self) -> usize;
    fn stack_size(&self) -> usize;
    fn code(&self) -> &[Insn];

    fn has_debug_info(&self) -> bool;
    fn num_locals(&self) -> usize;
    fn local_at(&self, i: usize) -> Local;

    fn literal(&self, at: usize) -> impl std::fmt::Display + '_;
}

impl<'a, T: DisasmFunc> DisasmFunc for &'a T {
    fn name(&self) -> impl std::fmt::Display + '_ {
        T::name(self)
    }

    fn arity(&self) -> usize {
        T::arity(self)
    }

    fn stack_size(&self) -> usize {
        T::stack_size(self)
    }

    fn code(&self) -> &[Insn] {
        T::code(self)
    }

    fn has_debug_info(&self) -> bool {
        T::has_debug_info(self)
    }

    fn num_locals(&self) -> usize {
        T::num_locals(self)
    }

    fn local_at(&self, i: usize) -> Local {
        T::local_at(self, i)
    }

    fn literal(&self, at: usize) -> impl std::fmt::Display + '_ {
        T::literal(self, at)
    }
}

struct DisasmFuncWithSrc<'a, Mod: DisasmModule>(Mod, Mod::Func, &'a str);

impl<'a, Mod: Copy + DisasmModule> std::fmt::Display for DisasmFuncWithSrc<'a, Mod>
where
    Mod::Func: Copy + DisasmFunc,
{
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

            use crate::codegen::opcodes::DecodedInsn as I;
            write!(f, "  {i:width$}  ")?;
            match insn.into_enum() {
                I::Nop {} => writeln!(f, "nop")?,
                I::Mov { dst, src } => writeln!(f, "mov {dst}, {src}")?,

                I::Lmvar { dst, src } => writeln!(f, "lmvar {dst}, {src}")?,
                I::Smvar { src, dst } => writeln!(f, "smvar {dst}, {src}")?,
                I::Lcap { dst, src } => writeln!(f, "lcap {dst}, {src}")?,
                I::Scap { dst, src } => writeln!(f, "scap {dst}, {src}")?,
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
                I::Lint { dst, id } => writeln!(f, "lint {dst}, {v}", v = func.literal(id.zx()),)?,
                I::Lnum { dst, id } => writeln!(f, "lnum {dst}, {v}", v = func.literal(id.zx()),)?,
                I::Lstr { dst, id } => writeln!(f, "lstr {dst}, {v}", v = func.literal(id.zx()),)?,
                I::Lclosure { dst, id } => writeln!(f, "lclosure {dst}, {id}")?,
                I::Lfunc { dst, id } => writeln!(f, "lfunc {dst}, {id}")?,
                I::Llist { dst, cap } => writeln!(f, "llist {dst}, {cap}")?,
                I::Ltable { dst, cap } => writeln!(f, "ltable {dst}, {cap}")?,
                I::Jmp { rel } => writeln!(f, "jmp {rel}   ; to {}", (i as isize) + rel.sz())?,
                I::Istrue { v } => writeln!(f, "istrue {v}")?,
                I::Istruec { dst, v } => writeln!(f, "istruec {dst}, {v}")?,
                I::Isfalse { v } => writeln!(f, "isfalse {v}")?,
                I::Isfalsec { dst, v } => writeln!(f, "isfalsec {dst}, {v}")?,
                I::Islt { lhs, rhs } => writeln!(f, "islt {lhs}, {rhs}")?,
                I::Isle { lhs, rhs } => writeln!(f, "isle {lhs}, {rhs}")?,
                I::Isgt { lhs, rhs } => writeln!(f, "isgt {lhs}, {rhs}")?,
                I::Isge { lhs, rhs } => writeln!(f, "isge {lhs}, {rhs}")?,
                I::Iseq { lhs, rhs } => writeln!(f, "iseq {lhs}, {rhs}")?,
                I::Isne { lhs, rhs } => writeln!(f, "isne {lhs}, {rhs}")?,
                I::Iseqs { lhs, rhs } => {
                    writeln!(f, "iseqs {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
                }
                I::Isnes { lhs, rhs } => {
                    writeln!(f, "isnes {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
                }
                I::Iseqn { lhs, rhs } => {
                    writeln!(f, "iseqn {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
                }
                I::Isnen { lhs, rhs } => {
                    writeln!(f, "isnen {lhs}, {rhs}", rhs = func.literal(rhs.zx()))?
                }
                I::Iseqp { lhs, rhs } => writeln!(
                    f,
                    "iseqp {lhs}, {rhs}",
                    rhs = match rhs.get() {
                        0 => "false",
                        1 => "true",
                        0xFF => "nil",
                        _ => unreachable!(),
                    },
                )?,
                I::Isnep { lhs, rhs } => writeln!(
                    f,
                    "isnep {lhs}, {rhs}",
                    rhs = match rhs.get() {
                        0 => "false",
                        1 => "true",
                        0xFF => "nil",
                        _ => unreachable!(),
                    },
                )?,
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
                I::Unm { dst, rhs } => writeln!(f, "unm {dst}, {rhs}")?,
                I::Not { dst, rhs } => writeln!(f, "not {dst}, {rhs}")?,
                I::Call { dst, args } => writeln!(f, "call {dst}, {args}   ; dyn")?,
                I::Fastcall { dst, id } => writeln!(
                    f,
                    "call {dst}, {id}   ; {id}={name}",
                    name = module.function_at(id).name(),
                )?,
                I::Ret {} => writeln!(f, "ret")?,
                I::Stop {} => writeln!(f, "stop")?,
            }
        }
        writeln!(f)?;

        Ok(())
    }
}

fn num_digits(v: usize) -> usize {
    use core::iter::successors;

    successors(Some(v), |&n| (n >= 10).then_some(n / 10)).count()
}

impl crate::module::Module {
    pub fn disasm(&self, src: &str) -> impl std::fmt::Display {
        DisasmModuleWithSrc(self, src)
    }
}

impl<'a> DisasmModule for &'a crate::module::Module {
    type Func = &'a crate::module::FuncInfo;

    fn functions(&self) -> impl Iterator<Item = Self::Func> + '_ {
        crate::module::Module::functions(self).iter()
    }

    fn function_at(&self, id: FnId) -> Self::Func {
        crate::module::Module::functions(self).get(id.zx()).unwrap()
    }
}

impl DisasmFunc for crate::module::FuncInfo {
    fn name(&self) -> impl std::fmt::Display + '_ {
        self.name()
    }

    fn arity(&self) -> usize {
        self.arity() as usize
    }

    fn stack_size(&self) -> usize {
        self.stack_size() as usize
    }

    fn code(&self) -> &[Insn] {
        self.code()
    }

    fn has_debug_info(&self) -> bool {
        self.dbg().is_some()
    }

    fn num_locals(&self) -> usize {
        self.dbg().map(|dbg| dbg.locals.len()).unwrap_or_default()
    }

    fn local_at(&self, i: usize) -> Local {
        self.dbg().map(|dbg| dbg.locals[i]).unwrap()
    }

    fn literal(&self, at: usize) -> impl std::fmt::Display + '_ {
        &self.literals()[at]
    }
}

impl<'a> GcRef<'a, ModuleProto> {
    pub fn disasm(&self, src: &str) -> impl std::fmt::Display {
        DisasmModuleWithSrc(*self, src)
    }
}

impl<'a> DisasmModule for GcRef<'a, ModuleProto> {
    type Func = GcRef<'a, FunctionProto>;

    fn functions(&self) -> impl Iterator<Item = Self::Func> + '_ {
        self.functions()
    }

    fn function_at(&self, id: FnId) -> Self::Func {
        self.get_function(id).unwrap()
    }
}

impl<'a> DisasmFunc for GcRef<'a, FunctionProto> {
    fn name(&self) -> impl std::fmt::Display + '_ {
        self.name()
    }

    fn arity(&self) -> usize {
        self.arity()
    }

    fn stack_size(&self) -> usize {
        self.stack_size()
    }

    fn has_debug_info(&self) -> bool {
        self.dbg().is_some()
    }

    fn num_locals(&self) -> usize {
        self.dbg().map(|dbg| dbg.locals.len()).unwrap_or_default()
    }

    fn local_at(&self, i: usize) -> Local {
        self.dbg().map(|dbg| dbg.locals[i]).unwrap()
    }

    fn code(&self) -> &[Insn] {
        &self.code
    }

    fn literal(&self, at: usize) -> impl std::fmt::Display + '_ {
        DisplayValueRef(GcRef::map_value(self, |this| &this.literals()[at]))
    }
}

struct DisplayValueRef<'a>(ValueRef<'a>);

impl std::fmt::Display for DisplayValueRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            ValueRef::Nil => write!(f, "nil"),
            ValueRef::Bool(v) => write!(f, "{v}"),
            ValueRef::Int(v) => write!(f, "{v}"),
            ValueRef::Float(v) => write!(f, "{v}"),
            ValueRef::Object(v) => write!(f, "{}", v.type_name()),
        }
    }
}
