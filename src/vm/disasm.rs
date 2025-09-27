// use crate::{error::Location, span::Span};

use super::{FuncInfo, Module, dbg::Local};

impl Module {
    pub fn disasm<'a>(&'a self, src: &'a str) -> Disasm<'a> {
        Disasm { module: self, src }
    }
}

pub struct Disasm<'a> {
    module: &'a Module,
    src: &'a str,
}

impl<'a> std::fmt::Display for Disasm<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in self.module.functions() {
            std::fmt::Display::fmt(
                &DisasmFunc {
                    module: self.module,
                    func,
                    src: self.src,
                },
                f,
            )?;
        }
        Ok(())
    }
}

pub struct DisasmFunc<'a> {
    module: &'a Module,
    func: &'a FuncInfo,
    src: &'a str,
}

impl<'a> std::fmt::Display for DisasmFunc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FuncInfo {
            name,
            nparams,
            nstack,
            code,
            literals,
            dbg,
            module: _,
        } = &self.func;

        let module = self.module;
        let src = self.src;
        let dbg = dbg;
        let code = &code[..];
        let lit = &literals[..];

        writeln!(f, ".fn {name}")?;
        writeln!(f, "  params {nparams}")?;
        writeln!(f, "  stack {nstack}")?;
        if let Some(dbg) = dbg {
            if dbg.locals.len() > 0 {
                writeln!(f, "  .locals")?;
                for Local { span, reg } in &dbg.locals {
                    let name = &src[*span];
                    writeln!(f, "    {name} = {reg}")?;
                }
            }
        }
        writeln!(f, "  .code")?;
        let width = num_digits(code.len());
        // let mut prev_span = None;
        for (i, inst) in code.iter().enumerate() {
            // let span = Location::from_source_span(src, dbg.spans[i]).line_span();
            // if prev_span != Some(span) {
            //     prev_span = Some(span);

            //     writeln!(f, "  # {}", &src[span])?;
            // }

            use crate::codegen::opcodes::Instruction as I;
            write!(f, "  {i:width$}  ")?;
            match inst {
                I::Nop { _a, _b, _c } => writeln!(f, "nop")?,
                I::Mov { dst, src, _c } => writeln!(f, "mov {dst}, {src}")?,

                I::Lmvar { dst, src } => writeln!(f, "lmvar {dst}, {src}")?,
                I::Smvar { src, dst } => writeln!(f, "smvar {dst}, {src}")?,
                I::Lcap { dst, src, _c } => writeln!(f, "lcap {dst}, {src}")?,
                I::Scap { dst, src, _c } => writeln!(f, "scap {dst}, {src}")?,
                I::Lidx { dst, target, idx } => writeln!(f, "lidx {dst}, {target}, {idx}")?,
                I::Lidxn { dst, target, idx } => writeln!(
                    f,
                    "lidxn {dst}, {target}, {idx}",
                    idx = lit[idx.zx()].int().unwrap()
                )?,
                I::Sidx { target, idx, src } => writeln!(f, "sidx {target}, {idx}, {src}")?,
                I::Sidxn { target, idx, src } => writeln!(
                    f,
                    "sidxn {target}, {idx}, {src}",
                    idx = lit[idx.zx()].int().unwrap(),
                )?,
                I::Lkey { dst, target, key } => writeln!(f, "lkey {dst}, {target}, {key}")?,
                I::Lkeyc { dst, target, key } => writeln!(
                    f,
                    "lkeyc {dst}, {target}, {key}",
                    key = lit[key.zx()].str().unwrap()
                )?,
                I::Skey { target, key, src } => writeln!(f, "skey {target}, {key}, {src}")?,
                I::Skeyc { target, key, src } => writeln!(
                    f,
                    "skeyc {target}, {key}, {src}",
                    key = lit[key.zx()].str().unwrap()
                )?,

                I::Lnil { dst, _b, _c } => writeln!(f, "lnil {dst}")?,
                I::Lsmi { dst, v } => writeln!(f, "lsmi {dst}, {v}")?,
                I::Ltrue { dst, _b, _c } => writeln!(f, "ltrue {dst}")?,
                I::Lfalse { dst, _b, _c } => writeln!(f, "lfalse {dst}")?,
                I::Lint { dst, id } => {
                    let v = lit[id.zx()].int().unwrap();
                    writeln!(f, "lint {dst}, {id}   ; {id}={v}")?
                }
                I::Lnum { dst, id } => {
                    let v = lit[id.zx()].float().unwrap();
                    writeln!(f, "lnum {dst}, {id}   ; {id}={v}")?
                }
                I::Lstr { dst, id } => {
                    let v = lit[id.zx()].str().unwrap();
                    writeln!(f, "lstr {dst}, {id}   ; {id}={v:?}")?
                }
                I::Lcli { dst, id } => {
                    todo!()
                }
                I::Lfni { dst, id } => {
                    todo!()
                }
                I::Larr { dst, cap } => writeln!(f, "larr {dst}, {cap}")?,
                I::Lobj { dst, cap } => writeln!(f, "lobj {dst}, {cap}")?,
                I::Jmp { rel } => writeln!(f, "jmp {rel}   ; to {}", (i as isize) + rel.sz())?,
                I::Istrue { v, _b, _c } => writeln!(f, "istrue {v}")?,
                I::Istruec { dst, v, _c } => writeln!(f, "istruec {dst}, {v}")?,
                I::Isfalse { v, _b, _c } => writeln!(f, "isfalse {v}")?,
                I::Isfalsec { dst, v, _c } => writeln!(f, "isfalsec {dst}, {v}")?,
                I::Islt { lhs, rhs, _c } => writeln!(f, "islt {lhs}, {rhs}")?,
                I::Isle { lhs, rhs, _c } => writeln!(f, "isle {lhs}, {rhs}")?,
                I::Isgt { lhs, rhs, _c } => writeln!(f, "isgt {lhs}, {rhs}")?,
                I::Isge { lhs, rhs, _c } => writeln!(f, "isge {lhs}, {rhs}")?,
                I::Iseq { lhs, rhs, _c } => writeln!(f, "iseq {lhs}, {rhs}")?,
                I::Isne { lhs, rhs, _c } => writeln!(f, "isne {lhs}, {rhs}")?,
                I::Iseqs { lhs, rhs } => writeln!(f, "iseqs {lhs}, {}", &lit[rhs.zx()])?,
                I::Isnes { lhs, rhs } => writeln!(f, "isnes {lhs}, {}", &lit[rhs.zx()])?,
                I::Iseqn { lhs, rhs } => writeln!(f, "iseqn {lhs}, {}", &lit[rhs.zx()])?,
                I::Isnen { lhs, rhs } => writeln!(f, "isnen {lhs}, {}", &lit[rhs.zx()])?,
                I::Iseqp { lhs, rhs, _c } => writeln!(
                    f,
                    "iseqp {lhs}, {}",
                    match rhs.get() {
                        0 => "false",
                        1 => "true",
                        0xFF => "nil",
                        _ => unreachable!(),
                    }
                )?,
                I::Isnep { lhs, rhs, _c } => writeln!(
                    f,
                    "isnep {lhs}, {}",
                    match rhs.get() {
                        0 => "false",
                        1 => "true",
                        0xFF => "nil",
                        _ => unreachable!(),
                    }
                )?,
                I::Isltv { dst, lhs, rhs } => writeln!(f, "isltv {dst}, {lhs}, {rhs}")?,
                I::Islev { dst, lhs, rhs } => writeln!(f, "islev {dst}, {lhs}, {rhs}")?,
                I::Isgtv { dst, lhs, rhs } => writeln!(f, "isgtv {dst}, {lhs}, {rhs}")?,
                I::Isgev { dst, lhs, rhs } => writeln!(f, "isgev {dst}, {lhs}, {rhs}")?,
                I::Iseqv { dst, lhs, rhs } => writeln!(f, "iseqv {dst}, {lhs}, {rhs}")?,
                I::Isnev { dst, lhs, rhs } => writeln!(f, "isnev {dst}, {lhs}, {rhs}")?,
                I::Addvv { dst, lhs, rhs } => writeln!(f, "add {dst}, {lhs}, {rhs}")?,
                I::Addvn { dst, lhs, rhs } => writeln!(f, "add {dst}, {lhs}, {}", &lit[rhs.zx()])?,
                I::Addnv { dst, lhs, rhs } => writeln!(f, "add {dst}, {}, {rhs}", &lit[lhs.zx()])?,
                I::Subvv { dst, lhs, rhs } => writeln!(f, "sub {dst}, {lhs}, {rhs}")?,
                I::Subvn { dst, lhs, rhs } => writeln!(f, "sub {dst}, {lhs}, {}", &lit[rhs.zx()])?,
                I::Subnv { dst, lhs, rhs } => writeln!(f, "sub {dst}, {}, {rhs}", &lit[lhs.zx()])?,
                I::Mulvv { dst, lhs, rhs } => writeln!(f, "mul {dst}, {lhs}, {rhs}")?,
                I::Mulvn { dst, lhs, rhs } => writeln!(f, "mul {dst}, {lhs}, {}", &lit[rhs.zx()])?,
                I::Mulnv { dst, lhs, rhs } => writeln!(f, "mul {dst}, {}, {rhs}", &lit[lhs.zx()])?,
                I::Divvv { dst, lhs, rhs } => writeln!(f, "div {dst}, {lhs}, {rhs}")?,
                I::Divvn { dst, lhs, rhs } => writeln!(f, "div {dst}, {lhs}, {}", &lit[rhs.zx()])?,
                I::Divnv { dst, lhs, rhs } => writeln!(f, "div {dst}, {}, {rhs}", &lit[lhs.zx()])?,
                I::Unm { dst, rhs, _c } => writeln!(f, "unm {dst}, {rhs}")?,
                I::Not { dst, rhs, _c } => writeln!(f, "not {dst}, {rhs}")?,
                I::Call { dst, args, _c } => writeln!(f, "call {dst}, {args}   ; dyn")?,
                I::Fastcall { dst, id } => {
                    let name = module.functions().get(id.zx()).unwrap().name();
                    writeln!(f, "call {dst}, {id}   ; {id}={name}")?
                }
                I::Ret { _a, _b, _c } => writeln!(f, "ret")?,
                I::Stop { _a, _b, _c } => writeln!(f, "stop")?,
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
