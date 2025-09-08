// use crate::{error::Location, span::Span};

use super::{Chunk, FuncInfo, dbg::Local};

impl Chunk {
    pub fn disasm<'a>(&'a self, src: &'a str) -> Disasm<'a> {
        Disasm { chunk: self, src }
    }
}

pub struct Disasm<'a> {
    chunk: &'a Chunk,
    src: &'a str,
}

impl<'a> std::fmt::Display for Disasm<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let functions = &*self.chunk.functions;
        for func in functions {
            std::fmt::Display::fmt(
                &DisasmFunc {
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
        } = &self.func;

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
                I::Lcli { dst, id } => todo!(),
                I::Lfni { dst, id } => todo!(),
                I::Larr { dst, len, _c } => todo!(),
                I::Lobj { dst, len, _c } => todo!(),
                I::Jmp { rel } => writeln!(f, "jmp {rel}")?,
                I::Istrue { v, _b, _c } => todo!(),
                I::Istruec { dst, v, _c } => todo!(),
                I::Isfalse { v, _b, _c } => todo!(),
                I::Isfalsec { dst, v, _c } => todo!(),
                I::Islt { lhs, rhs, _c } => todo!(),
                I::Isle { lhs, rhs, _c } => todo!(),
                I::Isgt { lhs, rhs, _c } => todo!(),
                I::Isge { lhs, rhs, _c } => todo!(),
                I::Iseq { lhs, rhs, _c } => todo!(),
                I::Isne { lhs, rhs, _c } => todo!(),
                I::Iseqs { lhs, rhs } => todo!(),
                I::Isnes { lhs, rhs } => todo!(),
                I::Iseqn { lhs, rhs } => todo!(),
                I::Isnen { lhs, rhs } => todo!(),
                I::Iseqp { lhs, rhs, _c } => todo!(),
                I::Isnep { lhs, rhs, _c } => todo!(),
                I::Addvv { dst, lhs, rhs } => todo!(),
                I::Addvn { dst, lhs, rhs } => todo!(),
                I::Addnv { dst, lhs, rhs } => todo!(),
                I::Subvv { dst, lhs, rhs } => todo!(),
                I::Subvn { dst, lhs, rhs } => todo!(),
                I::Subnv { dst, lhs, rhs } => todo!(),
                I::Mulvv { dst, lhs, rhs } => todo!(),
                I::Mulvn { dst, lhs, rhs } => todo!(),
                I::Mulnv { dst, lhs, rhs } => todo!(),
                I::Divvv { dst, lhs, rhs } => todo!(),
                I::Divvn { dst, lhs, rhs } => todo!(),
                I::Divnv { dst, lhs, rhs } => todo!(),
                I::Unm { dst, rhs, _c } => todo!(),
                I::Not { dst, rhs, _c } => todo!(),
                I::Call { dst, func, args } => todo!(),
                I::Fastcall { dst, id } => todo!(),
                I::Ret { _a, _b, _c } => writeln!(f, "ret")?,
                I::Stop { _a, _b, _c } => writeln!(f, "stop")?,
                I::Trap { _a, _b, _c } => writeln!(f, "trap")?,
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
