use std::marker::PhantomData;

use bumpalo::{Bump, collections::Vec};

use crate::{
    ast::{
        self, Ast, AstBuilder, Expr, ExprKind, InfixOp, Opt, Pack, PackedNode, PrefixOp, Stmt,
        spanned::*, u56,
    },
    error::{Error, error},
    span::{Span, Spanned},
    token::{Token, TokenCursor, TokenKind, Tokens},
};

type Result<T, E = Error> = std::result::Result<T, E>;

pub fn parse(tokens: &Tokens<'_>) -> Result<Ast> {
    let parser = Parser::new(tokens);
    parse_root(parser)
}

struct Parser<'t, 'src> {
    cursor: TokenCursor<'src, 't>,
    ast: AstBuilder,
}

impl<'t, 'src> Parser<'t, 'src> {
    fn new(tokens: &'t Tokens<'src>) -> Self {
        Self {
            cursor: tokens.cursor(),
            ast: AstBuilder::new(tokens),
        }
    }

    #[inline]
    fn kind(&self) -> TokenKind {
        let token = self.cursor.current();
        self.cursor.kind(token)
    }

    #[inline]
    fn current(&self) -> Token {
        self.cursor.current()
    }

    #[inline]
    fn peek(&self) -> TokenKind {
        let token = self.cursor.peek();
        self.cursor.kind(token)
    }

    #[inline]
    fn lexeme(&self) -> &'src str {
        self.cursor.lexeme(self.cursor.current())
    }

    #[inline]
    fn span(&self) -> Span {
        self.cursor.span(self.cursor.current())
    }

    #[inline]
    fn advance(&mut self) {
        self.cursor.advance();
    }

    #[inline]
    fn end(&self) -> bool {
        self.at(t![EOF])
    }

    /// Iff current token is `kind`, returns `true`.
    ///
    /// Does not advance.
    #[inline]
    fn at(&self, kind: TokenKind) -> bool {
        self.kind() == kind
    }

    /// Iff current token is `kind` advances and returns `true`,
    /// otherwise returns `false` without advancing.
    #[inline]
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Iff current token is `kind`, returns `Ok(token)` and advances,
    /// otherwise returns `Err` without advancing.
    #[inline]
    fn must(&mut self, kind: TokenKind) -> Result<Token> {
        let tok = self.cursor.current();
        if self.eat(kind) {
            Ok(tok)
        } else {
            error(
                format!(
                    "expected '{}', found '{}'",
                    kind.bare_lexeme(),
                    self.cursor.lexeme(tok)
                ),
                self.cursor.span(tok),
            )
            .into()
        }
    }

    fn open<T: Pack>(&mut self) -> Marker<T> {
        Marker {
            start: self.cursor.span(self.cursor.current()).start,
            ty: PhantomData,
        }
    }

    fn open_at<T: Pack>(&mut self, kind: TokenKind) -> Result<Marker<T>> {
        let tok = self.must(kind)?;
        Ok(Marker {
            start: self.cursor.span(tok).start,
            ty: PhantomData,
        })
    }

    #[inline]
    fn close<T: Pack>(&mut self, marker: Marker<T>, node: T) -> Spanned<T::Node> {
        let tok = self.cursor.prev();
        let span = Span {
            start: marker.start,
            end: self.cursor.span(tok).end,
        };
        Spanned::new(Pack::pack(node, &mut self.ast), span)
    }
}

#[derive(Clone, Copy)]
struct Marker<T> {
    start: u32,
    ty: PhantomData<T>,
}

/// Allocate a vec with some small initial capacity in `buf`
fn temp<T>(buf: &Bump) -> Vec<'_, T> {
    // *shrug*
    Vec::with_capacity_in(16, buf)
}

fn parse_root(mut p: Parser) -> Result<Ast> {
    let buf = Bump::new();

    let mut body = temp(&buf);
    while !p.end() {
        body.push(parse_stmt(&mut p, &buf)?);
    }

    let root = Root {
        body: body.as_slice(),
    }
    .pack(&mut p.ast);

    Ok(p.ast.build(root))
}

fn parse_stmt(p: &mut Parser, buf: &Bump) -> Result<Spanned<Stmt>> {
    match p.kind() {
        t![var] => parse_stmt_var(p, buf),
        t![fn] => parse_stmt_fn(p, buf),
        t![loop] => parse_stmt_loop(p, buf),
        _ => parse_stmt_expr(p, buf),
    }
}

/// `"var" name:IDENT "=" value:EXPR`
///
/// `p` must be at "var"
fn parse_stmt_var(p: &mut Parser, buf: &Bump) -> Result<Spanned<Stmt>> {
    let node = p.open_at(t![var])?;

    let name = parse_ident(p, buf)?;
    p.must(t![=])?;
    let value = parse_expr(p, buf)?;

    Ok(p.close(node, Var { name, value }).map_into())
}

/// `"fn" name:IDENT "(" param:IDENT,* ")" "do" stmt* "end"`
fn parse_stmt_fn(p: &mut Parser, buf: &Bump) -> Result<Spanned<Stmt>> {
    let stmt_node = p.open();
    let func_node = p.open_at(t![fn])?;

    let name = parse_ident(p, buf)?;
    let params = paren_list(p, buf, parse_ident)?;
    let body = parse_block(p, buf)?;

    let func = p.close(
        func_node,
        Func {
            name,
            body,
            params: params.as_slice(),
        },
    );
    let inner = func.map_into();

    Ok(p.close(stmt_node, StmtExpr { inner }).map_into())
}

fn parse_stmt_loop(p: &mut Parser, buf: &Bump) -> Result<Spanned<Stmt>> {
    let node = p.open_at(t![loop])?;

    let mut body = temp(buf);
    while !p.end() && !p.at(t![end]) {
        body.push(parse_stmt(p, buf)?);
    }
    p.must(t![end])?;

    Ok(p.close(
        node,
        Loop {
            body: body.as_slice(),
        },
    )
    .map_into())
}

fn parse_stmt_expr(p: &mut Parser, buf: &Bump) -> Result<Spanned<Stmt>> {
    let node = p.open();

    let inner = parse_expr_top_level(p, buf)?;

    Ok(p.close(node, StmtExpr { inner }).map_into())
}

fn parse_ident(p: &mut Parser, _: &Bump) -> Result<Spanned<ast::Ident>> {
    let node = p.open();
    let lexeme = p.lexeme();
    p.must(t![ident])?;
    let id = p.ast.intern_ident(lexeme);
    Ok(p.close(node, Ident { id }))
}

fn parse_expr_top_level(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    match p.kind() {
        t![return] => parse_expr_return(p, buf),
        t![break] => parse_expr_break(p, buf),
        t![continue] => parse_expr_continue(p, buf),
        t![if] => parse_expr_if(p, buf),
        t![do] => parse_block(p, buf).map(|v| v.map_into()),
        t![fn] => parse_expr_fn(p, buf),
        _ => parse_expr_assign(p, buf),
    }
}

fn parse_expr_return(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open_at(t![return])?;

    let value: Spanned<Opt<Expr>> = if can_begin_expr(p) {
        parse_expr(p, buf)?.map(Opt::some)
    } else {
        Spanned::new(Opt::none(), Span::empty())
    };

    Ok(p.close(node, Return { value }).map_into())
}

fn parse_expr_break(p: &mut Parser, _: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open_at(t![break])?;

    Ok(p.close(node, Break {}).map_into())
}

fn parse_expr_continue(p: &mut Parser, _: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open_at(t![continue])?;

    Ok(p.close(node, Continue {}).map_into())
}

fn parse_expr_if(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    assert!(p.eat(t![if]));

    todo!("if expr")
}

fn parse_block(p: &mut Parser, buf: &Bump) -> Result<Spanned<ast::Block>> {
    let node = p.open_at(t![do])?;

    let mut body = temp(buf);
    while !p.end() && !p.at(t![end]) {
        body.push(parse_stmt(p, buf)?);
    }
    p.must(t![end])?;

    Ok(p.close(
        node,
        Block {
            body: body.as_slice(),
        },
    ))
}

fn parse_expr_fn(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    assert!(p.eat(t![fn]));

    todo!("fn expr")
}

fn parse_expr_assign(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let lhs = parse_expr(p, buf)?;
    let op = match p.kind() {
        t![=] => None,
        t![+=] => Some(InfixOp::Add),
        t![-=] => Some(InfixOp::Sub),
        t![*=] => Some(InfixOp::Mul),
        t![/=] => Some(InfixOp::Div),
        _ => return Ok(lhs),
    };
    p.advance(); // eat op
    let value = parse_expr(p, buf)?;

    use ExprKind as E;
    match lhs.kind() {
        E::GetIndex => todo!("index assignment"),
        E::GetField => todo!("field assignment"),
        E::GetVar => todo!("variable assignment"),
        _ => error("invalid assignment target", lhs.span).into(),
    }
}

fn parse_expr(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    match p.kind() {
        t![return] => parse_expr_return(p, buf),
        t![break] => parse_expr_break(p, buf),
        t![continue] => parse_expr_continue(p, buf),
        _ => parse_expr_infix(p, buf),
    }
}

fn parse_expr_infix(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    parse_expr_or(p, buf)
}

#[inline]
fn parse_binop<F, E>(p: &mut Parser, buf: &Bump, token_to_op: F, next: E) -> Result<Spanned<Expr>>
where
    F: Fn(TokenKind) -> Option<InfixOp>,
    E: Fn(&mut Parser, &Bump) -> Result<Spanned<Expr>>,
{
    let mut lhs = next(p, buf)?;
    while !p.end() {
        let op = match token_to_op(p.kind()) {
            Some(op) => op,
            None => break,
        };
        p.advance(); // eat op
        let rhs = next(p, buf)?;
        lhs = Spanned::new(
            Infix { lhs, rhs, op }.pack(&mut p.ast),
            lhs.span.to(rhs.span),
        )
        .map_into();
    }
    Ok(lhs)
}

macro_rules! token_map {
    ($($token:tt => $into:expr),* $(,)*) => {
        |tok| match tok {
            $(t![$token] => Some($into),)*
            _ => None,
        }
    };
}

fn parse_expr_or(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        or => InfixOp::Or,
    };
    let next = parse_expr_and;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_and(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        and => InfixOp::And,
    };
    let next = parse_expr_eq;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_eq(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        == => InfixOp::Eq,
        != => InfixOp::Ne,
    };
    let next = parse_expr_cmp;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_cmp(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        > => InfixOp::Gt,
        >= => InfixOp::Ge,
        < => InfixOp::Lt,
        <= => InfixOp::Le,
    };
    let next = parse_expr_add;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_add(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        + => InfixOp::Add,
        - => InfixOp::Sub,
    };
    let next = parse_expr_mul;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_mul(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        * => InfixOp::Mul,
        / => InfixOp::Div,
    };
    let next = parse_expr_prefix;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_prefix(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let op = match p.kind() {
        t![-] => PrefixOp::Minus,
        t![not] => PrefixOp::Minus,
        _ => return parse_expr_postfix(p, buf),
    };
    let node = p.open();
    p.advance(); // eat op
    let rhs = parse_expr_prefix(p, buf)?;
    Ok(p.close(node, Prefix { rhs, op }).map_into())
}

fn parse_expr_postfix(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let mut expr = parse_expr_primary(p, buf)?;
    while !p.end() {
        match p.kind() {
            t!["("] => expr = parse_expr_call(p, buf, expr)?,
            t!["{"] => expr = parse_expr_call_object(p, buf, expr)?,
            t!["["] => expr = parse_expr_index(p, buf, expr)?,
            t![.] => expr = parse_expr_field(p, buf, expr)?,
            _ => break,
        }
    }
    Ok(expr)
}

fn parse_expr_call(p: &mut Parser, buf: &Bump, parent: Spanned<Expr>) -> Result<Spanned<Expr>> {
    todo!("call expr")
}

fn parse_expr_call_object(
    p: &mut Parser,
    buf: &Bump,
    parent: Spanned<Expr>,
) -> Result<Spanned<Expr>> {
    todo!("call object expr")
}

fn parse_expr_index(p: &mut Parser, buf: &Bump, parent: Spanned<Expr>) -> Result<Spanned<Expr>> {
    let node = p.open_at(t!["["])?;

    let key = parse_expr(p, buf)?;
    p.must(t!["]"])?;

    Ok(p.close(node, GetIndex { parent, key }).map_into())
}

fn parse_expr_field(p: &mut Parser, buf: &Bump, parent: Spanned<Expr>) -> Result<Spanned<Expr>> {
    let node = p.open_at(t![.])?;

    let key = parse_ident(p, buf)?;

    Ok(p.close(node, GetField { parent, key }).map_into())
}

fn parse_expr_primary(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    match p.kind() {
        t!["["] => parse_expr_array(p, buf),
        t!["{"] => parse_expr_object(p, buf),
        t![int] => parse_expr_int(p, buf),
        t![float] => parse_expr_float(p, buf),
        t![true] | t![false] => parse_expr_bool(p, buf),
        t![str] => parse_expr_str(p, buf),
        t![nil] => parse_expr_nil(p, buf),
        t![ident] => parse_expr_use(p, buf),
        t!["("] => parse_expr_group(p, buf),
        _ => error(format!("unexpected token {:?}", p.lexeme()), p.span()).into(),
    }
}

fn parse_expr_array(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    todo!("array expr")
}

fn parse_expr_object(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    todo!("object expr")
}

fn parse_expr_int(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();

    let lexeme = p.lexeme();
    let span = p.span();
    p.must(t![int])?;
    let value: u64 = lexeme
        .parse()
        .map_err(|err| error(format!("failed to parse int: {err}"), span))?;
    if value > u56::MAX.get() {
        return error(
            format!(
                "int ({}) is larger than u56::MAX ({})",
                value,
                u56::MAX.get()
            ),
            span,
        )
        .into();
    }
    let value = u56::new(value);

    Ok(p.close(node, Int { value }).map_into())
}

fn parse_expr_float(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    todo!("float expr")
}

fn parse_expr_bool(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    todo!("bool expr")
}

fn parse_expr_str(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    todo!("str expr")
}

fn parse_expr_nil(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    todo!("nil expr")
}

fn parse_expr_use(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    todo!("variable use expr")
}

fn parse_expr_group(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    todo!("group expr")
}

#[inline]
fn paren_list<'bump, F, T>(p: &mut Parser, buf: &'bump Bump, f: F) -> Result<Vec<'bump, T>>
where
    F: Fn(&mut Parser, &Bump) -> Result<T>,
{
    p.must(t!["("])?;
    let mut out = temp(buf);
    if !p.end() && !p.at(t![")"]) {
        out.push(f(p, buf)?);
        while !p.end() && p.eat(t![,]) && !p.at(t![")"]) {
            out.push(f(p, buf)?);
        }
    }
    p.must(t![")"])?;
    Ok(out)
}

#[inline]
fn can_begin_expr(p: &mut Parser) -> bool {
    matches!(
        p.kind(),
        t![return]
            | t![do]
            | t![break]
            | t![continue]
            | t![if]
            | t!["("]
            | t!["["]
            | t!["{"]
            | t![ident]
            | t![int]
            | t![float]
            | t![str]
            | t![true]
            | t![false]
            | t![nil]
    )
}

#[cfg(test)]
mod tests;
