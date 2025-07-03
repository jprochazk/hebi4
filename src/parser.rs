use std::marker::PhantomData;

use bumpalo::{
    Bump,
    collections::{CollectIn, Vec},
};

use crate::{
    ast::{
        self, AssignOp, Ast, Expr, ExprKind, InfixOp, Opt, Pack, PrefixOp, Stmt, spanned::*, u56,
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
    ast: Ast,
    loop_depth: u32,
}

impl<'t, 'src> Parser<'t, 'src> {
    fn new(tokens: &'t Tokens<'src>) -> Self {
        Self {
            cursor: tokens.cursor(),
            ast: Ast::new(tokens),
            loop_depth: 0,
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
    let body = body.as_slice();

    let root = Root { body }.pack(&mut p.ast);

    p.ast.set_root(root);

    Ok(p.ast)
}

fn parse_stmt(p: &mut Parser, buf: &Bump) -> Result<Spanned<Stmt>> {
    match p.kind() {
        t![var] => parse_stmt_var(p, buf),
        t![fn] => parse_stmt_func_decl(p, buf),
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
fn parse_stmt_func_decl(p: &mut Parser, buf: &Bump) -> Result<Spanned<Stmt>> {
    let bare = parse_bare_func(p, buf)?;

    let name = bare.name.ok_or_else(|| {
        let span = bare.params.span.start()..bare.params.span.start() + 1;
        error("expected function name", span)
    })?;
    let body = bare.body;
    let params = bare.params.as_slice();

    Ok(Spanned::new(FuncDecl { name, body, params }.pack(&mut p.ast), bare.span).map_into())
}

fn parse_stmt_loop(p: &mut Parser, buf: &Bump) -> Result<Spanned<Stmt>> {
    let node = p.open_at(t![loop])?;

    p.loop_depth += 1;

    let mut body = temp(buf);
    while !p.end() && !p.at(t![end]) {
        body.push(parse_stmt(p, buf)?);
    }
    let body = body.as_slice();
    p.must(t![end])?;

    p.loop_depth -= 1;

    Ok(p.close(node, Loop { body }).map_into())
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
        t![if] => parse_expr_if(p, buf, true),
        t![do] => parse_block(p, buf).map(|v| v.map_into()),
        t![fn] => parse_expr_func_anon(p, buf),
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
    let span = p.span();
    let node = p.open_at(t![break])?;

    if p.loop_depth == 0 {
        return error("`break` outside of a loop", span).into();
    }

    Ok(p.close(node, Break {}).map_into())
}

fn parse_expr_continue(p: &mut Parser, _: &Bump) -> Result<Spanned<Expr>> {
    let span = p.span();
    let node = p.open_at(t![continue])?;

    if p.loop_depth == 0 {
        return error("`continue` outside of a loop", span).into();
    }

    Ok(p.close(node, Continue {}).map_into())
}

fn parse_expr_if(p: &mut Parser, buf: &Bump, is_top_level: bool) -> Result<Spanned<Expr>> {
    // simple:
    //   if a do ... end
    //   if a do ... else ... end
    // multi:
    //   if a do ... else if b do ... end
    //   if a do ... else if b do ... else ... end

    let node_simple = p.open();
    let node_multi = p.open_at(t![if])?;

    let mut branches = temp(buf);
    let mut tail = None;

    branches.push(parse_bare_branch(p, buf)?);
    while !p.end() && p.at(t![else]) {
        let else_span = p.span();
        p.advance(); // else

        if p.eat(t![if]) {
            branches.push(parse_bare_branch(p, buf)?);
        } else if tail.is_none() {
            if p.at(t![do]) {
                // explicitly handle this so that stmt->expr->block doesnt pick it up
                return error("unexpected token: `do`", p.span()).into();
            }
            tail = Some(parse_if_tail(p, buf, else_span)?);
        } else {
            // duplicate tail
            return error("duplicate `else` branch", else_span).into();
        }
    }

    let end_span = p.span();
    p.must(t![end])?;

    let tail = tail
        .map(|tail| tail.map(Opt::some))
        .unwrap_or_else(|| Spanned::empty(Opt::none()));

    if tail.is_none() && !is_top_level {
        return error(
            "if expression without `else` is not allowed in this context",
            end_span,
        )
        .into();
    }

    match branches.len() {
        0 => unreachable!("always at least one branch"),
        1 => {
            let cond = branches[0].cond;
            let body = branches[0].body.as_slice();
            Ok(p.close(node_simple, IfSimple { cond, tail, body })
                .map_into())
        }

        _ => {
            let branches = branches
                .into_iter()
                .map(|b| b.map(|b| b.pack(p)))
                .collect_in::<Vec<'_, _>>(buf);
            let branches = branches.as_slice();
            Ok(p.close(node_multi, IfMulti { tail, branches }).map_into())
        }
    }
}

struct BareBranch<'bump> {
    cond: Spanned<Expr>,
    body: Vec<'bump, Spanned<Stmt>>,
}

impl BareBranch<'_> {
    fn pack(&self, p: &mut Parser) -> ast::Branch {
        Branch {
            cond: self.cond,
            body: self.body.as_slice(),
        }
        .pack(&mut p.ast)
    }
}

// <cond:expr> do <body:[stmt]> (?= else|end)
fn parse_bare_branch<'bump>(
    p: &mut Parser,
    buf: &'bump Bump,
) -> Result<Spanned<BareBranch<'bump>>> {
    let start = p.span().start();

    let cond = parse_expr(p, buf)?;

    p.must(t![do])?;

    let mut body = temp(buf);
    while !p.end() && !p.at(t![end]) && !p.at(t![else]) {
        body.push(parse_stmt(p, buf)?);
    }

    let end = p.span().end();
    Ok(Spanned::new(BareBranch { cond, body }, start..end))
}

fn parse_if_tail(p: &mut Parser, buf: &Bump, else_span: Span) -> Result<Spanned<ast::Block>> {
    assert!(p.cursor.kind(p.cursor.prev()) == t![else]);

    let start = else_span.start();
    let mut body = temp(buf);
    while !p.end() && !p.at(t![end]) {
        body.push(parse_stmt(p, buf)?);
    }
    let body = body.as_slice();
    // don't eat `end`
    let end = p.span().end();

    Ok(Spanned::new(Block { body }.pack(&mut p.ast), start..end))
}

fn parse_block(p: &mut Parser, buf: &Bump) -> Result<Spanned<ast::Block>> {
    let node = p.open_at(t![do])?;

    let mut body = temp(buf);
    while !p.end() && !p.at(t![end]) {
        body.push(parse_stmt(p, buf)?);
    }
    let body = body.as_slice();
    p.must(t![end])?;

    Ok(p.close(node, Block { body }))
}

fn parse_expr_func_anon(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let bare = parse_bare_func(p, buf)?;

    Ok(bare
        .map(|func| {
            FuncAnon {
                name: match func.name {
                    Some(name) => name.map(Opt::some),
                    None => Spanned::empty(Opt::none()),
                },
                body: func.body,
                params: func.params.as_slice(),
            }
            .pack(&mut p.ast)
        })
        .map_into())
}

struct BareFunc<'bump> {
    name: Option<Spanned<ast::Ident>>,
    params: Spanned<Vec<'bump, Spanned<ast::Ident>>>,
    body: Spanned<ast::Block>,
}

fn parse_bare_func<'bump>(p: &mut Parser, buf: &'bump Bump) -> Result<Spanned<BareFunc<'bump>>> {
    let start = p.span().start();
    p.must(t![fn])?;

    let name = if p.at(t![ident]) {
        Some(parse_ident(p, buf)?)
    } else {
        None
    };
    let params_start = p.span().start();
    let params = bracketed_list(p, buf, Brackets::Paren, parse_ident)?;
    let params_end = p.cursor.span(p.cursor.prev()).end();
    let params = Spanned::new(params, params_start..params_end);

    let prev_loop_depth = p.loop_depth;
    p.loop_depth = 0;
    let body = parse_block(p, buf)?;
    p.loop_depth = prev_loop_depth;

    let end = p.cursor.span(p.cursor.prev()).end();
    Ok(Spanned::new(BareFunc { name, params, body }, start..end))
}

fn parse_expr_assign(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let start = p.span().start();

    let lhs = parse_expr(p, buf)?;
    let op = match p.kind() {
        t![=] => AssignOp::None,
        t![+=] => AssignOp::Add,
        t![-=] => AssignOp::Sub,
        t![*=] => AssignOp::Mul,
        t![/=] => AssignOp::Div,
        _ => return Ok(lhs),
    };
    p.advance(); // eat op
    let value = parse_expr(p, buf)?;
    let end = p.cursor.span(p.cursor.prev()).end();
    let span = start..end;

    use ast::nodes::{Unpack as _, parts::Expr as E};
    match unsafe { lhs.unpack(&p.ast) } {
        E::GetIndex(base) => {
            let base = lhs.map(|_| base);
            Ok(Spanned::new(SetIndex { base, value, op }.pack(&mut p.ast), span).map_into())
        }
        E::GetField(base) => {
            let base = lhs.map(|_| base);
            Ok(Spanned::new(SetField { base, value, op }.pack(&mut p.ast), span).map_into())
        }
        E::GetVar(base) => {
            let base = lhs.map(|_| base);
            Ok(Spanned::new(SetVar { base, value, op }.pack(&mut p.ast), span).map_into())
        }
        _ => error("invalid assignment target", lhs.span).into(),
    }
}

fn parse_expr(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    match p.kind() {
        t![return] => parse_expr_return(p, buf),
        t![break] => parse_expr_break(p, buf),
        t![continue] => parse_expr_continue(p, buf),
        t![if] => parse_expr_if(p, buf, false),
        t![do] => parse_block(p, buf).map(|v| v.map_into()),
        t![fn] => parse_expr_func_anon(p, buf),
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
    ($([$token:tt] => $into:expr),* $(,)*) => {
        |tok| match tok {
            $(t![$token] => Some($into),)*
            _ => None,
        }
    };
}

fn parse_expr_or(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [or] => InfixOp::Or,
    };
    let next = parse_expr_and;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_and(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [and] => InfixOp::And,
    };
    let next = parse_expr_eq;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_eq(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [==] => InfixOp::Eq,
        [!=] => InfixOp::Ne,
    };
    let next = parse_expr_cmp;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_cmp(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [>] => InfixOp::Gt,
        [>=] => InfixOp::Ge,
        [<] => InfixOp::Lt,
        [<=] => InfixOp::Le,
    };
    let next = parse_expr_add;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_add(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [+] => InfixOp::Add,
        [-] => InfixOp::Sub,
    };
    let next = parse_expr_mul;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_mul(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [*] => InfixOp::Mul,
        [/] => InfixOp::Div,
    };
    let next = parse_expr_prefix;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_prefix(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let op = match p.kind() {
        t![-] => PrefixOp::Minus,
        t![not] => PrefixOp::Not,
        _ => return parse_expr_postfix(p, buf),
    };
    let node = p.open();
    p.advance(); // eat op
    let rhs = parse_expr_prefix(p, buf)?;
    Ok(p.close(node, Prefix { rhs, op }).map_into())
}

fn parse_expr_postfix(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let expr = parse_expr_primary(p, buf)?;
    if !matches!(
        expr.kind(),
        ExprKind::GetVar
            | ExprKind::GetField
            | ExprKind::GetIndex
            | ExprKind::Call
            | ExprKind::CallObject,
    ) {
        return Ok(expr);
    }

    let mut expr = expr;
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

fn parse_expr_call(p: &mut Parser, buf: &Bump, callee: Spanned<Expr>) -> Result<Spanned<Expr>> {
    let node = p.open();

    let args = bracketed_list(p, buf, Brackets::Paren, parse_expr)?;
    let args = args.as_slice();

    Ok(p.close(node, Call { callee, args }).map_into())
}

fn parse_expr_call_object(
    p: &mut Parser,
    buf: &Bump,
    callee: Spanned<Expr>,
) -> Result<Spanned<Expr>> {
    let node = p.open();

    let args = bracketed_list(p, buf, Brackets::Curly, parse_object_entry)?;
    let args = args.as_slice();

    Ok(p.close(node, CallObject { callee, args }).map_into())
}

fn parse_object_entry(p: &mut Parser, buf: &Bump) -> Result<Spanned<ast::ObjectEntry>> {
    let node = p.open();

    let (key, value) = match p.kind() {
        t![ident] => {
            let span = p.span();
            let lexeme = p.lexeme();
            p.advance();

            let key = str_from_lexeme_span(p, lexeme, span);

            let value: Spanned<Expr> = if p.eat(t![=]) {
                parse_expr(p, buf)?
            } else {
                Spanned::new(
                    GetVar {
                        name: ident_from_lexeme_span(p, lexeme, span),
                    }
                    .pack(&mut p.ast),
                    span,
                )
                .map_into()
            };

            (key, value)
        }
        t![str] => {
            let key = parse_str(p, buf)?;
            p.must(t![=])?;
            let value = parse_expr(p, buf)?;

            (key, value)
        }
        _ => return error("unexpected token", p.span()).into(),
    };

    Ok(p.close(node, ObjectEntry { key, value }))
}

fn str_from_lexeme_span(p: &mut Parser, lexeme: &str, span: Span) -> Spanned<ast::Str> {
    Spanned::new(
        Str {
            value: p.ast.intern_str(lexeme),
        }
        .pack(&mut p.ast),
        span,
    )
}

fn ident_from_lexeme_span(p: &mut Parser, lexeme: &str, span: Span) -> Spanned<ast::Ident> {
    Spanned::new(
        Ident {
            id: p.ast.intern_ident(lexeme),
        }
        .pack(&mut p.ast),
        span,
    )
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
    let node = p.open();

    let items = bracketed_list(p, buf, Brackets::Brace, parse_expr)?;
    let items = items.as_slice();

    Ok(p.close(node, Array { items }).map_into())
}

fn parse_expr_object(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();

    let entries = bracketed_list(p, buf, Brackets::Curly, parse_object_entry)?;
    let entries = entries.as_slice();

    Ok(p.close(node, Object { entries }).map_into())
}

fn parse_expr_int(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();

    let lexeme = p.lexeme();
    let span = p.span();
    p.must(t![int])?;
    let value: u64 = lexeme
        .parse()
        .map_err(|err| error(format!("invalid int: {err}"), span))?;
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
    let node_f32 = p.open();
    let node_f64 = p.open();

    let lexeme = p.lexeme();
    let span = p.span();
    p.must(t![float])?;
    let value: f64 = lexeme
        .parse()
        .map_err(|err| error(format!("invalid float: {err}"), span))?;

    if (value as f32) as f64 == value {
        Ok(p.close(
            node_f32,
            Float32 {
                value: value as f32,
            },
        )
        .map_into())
    } else {
        let value = p.ast.intern_float(value);
        Ok(p.close(node_f64, Float64 { value }).map_into())
    }
}

fn parse_expr_bool(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();

    let value = match p.kind() {
        t![true] => true,
        t![false] => false,
        _ => return error("not a bool", p.span()).into(),
    };
    p.advance();

    Ok(p.close(node, Bool { value }).map_into())
}

fn parse_expr_str(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    parse_str(p, buf).map(|v| v.map_into())
}

fn parse_str(p: &mut Parser, buf: &Bump) -> Result<Spanned<ast::Str>> {
    let node = p.open();

    let span = p.span();
    let lexeme = p.lexeme();

    p.must(t![str])?;
    let value = lexeme
        .strip_prefix('"')
        .ok_or_else(|| error("invalid string", span))?
        .strip_suffix('"')
        .ok_or_else(|| error("invalid string", span))?;

    let value = match escape::unescape(value) {
        Ok(value) => value,
        Err(err) => {
            return error(
                "invalid escape",
                span.start() + err.pos - 1..span.start() + err.pos + 2,
            )
            .into();
        }
    };
    let value = p.ast.intern_str(value.as_ref());

    Ok(p.close(node, Str { value }))
}

mod escape;

fn parse_expr_nil(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();
    p.must(t![nil])?;

    Ok(p.close(node, Nil {}).map_into())
}

fn parse_expr_use(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();
    let name = parse_ident(p, buf)?;

    Ok(p.close(node, GetVar { name }).map_into())
}

fn parse_expr_group(p: &mut Parser, buf: &Bump) -> Result<Spanned<Expr>> {
    p.must(t!["("])?;
    let inner = parse_expr(p, buf)?;
    p.must(t![")"])?;
    Ok(inner)
}

enum Brackets {
    Paren,
    Brace,
    Curly,
}

#[inline]
fn bracketed_list<'bump, F, T>(
    p: &mut Parser,
    buf: &'bump Bump,
    brackets: Brackets,
    f: F,
) -> Result<Vec<'bump, T>>
where
    F: Fn(&mut Parser, &Bump) -> Result<T>,
{
    let (opening, closing) = match brackets {
        Brackets::Paren => (t!["("], t![")"]),
        Brackets::Brace => (t!["["], t!["]"]),
        Brackets::Curly => (t!["{"], t!["}"]),
    };

    p.must(opening)?;
    let mut out = temp(buf);
    if !p.end() && !p.at(closing) {
        out.push(f(p, buf)?);
        while !p.end() && p.eat(t![,]) && !p.at(closing) {
            out.push(f(p, buf)?);
        }
    }
    p.must(closing)?;
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
