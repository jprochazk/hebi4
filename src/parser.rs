use std::marker::PhantomData;

use bumpalo::{
    Bump,
    collections::{CollectIn, Vec},
};

use crate::{
    ast::{
        self, AssignOp, Ast, Expr, InfixOp, NodeKind, Opt, Pack, Packed, PrefixOp, Stmt, spanned::*,
    },
    error::{Result, error_span},
    span::{Span, Spanned},
    token::{Token, TokenCursor, TokenKind, Tokens},
};

pub fn parse(tokens: &Tokens<'_>) -> Result<Ast> {
    let parser = State::new(tokens);
    parse_root(parser)
}

struct State<'t, 'src> {
    cursor: TokenCursor<'src, 't>,
    ast: Ast,
    loop_depth: u32,
}

impl<'t, 'src> State<'t, 'src> {
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
            error_span(
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

    fn open_extend<T: Pack>(&mut self, parent: Span) -> Marker<T> {
        Marker {
            start: parent.start,
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

fn parse_root(mut p: State) -> Result<Ast> {
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

fn parse_stmt(p: &mut State, buf: &Bump) -> Result<Spanned<Stmt>> {
    match p.kind() {
        t![let] => parse_stmt_var(p, buf),
        t![fn] => parse_stmt_func_decl(p, buf),
        t![loop] => parse_stmt_loop(p, buf),
        t![import] => parse_stmt_import(p, buf),
        _ => parse_stmt_expr(p, buf),
    }
}

/// `"let" name:IDENT "=" value:EXPR`
///
/// `p` must be at "let"
fn parse_stmt_var(p: &mut State, buf: &Bump) -> Result<Spanned<Stmt>> {
    let node = p.open_at(t![let])?;

    let name = parse_ident(p, buf)?;
    p.must(t![=])?;
    let value = parse_expr(p, buf)?;

    Ok(p.close(node, Var { name, value }).map_into())
}

/// `"fn" name:IDENT "(" param:IDENT,* ")" "{" stmt* "}"`
fn parse_stmt_func_decl(p: &mut State, buf: &Bump) -> Result<Spanned<Stmt>> {
    let bare = parse_bare_func(p, buf)?;

    let name = bare.name.ok_or_else(|| {
        let span = bare.params.span.start()..bare.params.span.start() + 1;
        error_span("expected function name", span)
    })?;
    let body = bare.body;
    let params = bare.params.as_slice();

    if params.len() > 100 {
        return error_span("too many parameters, maximum is 100", name.span).into();
    }

    Ok(Spanned::new(FuncDecl { name, body, params }.pack(&mut p.ast), bare.span).map_into())
}

fn parse_stmt_loop(p: &mut State, buf: &Bump) -> Result<Spanned<Stmt>> {
    let node = p.open_at(t![loop])?;

    p.loop_depth += 1;

    let body = parse_bare_block(p, buf)?;
    let body = body.as_slice();

    p.loop_depth -= 1;

    Ok(p.close(node, Loop { body }).map_into())
}

/// - `"import" IDENT`
/// - `"import" IDENT "from" STRING|IDENT`
/// - `"import" { import_item,+ } "from" STRING|IDENT`
///
/// `p` must be at "import"
fn parse_stmt_import(p: &mut State, buf: &Bump) -> Result<Spanned<Stmt>> {
    let import_token_span = p.span();
    p.must(t![import])?;

    if p.at(t!["{"]) {
        parse_stmt_import_named(p, buf, import_token_span)
    } else {
        parse_stmt_import_bare(p, buf, import_token_span)
    }
}

/// `"import" { import_item,+ } "from" STRING`
///
/// `p` must be after "import" keyword
fn parse_stmt_import_named(
    p: &mut State,
    buf: &Bump,
    import_token_span: Span,
) -> Result<Spanned<Stmt>> {
    let mut node = p.open();
    node.start = import_token_span.start;

    let items = bracketed_list(p, buf, Brackets::Curly, parse_import_item)?;

    p.must(t![from])?;
    let path = parse_str_or_ident_as_str(p, buf)?;

    let items = items.as_slice();

    Ok(p.close(node, Import { items, path }).map_into())
}

/// - `"import" IDENT`
/// - `"import" IDENT "from" STRING|IDENT`
///
/// `p` must be after "import" keyword
fn parse_stmt_import_bare(
    p: &mut State,
    buf: &Bump,
    import_token_span: Span,
) -> Result<Spanned<Stmt>> {
    let mut node = p.open();
    node.start = import_token_span.start;

    let binding = parse_ident(p, buf)?;
    let path = if p.eat(t![from]) {
        parse_str_or_ident_as_str(p, buf)?
    } else {
        ident_to_str(p, buf, binding)
    };

    Ok(p.close(node, ImportBare { path, binding }).map_into())
}

/// `IDENT` or `IDENT "as" IDENT`
fn parse_import_item(p: &mut State, buf: &Bump) -> Result<Spanned<ast::ImportItem>> {
    let node = p.open();

    let name = parse_ident(p, buf)?;
    let alias = if p.eat(t![as]) {
        parse_ident(p, buf)?.map(Opt::some)
    } else {
        Spanned::empty(Opt::none())
    };

    Ok(p.close(node, ImportItem { name, alias }))
}

fn parse_stmt_expr(p: &mut State, buf: &Bump) -> Result<Spanned<Stmt>> {
    let node = p.open();

    let inner = parse_expr_top_level(p, buf)?;

    Ok(p.close(node, StmtExpr { inner }).map_into())
}

fn parse_expr_top_level(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    match p.kind() {
        t![return] => parse_expr_return(p, buf),
        t![break] => parse_expr_break(p, buf),
        t![continue] => parse_expr_continue(p, buf),
        t![if] => parse_expr_if(p, buf, true),
        t![do] => parse_do_expr(p, buf),
        t![fn] => parse_expr_func_anon(p, buf),
        _ => parse_expr_assign(p, buf),
    }
}

fn parse_expr_return(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open_at(t![return])?;

    // Check for ambiguous `return fn`
    if p.at(t![fn]) {
        return error_span("ambiguous", p.span())
            .with_help("disambiguate with `return (fn ...)` or `return nil fn ...`")
            .into();
    }

    let value: Spanned<Opt<Expr>> = if can_begin_expr(p) {
        parse_expr(p, buf)?.map(Opt::some)
    } else {
        Spanned::new(Opt::none(), Span::empty())
    };

    Ok(p.close(node, Return { value }).map_into())
}

fn parse_expr_break(p: &mut State, _: &Bump) -> Result<Spanned<Expr>> {
    let span = p.span();
    let node = p.open_at(t![break])?;

    if p.loop_depth == 0 {
        return error_span("`break` outside of a loop", span).into();
    }

    Ok(p.close(node, Break {}).map_into())
}

fn parse_expr_continue(p: &mut State, _: &Bump) -> Result<Spanned<Expr>> {
    let span = p.span();
    let node = p.open_at(t![continue])?;

    if p.loop_depth == 0 {
        return error_span("`continue` outside of a loop", span).into();
    }

    Ok(p.close(node, Continue {}).map_into())
}

fn parse_expr_if(p: &mut State, buf: &Bump, is_top_level: bool) -> Result<Spanned<Expr>> {
    // simple:
    //   if a { ... }
    //   if a { ... } else { ... }
    // multi:
    //   if a { ... } else if b { ... }
    //   if a { ... } else if b { ... } else { ... }

    let node_simple = p.open();
    let node_multi = p.open_at(t![if])?;

    let mut branches = temp(buf);
    let mut tail = None;

    branches.push(parse_bare_branch(p, buf)?);
    while p.at(t![else]) {
        let else_span = p.span();
        p.advance(); // else

        if p.eat(t![if]) {
            branches.push(parse_bare_branch(p, buf)?);
        } else if tail.is_none() {
            tail = Some(parse_block(p, buf)?);
        } else {
            // duplicate tail
            return error_span("duplicate `else` branch", else_span).into();
        }
    }

    let end_span = p.span();

    let tail = tail
        .map(|tail| tail.map(Opt::some))
        .unwrap_or_else(|| Spanned::empty(Opt::none()));

    if tail.is_none() && !is_top_level {
        return error_span("expected `else`", end_span).into();
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
    fn pack(&self, p: &mut State) -> ast::Branch {
        Branch {
            cond: self.cond,
            body: self.body.as_slice(),
        }
        .pack(&mut p.ast)
    }
}

// <cond:expr> { <body:[stmt]> } (?= else)
fn parse_bare_branch<'bump>(p: &mut State, buf: &'bump Bump) -> Result<Spanned<BareBranch<'bump>>> {
    let start = p.span().start();

    let cond = parse_expr(p, buf)?;
    let body = parse_bare_block(p, buf)?;

    let end = p.span().end();
    Ok(Spanned::new(BareBranch { cond, body }, start..end))
}

fn parse_bare_block<'b>(p: &mut State, buf: &'b Bump) -> Result<Vec<'b, Spanned<Stmt>>> {
    p.must(t!["{"])?;
    let mut body = temp(buf);
    if !p.at(t!["}"]) {
        loop {
            body.push(parse_stmt(p, buf)?);
            if p.end() || p.at(t!["}"]) {
                break;
            }
        }
    }
    p.must(t!["}"])?;

    Ok(body)
}

fn parse_block(p: &mut State, buf: &Bump) -> Result<Spanned<ast::Block>> {
    let node = p.open();

    let body = parse_bare_block(p, buf)?;
    let body = body.as_slice();

    Ok(p.close(node, Block { body }))
}

fn parse_do_expr(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open_at(t![do])?;

    let body = parse_bare_block(p, buf)?;
    let body = body.as_slice();

    Ok(p.close(node, Block { body }).map_into())
}

fn parse_expr_func_anon(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
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

fn parse_bare_func<'bump>(p: &mut State, buf: &'bump Bump) -> Result<Spanned<BareFunc<'bump>>> {
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

fn parse_expr_assign(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let start = p.span().start();

    let lhs = parse_expr(p, buf)?;
    let op = match p.kind() {
        t![=] => AssignOp::None,
        t![+=] => AssignOp::Add,
        t![-=] => AssignOp::Sub,
        t![*=] => AssignOp::Mul,
        t![/=] => AssignOp::Div,
        t![%=] => AssignOp::Rem,
        _ => return Ok(lhs),
    };
    p.advance(); // eat op
    let value = parse_expr(p, buf)?;
    let end = p.cursor.span(p.cursor.prev()).end();
    let span = start..end;

    match Packed::kind(&lhs.into_inner().into()) {
        NodeKind::GetIndex => {
            let base =
                lhs.map(|lhs| ast::GetIndex::try_from(lhs).expect("node should be GetIndex"));
            Ok(Spanned::new(SetIndex { base, value, op }.pack(&mut p.ast), span).map_into())
        }
        NodeKind::GetField => {
            let base =
                lhs.map(|lhs| ast::GetField::try_from(lhs).expect("node should be GetField"));
            Ok(Spanned::new(SetField { base, value, op }.pack(&mut p.ast), span).map_into())
        }
        NodeKind::GetVar => {
            let base = lhs.map(|lhs| ast::GetVar::try_from(lhs).expect("node should be GetVar"));
            Ok(Spanned::new(SetVar { base, value, op }.pack(&mut p.ast), span).map_into())
        }
        _ => error_span("invalid assignment target", lhs.span).into(),
    }
}

fn parse_expr(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    match p.kind() {
        t![return] => parse_expr_return(p, buf),
        t![break] => parse_expr_break(p, buf),
        t![continue] => parse_expr_continue(p, buf),
        t![if] => parse_expr_if(p, buf, false),
        t![do] => parse_do_expr(p, buf),
        t![fn] => parse_expr_func_anon(p, buf),
        _ => parse_expr_infix(p, buf),
    }
}

fn parse_expr_infix(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    parse_expr_or(p, buf)
}

#[inline]
fn parse_binop<F, E>(p: &mut State, buf: &Bump, token_to_op: F, next: E) -> Result<Spanned<Expr>>
where
    F: Fn(TokenKind) -> Option<InfixOp>,
    E: Fn(&mut State, &Bump) -> Result<Spanned<Expr>>,
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

fn parse_expr_or(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [or] => InfixOp::Or,
    };
    let next = parse_expr_and;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_and(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [and] => InfixOp::And,
    };
    let next = parse_expr_eq;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_eq(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [==] => InfixOp::Eq,
        [!=] => InfixOp::Ne,
    };
    let next = parse_expr_cmp;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_cmp(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [>] => InfixOp::Gt,
        [>=] => InfixOp::Ge,
        [<] => InfixOp::Lt,
        [<=] => InfixOp::Le,
    };
    let next = parse_expr_add;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_add(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [+] => InfixOp::Add,
        [-] => InfixOp::Sub,
    };
    let next = parse_expr_mul;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_mul(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let token_to_op = token_map! {
        [*] => InfixOp::Mul,
        [/] => InfixOp::Div,
        [%] => InfixOp::Rem,
    };
    let next = parse_expr_prefix;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_prefix(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
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

fn parse_expr_postfix(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let expr = parse_expr_primary(p, buf)?;
    // if !matches!(
    //     Packed::kind(&expr.into_inner().into()),
    //     NodeKind::GetVar
    //         | NodeKind::GetField
    //         | NodeKind::GetIndex
    //         | NodeKind::Call
    //         | NodeKind::CallObject
    // ) {
    //     return Ok(expr);
    // }

    let mut expr = expr;
    while !p.end() {
        match p.kind() {
            t!["("] => expr = parse_expr_call(p, buf, expr)?,
            t!["["] => expr = parse_expr_index(p, buf, expr)?,
            t![.] => expr = parse_expr_field(p, buf, expr)?,
            _ => break,
        }
    }
    Ok(expr)
}

fn parse_expr_call(p: &mut State, buf: &Bump, callee: Spanned<Expr>) -> Result<Spanned<Expr>> {
    let node = p.open_extend(callee.span);

    let args = bracketed_list(p, buf, Brackets::Paren, parse_expr)?;
    let args = args.as_slice();

    if args.len() > 100 {
        return error_span(
            "too many arguments, maximum is 100",
            node.start..p.span().end,
        )
        .into();
    }

    Ok(p.close(node, Call { callee, args }).map_into())
}

fn parse_table_entry(p: &mut State, buf: &Bump) -> Result<Spanned<ast::TableEntry>> {
    let node = p.open();

    let (key, value) = match p.kind() {
        t![ident] => {
            let span = p.span();
            let lexeme = p.lexeme();
            p.advance();

            let key = str_from_lexeme_span(p, lexeme, span);

            let value: Spanned<Expr> = if p.eat(t![:]) {
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
            p.must(t![:])?;
            let value = parse_expr(p, buf)?;

            (key, value)
        }
        _ => return error_span("unexpected token", p.span()).into(),
    };

    Ok(p.close(node, TableEntry { key, value }))
}

fn str_from_lexeme_span(p: &mut State, lexeme: &str, span: Span) -> Spanned<ast::Str> {
    Spanned::new(
        Str {
            id: p.ast.intern_str(lexeme),
        }
        .pack(&mut p.ast),
        span,
    )
}

fn ident_from_lexeme_span(p: &mut State, lexeme: &str, span: Span) -> Spanned<ast::Ident> {
    Spanned::new(
        Ident {
            id: p.ast.intern_ident(lexeme),
        }
        .pack(&mut p.ast),
        span,
    )
}

fn parse_expr_index(p: &mut State, buf: &Bump, parent: Spanned<Expr>) -> Result<Spanned<Expr>> {
    let node = p.open_at(t!["["])?;

    let key = parse_expr(p, buf)?;
    p.must(t!["]"])?;

    Ok(p.close(node, GetIndex { parent, key }).map_into())
}

fn parse_expr_field(p: &mut State, buf: &Bump, parent: Spanned<Expr>) -> Result<Spanned<Expr>> {
    let node = p.open_at(t![.])?;

    let key = parse_ident(p, buf)?;

    Ok(p.close(node, GetField { parent, key }).map_into())
}

fn parse_expr_primary(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    match p.kind() {
        t!["["] => parse_expr_list(p, buf),
        t!["{"] => parse_expr_table(p, buf),
        t![int] => parse_expr_int(p, buf),
        t![float] => parse_expr_float(p, buf),
        t![true] | t![false] => parse_expr_bool(p, buf),
        t![str] => parse_expr_str(p, buf),
        t![nil] => parse_expr_nil(p, buf),
        t![ident] => parse_expr_use(p, buf),
        t!["("] => parse_expr_group(p, buf),
        _ => error_span(format!("unexpected token {:?}", p.lexeme()), p.span()).into(),
    }
}

fn parse_expr_list(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();

    let items = bracketed_list(p, buf, Brackets::Brace, parse_expr)?;
    let items = items.as_slice();

    Ok(p.close(node, List { items }).map_into())
}

fn parse_expr_table(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();

    let entries = bracketed_list(p, buf, Brackets::Curly, parse_table_entry)?;
    let entries = entries.as_slice();

    Ok(p.close(node, Table { entries }).map_into())
}

fn parse_expr_int(p: &mut State, _: &Bump) -> Result<Spanned<Expr>> {
    let node32 = p.open();
    let node64 = p.open();

    let lexeme = p.lexeme();
    let span = p.span();
    p.must(t![int])?;
    let value: i64 = i64_from_str_with_underscores(lexeme)
        .map_err(|err| error_span(format!("invalid int: {err}"), span))?;
    if value <= i32::MAX as i64 {
        let value = value as i32;
        Ok(p.close(node32, Int32 { value }).map_into())
    } else if value <= i64::MAX as i64 {
        let id = p.ast.intern_int(value);
        Ok(p.close(node64, Int64 { id }).map_into())
    } else {
        return error_span("integer would overflow", span).into();
    }
}

#[derive(Debug, Clone)]
pub enum IntParseError {
    InvalidDigit,
    Overflow,
}

impl std::fmt::Display for IntParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntParseError::InvalidDigit => f.write_str("invalid digit"),
            IntParseError::Overflow => f.write_str("integer would overflow"),
        }
    }
}

impl std::error::Error for IntParseError {}

fn i64_from_str_with_underscores(s: &str) -> Result<i64, IntParseError> {
    // Largely taken from Rust's `core::num` module, licensed MIT.
    // Specialized to `i64` in decimal, ignores underscores.

    let mut digits = s.as_bytes();
    let mut result: i64 = 0;

    if s.is_empty() {
        unreachable!("ICE: empty integer");
    }

    // NOTE: `std` first checks for potential overflow and uses a faster
    //       unchecked loop in case it's not possible, but for us that's
    //       harder to determine without reading the full string to get
    //       the number of digits, and that seems not worth it.
    while let [char, rest @ ..] = digits {
        let char = *char;
        digits = rest;

        // NOTE: valid placement of `_` is checked by tokenizer,
        //       here we just ignore them
        if char == b'_' {
            continue;
        }

        // multiply first, then look at the result later.
        let mul = result.checked_mul(10);
        let x = match (char as char).to_digit(10) {
            Some(v) => v as i64,
            None => return Err(IntParseError::InvalidDigit),
        };
        result = match mul {
            Some(v) => v,
            None => return Err(IntParseError::Overflow),
        };
        result = match result.checked_add(x) {
            Some(v) => v,
            None => return Err(IntParseError::Overflow),
        };
    }

    Ok(result)
}

fn parse_expr_float(p: &mut State, _: &Bump) -> Result<Spanned<Expr>> {
    let node_f32 = p.open();
    let node_f64 = p.open();

    let lexeme = p.lexeme();
    let span = p.span();
    p.must(t![float])?;
    let value: f64 = lexeme
        .parse()
        .map_err(|err| error_span(format!("invalid float: {err}"), span))?;

    if (value as f32) as f64 == value {
        Ok(p.close(
            node_f32,
            Float32 {
                value: value as f32,
            },
        )
        .map_into())
    } else {
        let id = p.ast.intern_float(value);
        Ok(p.close(node_f64, Float64 { id }).map_into())
    }
}

fn parse_expr_bool(p: &mut State, _: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();

    let value = match p.kind() {
        t![true] => true,
        t![false] => false,
        _ => return error_span("not a bool", p.span()).into(),
    };
    p.advance();

    Ok(p.close(node, Bool { value }).map_into())
}

fn parse_expr_str(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    parse_str(p, buf).map(|v| v.map_into())
}

fn parse_str(p: &mut State, _: &Bump) -> Result<Spanned<ast::Str>> {
    let node = p.open();

    let span = p.span();
    let lexeme = p.lexeme();

    p.must(t![str])?;
    let value = lexeme
        .strip_prefix('"')
        .ok_or_else(|| error_span("invalid string", span))?
        .strip_suffix('"')
        .ok_or_else(|| error_span("invalid string", span))?;

    let value = match escape::unescape(value) {
        Ok(value) => value,
        Err(err) => {
            return error_span(
                "invalid escape",
                span.start() + err.pos - 1..span.start() + err.pos + 2,
            )
            .into();
        }
    };
    let id = p.ast.intern_str(value.as_ref());

    Ok(p.close(node, Str { id }))
}

fn parse_ident(p: &mut State, _: &Bump) -> Result<Spanned<ast::Ident>> {
    let node = p.open();
    let lexeme = p.lexeme();
    p.must(t![ident])?;
    let id = p.ast.intern_ident(lexeme);
    Ok(p.close(node, Ident { id }))
}

fn parse_str_or_ident_as_str(p: &mut State, buf: &Bump) -> Result<Spanned<ast::Str>> {
    if p.at(t![ident]) {
        parse_ident_as_str(p, buf)
    } else if p.at(t![str]) {
        parse_str(p, buf)
    } else {
        return error_span("expected string or identifier", p.span()).into();
    }
}

fn parse_ident_as_str(p: &mut State, buf: &Bump) -> Result<Spanned<ast::Str>> {
    let node = p.open();
    let lexeme = p.lexeme();
    p.must(t![ident])?;
    let id = p.ast.intern_str(lexeme);
    Ok(p.close(node, Str { id }))
}

fn ident_to_str(p: &mut State, buf: &Bump, ident: Spanned<ast::Ident>) -> Spanned<ast::Str> {
    let str = p.cursor.tokens().span_lexeme(ident.span);
    let id = p.ast.intern_str(str);
    Spanned::new(Str { id }.pack(&mut p.ast), ident.span)
}

mod escape;

fn parse_expr_nil(p: &mut State, _: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();
    p.must(t![nil])?;

    Ok(p.close(node, Nil {}).map_into())
}

fn parse_expr_use(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
    let node = p.open();
    let name = parse_ident(p, buf)?;

    Ok(p.close(node, GetVar { name }).map_into())
}

fn parse_expr_group(p: &mut State, buf: &Bump) -> Result<Spanned<Expr>> {
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
    p: &mut State,
    buf: &'bump Bump,
    brackets: Brackets,
    f: F,
) -> Result<Vec<'bump, T>>
where
    F: Fn(&mut State, &Bump) -> Result<T>,
{
    let (opening, closing) = match brackets {
        Brackets::Paren => (t!["("], t![")"]),
        Brackets::Brace => (t!["["], t!["]"]),
        Brackets::Curly => (t!["{"], t!["}"]),
    };

    p.must(opening)?;
    let mut out = temp(buf);
    if !p.at(closing) {
        loop {
            out.push(f(p, buf)?);
            if !p.eat(t![,]) || p.at(closing) {
                break;
            }
        }
    }
    p.must(closing)?;

    Ok(out)
}

#[inline]
fn can_begin_expr(p: &mut State) -> bool {
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
