use bumpalo::{Bump, collections::Vec};

use crate::{
    ast::{
        Ast, AstBuilder, Expr, Node, Stmt,
        node::{self, expr, stmt},
    },
    token::{Token, TokenCursor, TokenKind, Tokens},
};

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug)]
pub struct Error {}

impl std::error::Error for Error {}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

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
    fn current(&self) -> TokenKind {
        let token = self.cursor.current();
        self.cursor.kind(token)
    }

    #[inline]
    fn peek(&self) -> TokenKind {
        let token = self.cursor.peek();
        self.cursor.kind(token)
    }

    #[inline]
    fn lexeme(&self, token: Token) -> &'src str {
        self.cursor.lexeme(token)
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
        self.current() == kind
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
            todo!("error in `p.must`")
        }
    }

    #[inline]
    fn pack<T: Node>(&mut self, node: T) -> T::NodeKind {
        node.pack_into(&mut self.ast)
    }
}

fn temp<T>(buf: &Bump) -> Vec<'_, T> {
    // *shrug*
    Vec::with_capacity_in(16, buf)
}

fn parse_root(mut p: Parser) -> Result<Ast> {
    let buf = Bump::new();

    let mut body = temp(&buf);
    body.push(parse_stmt(&mut p, &buf)?);

    let root = p.pack(node::Root {
        body: body.as_slice(),
    });

    Ok(p.ast.build(root))
}

fn parse_stmt(p: &mut Parser, buf: &Bump) -> Result<Stmt> {
    match p.current() {
        t![var] => parse_stmt_var(p, buf),
        t![fn] => parse_stmt_fn(p, buf),
        t![loop] => parse_stmt_loop(p, buf),
        _ => parse_stmt_expr(p, buf),
    }
}

/// `"var" name:IDENT "=" value:EXPR`
///
/// `p` must be at "var"
fn parse_stmt_var(p: &mut Parser, buf: &Bump) -> Result<Stmt> {
    assert!(p.eat(t![var]));

    let name = parse_ident(p, buf)?;
    p.must(t![=])?;
    let value = parse_expr(p, buf)?;

    Ok(p.pack(stmt::Var { name, value }))
}

/// `"fn" name:IDENT "(" param:IDENT,* ")" "do" stmt* "end"`
fn parse_stmt_fn(p: &mut Parser, buf: &Bump) -> Result<Stmt> {
    assert!(p.eat(t![fn]));

    let name = parse_ident(p, buf)?;
    let params = paren_list(p, buf, parse_ident)?;
    let body = parse_expr_do(p, buf)?;

    Ok(p.pack(stmt::Fn {
        name,
        body,
        params: params.as_slice(),
    }))
}

fn parse_stmt_loop(p: &mut Parser, buf: &Bump) -> Result<Stmt> {
    assert!(p.eat(t![loop]));

    let mut body = temp(buf);
    while !p.at(t![end]) && !p.at(t![EOF]) {
        body.push(parse_stmt(p, buf)?);
    }
    p.must(t![end])?;

    Ok(p.pack(stmt::Loop {
        body: body.as_slice(),
    }))
}

fn parse_stmt_expr(p: &mut Parser, buf: &Bump) -> Result<Stmt> {
    let inner = parse_expr_top_level(p, buf)?;

    Ok(p.pack(stmt::Expr { inner }))
}

fn parse_ident(p: &mut Parser, _: &Bump) -> Result<node::Ident> {
    let ident = p.must(t![ident])?;
    let lexeme = p.lexeme(ident);
    Ok(p.ast.intern_ident(lexeme))
}

fn parse_expr_top_level(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    match p.current() {
        t![return] => parse_expr_return(p, buf),
        t![break] => parse_expr_break(p, buf),
        t![continue] => parse_expr_continue(p, buf),
        t![if] => parse_expr_if(p, buf),
        t![do] => parse_expr_do(p, buf),
        t![fn] => parse_expr_fn(p, buf),
        _ => parse_expr_assign(p, buf),
    }
}

fn parse_expr_return(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    assert!(p.eat(t![return]));

    let value: node::Opt<Expr> = if can_begin_expr(p) {
        node::Opt::some(parse_expr(p, buf)?)
    } else {
        node::Opt::none()
    };

    Ok(p.pack(expr::Return { value }))
}

fn parse_expr_break(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    assert!(p.eat(t![break]));

    Ok(p.pack(expr::Break {}))
}

fn parse_expr_continue(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    assert!(p.eat(t![continue]));

    Ok(p.pack(expr::Continue {}))
}

fn parse_expr_if(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    assert!(p.eat(t![if]));

    todo!()
}

fn parse_expr_do(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    assert!(p.eat(t![do]));

    todo!()
}

fn parse_expr_fn(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    assert!(p.eat(t![fn]));

    todo!()
}

fn parse_expr_assign(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    match p.current() {
        t![return] => parse_expr_return(p, buf),
        t![break] => parse_expr_break(p, buf),
        t![continue] => parse_expr_continue(p, buf),
        _ => parse_expr_infix(p, buf),
    }
}

fn parse_expr_infix(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    parse_expr_or(p, buf)
}

fn parse_binop<F, E>(p: &mut Parser, buf: &Bump, token_to_op: F, next: E) -> Result<Expr>
where
    F: Fn(TokenKind) -> Option<expr::InfixOp>,
    E: Fn(&mut Parser, &Bump) -> Result<Expr>,
{
    let mut lhs = next(p, buf)?;
    while !p.end() {
        let op = match token_to_op(p.current()) {
            Some(op) => op,
            None => break,
        };
        p.advance(); // eat op
        let rhs = next(p, buf)?;
        lhs = p.pack(expr::Infix { lhs, rhs, op });
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

fn parse_expr_or(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    let token_to_op = token_map! {
        or => expr::InfixOp::Or,
    };
    let next = parse_expr_and;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_and(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    let token_to_op = token_map! {
        and => expr::InfixOp::And,
    };
    let next = parse_expr_eq;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_eq(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    let token_to_op = token_map! {
        == => expr::InfixOp::Eq,
        != => expr::InfixOp::Ne,
    };
    let next = parse_expr_cmp;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_cmp(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    let token_to_op = token_map! {
        > => expr::InfixOp::Gt,
        >= => expr::InfixOp::Ge,
        < => expr::InfixOp::Lt,
        <= => expr::InfixOp::Le,
    };
    let next = parse_expr_add;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_add(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    let token_to_op = token_map! {
        + => expr::InfixOp::Add,
        - => expr::InfixOp::Sub,
    };
    let next = parse_expr_mul;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_mul(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    let token_to_op = token_map! {
        * => expr::InfixOp::Mul,
        / => expr::InfixOp::Div,
    };
    let next = parse_expr_prefix;
    parse_binop(p, buf, token_to_op, next)
}

fn parse_expr_prefix(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    let op = match p.current() {
        t![-] => expr::PrefixOp::Minus,
        t![not] => expr::PrefixOp::Minus,
        _ => return parse_expr_postfix(p, buf),
    };
    p.advance(); // eat op
    let rhs = parse_expr_prefix(p, buf)?;
    Ok(p.pack(expr::Prefix { rhs, op }))
}

fn parse_expr_postfix(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    let mut expr = parse_expr_primary(p, buf)?;
    while !p.end() {
        match p.current() {
            t!["("] => expr = parse_expr_call(p, buf)?,
            t!["{"] => expr = parse_expr_call_object(p, buf)?,
            t!["["] => expr = parse_expr_index(p, buf)?,
            t![.] => expr = parse_expr_field(p, buf)?,
            _ => break,
        }
    }
    Ok(expr)
}

fn parse_expr_call(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr_call_object(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr_index(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    assert!(p.eat(t!["["]));
    let key = parse_expr(p, buf)?;
    p.must(t!["]"])?;

    Ok(p.pack(expr::Index { key }))
}

fn parse_expr_field(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    assert!(p.eat(t![.]));
    let key = parse_ident(p, buf)?;

    Ok(p.pack(expr::Field { key }))
}

fn parse_expr_primary(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    match p.current() {
        t!["["] => parse_expr_array(p, buf),
        t!["{"] => parse_expr_object(p, buf),
        t![int] => parse_expr_int(p, buf),
        t![float] => parse_expr_float(p, buf),
        t![true] | t![false] => parse_expr_bool(p, buf),
        t![str] => parse_expr_str(p, buf),
        t![nil] => parse_expr_nil(p, buf),
        t![ident] => parse_expr_use(p, buf),
        t!["("] => parse_expr_group(p, buf),
        _ => todo!("error when nothing matches"),
    }
}

fn parse_expr_array(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr_object(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr_int(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    let token = p.must(t![int])?;
    let lexeme = p.lexeme(token);
    let value = expr::Int::parse(lexeme).ok_or_else(|| todo!("int parse error"))?;
    Ok(p.pack(value))
}

fn parse_expr_float(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr_bool(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr_str(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr_nil(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr_use(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

fn parse_expr_group(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}

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

fn can_begin_expr(p: &mut Parser) -> bool {
    todo!()
}
