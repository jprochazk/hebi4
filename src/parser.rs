use bumpalo::{Bump, collections::Vec};

use crate::{
    ast::{
        Ast, AstBuilder, Expr, Node, Stmt,
        node::{self, expr, stmt},
    },
    token::{Token, TokenCursor, TokenKind, Tokens},
};

type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Error {}

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
            // TODO: error
            Err(Error {})
        }
    }

    #[inline]
    fn pack<T: Node>(&mut self, node: T) -> T::NodeKind {
        node.pack_into(&mut self.ast)
    }
}

fn parse_root(mut p: Parser) -> Result<Ast> {
    let buf = Bump::new();

    // *shrug*
    let mut body = Vec::with_capacity_in(64, &buf);
    body.push(parse_stmt(&mut p, &buf)?);
    let root = node::Root { body: &body[..] }.pack_into(&mut p.ast);
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
    assert!(p.at(t![var]));
    p.advance();

    let name = parse_ident(p)?;
    p.must(t![=])?;
    let value = parse_expr(p, buf)?;

    Ok(p.pack(stmt::Var { name, value }))
}

fn parse_stmt_fn(p: &mut Parser, buf: &Bump) -> Result<Stmt> {
    assert!(p.at(t![fn]));

    todo!()
}

fn parse_stmt_loop(p: &mut Parser, buf: &Bump) -> Result<Stmt> {
    assert!(p.at(t![loop]));

    todo!()
}

fn parse_stmt_expr(p: &mut Parser, buf: &Bump) -> Result<Stmt> {
    todo!()
}

fn parse_ident(p: &mut Parser) -> Result<node::Ident> {
    let ident = p.must(t![ident])?;
    let lexeme = p.lexeme(ident);
    Ok(p.ast.intern_ident(lexeme))
}

fn parse_expr(p: &mut Parser, buf: &Bump) -> Result<Expr> {
    todo!()
}
