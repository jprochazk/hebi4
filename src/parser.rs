use bumpalo::{Bump, collections::Vec};

use crate::{
    ast::{Ast, AstBuilder, Expr, Node as _, Stmt, node},
    token::{TokenCursor, Tokens},
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
}

fn parse_root(mut p: Parser) -> Result<Ast> {
    let scratch = Bump::new();

    // *shrug*
    let mut body = Vec::with_capacity_in(64, &scratch);
    body.push(parse_stmt(&mut p, &scratch)?);
    let root = node::Root { body: &body[..] }.pack_into(&mut p.ast);
    Ok(p.ast.build(root))
}

fn parse_stmt(p: &mut Parser, scratch: &Bump) -> Result<Stmt> {}
