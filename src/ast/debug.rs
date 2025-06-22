use super::{
    Ast, Packed, PackedAbi,
    node::{Ident, expr, stmt},
    visitor,
};

impl std::fmt::Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        visitor::visit(self, &mut DebugVisitor { f })
    }
}

struct DebugVisitor<'a, 'fmt> {
    f: &'a mut std::fmt::Formatter<'fmt>,
}

impl visitor::Visitor for DebugVisitor<'_, '_> {
    type Error = std::fmt::Error;

    fn visit_stmt_var(&mut self, ast: &Ast, node: stmt::Var) -> Result<(), Self::Error> {
        let name = node.name.as_str(ast).unwrap();
        let value = DebugNode {
            ast,
            node: node.value.into_packed(),
        };

        self.f
            .debug_tuple("Var")
            .field(&name)
            .field(&value)
            .finish()
    }

    fn visit_stmt_fn(&mut self, ast: &Ast, node: stmt::Fn) -> Result<(), Self::Error> {
        let name = node.name.as_str(ast).unwrap();
        let params = node.params;
        let body = DebugNode {
            ast,
            node: node.body.into_packed(),
        };

        self.f
            .debug_tuple("stmt.Fn")
            .field(&name)
            .field(&Params { ast, params })
            .field(&body)
            .finish()
    }

    fn visit_expr_do(&mut self, ast: &Ast, node: expr::Do) -> Result<(), Self::Error> {
        self.f
            .debug_tuple("Do")
            .field(&DebugNodeList {
                ast,
                list: node.body,
            })
            .finish()
    }

    fn visit_expr_int(&mut self, _: &Ast, node: expr::Int) -> Result<(), Self::Error> {
        self.f.debug_tuple("Int").field(&node.into_u56()).finish()
    }
}

struct DebugNodeList<'a, T> {
    ast: &'a Ast,
    list: &'a [T],
}

impl<T: PackedAbi + Copy> std::fmt::Debug for DebugNodeList<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.list.iter().copied().map(|node| DebugNode {
                ast: self.ast,
                node: node.into_packed(),
            }))
            .finish()
    }
}

struct Params<'a> {
    ast: &'a Ast,
    params: &'a [Ident],
}

impl std::fmt::Debug for Params<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(
                self.params
                    .iter()
                    .copied()
                    .map(|v| v.as_str(self.ast).unwrap()),
            )
            .finish()
    }
}

struct DebugNode<'ast> {
    ast: &'ast Ast,
    node: Packed,
}

impl<'ast> std::fmt::Debug for DebugNode<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        visitor::visit_node(self.ast, &mut DebugVisitor { f }, self.node)
    }
}
