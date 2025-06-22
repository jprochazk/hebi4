use super::{
    Ast, Expr, ExprKind, Packed, Stmt, StmtKind, Tag,
    node::{self, expr, stmt},
};

pub fn visit<V: Visitor>(ast: &Ast, visitor: &mut V) -> Result<(), V::Error> {
    visitor.visit_root(ast)
}

pub fn visit_node<V: Visitor>(ast: &Ast, visitor: &mut V, node: Packed) -> Result<(), V::Error> {
    if let Some(node) = node.as_stmt() {
        visitor.visit_stmt(ast, node)
    } else if let Some(node) = node.as_expr() {
        visitor.visit_expr(ast, node)
    } else if node.tag() == Tag::Root {
        visitor.visit_root(ast)
    } else {
        panic!("cannot visit starting from {:?}", node.tag());
    }
}

pub trait Visitor {
    type Error;

    fn visit_root(&mut self, ast: &Ast) -> Result<(), Self::Error> {
        for stmt in ast.root().body() {
            self.visit_stmt(ast, stmt)?;
        }

        Ok(())
    }

    fn visit_stmt(&mut self, ast: &Ast, node: Stmt) -> Result<(), Self::Error> {
        use StmtKind as S;
        match node.kind() {
            S::Var => self.visit_stmt_var(ast, node.unpack::<stmt::Var>(ast)),
            S::Fn => self.visit_stmt_fn(ast, node.unpack::<stmt::Fn>(ast)),
            S::Loop => self.visit_stmt_loop(ast, node.unpack::<stmt::Loop>(ast)),
            S::Expr => self.visit_stmt_expr(ast, node.unpack::<stmt::Expr>(ast)),
        }
    }

    fn visit_stmt_var(&mut self, ast: &Ast, node: stmt::Var) -> Result<(), Self::Error> {
        self.visit_expr(ast, node.value)
    }

    fn visit_stmt_fn(&mut self, ast: &Ast, node: stmt::Fn) -> Result<(), Self::Error> {
        self.visit_expr(ast, node.body)
    }

    fn visit_stmt_loop(&mut self, ast: &Ast, node: stmt::Loop) -> Result<(), Self::Error> {
        for stmt in node.body() {
            self.visit_stmt(ast, stmt)?;
        }
        Ok(())
    }

    fn visit_stmt_expr(&mut self, ast: &Ast, node: stmt::Expr) -> Result<(), Self::Error> {
        self.visit_expr(ast, node.inner)
    }

    fn visit_expr(&mut self, ast: &Ast, node: Expr) -> Result<(), Self::Error> {
        use ExprKind as E;

        match node.kind() {
            E::Return => todo!(),
            E::Break => todo!(),
            E::Continue => todo!(),
            E::If => todo!(),
            E::Do => self.visit_expr_do(ast, node.unpack::<expr::Do>(ast)),
            E::Fn => todo!(),
            E::Assign => todo!(),
            E::Infix => todo!(),
            E::Prefix => todo!(),
            E::Call => todo!(),
            E::Index => todo!(),
            E::Field => todo!(),
            E::Array => todo!(),
            E::Object => todo!(),
            E::Int => self.visit_expr_int(ast, node.unpack::<expr::Int>(ast)),
            E::Float => todo!(),
            E::Bool => todo!(),
            E::Str => todo!(),
            E::Nil => todo!(),
            E::Use => todo!(),
        }
    }

    fn visit_expr_do(&mut self, ast: &Ast, node: expr::Do) -> Result<(), Self::Error> {
        for stmt in node.body() {
            self.visit_stmt(ast, stmt)?;
        }
        Ok(())
    }

    fn visit_expr_int(&mut self, ast: &Ast, node: expr::Int) -> Result<(), Self::Error> {
        let _ = (ast, node);
        Ok(())
    }
}
