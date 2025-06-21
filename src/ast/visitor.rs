use super::{
    Ast, Expr, ExprKind, Packed, Stmt, StmtKind,
    node::{self, expr, stmt},
};

pub fn visit(ast: &Ast, visitor: &mut impl Visitor) {
    visitor.visit_root(ast);
}

pub trait Visitor {
    fn visit_root(&mut self, ast: &Ast) {
        for stmt in ast.root().body() {
            self.visit_stmt(ast, stmt);
        }
    }

    fn visit_stmt(&mut self, ast: &Ast, node: Stmt) {
        use StmtKind as S;
        match node.kind() {
            S::Var => self.visit_stmt_var(ast, node.unpack::<stmt::Var>(ast)),
            S::Fn => self.visit_stmt_fn(ast, node.unpack::<stmt::Fn>(ast)),
            S::Loop => self.visit_stmt_loop(ast, node.unpack::<stmt::Loop>(ast)),
            S::Expr => self.visit_stmt_expr(ast, node.unpack::<stmt::Expr>(ast)),
        }
    }

    fn visit_stmt_var(&mut self, ast: &Ast, node: stmt::Var) {
        self.visit_expr(ast, node.value);
    }

    fn visit_stmt_fn(&mut self, ast: &Ast, node: stmt::Fn) {
        self.visit_expr(ast, node.body);
    }

    fn visit_stmt_loop(&mut self, ast: &Ast, node: stmt::Loop) {
        for stmt in node.body() {
            self.visit_stmt(ast, stmt);
        }
    }

    fn visit_stmt_expr(&mut self, ast: &Ast, node: stmt::Expr) {
        self.visit_expr(ast, node.inner);
    }

    fn visit_expr(&mut self, ast: &Ast, node: Expr) {
        use ExprKind as E;

        match node.kind() {
            E::Return => todo!(),
            E::Break => todo!(),
            E::Continue => todo!(),
            E::If => todo!(),
            E::Do => todo!(),
            E::Fn => todo!(),
            E::Assign => todo!(),
            E::Infix => todo!(),
            E::Prefix => todo!(),
            E::Call => todo!(),
            E::Index => todo!(),
            E::Field => todo!(),
            E::Array => todo!(),
            E::Object => todo!(),
            E::Int => todo!(),
            E::Float => todo!(),
            E::Bool => todo!(),
            E::Str => todo!(),
            E::Nil => todo!(),
            E::Use => todo!(),
        }
    }
}
