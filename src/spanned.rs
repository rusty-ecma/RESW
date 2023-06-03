use resast::spanned::{
    decl::{
        Alias, Decl, DefaultExportDeclValue, DefaultImportSpec, ExportList, ExportSpecifier,
        ImportSpecifier, ModExport, ModExportSpecifier, ModImport, NamedExportDecl,
        NamedExportSource, NamedExportSpec, NamespaceImportSpec, NormalImportSpec,
        NormalImportSpecs, VarDecl, VarDecls,
    },
    expr::{
        ArrayExpr, ArrowFuncBody, ArrowFuncExpr, AssignExpr, AssignLeft, AwaitExpr, BinaryExpr,
        CallExpr, ConditionalExpr, Expr, Lit, LogicalExpr, MemberExpr, MemberIndexer, MetaProp,
        NewExpr, ObjExpr, ObjProp, Prop, PropCtor, PropGet, PropInit, PropInitKey, PropKey,
        PropMethod, PropSet, PropValue, TaggedTemplateExpr, TemplateLit, UnaryExpr, UpdateExpr,
        YieldExpr,
    },
    pat::{ArrayPat, ArrayPatPart, AssignPat, ObjPat, ObjPatPart, Pat, RestPat},
    stmt::{
        self, CatchClause, DoWhileStmt, FinallyClause, ForInStmt, ForOfStmt, ForStmt, IfStmt,
        LoopInit, LoopLeft, Stmt, SwitchCase, SwitchStmt, WhileStmt,
    },
    tokens::{AssignOp, BinaryOp, LogicalOp, QuasiQuote, Token, UnaryOp, UpdateOp},
    Class, ClassBody, Dir, Func, FuncArg, FuncBody, Ident, ListEntry, Node, Position, Program,
    ProgramPart, Slice, SourceLocation, VarKind,
};
use ress::tokens::{Comment, CommentKind};
use std::io::Write;

type Res = Result<(), std::io::Error>;

pub struct SpannedWriter<T> {
    out: T,
    last_out: Position,
}

impl<T> SpannedWriter<T>
where
    T: Write,
{
    pub fn new(out: T) -> Self {
        Self {
            out,
            last_out: Position { line: 1, column: 1 },
        }
    }

    pub fn write_program<Ast: AsRef<str>>(&mut self, program: &Program<Ast>) -> Res {
        match program {
            Program::Mod(inner) => self.write_parts(inner),
            Program::Script(inner) => self.write_parts(inner),
        }
    }

    pub fn write_parts<Ast: AsRef<str>>(&mut self, parts: &[ProgramPart<Ast>]) -> Res {
        for part in parts {
            self.write_part(part)?;
        }
        Ok(())
    }

    pub fn write_part<Ast: AsRef<str>>(&mut self, part: &ProgramPart<Ast>) -> Res {
        match part {
            ProgramPart::Dir(dir) => self.write_dir(dir),
            ProgramPart::Decl(decl) => self.write_decl(decl),
            ProgramPart::Stmt(stmt) => self.write_stmt(stmt),
        }
    }

    pub fn write_dir<Ast: AsRef<str>>(&mut self, dir: &Dir<Ast>) -> Res {
        self.write_lit(&dir.expr)?;
        self.write_maybe_token(dir.semi_colon.as_ref())
    }

    pub fn write_decl<Ast: AsRef<str>>(&mut self, decl: &Decl<Ast>) -> Res {
        match decl {
            Decl::Var { decls, semi_colon } => {
                self.write_var_decls(decls)?;
                if let Some(semi) = semi_colon {
                    self.write_token(semi)?;
                }
                Ok(())
            }
            Decl::Func(func) => self.write_func(func),
            Decl::Class(class) => self.write_class(class),
            Decl::Import { import, semi_colon } => {
                self.write_mod_import(import)?;
                self.write_maybe_token(semi_colon.as_ref())
            }
            Decl::Export { export, semi_colon } => {
                self.write_mod_export(export)?;
                self.write_maybe_token(semi_colon.as_ref())
            }
        }
    }

    pub fn write_mod_import<Ast: AsRef<str>>(&mut self, import: &ModImport<Ast>) -> Res {
        self.write_token(&import.keyword_import)?;
        self.write_mod_import_specs(&import.specifiers)?;
        self.write_maybe_token(import.keyword_from.as_ref())?;
        self.write_lit(&import.source)
    }

    pub fn write_mod_import_specs<Ast: AsRef<str>>(
        &mut self,
        specs: &[ListEntry<ImportSpecifier<Ast>>],
    ) -> Res {
        for spec in specs {
            self.write_import_spec(&spec.item)?;
            self.write_maybe_token(spec.comma.as_ref())?;
        }
        Ok(())
    }

    pub fn write_import_spec<Ast: AsRef<str>>(&mut self, spec: &ImportSpecifier<Ast>) -> Res {
        match spec {
            ImportSpecifier::Normal(normal) => self.write_normal_import_specs(normal),
            ImportSpecifier::Default(default) => self.write_default_import(default),
            ImportSpecifier::Namespace(namespace) => self.write_namespace_import(namespace),
        }
    }

    pub fn write_normal_import_specs<Ast: AsRef<str>>(
        &mut self,
        specs: &NormalImportSpecs<Ast>,
    ) -> Res {
        self.write_token(&specs.open_brace)?;
        for spec in &specs.specs {
            self.write_normal_import_spec(&spec.item)?;
            self.write_maybe_token(spec.comma.as_ref())?;
        }
        self.write_token(&specs.close_brace)
    }

    pub fn write_normal_import_spec<Ast: AsRef<str>>(
        &mut self,
        spec: &NormalImportSpec<Ast>,
    ) -> Res {
        self.write_ident(&spec.imported)?;
        if let Some(alias) = &spec.alias {
            self.write_token(&alias.keyword)?;
            self.write_ident(&alias.ident)?;
        }
        Ok(())
    }

    pub fn write_default_import<Ast: AsRef<str>>(&mut self, spec: &DefaultImportSpec<Ast>) -> Res {
        self.write_ident(&spec.id)
    }

    pub fn write_namespace_import<Ast: AsRef<str>>(
        &mut self,
        spec: &NamespaceImportSpec<Ast>,
    ) -> Res {
        self.write_token(&spec.star)?;
        self.write_token(&spec.keyword)?;
        self.write_ident(&spec.ident)
    }

    pub fn write_mod_export<Ast: AsRef<str>>(&mut self, export: &ModExport<Ast>) -> Res {
        self.write_token(&export.keyword)?;
        self.write_mod_export_spec(&export.spec)
    }

    pub fn write_mod_export_spec<Ast: AsRef<str>>(
        &mut self,
        spec: &ModExportSpecifier<Ast>,
    ) -> Res {
        match spec {
            ModExportSpecifier::Default { keyword, value } => {
                self.write_token(keyword)?;
                self.write_default_export_value(value)
            }
            ModExportSpecifier::Named(named) => self.write_named_export_decl(named),
            ModExportSpecifier::All {
                star,
                keyword,
                name,
                alias,
            } => {
                self.write_token(star)?;
                if let Some(alias) = alias {
                    self.write_alias(alias)?;
                }
                self.write_token(keyword)?;
                self.write_lit(name)
            }
        }
    }

    pub fn write_default_export_value<Ast: AsRef<str>>(
        &mut self,
        value: &DefaultExportDeclValue<Ast>,
    ) -> Res {
        match value {
            DefaultExportDeclValue::Decl(decl) => self.write_decl(decl),
            DefaultExportDeclValue::Expr(expr) => self.write_expr(expr),
        }
    }

    pub fn write_named_export_decl<Ast: AsRef<str>>(
        &mut self,
        named: &NamedExportDecl<Ast>,
    ) -> Res {
        match named {
            NamedExportDecl::Decl(decl) => self.write_decl(decl),
            NamedExportDecl::Specifier(spec) => self.write_named_export_spec(spec),
        }
    }

    pub fn write_named_export_spec<Ast: AsRef<str>>(&mut self, spec: &NamedExportSpec<Ast>) -> Res {
        self.write_export_list(&spec.list)?;
        if let Some(source) = &spec.source {
            self.write_named_export_source(source)?;
        }
        Ok(())
    }

    pub fn write_export_list<Ast: AsRef<str>>(&mut self, list: &ExportList<Ast>) -> Res {
        self.write_token(&list.open_brace)?;
        for spec in &list.elements {
            self.write_export_spec(&spec.item)?;
            self.write_maybe_token(spec.comma.as_ref())?;
        }
        self.write_token(&list.close_brace)
    }

    pub fn write_named_export_source<Ast: AsRef<str>>(
        &mut self,
        source: &NamedExportSource<Ast>,
    ) -> Res {
        self.write_token(&source.keyword_from)?;
        self.write_lit(&source.module)
    }

    pub fn write_export_spec<Ast: AsRef<str>>(&mut self, spec: &ExportSpecifier<Ast>) -> Res {
        self.write_ident(&spec.local)?;
        if let Some(alias) = &spec.alias {
            self.write_token(&alias.keyword)?;
            self.write_ident(&alias.ident)?;
        }
        Ok(())
    }

    pub fn write_stmt<Ast: AsRef<str>>(&mut self, stmt: &Stmt<Ast>) -> Res {
        match stmt {
            Stmt::Expr { expr, semi_colon } => {
                self.write_expr(expr)?;
                self.write_maybe_token(semi_colon.as_ref())
            }
            Stmt::Block(block) => {
                self.write_token(&block.open_brace)?;
                self.write_parts(&block.stmts)?;
                self.write_token(&block.close_brace)
            }
            Stmt::Empty(slice) => self.write_token(slice),
            Stmt::Debugger {
                keyword,
                semi_colon,
            } => {
                self.write_token(keyword)?;
                self.write_maybe_token(semi_colon.as_ref())
            }
            Stmt::With(with) => {
                self.write_token(&with.keyword)?;
                self.write_token(&with.open_paren)?;
                self.write_expr(&with.object)?;
                self.write_token(&with.close_paren)?;
                self.write_stmt(&with.body)
            }
            Stmt::Return {
                keyword,
                value,
                semi_colon,
            } => {
                self.write_token(keyword)?;
                if let Some(value) = value {
                    self.write_expr(value)?;
                }
                self.write_maybe_token(semi_colon.as_ref())
            }
            Stmt::Labeled(labeled) => {
                self.write_ident(&labeled.label)?;
                self.write_token(&labeled.colon)?;
                self.write_stmt(&labeled.body)
            }
            Stmt::Break {
                keyword,
                label,
                semi_colon,
            } => {
                self.write_token(keyword)?;
                if let Some(label) = label {
                    self.write_ident(label)?;
                }
                self.write_maybe_token(semi_colon.as_ref())
            }
            Stmt::Continue {
                keyword,
                label,
                semi_colon,
            } => {
                self.write_token(keyword)?;
                if let Some(label) = label {
                    self.write_ident(label)?;
                }
                self.write_maybe_token(semi_colon.as_ref())
            }
            Stmt::If(stmt) => self.write_if_stmt(stmt),
            Stmt::Switch(switch) => self.write_switch_stmt(switch),
            Stmt::Throw {
                keyword,
                expr,
                semi_colon,
            } => {
                self.write_token(keyword)?;
                self.write_expr(expr)?;
                self.write_maybe_token(semi_colon.as_ref())
            }
            Stmt::Try(try_stmt) => self.write_try_stmt(try_stmt),
            Stmt::While(while_stmt) => self.write_while_stmt(while_stmt),
            Stmt::DoWhile(do_while) => self.write_do_while(do_while),
            Stmt::For(for_stmt) => self.write_for_stmt(for_stmt),
            Stmt::ForIn(for_in) => self.write_for_in(for_in),
            Stmt::ForOf(for_of) => self.write_for_of(for_of),
            Stmt::Var { decls, semi_colon } => {
                self.write_var_decls(decls)?;
                self.write_maybe_token(semi_colon.as_ref())
            }
        }
    }

    pub fn write_var_decls<Ast: AsRef<str>>(&mut self, decls: &VarDecls<Ast>) -> Res {
        self.write_variable_kind(&decls.keyword)?;
        self.write_var_decls_(&decls.decls)
    }

    pub fn write_variable_kind(&mut self, kind: &VarKind) -> Res {
        match kind {
            VarKind::Var(Some(slice)) => self.write_token(slice),
            VarKind::Let(slice) => self.write_token(slice),
            VarKind::Const(slice) => self.write_token(slice),
            _ => Ok(()),
        }
    }

    fn write_var_decls_<Ast: AsRef<str>>(&mut self, decls: &[ListEntry<VarDecl<Ast>>]) -> Res {
        for entry in decls {
            self.write_var_decl(&entry.item)?;
            self.write_maybe_token(entry.comma.as_ref())?;
        }
        Ok(())
    }

    pub fn write_var_decl<Ast: AsRef<str>>(&mut self, decl: &VarDecl<Ast>) -> Res {
        self.write_pat(&decl.id)?;
        if let Some(eq) = &decl.eq {
            self.write_token(eq)?;
        }
        if let Some(init) = &decl.init {
            self.write_expr(init)?;
        }
        Ok(())
    }

    pub fn write_if_stmt<Ast: AsRef<str>>(&mut self, if_stmt: &IfStmt<Ast>) -> Res {
        self.write_token(&if_stmt.keyword)?;
        self.write_token(&if_stmt.open_paren)?;
        self.write_expr(&if_stmt.test)?;
        self.write_token(&if_stmt.close_paren)?;
        self.write_stmt(&if_stmt.consequent)?;
        if let Some(alt) = &if_stmt.alternate {
            self.write_token(&alt.keyword)?;
            self.write_stmt(&alt.body)?;
        }
        Ok(())
    }

    pub fn write_switch_stmt<Ast: AsRef<str>>(&mut self, switch: &SwitchStmt<Ast>) -> Res {
        self.write_token(&switch.keyword)?;
        self.write_token(&switch.open_paren)?;
        self.write_expr(&switch.discriminant)?;
        self.write_token(&switch.close_paren)?;
        self.write_token(&switch.open_brace)?;
        for case in &switch.cases {
            self.write_switch_case(case)?;
        }
        self.write_token(&switch.close_brace)
    }

    pub fn write_switch_case<Ast: AsRef<str>>(&mut self, case: &SwitchCase<Ast>) -> Res {
        self.write_token(&case.keyword)?;
        if let Some(test) = &case.test {
            self.write_expr(test)?;
        }
        self.write_token(&case.colon)?;
        self.write_parts(&case.consequent)
    }

    pub fn write_class<Ast: AsRef<str>>(&mut self, class: &Class<Ast>) -> Res {
        self.write_token(&class.keyword)?;
        if let Some(id) = &class.id {
            self.write_ident(id)?;
        }
        if let Some(super_class) = &class.super_class {
            self.write_token(&super_class.keyword_extends)?;
            self.write_expr(&super_class.expr)?;
        }
        self.write_class_body(&class.body)
    }

    pub fn write_try_stmt<Ast: AsRef<str>>(&mut self, try_stmt: &stmt::TryStmt<Ast>) -> Res {
        self.write_token(&try_stmt.keyword)?;
        self.write_block_stmt(&try_stmt.block)?;
        if let Some(catch) = &try_stmt.handler {
            self.write_catch_clause(catch)?
        }
        if let Some(finally) = &try_stmt.finalizer {
            self.write_finally_clause(finally)?;
        }
        Ok(())
    }

    pub fn write_catch_clause<Ast: AsRef<str>>(&mut self, catch: &CatchClause<Ast>) -> Res {
        self.write_token(&catch.keyword)?;
        if let Some(param) = &catch.param {
            self.write_token(&param.open_paren)?;
            self.write_pat(&param.param)?;
            self.write_token(&param.close_paren)?;
        }
        self.write_block_stmt(&catch.body)
    }

    pub fn write_finally_clause<Ast: AsRef<str>>(&mut self, finally: &FinallyClause<Ast>) -> Res {
        self.write_token(&finally.keyword)?;
        self.write_block_stmt(&finally.body)
    }

    pub fn write_while_stmt<Ast: AsRef<str>>(&mut self, while_stmt: &WhileStmt<Ast>) -> Res {
        self.write_token(&while_stmt.keyword)?;
        self.write_token(&while_stmt.open_paren)?;
        self.write_expr(&while_stmt.test)?;
        self.write_token(&while_stmt.close_paren)?;
        self.write_stmt(&while_stmt.body)
    }

    pub fn write_do_while<Ast: AsRef<str>>(&mut self, do_while: &DoWhileStmt<Ast>) -> Res {
        self.write_token(&do_while.keyword_do)?;
        self.write_stmt(&do_while.body)?;
        self.write_token(&do_while.keyword_while)?;
        self.write_token(&do_while.open_paren)?;
        self.write_expr(&do_while.test)?;
        self.write_token(&do_while.close_paren)?;
        self.write_maybe_token(do_while.semi_colon.as_ref())
    }

    pub fn write_for_stmt<Ast: AsRef<str>>(&mut self, for_stmt: &ForStmt<Ast>) -> Res {
        self.write_token(&for_stmt.keyword)?;
        self.write_token(&for_stmt.open_paren)?;
        if let Some(init) = &for_stmt.init {
            self.write_loop_init(init)?;
        }
        self.write_token(&for_stmt.semi1)?;
        if let Some(test) = &for_stmt.test {
            self.write_expr(test)?;
        }
        self.write_token(&for_stmt.semi2)?;
        if let Some(update) = &for_stmt.update {
            self.write_expr(update)?;
        }
        self.write_token(&for_stmt.close_paren)?;
        self.write_stmt(&for_stmt.body)
    }

    pub fn write_loop_init<Ast: AsRef<str>>(&mut self, loop_init: &LoopInit<Ast>) -> Res {
        match loop_init {
            LoopInit::Variable(kind, decls) => {
                self.write_variable_kind(kind)?;
                self.write_var_decls_(decls)
            }
            LoopInit::Expr(expr) => self.write_expr(expr),
        }
    }

    pub fn write_for_in<Ast: AsRef<str>>(&mut self, for_in: &ForInStmt<Ast>) -> Res {
        self.write_token(&for_in.keyword_for)?;
        self.write_token(&for_in.open_paren)?;
        self.write_loop_left(&for_in.left)?;
        self.write_token(&for_in.keyword_in)?;
        self.write_expr(&for_in.right)?;
        self.write_token(&for_in.close_paren)?;
        self.write_stmt(&for_in.body)
    }

    pub fn write_for_of<Ast: AsRef<str>>(&mut self, for_of: &ForOfStmt<Ast>) -> Res {
        self.write_token(&for_of.keyword_for)?;
        self.write_token(&for_of.open_paren)?;
        self.write_loop_left(&for_of.left)?;
        self.write_token(&for_of.keyword_of)?;
        self.write_expr(&for_of.right)?;
        self.write_token(&for_of.close_paren)?;
        self.write_stmt(&for_of.body)
    }

    pub fn write_loop_left<Ast: AsRef<str>>(&mut self, loop_left: &LoopLeft<Ast>) -> Res {
        match loop_left {
            LoopLeft::Expr(expr) => self.write_expr(expr),
            LoopLeft::Variable(kind, decl) => {
                self.write_variable_kind(kind)?;
                self.write_var_decl(decl)
            }
            LoopLeft::Pat(pat) => self.write_pat(pat),
        }
    }

    pub fn write_block_stmt<Ast: AsRef<str>>(&mut self, block: &stmt::BlockStmt<Ast>) -> Res {
        self.write_token(&block.open_brace)?;
        self.write_parts(&block.stmts)?;
        self.write_token(&block.close_brace)
    }

    pub fn write_class_body<Ast: AsRef<str>>(&mut self, body: &ClassBody<Ast>) -> Res {
        self.write_token(&body.open_brace)?;
        for prop in &body.props {
            self.write_prop(prop)?;
        }
        self.write_token(&body.close_brace)
    }

    pub fn write_prop<Ast: AsRef<str>>(&mut self, prop: &Prop<Ast>) -> Res {
        match prop {
            Prop::Init(init) => self.write_prop_init(init),
            Prop::Method(method) => self.write_prop_method(method),
            Prop::Ctor(ctor) => self.write_ctor(ctor),
            Prop::Get(get) => self.write_prop_get(get),
            Prop::Set(set) => self.write_prop_set(set),
        }
    }

    pub fn write_prop_init<Ast: AsRef<str>>(&mut self, prop_init: &PropInit<Ast>) -> Res {
        self.write_prop_init_key(&prop_init.key)?;
        if let Some(colon) = &prop_init.colon {
            self.write_token(colon)?;
        }
        if let Some(init) = &prop_init.value {
            // special case for `{ i = 0 }`
            if let PropValue::Pat(Pat::Assign(assign)) = init {
                if assign.left.loc() == prop_init.key.loc() {
                    self.write_assign_op(&assign.operator)?;
                    self.write_expr(&assign.right)?;
                    return Ok(());
                }
            }
            self.write_prop_value(init)?;
        }
        Ok(())
    }

    pub fn write_prop_init_key<Ast: AsRef<str>>(&mut self, prop_key: &PropInitKey<Ast>) -> Res {
        if let Some((open, close)) = &prop_key.brackets {
            self.write_token(open)?;
            self.write_prop_key(&prop_key.value)?;
            self.write_token(close)
        } else {
            self.write_prop_key(&prop_key.value)
        }
    }

    pub fn write_prop_key<Ast: AsRef<str>>(&mut self, key: &PropKey<Ast>) -> Res {
        match key {
            PropKey::Lit(lit) => self.write_lit(lit),
            PropKey::Expr(expr) => self.write_expr(expr),
            PropKey::Pat(pat) => self.write_pat(pat),
        }
    }

    pub fn write_prop_value<Ast: AsRef<str>>(&mut self, value: &PropValue<Ast>) -> Res {
        match value {
            PropValue::Expr(expr) => self.write_expr(expr),
            PropValue::Pat(pat) => self.write_pat(pat),
            PropValue::Method(method) => self.write_prop_method(method),
        }
    }

    pub fn write_prop_method<Ast: AsRef<str>>(&mut self, method: &PropMethod<Ast>) -> Res {
        if let Some(keyword) = &method.keyword_static {
            self.write_token(keyword)?;
        }
        if let Some(keyword) = &method.keyword_async {
            self.write_token(keyword)?;
        }
        if let Some(star) = &method.star {
            self.write_token(star)?;
        }
        self.write_prop_init_key(&method.id)?;
        self.write_token(&method.open_paren)?;
        self.write_func_params(&method.params)?;
        self.write_token(&method.close_paren)?;
        self.write_func_body(&method.body)
    }

    pub fn write_ctor<Ast: AsRef<str>>(&mut self, ctor: &PropCtor<Ast>) -> Res {
        self.write_prop_init_key(&ctor.keyword)?;
        self.write_token(&ctor.open_paren)?;
        self.write_func_params(&ctor.params)?;
        self.write_token(&ctor.close_paren)?;
        self.write_func_body(&ctor.body)
    }

    pub fn write_prop_get<Ast: AsRef<str>>(&mut self, get: &PropGet<Ast>) -> Res {
        if let Some(slice) = &get.keyword_static {
            self.write_token(slice)?;
        }
        self.write_token(&get.keyword_get)?;
        self.write_prop_init_key(&get.id)?;
        self.write_token(&get.open_paren)?;
        self.write_token(&get.close_paren)?;
        self.write_func_body(&get.body)
    }

    pub fn write_prop_set<Ast: AsRef<str>>(&mut self, set: &PropSet<Ast>) -> Res {
        if let Some(slice) = &set.keyword_static {
            self.write_token(slice)?;
        }
        self.write_token(&set.keyword_set)?;
        self.write_prop_init_key(&set.id)?;
        self.write_token(&set.open_paren)?;
        self.write_func_arg(&set.arg.item)?;
        self.write_maybe_token(set.arg.comma.as_ref())?;
        self.write_token(&set.close_paren)?;
        self.write_func_body(&set.body)
    }

    pub fn write_func<Ast: AsRef<str>>(&mut self, func: &Func<Ast>) -> Res {
        if let Some(a) = &func.keyword_async {
            self.write_token(a)?;
        }
        self.write_token(&func.keyword)?;
        if let Some(star) = &func.star {
            self.write_token(star)?;
        }
        if let Some(id) = &func.id {
            self.write_ident(id)?;
        }
        self.write_token(&func.open_paren)?;
        self.write_func_params(&func.params)?;
        self.write_token(&func.close_paren)?;
        self.write_func_body(&func.body)
    }

    pub fn write_func_params<Ast: AsRef<str>>(&mut self, args: &[ListEntry<FuncArg<Ast>>]) -> Res {
        for entry in args {
            self.write_func_arg(&entry.item)?;
            self.write_maybe_token(entry.comma.as_ref())?;
        }
        Ok(())
    }

    pub fn write_func_arg<Ast: AsRef<str>>(&mut self, arg: &FuncArg<Ast>) -> Res {
        match arg {
            FuncArg::Expr(expr) => self.write_expr(expr),
            FuncArg::Pat(pat) => self.write_pat(pat),
            FuncArg::Rest(rest) => {
                self.write_token(&rest.dots)?;
                self.write_pat(&rest.pat)
            }
        }
    }

    pub fn write_func_body<Ast: AsRef<str>>(&mut self, body: &FuncBody<Ast>) -> Res {
        self.write_token(&body.open_brace)?;
        self.write_parts(&body.stmts)?;
        self.write_token(&body.close_brace)
    }

    pub fn write_pat<Ast: AsRef<str>>(&mut self, pat: &Pat<Ast>) -> Res {
        match pat {
            Pat::Ident(ident) => self.write_ident(ident),
            Pat::Obj(obj) => self.write_obj_pat(obj),
            Pat::Array(arr) => self.write_arr_pat(arr),
            Pat::Assign(assign) => self.write_assign_pat(assign),
        }
    }

    pub fn write_obj_pat<Ast: AsRef<str>>(&mut self, pat: &ObjPat<Ast>) -> Res {
        self.write_token(&pat.open_brace)?;
        self.write_obj_pat_parts(&pat.props)?;
        self.write_token(&pat.close_brace)
    }

    pub fn write_obj_pat_parts<Ast: AsRef<str>>(
        &mut self,
        props: &[ListEntry<ObjPatPart<Ast>>],
    ) -> Res {
        for entry in props {
            self.write_obj_pat_part(&entry.item)?;
            self.write_maybe_token(entry.comma.as_ref())?;
        }
        Ok(())
    }
    pub fn write_obj_pat_part<Ast: AsRef<str>>(&mut self, props: &ObjPatPart<Ast>) -> Res {
        match props {
            ObjPatPart::Assign(pat) => self.write_prop(pat),
            ObjPatPart::Rest(rest) => self.write_rest_pat(rest),
        }
    }

    pub fn write_arr_pat<Ast: AsRef<str>>(&mut self, arr: &ArrayPat<Ast>) -> Res {
        self.write_token(&arr.open_bracket)?;
        self.write_arr_pat_parts(&arr.elements)?;
        self.write_token(&arr.close_bracket)
    }

    pub fn write_arr_pat_parts<Ast: AsRef<str>>(
        &mut self,
        parts: &[ListEntry<Option<ArrayPatPart<Ast>>>],
    ) -> Res {
        for part in parts {
            if let Some(part) = &part.item {
                self.write_arr_pat_part(part)?;
            }
            self.write_maybe_token(part.comma.as_ref())?;
        }
        Ok(())
    }

    pub fn write_arr_pat_part<Ast: AsRef<str>>(&mut self, part: &ArrayPatPart<Ast>) -> Res {
        match part {
            ArrayPatPart::Pat(pat) => self.write_pat(pat),
            ArrayPatPart::Expr(expr) => self.write_expr(expr),
            ArrayPatPart::Rest(rest) => self.write_rest_pat(rest),
        }
    }

    pub fn write_assign_pat<Ast: AsRef<str>>(&mut self, assign: &AssignPat<Ast>) -> Res {
        self.write_pat(&assign.left)?;
        self.write_assign_op(&assign.operator)?;
        self.write_expr(&assign.right)
    }

    pub fn write_rest_pat<Ast: AsRef<str>>(&mut self, rest: &RestPat<Ast>) -> Res {
        self.write_token(&rest.dots)?;
        self.write_pat(&rest.pat)
    }

    pub fn write_expr<Ast: AsRef<str>>(&mut self, expr: &Expr<Ast>) -> Res {
        match expr {
            Expr::Array(arr) => self.write_arr_expr(arr),
            Expr::ArrowFunc(arrow) => self.write_arrow_func(arrow),
            Expr::ArrowParamPlaceHolder(_apph) => {
                panic!("Write arrow parameter_place_holder...: ")
            }
            Expr::Assign(assign) => self.write_assign_expr(assign),
            Expr::Await(await_expr) => self.write_await_expr(await_expr),
            Expr::Binary(bin) => self.write_bin_expr(bin),
            Expr::Class(class) => self.write_class(class),
            Expr::Call(call) => self.write_call_expr(call),
            Expr::Conditional(cond) => self.write_conditional(cond),
            Expr::Func(func) => self.write_func(func),
            Expr::Ident(ident) => self.write_ident(ident),
            Expr::Lit(lit) => self.write_lit(lit),
            Expr::Logical(log) => self.write_logical_expr(log),
            Expr::Member(member) => self.write_member_expr(member),
            Expr::MetaProp(meta) => self.write_meta_prop(meta),
            Expr::New(new) => self.write_new_expr(new),
            Expr::Obj(obj) => self.write_obj_expr(obj),
            Expr::Sequence(seq) => self.write_exprs(seq),
            Expr::Spread(spread) => {
                self.write_token(&spread.dots)?;
                self.write_expr(&spread.expr)
            }
            Expr::Super(slice) => self.write_token(slice),
            Expr::TaggedTemplate(temp) => self.write_tagged_template_expr(temp),
            Expr::This(slice) => self.write_token(slice),
            Expr::Unary(unary) => self.write_unary_expr(unary),
            Expr::Update(update) => self.write_update_expr(update),
            Expr::Wrapped(wrapped) => {
                self.write_token(&wrapped.open_paren)?;
                self.write_expr(&wrapped.expr)?;
                self.write_token(&wrapped.close_paren)
            }
            Expr::Yield(expr) => self.write_yield_expr(expr),
        }
    }

    pub fn write_arr_expr<Ast: AsRef<str>>(&mut self, arr: &ArrayExpr<Ast>) -> Res {
        self.write_token(&arr.open_bracket)?;
        for ele in &arr.elements {
            if let Some(expr) = &ele.item {
                self.write_expr(expr)?;
            }
            self.write_maybe_token(ele.comma.as_ref())?;
        }
        self.write_token(&arr.close_bracket)
    }

    pub fn write_arrow_func<Ast: AsRef<str>>(&mut self, arrow: &ArrowFuncExpr<Ast>) -> Res {
        if let Some(slice) = &arrow.keyword {
            self.write_token(slice)?;
        }
        if let Some(star) = &arrow.star {
            self.write_token(star)?;
        }
        if let Some(open) = &arrow.open_paren {
            self.write_token(open)?;
        }
        self.write_func_params(&arrow.params)?;
        if let Some(close) = &arrow.close_paren {
            self.write_token(close)?;
        }
        self.write_token(&arrow.arrow)?;
        match &arrow.body {
            ArrowFuncBody::FuncBody(body) => self.write_func_body(body),
            ArrowFuncBody::Expr(expr) => self.write_expr(expr),
        }
    }

    pub fn write_assign_expr<Ast: AsRef<str>>(&mut self, assign: &AssignExpr<Ast>) -> Res {
        self.write_assign_left(&assign.left)?;
        self.write_assign_op(&assign.operator)?;
        self.write_expr(&assign.right)
    }

    pub fn write_assign_left<Ast: AsRef<str>>(&mut self, assign_left: &AssignLeft<Ast>) -> Res {
        match assign_left {
            AssignLeft::Pat(pat) => self.write_pat(pat),
            AssignLeft::Expr(expr) => self.write_expr(expr),
        }
    }

    pub fn write_await_expr<Ast: AsRef<str>>(&mut self, await_expr: &AwaitExpr<Ast>) -> Res {
        self.write_token(&await_expr.keyword)?;
        self.write_expr(&await_expr.expr)
    }

    pub fn write_bin_expr<Ast: AsRef<str>>(&mut self, bin: &BinaryExpr<Ast>) -> Res {
        self.write_expr(&bin.left)?;
        self.write_bin_op(&bin.operator)?;
        self.write_expr(&bin.right)
    }

    pub fn write_bin_op(&mut self, op: &BinaryOp) -> Res {
        match op {
            BinaryOp::Equal(slice) => self.write_token(slice),
            BinaryOp::NotEqual(slice) => self.write_token(slice),
            BinaryOp::StrictEqual(slice) => self.write_token(slice),
            BinaryOp::StrictNotEqual(slice) => self.write_token(slice),
            BinaryOp::LessThan(slice) => self.write_token(slice),
            BinaryOp::GreaterThan(slice) => self.write_token(slice),
            BinaryOp::LessThanEqual(slice) => self.write_token(slice),
            BinaryOp::GreaterThanEqual(slice) => self.write_token(slice),
            BinaryOp::LeftShift(slice) => self.write_token(slice),
            BinaryOp::RightShift(slice) => self.write_token(slice),
            BinaryOp::UnsignedRightShift(slice) => self.write_token(slice),
            BinaryOp::Plus(slice) => self.write_token(slice),
            BinaryOp::Minus(slice) => self.write_token(slice),
            BinaryOp::Times(slice) => self.write_token(slice),
            BinaryOp::Over(slice) => self.write_token(slice),
            BinaryOp::Mod(slice) => self.write_token(slice),
            BinaryOp::Or(slice) => self.write_token(slice),
            BinaryOp::XOr(slice) => self.write_token(slice),
            BinaryOp::And(slice) => self.write_token(slice),
            BinaryOp::In(slice) => self.write_token(slice),
            BinaryOp::InstanceOf(slice) => self.write_token(slice),
            BinaryOp::PowerOf(slice) => self.write_token(slice),
        }
    }

    pub fn write_call_expr<Ast: AsRef<str>>(&mut self, call: &CallExpr<Ast>) -> Res {
        self.write_expr(&call.callee)?;
        self.write_token(&call.open_paren)?;
        self.write_exprs(&call.arguments)?;
        self.write_token(&call.close_paren)
    }

    pub fn write_exprs<Ast: AsRef<str>>(&mut self, exprs: &[ListEntry<Expr<Ast>>]) -> Res {
        for expr in exprs {
            self.write_expr(&expr.item)?;
            self.write_maybe_token(expr.comma.as_ref())?;
        }
        Ok(())
    }

    pub fn write_conditional<Ast: AsRef<str>>(&mut self, cond: &ConditionalExpr<Ast>) -> Res {
        self.write_expr(&cond.test)?;
        self.write_token(&cond.question_mark)?;
        self.write_expr(&cond.consequent)?;
        self.write_token(&cond.colon)?;
        self.write_expr(&cond.alternate)
    }

    pub fn write_logical_expr<Ast: AsRef<str>>(&mut self, log: &LogicalExpr<Ast>) -> Res {
        self.write_expr(&log.left)?;
        match &log.operator {
            LogicalOp::Or(op) => self.write_token(op)?,
            LogicalOp::And(op) => self.write_token(op)?,
        };
        self.write_expr(&log.right)
    }

    pub fn write_member_expr<Ast: AsRef<str>>(&mut self, member: &MemberExpr<Ast>) -> Res {
        self.write_expr(&member.object)?;
        match &member.indexer {
            MemberIndexer::Period(period) => {
                self.write_token(period)?;
                self.write_expr(&member.property)
            }
            MemberIndexer::Computed {
                open_bracket,
                close_bracket,
            } => {
                self.write_token(open_bracket)?;
                self.write_expr(&member.property)?;
                self.write_token(close_bracket)
            }
        }
    }

    pub fn write_meta_prop<Ast: AsRef<str>>(&mut self, meta: &MetaProp<Ast>) -> Res {
        self.write_ident(&meta.meta)?;
        self.write_token(&meta.dot)?;
        self.write_ident(&meta.property)
    }

    pub fn write_new_expr<Ast: AsRef<str>>(&mut self, new: &NewExpr<Ast>) -> Res {
        self.write_token(&new.keyword)?;
        self.write_expr(&new.callee)?;
        self.write_maybe_token(new.open_paren.as_ref())?;
        self.write_exprs(&new.arguments)?;
        self.write_maybe_token(new.close_paren.as_ref())
    }

    pub fn write_obj_expr<Ast: AsRef<str>>(&mut self, obj: &ObjExpr<Ast>) -> Res {
        self.write_token(&obj.open_brace)?;
        self.write_obj_props(&obj.props)?;
        self.write_token(&obj.close_brace)
    }

    pub fn write_obj_props<Ast: AsRef<str>>(&mut self, props: &[ListEntry<ObjProp<Ast>>]) -> Res {
        for prop in props {
            match &prop.item {
                ObjProp::Prop(prop) => self.write_prop(prop)?,
                ObjProp::Spread(spread) => {
                    self.write_token(&spread.dots)?;
                    self.write_expr(&spread.expr)?;
                }
            }
            self.write_maybe_token(prop.comma.as_ref())?;
        }
        Ok(())
    }

    pub fn write_tagged_template_expr<Ast: AsRef<str>>(
        &mut self,
        temp: &TaggedTemplateExpr<Ast>,
    ) -> Res {
        self.write_expr(&temp.tag)?;
        self.write_template_lit(&temp.quasi)
    }

    pub fn write_unary_expr<Ast: AsRef<str>>(&mut self, unary: &UnaryExpr<Ast>) -> Res {
        if unary.prefix() {
            self.write_unary_op(&unary.operator)?;
            self.write_expr(&unary.argument)
        } else {
            self.write_expr(&unary.argument)?;
            self.write_unary_op(&unary.operator)
        }
    }

    pub fn write_unary_op(&mut self, op: &UnaryOp) -> Res {
        match op {
            UnaryOp::Minus(slice) => self.write_token(slice),
            UnaryOp::Plus(slice) => self.write_token(slice),
            UnaryOp::Not(slice) => self.write_token(slice),
            UnaryOp::Tilde(slice) => self.write_token(slice),
            UnaryOp::TypeOf(slice) => self.write_token(slice),
            UnaryOp::Void(slice) => self.write_token(slice),
            UnaryOp::Delete(slice) => self.write_token(slice),
        }
    }

    pub fn write_update_expr<Ast: AsRef<str>>(&mut self, expr: &UpdateExpr<Ast>) -> Res {
        let op: &dyn Token = match &expr.operator {
            UpdateOp::Increment(slice) => slice,
            UpdateOp::Decrement(slice) => slice,
        };
        if expr.prefix() {
            self.write_token(op)?;
            self.write_expr(&expr.argument)
        } else {
            self.write_expr(&expr.argument)?;
            self.write_token(op)
        }
    }

    pub fn write_yield_expr<Ast: AsRef<str>>(&mut self, expr: &YieldExpr<Ast>) -> Res {
        self.write_token(&expr.keyword)?;
        if let Some(star) = &expr.star {
            self.write_token(star)?;
        }
        if let Some(arg) = &expr.argument {
            self.write_expr(arg)?;
        }
        Ok(())
    }

    pub fn write_maybe_token<Ast: Token>(&mut self, token: Option<&Ast>) -> Res {
        if let Some(token) = token {
            self.write_token(token)?;
        }
        Ok(())
    }

    pub fn write_token(&mut self, token: &dyn Token) -> Res {
        self.write_raw_slice(
            SourceLocation {
                start: token.start(),
                end: token.end(),
            },
            &token.as_str(),
        )
    }

    pub fn write_maybe_slice<Ast: AsRef<str>>(&mut self, slice: &Option<Slice<Ast>>) -> Res {
        if let Some(slice) = slice.as_ref() {
            self.write_slice(slice)?;
        }
        Ok(())
    }

    pub fn write_lit<Ast: AsRef<str>>(&mut self, lit: &Lit<Ast>) -> Res {
        match lit {
            Lit::Null(slice) => self.write_token(slice),
            Lit::String(s) => {
                self.write_token(&s.open_quote)?;
                self.write_slice(&s.content)?;
                self.write_token(&s.close_quote)
            }
            Lit::Number(slice) => self.write_slice(slice),
            Lit::Boolean(slice) => self.write_token(slice),
            Lit::RegEx(re) => {
                self.write_token(&re.open_slash)?;
                self.write_slice(&re.pattern)?;
                self.write_token(&re.close_slash)?;
                if let Some(flags) = re.flags.as_ref() {
                    self.write_slice(flags)?;
                }
                Ok(())
            }
            Lit::Template(temp) => self.write_template_lit(temp),
        }
    }

    pub fn write_template_lit<Ast: AsRef<str>>(&mut self, temp: &TemplateLit<Ast>) -> Res {
        let mut quasi = temp.quasis.iter();
        let mut exprs = temp.expressions.iter();
        while let Some(quasi) = quasi.next() {
            self.write_token(&quasi.open_quote)?;
            self.write_slice(&quasi.content)?;
            self.write_token(&quasi.close_quote)?;
            if matches!(quasi.close_quote, QuasiQuote::BackTick(_)) {
                break;
            }
            if let Some(expr) = exprs.next() {
                self.write_expr(expr)?;
            }
        }
        Ok(())
    }

    pub fn write_assign_op(&mut self, op: &AssignOp) -> Res {
        match op {
            AssignOp::Equal(slice) => self.write_token(slice),
            AssignOp::PlusEqual(slice) => self.write_token(slice),
            AssignOp::MinusEqual(slice) => self.write_token(slice),
            AssignOp::TimesEqual(slice) => self.write_token(slice),
            AssignOp::DivEqual(slice) => self.write_token(slice),
            AssignOp::ModEqual(slice) => self.write_token(slice),
            AssignOp::LeftShiftEqual(slice) => self.write_token(slice),
            AssignOp::RightShiftEqual(slice) => self.write_token(slice),
            AssignOp::UnsignedRightShiftEqual(slice) => self.write_token(slice),
            AssignOp::OrEqual(slice) => self.write_token(slice),
            AssignOp::XOrEqual(slice) => self.write_token(slice),
            AssignOp::AndEqual(slice) => self.write_token(slice),
            AssignOp::PowerOfEqual(slice) => self.write_token(slice),
        }
    }

    pub fn write_ident<Ast: AsRef<str>>(&mut self, ident: &Ident<Ast>) -> Res {
        self.write_slice(&ident.slice)
    }

    pub fn write_alias<Ast: AsRef<str>>(&mut self, alias: &Alias<Ast>) -> Res {
        self.write_token(&alias.keyword)?;
        self.write_ident(&alias.ident)
    }

    fn write_slice<Ast: AsRef<str>>(&mut self, slice: &Slice<Ast>) -> Res {
        self.write_raw_slice(slice.loc, &slice.source)
    }

    fn write_raw_slice<Ast: AsRef<str>>(&mut self, loc: SourceLocation, text: &Ast) -> Res {
        println!("{loc:?}, {}", text.as_ref());
        let new_lines = loc.start.line - self.last_out.line;
        self.write(&"\n".repeat(new_lines as usize))?;
        let leading = if new_lines == 0 {
            if self.last_out.column > loc.start.column {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!(
                        "attempt to subtract with overflow {:?} - {:?} ({:?})",
                        self.last_out,
                        loc.start,
                        text.as_ref()
                    ),
                ));
            }
            let prefix = loc.start.column - self.last_out.column;
            " ".repeat(prefix as usize)
        } else {
            " ".repeat((loc.start.column.saturating_sub(1)) as usize)
        };
        self.write(&leading)?;
        self.last_out = loc.end;
        self.write(text.as_ref())?;
        Ok(())
    }

    fn write(&mut self, s: impl AsRef<str>) -> Res {
        let _ = self.out.write(s.as_ref().as_bytes())?;
        Ok(())
    }

    pub fn write_comment(&mut self, comment: Comment<&str>) -> Res {
        match comment.kind {
            CommentKind::Single => self.write(&format!("//{}", comment.content))?,
            CommentKind::Multi => self.write(&format!("/*{}\n*/", comment.content))?,
            CommentKind::Html => self.write(&format!(
                "<!--{}-->{}",
                comment.content,
                comment.tail_content.unwrap_or("")
            ))?,
            CommentKind::Hashbang => self.write(&format!("#! {}", comment.content))?,
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::write_str::WriteString;

    #[test]
    fn write_slices() {
        let js = "function x() {\n    console.log('hi');\n}";
        let mut parser = ressa::spanned::Parser::builder().js(js).build().unwrap();
        let prog = parser.parse().unwrap();
        let mut write = WriteString::new();
        let mut writer = SpannedWriter::new(write.generate_child());
        writer.write_program(&prog).unwrap();
        assert_eq!(js, write.get_string().unwrap())
    }

    #[test]
    fn string_with_new_lines() {
        let js = r"' \
        '";
        let mut parser = ressa::spanned::Parser::builder().js(js).build().unwrap();
        let prog = parser.parse().unwrap();
        let mut write = WriteString::new();
        let mut writer = SpannedWriter::new(write.generate_child());
        writer.write_program(&prog).unwrap();
        assert_eq!(js, write.get_string().unwrap())
    }
}
