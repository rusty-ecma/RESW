use resast::spanned::{
    decl::{
        Decl, DefaultExportDeclValue, DefaultImportSpec, ExportList, ExportSpecifier,
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
    AssignOp, BinaryOp, Class, ClassBody, Dir, Func, FuncArg, FuncBody, Ident, ListEntry,
    LogicalOp, Node, Position, Program, ProgramPart, Slice, UnaryOp, UpdateOp, VarKind,
};
use ress::tokens::{Comment, CommentKind};
use std::io::Write;

type Res = Result<(), std::io::Error>;

pub struct SpannedWriter<T> {
    out: T,
    last_out: Position,
    last_slice: Slice<'static>,
}

impl<T> SpannedWriter<T>
where
    T: Write,
{
    pub fn new(out: T) -> Self {
        Self {
            out,
            last_out: Position { line: 0, column: 0 },
            last_slice: Slice {
                source: std::borrow::Cow::Owned(String::new()),
                loc: resast::spanned::SourceLocation {
                    start: Position { line: 0, column: 0 },
                    end: Position { line: 0, column: 0 },
                },
            },
        }
    }

    pub fn write_program(&mut self, program: &Program) -> Res {
        match program {
            Program::Mod(inner) => self.write_parts(inner),
            Program::Script(inner) => self.write_parts(inner),
        }
    }

    pub fn write_parts(&mut self, parts: &[ProgramPart]) -> Res {
        for part in parts {
            self.write_part(part)?;
        }
        Ok(())
    }

    pub fn write_part(&mut self, part: &ProgramPart) -> Res {
        match part {
            ProgramPart::Dir(dir) => self.write_dir(dir),
            ProgramPart::Decl(decl) => self.write_decl(decl),
            ProgramPart::Stmt(stmt) => self.write_stmt(stmt),
        }
    }

    pub fn write_dir(&mut self, dir: &Dir) -> Res {
        self.write_lit(&dir.expr)
    }

    pub fn write_decl(&mut self, decl: &Decl) -> Res {
        match decl {
            Decl::Var { decls, semi_colon } => {
                self.write_var_decls(decls)?;
                if let Some(semi) = semi_colon {
                    self.write_slice(semi)?;
                }
                Ok(())
            }
            Decl::Func(func) => self.write_func(func),
            Decl::Class(class) => self.write_class(class),
            Decl::Import { import, semi_colon } => {
                self.write_mod_import(import)?;
                self.write_maybe_slice(semi_colon)
            }
            Decl::Export { export, semi_colon } => {
                self.write_mod_export(export)?;
                self.write_maybe_slice(semi_colon)
            }
        }
    }

    pub fn write_mod_import(&mut self, import: &ModImport) -> Res {
        self.write_slice(&import.keyword_import)?;
        self.write_mod_import_specs(&import.specifiers)?;
        self.write_maybe_slice(&import.keyword_from)?;
        self.write_lit(&import.source)
    }

    pub fn write_mod_import_specs(&mut self, specs: &[ListEntry<ImportSpecifier>]) -> Res {
        for spec in specs {
            self.write_import_spec(&spec.item)?;
            self.write_maybe_slice(&spec.comma)?;
        }
        Ok(())
    }

    pub fn write_import_spec(&mut self, spec: &ImportSpecifier) -> Res {
        match spec {
            ImportSpecifier::Normal(normal) => self.write_normal_import_specs(normal),
            ImportSpecifier::Default(default) => self.write_default_import(default),
            ImportSpecifier::Namespace(namespace) => self.write_namespace_import(namespace),
        }
    }

    pub fn write_normal_import_specs(&mut self, specs: &NormalImportSpecs) -> Res {
        self.write_slice(&specs.open_brace)?;
        for spec in &specs.specs {
            self.write_normal_import_spec(&spec)?;
        }
        self.write_slice(&specs.close_brace)
    }

    pub fn write_normal_import_spec(&mut self, spec: &NormalImportSpec) -> Res {
        self.write_ident(&spec.imported)?;
        if let Some(alias) = &spec.alias {
            self.write_slice(&alias.keyword)?;
            self.write_ident(&alias.ident)?;
        }
        Ok(())
    }

    pub fn write_default_import(&mut self, spec: &DefaultImportSpec) -> Res {
        self.write_ident(&spec.id)
    }

    pub fn write_namespace_import(&mut self, spec: &NamespaceImportSpec) -> Res {
        self.write_slice(&spec.star)?;
        self.write_slice(&spec.keyword)?;
        self.write_ident(&spec.ident)
    }

    pub fn write_mod_export(&mut self, export: &ModExport) -> Res {
        self.write_slice(&export.keyword)?;
        self.write_mod_export_spec(&export.spec)
    }

    pub fn write_mod_export_spec(&mut self, spec: &ModExportSpecifier) -> Res {
        match spec {
            ModExportSpecifier::Default { keyword, value } => {
                self.write_slice(keyword)?;
                self.write_default_export_value(value)
            }
            ModExportSpecifier::Named(named) => self.write_named_export_decl(named),
            ModExportSpecifier::All {
                star,
                keyword,
                name,
            } => {
                self.write_slice(star)?;
                self.write_slice(keyword)?;
                self.write_lit(name)
            }
        }
    }

    pub fn write_default_export_value(&mut self, value: &DefaultExportDeclValue) -> Res {
        match value {
            DefaultExportDeclValue::Decl(decl) => self.write_decl(decl),
            DefaultExportDeclValue::Expr(expr) => self.write_expr(expr),
        }
    }

    pub fn write_named_export_decl(&mut self, named: &NamedExportDecl) -> Res {
        match named {
            NamedExportDecl::Decl(decl) => self.write_decl(decl),
            NamedExportDecl::Specifier(spec) => self.write_named_export_spec(spec),
        }
    }

    pub fn write_named_export_spec(&mut self, spec: &NamedExportSpec) -> Res {
        self.write_export_list(&spec.list)?;
        if let Some(source) = &spec.source {
            self.write_named_export_source(source)?;
        }
        Ok(())
    }

    pub fn write_export_list(&mut self, list: &ExportList) -> Res {
        self.write_slice(&list.open_brace)?;
        for spec in &list.elements {
            self.write_export_spec(&spec.item)?;
            self.write_maybe_slice(&spec.comma)?;
        }
        self.write_slice(&list.close_brace)
    }

    pub fn write_named_export_source(&mut self, source: &NamedExportSource) -> Res {
        self.write_slice(&source.keyword_from)?;
        self.write_lit(&source.module)
    }

    pub fn write_export_spec(&mut self, spec: &ExportSpecifier) -> Res {
        self.write_ident(&spec.local)?;
        if let Some(alias) = &spec.alias {
            self.write_slice(&alias.keyword)?;
            self.write_ident(&alias.ident)?;
        }
        Ok(())
    }

    pub fn write_stmt(&mut self, stmt: &Stmt) -> Res {
        match stmt {
            Stmt::Expr { expr, semi_colon } => {
                self.write_expr(expr)?;
                self.write_maybe_slice(semi_colon)
            }
            Stmt::Block(block) => {
                self.write_slice(&block.open_brace)?;
                self.write_parts(&block.stmts)?;
                self.write_slice(&block.close_brace)
            }
            Stmt::Empty(slice) => self.write_slice(slice),
            Stmt::Debugger {
                keyword,
                semi_colon,
            } => {
                self.write_slice(keyword)?;
                self.write_maybe_slice(semi_colon)
            }
            Stmt::With(with) => {
                self.write_slice(&with.keyword)?;
                self.write_slice(&with.open_paren)?;
                self.write_expr(&with.object)?;
                self.write_slice(&with.close_paren)?;
                self.write_stmt(&with.body)
            }
            Stmt::Return {
                keyword,
                value,
                semi_colon,
            } => {
                self.write_slice(keyword)?;
                if let Some(value) = value {
                    self.write_expr(value)?;
                }
                self.write_maybe_slice(semi_colon)
            }
            Stmt::Labeled(labeled) => {
                self.write_ident(&labeled.label)?;
                self.write_slice(&labeled.colon)?;
                self.write_stmt(&labeled.body)
            }
            Stmt::Break {
                keyword,
                label,
                semi_colon,
            } => {
                self.write_slice(keyword)?;
                if let Some(label) = label {
                    self.write_ident(label)?;
                }
                self.write_maybe_slice(semi_colon)
            }
            Stmt::Continue {
                keyword,
                label,
                semi_colon,
            } => {
                self.write_slice(keyword)?;
                if let Some(label) = label {
                    self.write_ident(label)?;
                }
                self.write_maybe_slice(semi_colon)
            }
            Stmt::If(stmt) => self.write_if_stmt(stmt),
            Stmt::Switch(switch) => self.write_switch_stmt(switch),
            Stmt::Throw {
                keyword,
                expr,
                semi_colon,
            } => {
                self.write_slice(keyword)?;
                self.write_expr(expr)?;
                self.write_maybe_slice(semi_colon)
            }
            Stmt::Try(try_stmt) => self.write_try_stmt(try_stmt),
            Stmt::While(while_stmt) => self.write_while_stmt(while_stmt),
            Stmt::DoWhile(do_while) => self.write_do_while(do_while),
            Stmt::For(for_stmt) => self.write_for_stmt(for_stmt),
            Stmt::ForIn(for_in) => self.write_for_in(for_in),
            Stmt::ForOf(for_of) => self.write_for_of(for_of),
            Stmt::Var { decls, semi_colon } => {
                self.write_var_decls(decls)?;
                self.write_maybe_slice(semi_colon)
            }
        }
    }

    pub fn write_var_decls(&mut self, decls: &VarDecls) -> Res {
        self.write_var_kind(&decls.keyword)?;
        self.write_var_decls_(&decls.decls)
    }

    pub fn write_var_kind(&mut self, kind: &VarKind) -> Res {
        match kind {
            VarKind::Var(Some(slice)) => self.write_slice(slice),
            VarKind::Let(slice) => self.write_slice(slice),
            VarKind::Const(slice) => self.write_slice(slice),
            _ => Ok(()),
        }
    }

    fn write_var_decls_(&mut self, decls: &[ListEntry<VarDecl>]) -> Res {
        for entry in decls {
            self.write_var_decl(&entry.item)?;
            self.write_maybe_slice(&entry.comma)?;
        }
        Ok(())
    }

    pub fn write_var_decl(&mut self, decl: &VarDecl) -> Res {
        self.write_pat(&decl.id)?;
        if let Some(eq) = &decl.eq {
            self.write_slice(eq)?;
        }
        if let Some(init) = &decl.init {
            self.write_expr(init)?;
        }
        Ok(())
    }

    pub fn write_if_stmt(&mut self, if_stmt: &IfStmt) -> Res {
        self.write_slice(&if_stmt.keyword)?;
        self.write_slice(&if_stmt.open_paren)?;
        self.write_expr(&if_stmt.test)?;
        self.write_slice(&if_stmt.close_paren)?;
        self.write_stmt(&if_stmt.consequent)?;
        if let Some(alt) = &if_stmt.alternate {
            self.write_slice(&alt.keyword)?;
            self.write_stmt(&alt.body)?;
        }
        Ok(())
    }

    pub fn write_switch_stmt(&mut self, switch: &SwitchStmt) -> Res {
        self.write_slice(&switch.keyword)?;
        self.write_slice(&switch.open_paren)?;
        self.write_expr(&switch.discriminant)?;
        self.write_slice(&switch.close_paren)?;
        self.write_slice(&switch.open_brace)?;
        for case in &switch.cases {
            self.write_switch_case(case)?;
        }
        self.write_slice(&switch.close_brace)
    }

    pub fn write_switch_case(&mut self, case: &SwitchCase) -> Res {
        self.write_slice(&case.keyword)?;
        if let Some(test) = &case.test {
            self.write_expr(test)?;
        }
        self.write_slice(&case.colon)?;
        self.write_parts(&case.consequent)
    }

    pub fn write_class(&mut self, class: &Class) -> Res {
        self.write_slice(&class.keyword)?;
        if let Some(id) = &class.id {
            self.write_ident(id)?;
        }
        if let Some(super_class) = &class.super_class {
            self.write_slice(&super_class.keyword_extends)?;
            self.write_expr(&super_class.expr)?;
        }
        self.write_class_body(&class.body)
    }

    pub fn write_try_stmt(&mut self, try_stmt: &stmt::TryStmt) -> Res {
        self.write_slice(&try_stmt.keyword)?;
        self.write_block_stmt(&try_stmt.block)?;
        if let Some(catch) = &try_stmt.handler {
            self.write_catch_clause(catch)?
        }
        if let Some(finally) = &try_stmt.finalizer {
            self.write_finally_clause(finally)?;
        }
        Ok(())
    }

    pub fn write_catch_clause(&mut self, catch: &CatchClause) -> Res {
        self.write_slice(&catch.keyword)?;
        if let Some(param) = &catch.param {
            self.write_slice(&param.open_paren)?;
            self.write_pat(&param.param)?;
            self.write_slice(&param.close_paren)?;
        }
        self.write_block_stmt(&catch.body)
    }

    pub fn write_finally_clause(&mut self, finally: &FinallyClause) -> Res {
        self.write_slice(&finally.keyword)?;
        self.write_block_stmt(&finally.body)
    }

    pub fn write_while_stmt(&mut self, while_stmt: &WhileStmt) -> Res {
        self.write_slice(&while_stmt.keyword)?;
        self.write_slice(&while_stmt.open_paren)?;
        self.write_expr(&while_stmt.test)?;
        self.write_slice(&while_stmt.close_paren)?;
        self.write_stmt(&while_stmt.body)
    }

    pub fn write_do_while(&mut self, do_while: &DoWhileStmt) -> Res {
        self.write_slice(&do_while.keyword_do)?;
        self.write_stmt(&do_while.body)?;
        self.write_slice(&do_while.keyword_while)?;
        self.write_slice(&do_while.open_paren)?;
        self.write_expr(&do_while.test)?;
        self.write_slice(&do_while.close_paren)?;
        self.write_maybe_slice(&do_while.semi_colon)
    }

    pub fn write_for_stmt(&mut self, for_stmt: &ForStmt) -> Res {
        self.write_slice(&for_stmt.keyword)?;
        self.write_slice(&for_stmt.open_paren)?;
        if let Some(init) = &for_stmt.init {
            self.write_loop_init(init)?;
        }
        self.write_slice(&for_stmt.semi1)?;
        if let Some(test) = &for_stmt.test {
            self.write_expr(test)?;
        }
        self.write_slice(&for_stmt.semi2)?;
        if let Some(update) = &for_stmt.update {
            self.write_expr(update)?;
        }
        self.write_slice(&for_stmt.close_paren)?;
        self.write_stmt(&for_stmt.body)
    }

    pub fn write_loop_init(&mut self, loop_init: &LoopInit) -> Res {
        match loop_init {
            LoopInit::Variable(kind, decls) => {
                self.write_var_kind(kind)?;
                self.write_var_decls_(decls)
            }
            LoopInit::Expr(expr) => self.write_expr(expr),
        }
    }

    pub fn write_for_in(&mut self, for_in: &ForInStmt) -> Res {
        self.write_slice(&for_in.keyword_for)?;
        self.write_slice(&for_in.open_paren)?;
        self.write_loop_left(&for_in.left)?;
        self.write_slice(&for_in.keyword_in)?;
        self.write_expr(&for_in.right)?;
        self.write_slice(&for_in.close_paren)?;
        self.write_stmt(&for_in.body)
    }

    pub fn write_for_of(&mut self, for_of: &ForOfStmt) -> Res {
        self.write_slice(&for_of.keyword_for)?;
        self.write_slice(&for_of.open_paren)?;
        self.write_loop_left(&for_of.left)?;
        self.write_slice(&for_of.keyword_of)?;
        self.write_expr(&for_of.right)?;
        self.write_slice(&for_of.close_paren)?;
        self.write_stmt(&for_of.body)
    }

    pub fn write_loop_left(&mut self, loop_left: &LoopLeft) -> Res {
        match loop_left {
            LoopLeft::Expr(expr) => self.write_expr(expr),
            LoopLeft::Variable(kind, decl) => {
                self.write_var_kind(kind)?;
                self.write_var_decl(decl)
            }
            LoopLeft::Pat(pat) => self.write_pat(pat),
        }
    }

    pub fn write_block_stmt(&mut self, block: &stmt::BlockStmt) -> Res {
        self.write_slice(&block.open_brace)?;
        self.write_parts(&block.stmts)?;
        self.write_slice(&block.close_brace)
    }

    pub fn write_class_body(&mut self, body: &ClassBody) -> Res {
        self.write_slice(&body.open_brace)?;
        for prop in &body.props {
            self.write_prop(prop)?;
        }
        self.write_slice(&body.close_brace)
    }

    pub fn write_prop(&mut self, prop: &Prop) -> Res {
        match prop {
            Prop::Init(init) => self.write_prop_init(init),
            Prop::Method(method) => self.write_prop_method(method),
            Prop::Ctor(ctor) => self.write_ctor(ctor),
            Prop::Get(get) => self.write_prop_get(get),
            Prop::Set(set) => self.write_prop_set(set),
        }
    }

    pub fn write_prop_init(&mut self, prop_init: &PropInit) -> Res {
        self.write_prop_init_key(&prop_init.key)?;
        if let Some(colon) = &prop_init.colon {
            self.write_slice(colon)?;
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

    pub fn write_prop_init_key(&mut self, prop_key: &PropInitKey) -> Res {
        if let Some((open, close)) = &prop_key.brackets {
            self.write_slice(open)?;
            self.write_prop_key(&prop_key.value)?;
            self.write_slice(close)
        } else {
            self.write_prop_key(&prop_key.value)
        }
    }

    pub fn write_prop_key(&mut self, key: &PropKey) -> Res {
        match key {
            PropKey::Lit(lit) => self.write_lit(lit),
            PropKey::Expr(expr) => self.write_expr(expr),
            PropKey::Pat(pat) => self.write_pat(pat),
        }
    }

    pub fn write_prop_value(&mut self, value: &PropValue) -> Res {
        match value {
            PropValue::Expr(expr) => self.write_expr(expr),
            PropValue::Pat(pat) => self.write_pat(pat),
            PropValue::Method(method) => self.write_prop_method(method),
        }
    }

    pub fn write_prop_method(&mut self, method: &PropMethod) -> Res {
        if let Some(keyword) = &method.keyword_static {
            self.write_slice(keyword)?;
        }
        if let Some(keyword) = &method.keyword_async {
            self.write_slice(keyword)?;
        }
        if let Some(star) = &method.star {
            self.write_slice(star)?;
        }
        self.write_prop_init_key(&method.id)?;
        self.write_slice(&method.open_paren)?;
        self.write_func_params(&method.params)?;
        self.write_slice(&method.close_paren)?;
        self.write_func_body(&method.body)
    }

    pub fn write_ctor(&mut self, ctor: &PropCtor) -> Res {
        self.write_prop_init_key(&ctor.keyword)?;
        self.write_slice(&ctor.open_paren)?;
        self.write_func_params(&ctor.params)?;
        self.write_slice(&ctor.close_paren)?;
        self.write_func_body(&ctor.body)
    }

    pub fn write_prop_get(&mut self, get: &PropGet) -> Res {
        if let Some(slice) = &get.keyword_static {
            self.write_slice(slice)?;
        }
        self.write_slice(&get.keyword_get)?;
        self.write_prop_init_key(&get.id)?;
        self.write_slice(&get.open_paren)?;
        self.write_slice(&get.close_paren)?;
        self.write_func_body(&get.body)
    }

    pub fn write_prop_set(&mut self, set: &PropSet) -> Res {
        if let Some(slice) = &set.keyword_static {
            self.write_slice(slice)?;
        }
        self.write_slice(&set.keyword_set)?;
        self.write_prop_init_key(&set.id)?;
        self.write_slice(&set.open_paren)?;
        self.write_func_arg(&set.arg.item)?;
        self.write_maybe_slice(&set.arg.comma)?;
        self.write_slice(&set.close_paren)?;
        self.write_func_body(&set.body)
    }

    pub fn write_func(&mut self, func: &Func) -> Res {
        if let Some(a) = &func.keyword_async {
            self.write_slice(a)?;
        }
        self.write_slice(&func.keyword)?;
        if let Some(star) = &func.star {
            self.write_slice(star)?;
        }
        if let Some(id) = &func.id {
            self.write_ident(id)?;
        }
        self.write_slice(&func.open_paren)?;
        self.write_func_params(&func.params)?;
        self.write_slice(&func.close_paren)?;
        self.write_func_body(&func.body)
    }

    pub fn write_func_params(&mut self, args: &[ListEntry<FuncArg>]) -> Res {
        for entry in args {
            self.write_func_arg(&entry.item)?;
            self.write_maybe_slice(&entry.comma)?;
        }
        Ok(())
    }

    pub fn write_func_arg(&mut self, arg: &FuncArg) -> Res {
        match arg {
            FuncArg::Expr(expr) => self.write_expr(expr),
            FuncArg::Pat(pat) => self.write_pat(pat),
            FuncArg::Rest(rest) => {
                self.write_slice(&rest.dots)?;
                self.write_pat(&rest.pat)
            }
        }
    }

    pub fn write_func_body(&mut self, body: &FuncBody) -> Res {
        self.write_slice(&body.open_brace)?;
        self.write_parts(&body.stmts)?;
        self.write_slice(&body.close_brace)
    }

    pub fn write_pat(&mut self, pat: &Pat) -> Res {
        match pat {
            Pat::Ident(ident) => self.write_ident(ident),
            Pat::Obj(obj) => self.write_obj_pat(obj),
            Pat::Array(arr) => self.write_arr_pat(arr),
            Pat::Assign(assign) => self.write_assign_pat(assign),
        }
    }

    pub fn write_obj_pat(&mut self, pat: &ObjPat) -> Res {
        self.write_slice(&pat.open_brace)?;
        self.write_obj_pat_parts(&pat.props)?;
        self.write_slice(&pat.close_brace)
    }

    pub fn write_obj_pat_parts(&mut self, props: &[ListEntry<ObjPatPart>]) -> Res {
        for entry in props {
            self.write_obj_pat_part(&entry.item)?;
            self.write_maybe_slice(&entry.comma)?;
        }
        Ok(())
    }
    pub fn write_obj_pat_part(&mut self, props: &ObjPatPart) -> Res {
        match props {
            ObjPatPart::Assign(pat) => self.write_prop(pat),
            ObjPatPart::Rest(rest) => self.write_rest_pat(rest),
        }
    }

    pub fn write_arr_pat(&mut self, arr: &ArrayPat) -> Res {
        self.write_slice(&arr.open_bracket)?;
        self.write_arr_pat_parts(&arr.elements)?;
        self.write_slice(&arr.close_bracket)
    }

    pub fn write_arr_pat_parts(&mut self, parts: &[ListEntry<Option<ArrayPatPart>>]) -> Res {
        for part in parts {
            if let Some(part) = &part.item {
                self.write_arr_pat_part(part)?;
            }
            self.write_maybe_slice(&part.comma)?;
        }
        Ok(())
    }

    pub fn write_arr_pat_part(&mut self, part: &ArrayPatPart) -> Res {
        match part {
            ArrayPatPart::Pat(pat) => self.write_pat(pat),
            ArrayPatPart::Expr(expr) => self.write_expr(expr),
            ArrayPatPart::Rest(rest) => self.write_rest_pat(rest),
        }
    }

    pub fn write_assign_pat(&mut self, assign: &AssignPat) -> Res {
        self.write_pat(&assign.left)?;
        self.write_assign_op(&assign.operator)?;
        self.write_expr(&assign.right)
    }

    pub fn write_rest_pat(&mut self, rest: &RestPat) -> Res {
        self.write_slice(&rest.dots)?;
        self.write_pat(&rest.pat)
    }

    pub fn write_expr(&mut self, expr: &Expr) -> Res {
        match expr {
            Expr::Array(arr) => self.write_arr_expr(arr),
            Expr::ArrowFunc(arrow) => self.write_arrow_func(arrow),
            Expr::ArrowParamPlaceHolder(apph) => {
                panic!("Write arrow parameter_place_holder...: {:?}", apph)
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
                self.write_slice(&spread.dots)?;
                self.write_expr(&spread.expr)
            }
            Expr::Super(slice) => self.write_slice(&slice),
            Expr::TaggedTemplate(temp) => self.write_tagged_template_expr(temp),
            Expr::This(slice) => self.write_slice(slice),
            Expr::Unary(unary) => self.write_unary_expr(unary),
            Expr::Update(update) => self.write_update_expr(update),
            Expr::Wrapped(wrapped) => {
                self.write_slice(&wrapped.open_paren)?;
                self.write_expr(&wrapped.expr)?;
                self.write_slice(&wrapped.close_paren)
            }
            Expr::Yield(expr) => self.write_yield_expr(expr),
        }
    }

    pub fn write_arr_expr(&mut self, arr: &ArrayExpr) -> Res {
        self.write_slice(&arr.open_bracket)?;
        for ele in &arr.elements {
            if let Some(expr) = &ele.item {
                self.write_expr(expr)?;
            }
            self.write_maybe_slice(&ele.comma)?;
        }
        self.write_slice(&arr.close_bracket)
    }

    pub fn write_arrow_func(&mut self, arrow: &ArrowFuncExpr) -> Res {
        if let Some(slice) = &arrow.keyword {
            self.write_slice(slice)?;
        }
        if let Some(star) = &arrow.star {
            self.write_slice(star)?;
        }
        if let Some(open) = &arrow.open_paren {
            self.write_slice(open)?;
        }
        self.write_func_params(&arrow.params)?;
        if let Some(close) = &arrow.close_paren {
            self.write_slice(close)?;
        }
        self.write_slice(&arrow.arrow)?;
        match &arrow.body {
            ArrowFuncBody::FuncBody(body) => self.write_func_body(body),
            ArrowFuncBody::Expr(expr) => self.write_expr(expr),
        }
    }

    pub fn write_assign_expr(&mut self, assign: &AssignExpr) -> Res {
        self.write_assign_left(&assign.left)?;
        self.write_assign_op(&assign.operator)?;
        self.write_expr(&assign.right)
    }

    pub fn write_assign_left(&mut self, assign_left: &AssignLeft) -> Res {
        match assign_left {
            AssignLeft::Pat(pat) => self.write_pat(pat),
            AssignLeft::Expr(expr) => self.write_expr(expr),
        }
    }

    pub fn write_await_expr(&mut self, await_expr: &AwaitExpr) -> Res {
        self.write_slice(&await_expr.keyword)?;
        self.write_expr(&await_expr.expr)
    }

    pub fn write_bin_expr(&mut self, bin: &BinaryExpr) -> Res {
        self.write_expr(&bin.left)?;
        self.write_bin_op(&bin.operator)?;
        self.write_expr(&bin.right)
    }

    pub fn write_bin_op(&mut self, op: &BinaryOp) -> Res {
        match op {
            BinaryOp::Equal(slice) => self.write_slice(slice),
            BinaryOp::NotEqual(slice) => self.write_slice(slice),
            BinaryOp::StrictEqual(slice) => self.write_slice(slice),
            BinaryOp::StrictNotEqual(slice) => self.write_slice(slice),
            BinaryOp::LessThan(slice) => self.write_slice(slice),
            BinaryOp::GreaterThan(slice) => self.write_slice(slice),
            BinaryOp::LessThanEqual(slice) => self.write_slice(slice),
            BinaryOp::GreaterThanEqual(slice) => self.write_slice(slice),
            BinaryOp::LeftShift(slice) => self.write_slice(slice),
            BinaryOp::RightShift(slice) => self.write_slice(slice),
            BinaryOp::UnsignedRightShift(slice) => self.write_slice(slice),
            BinaryOp::Plus(slice) => self.write_slice(slice),
            BinaryOp::Minus(slice) => self.write_slice(slice),
            BinaryOp::Times(slice) => self.write_slice(slice),
            BinaryOp::Over(slice) => self.write_slice(slice),
            BinaryOp::Mod(slice) => self.write_slice(slice),
            BinaryOp::Or(slice) => self.write_slice(slice),
            BinaryOp::XOr(slice) => self.write_slice(slice),
            BinaryOp::And(slice) => self.write_slice(slice),
            BinaryOp::In(slice) => self.write_slice(slice),
            BinaryOp::InstanceOf(slice) => self.write_slice(slice),
            BinaryOp::PowerOf(slice) => self.write_slice(slice),
        }
    }

    pub fn write_call_expr(&mut self, call: &CallExpr) -> Res {
        self.write_expr(&call.callee)?;
        self.write_slice(&call.open_paren)?;
        self.write_exprs(&call.arguments)?;
        self.write_slice(&call.close_paren)
    }

    pub fn write_exprs(&mut self, exprs: &[ListEntry<Expr>]) -> Res {
        for expr in exprs {
            self.write_expr(&expr.item)?;
            self.write_maybe_slice(&expr.comma)?;
        }
        Ok(())
    }

    pub fn write_conditional(&mut self, cond: &ConditionalExpr) -> Res {
        self.write_expr(&cond.test)?;
        self.write_slice(&cond.question_mark)?;
        self.write_expr(&cond.consequent)?;
        self.write_slice(&cond.colon)?;
        self.write_expr(&cond.alternate)
    }

    pub fn write_logical_expr(&mut self, log: &LogicalExpr) -> Res {
        self.write_expr(&log.left)?;
        let op = match &log.operator {
            LogicalOp::Or(slice) => slice,
            LogicalOp::And(slice) => slice,
        };
        self.write_slice(op)?;
        self.write_expr(&log.right)
    }

    pub fn write_member_expr(&mut self, member: &MemberExpr) -> Res {
        self.write_expr(&member.object)?;
        match &member.indexer {
            MemberIndexer::Period(period) => {
                self.write_slice(period)?;
                self.write_expr(&member.property)
            }
            MemberIndexer::Computed {
                open_bracket,
                close_bracket,
            } => {
                self.write_slice(open_bracket)?;
                self.write_expr(&member.property)?;
                self.write_slice(close_bracket)
            }
        }
    }

    pub fn write_meta_prop(&mut self, meta: &MetaProp) -> Res {
        self.write_ident(&meta.meta)?;
        self.write_slice(&meta.dot)?;
        self.write_ident(&meta.property)
    }

    pub fn write_new_expr(&mut self, new: &NewExpr) -> Res {
        self.write_slice(&new.keyword)?;
        self.write_expr(&new.callee)?;
        self.write_maybe_slice(&new.open_paren)?;
        self.write_exprs(&new.arguments)?;
        self.write_maybe_slice(&new.close_paren)
    }

    pub fn write_obj_expr(&mut self, obj: &ObjExpr) -> Res {
        self.write_slice(&obj.open_brace)?;
        self.write_obj_props(&obj.props)?;
        self.write_slice(&obj.close_brace)
    }

    pub fn write_obj_props(&mut self, props: &[ListEntry<ObjProp>]) -> Res {
        for prop in props {
            match &prop.item {
                ObjProp::Prop(prop) => self.write_prop(prop)?,
                ObjProp::Spread(spread) => {
                    self.write_slice(&spread.dots)?;
                    self.write_expr(&spread.expr)?;
                }
            }
            self.write_maybe_slice(&prop.comma)?;
        }
        Ok(())
    }

    pub fn write_tagged_template_expr(&mut self, temp: &TaggedTemplateExpr) -> Res {
        self.write_expr(&temp.tag)?;
        self.write_template_lit(&temp.quasi)
    }

    pub fn write_unary_expr(&mut self, unary: &UnaryExpr) -> Res {
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
            UnaryOp::Minus(slice) => self.write_slice(slice),
            UnaryOp::Plus(slice) => self.write_slice(slice),
            UnaryOp::Not(slice) => self.write_slice(slice),
            UnaryOp::Tilde(slice) => self.write_slice(slice),
            UnaryOp::TypeOf(slice) => self.write_slice(slice),
            UnaryOp::Void(slice) => self.write_slice(slice),
            UnaryOp::Delete(slice) => self.write_slice(slice),
        }
    }

    pub fn write_update_expr(&mut self, expr: &UpdateExpr) -> Res {
        let op = match &expr.operator {
            UpdateOp::Increment(slice) => slice,
            UpdateOp::Decrement(slice) => slice,
        };
        if expr.prefix() {
            self.write_slice(op)?;
            self.write_expr(&expr.argument)
        } else {
            self.write_expr(&expr.argument)?;
            self.write_slice(op)
        }
    }

    pub fn write_yield_expr(&mut self, expr: &YieldExpr) -> Res {
        self.write_slice(&expr.keyword)?;
        if let Some(star) = &expr.star {
            self.write_slice(star)?;
        }
        if let Some(arg) = &expr.argument {
            self.write_expr(arg)?;
        }
        Ok(())
    }

    pub fn write_maybe_slice(&mut self, slice: &Option<Slice>) -> Res {
        if let Some(slice) = slice.as_ref() {
            self.write_slice(slice)?;
        }
        Ok(())
    }

    pub fn write_lit(&mut self, lit: &Lit) -> Res {
        match lit {
            Lit::Null(slice) => self.write_slice(slice),
            Lit::String(s) => {
                self.write_slice(&s.open_quote)?;
                self.write_slice(&s.content)?;
                self.write_slice(&s.close_quote)
            }
            Lit::Number(slice) => self.write_slice(slice),
            Lit::Boolean(slice) => self.write_slice(slice),
            Lit::RegEx(re) => {
                self.write_slice(&re.open_slash)?;
                self.write_slice(&re.pattern)?;
                self.write_slice(&re.close_slash)?;
                if let Some(flags) = re.flags.as_ref() {
                    self.write_slice(flags)?;
                }
                Ok(())
            }
            Lit::Template(temp) => self.write_template_lit(temp),
        }
    }

    pub fn write_template_lit(&mut self, temp: &TemplateLit) -> Res {
        let mut quasi = temp.quasis.iter();
        let mut exprs = temp.expressions.iter();
        while let Some(quasi) = quasi.next() {
            self.write_slice(&quasi.raw)?;
            if quasi.raw.source.ends_with('`') {
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
            AssignOp::Equal(slice) => self.write_slice(slice),
            AssignOp::PlusEqual(slice) => self.write_slice(slice),
            AssignOp::MinusEqual(slice) => self.write_slice(slice),
            AssignOp::TimesEqual(slice) => self.write_slice(slice),
            AssignOp::DivEqual(slice) => self.write_slice(slice),
            AssignOp::ModEqual(slice) => self.write_slice(slice),
            AssignOp::LeftShiftEqual(slice) => self.write_slice(slice),
            AssignOp::RightShiftEqual(slice) => self.write_slice(slice),
            AssignOp::UnsignedRightShiftEqual(slice) => self.write_slice(slice),
            AssignOp::OrEqual(slice) => self.write_slice(slice),
            AssignOp::XOrEqual(slice) => self.write_slice(slice),
            AssignOp::AndEqual(slice) => self.write_slice(slice),
            AssignOp::PowerOfEqual(slice) => self.write_slice(slice),
        }
    }

    pub fn write_ident(&mut self, ident: &Ident) -> Res {
        self.write_slice(&ident.slice)
    }

    fn write_slice(&mut self, slice: &Slice) -> Res {
        eprintln!("write_slice: {:?}", slice.source);
        if self.last_out.line == 0 {
            self.write(&slice.source)?;
            self.last_out = slice.loc.end;
            return Ok(());
        }
        let new_lines = slice.loc.start.line - self.last_out.line;
        self.write(&"\n".repeat(new_lines))?;
        let leading = if new_lines == 0 {
            if slice.loc.start.column < self.last_out.column {
                eprintln!("{:?}\n{:?}", self.last_slice, slice);
            }
            " ".repeat(slice.loc.start.column - self.last_out.column)
        } else {
            " ".repeat(slice.loc.start.column.saturating_sub(1))
        };
        self.write(&leading)?;
        self.last_out = slice.loc.end;
        let last_slice_source = slice.source.clone().into_owned();
        self.last_slice = Slice {
            loc: slice.loc,
            source: std::borrow::Cow::Owned(last_slice_source),
        };
        self.write(&slice.source)?;
        Ok(())
    }

    fn write(&mut self, s: &str) -> Res {
        let _ = self.out.write(s.as_bytes())?;
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
