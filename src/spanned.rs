use std::{io::Write, f32::consts::E};
use resast::spanned::{
    Ident,
    Slice, Position, Program, ProgramPart, Dir, expr::{Lit, Expr, Prop, PropInit, PropInitKey, PropKey, PropValue, PropMethod, PropCtor, PropGet, PropSet}, decl::{Decl, VarDecl, VarDecls}, VarKind, ListEntry, pat::Pat, Func, FuncArg, FuncBody, Class, ClassBody, stmt::{Stmt, IfStmt, SwitchStmt, SwitchCase, self}};
use ress::tokens::{Comment, CommentKind};

type Res = Result<(), std::io::Error>;

pub struct SpannedWriter<T> {
    out: T,
    last_out: Position,
}

impl<T> SpannedWriter<T>
where T: Write {
    pub fn new(out: T) -> Self {
        Self {
            out,
            last_out: Position { line: 0, column: 0 }
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
            },
            Decl::Func(func) => self.write_func(func),
            Decl::Class(class) => self.write_class(class),
            Decl::Import { import, semi_colon } => todo!(),
            Decl::Export { export, semi_colon } => todo!(),
        }
    }

    pub fn write_stmt(&mut self, stmt: &Stmt) -> Res {
        match stmt {
            Stmt::Expr { expr, semi_colon } => {
                self.write_expr(expr)?;
                self.write_maybe_slice(semi_colon)
            },
            Stmt::Block(block) => {
                self.write_slice(&block.open_brace)?;
                self.write_parts(&block.stmts)?;
                self.write_slice(&block.close_brace)

            },
            Stmt::Empty(slice) => self.write_slice(slice),
            Stmt::Debugger { keyword, semi_colon } => {
                self.write_slice(keyword)?;
                self.write_maybe_slice(semi_colon)
            },
            Stmt::With(with) => {
                self.write_slice(&with.keyword)?;
                self.write_slice(&with.open_paren)?;
                self.write_expr(&with.object)?;
                self.write_slice(&with.close_paren)?;
                self.write_stmt(&with.body)
            },
            Stmt::Return { keyword, value, semi_colon } => {
                self.write_slice(keyword)?;
                if let Some(value) = value {
                    self.write_expr(value)?;
                }
                self.write_maybe_slice(semi_colon)
            },
            Stmt::Labeled(labeled) => {
                self.write_ident(&labeled.label)?;
                self.write_slice(&labeled.colon)?;
                self.write_stmt(&labeled.body)
            },
            Stmt::Break { keyword, label, semi_colon } => {
                self.write_slice(keyword)?;
                if let Some(label) = label {
                    self.write_ident(label)?;
                }
                self.write_maybe_slice(semi_colon)
            },
            Stmt::Continue { keyword, label, semi_colon } => {
                self.write_slice(keyword)?;
                if let Some(label) = label {
                    self.write_ident(label)?;
                }
                self.write_maybe_slice(semi_colon)
            },
            Stmt::If(stmt) => self.write_if_stmt(stmt),
            Stmt::Switch(switch) => self.write_switch_stmt(switch),
            Stmt::Throw { keyword, expr, semi_colon } => {
                self.write_slice(keyword)?;
                self.write_expr(expr)?;
                self.write_maybe_slice(semi_colon)
            },
            Stmt::Try(try_stmt) => self.write_try_stmt(try_stmt),
            Stmt::While(while_stmt) => self.write_while_stmt(while_stmt),
            Stmt::DoWhile(do_while) => self.write_do_while(do_while),
            Stmt::For(for_stmt) => self.write_for_stmt(for_stmt),
            Stmt::ForIn(for_in) => self.write_for_in(for_in),
            Stmt::ForOf(for_of) => self.write_for_of(for_of)?;,
            Stmt::Var { decls, semi_colon } => {
                self.write_var_decls(decls)?;
                self.write_maybe_slice(semi_colon)
            },
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
            _ => Ok(())
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
            self.write_stmt(alt)?;
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
            self.write_expr(super_class)?;
        }
        self.write_class_body(&class.body)
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
            },
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
            Pat::Obj(_) => todo!(),
            Pat::Array(_) => todo!(),
            Pat::Assign(_) => todo!(),
        }
    }

    pub fn write_expr(&mut self, expr: &Expr) -> Res {
        match expr {
            Expr::Array(_) => todo!(),
            Expr::ArrowFunc(_) => todo!(),
            Expr::ArrowParamPlaceHolder(_) => todo!(),
            Expr::Assign(_) => todo!(),
            Expr::Await(_) => todo!(),
            Expr::Binary(_) => todo!(),
            Expr::Class(_) => todo!(),
            Expr::Call(_) => todo!(),
            Expr::Conditional(_) => todo!(),
            Expr::Func(_) => todo!(),
            Expr::Ident(ident) => self.write_ident(ident),
            Expr::Lit(_) => todo!(),
            Expr::Logical(_) => todo!(),
            Expr::Member(_) => todo!(),
            Expr::MetaProp(_) => todo!(),
            Expr::New(_) => todo!(),
            Expr::Obj(_) => todo!(),
            Expr::Sequence(_) => todo!(),
            Expr::Spread(_) => todo!(),
            Expr::Super(_) => todo!(),
            Expr::TaggedTemplate(_) => todo!(),
            Expr::This(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Update(_) => todo!(),
            Expr::Wrapped(_) => todo!(),
            Expr::Yield(_) => todo!(),
        }
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
            },
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
            },
            Lit::Template(temp) => {
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
            },
        }
    }

    fn write_ident(&mut self, ident: &Ident) -> Res {
        self.write_slice(&ident.slice)
    }
    
    fn write_slice(&mut self, slice: &Slice) -> Res {
        if self.last_out.line < slice.loc.start.line {
            self.write_char('\n')?;
            self.write(&" ".repeat(slice.loc.end.column.saturating_sub(1)))?;
        } else {
            self.write(&" ".repeat(slice.loc.end.column.saturating_sub(self.last_out.column)))?;
        }
        self.last_out = slice.loc.end;
        self.write(&slice.source)?;
        Ok(())
    }

    fn write(&mut self, s: &str) -> Res {
        let _ = self.out.write(s.as_bytes())?;
        Ok(())
    }

    fn write_char(&mut self, c: char) -> Res {
        let mut buf = [0u8; 4];
        let _ = self.out.write(c.encode_utf8(&mut buf).as_bytes())?;
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

    fn write_try_stmt(&self, try_stmt: &stmt::TryStmt) -> Result<(), std::io::Error> {
        todo!()
    }
}