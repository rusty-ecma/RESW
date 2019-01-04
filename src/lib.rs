#[macro_use]
extern crate log;
use ressa::node::*;
use std::io::{Write, Error as IoError};


pub struct Writer<T: Write> {
    current_indent: usize,
    at_top_level: bool,
    in_for_init: bool,
    out: T,
}

type Res = Result<(), IoError>;

impl<T: Write> Writer<T> {

    pub fn new(out: T) -> Self {
        trace!("new");
        Self {
            current_indent: 0,
            at_top_level: true,
            in_for_init: false,
            out,
        }
    }

    pub fn write_part(&mut self, part: &ProgramPart) -> Res {
        trace!("write_part: {:#?}", part);
        self.at_top_level = true;
        self._write_part(part)?;
        self.write_new_line()?;
        Ok(())
    }

    fn _write_part(&mut self, part: &ProgramPart) -> Res {
        trace!("_write_part");
        self.write_leading_whitespace()?;
        match part {
            ProgramPart::Decl(decl) => self.write_decl(decl)?,
            ProgramPart::Directive(dir) => self.write_directive(dir)?,
            ProgramPart::Statement(stmt) => self.write_stmt(stmt)?,
        }
        Ok(())
    }

    pub fn write_decl(&mut self, decl: &Declaration) -> Res {
        trace!("write_decl");
        match decl {
            Declaration::Variable(ref kind, ref decls) => self.write_variable_decls(kind, decls),
            Declaration::Class(ref class) => {
                self.at_top_level = false;
                self.write_class(class)?;
                self.write_new_line()
            },
            Declaration::Function(ref func) => {
                self.at_top_level = false;
                self.write_function(func)?;
                self.write_new_line()
            },
            Declaration::Export(ref exp) => self.write_export_decl(exp),
            Declaration::Import(ref imp) => self.write_import_decl(imp),
        }?;
        Ok(())
    }

    pub fn write_variable_decls(&mut self, kind: &VariableKind, decls: &[VariableDecl]) -> Res {
        trace!("write_variable_decls");
        self.write_variable_kind(kind)?;
        let mut after_first = false;
        let mut write_semi = true;
        for decl in decls {
            if after_first {
                self.write(", ")?;
            } else {
                after_first = true;
            }
            write_semi = self.write_variable_decl(decl)?;
        }
        if write_semi {
            self.write_empty_stmt()?;
        }
        self.write_new_line()
    }

    pub fn write_class(&mut self, class: &Class) -> Res {
        trace!("write_class");
        self.write("class ")?;
        if let Some(ref id) = class.id {
            self.write_ident(id)?;
            self.write(" ")?;
        }
        if let Some(ref ex) = class.super_class {
            self.write("extends ")?;
            self.write_expr(ex)?;
            self.write(" ")?;
        }
        self.write_open_brace()?;
        self.write_new_line()?;
        for ref part in &class.body {
            self.write_new_line()?;
            self.write_leading_whitespace()?;
            self.write_property(part)?;
            self.write_new_line()?;
        }
        self.write_close_brace()?;
        Ok(())
    }

    pub fn write_export_decl(&mut self, exp: &ModuleExport) -> Res {
        trace!("write_export_decl");
        self.write("export ")?;
        match exp {
            ModuleExport::All(ref a) => self.write_all_export(a)?,
            ModuleExport::Default(ref d) => self.write_default_export(d)?,
            ModuleExport::Named(ref n) => self.write_named_export(n)?,
        }
        Ok(())
    }

    pub fn write_all_export(&mut self, exp: &Literal) -> Res {
        trace!("write_all_export");
        self.write("* from ")?;
        self.write_literal(exp)?;
        Ok(())
    }

    pub fn write_default_export(&mut self, exp: &DefaultExportDecl) -> Res {
        trace!("write_default_export");
        self.write("default ")?;
        match exp {
            DefaultExportDecl::Decl(ref d) => self.write_decl(d)?,
            DefaultExportDecl::Expr(ref e) => self.write_expr(e)?,
        }
        Ok(())
    }

    pub fn write_named_export(&mut self, exp: &NamedExportDecl) -> Res {
        trace!("write_named_export");
        match exp {
            NamedExportDecl::Decl(ref d) => self.write_decl(d)?,
            NamedExportDecl::Specifier(ref s, ref from) => self.write_export_specifiers(s, from)?,
        }
        Ok(())
    }

    pub fn write_export_specifiers(&mut self, specifiers: &[ExportSpecifier], from: &Option<Literal>) -> Res {
        trace!("write_export_specifiers");
        self.write("{")?;
        let mut after_first = false;
        for s in specifiers {
            if after_first {
                self.write(", ")?;
            }
            self.write_ident(&s.local)?;
            if let Some(ref name) = &s.exported {
                self.write(" as ")?;
                self.write(name)?;
            }
            after_first = true;
        }
        self.write("}")?;
        if let Some(ref from) = from {
            self.write(" from ")?;
            self.write_literal(from)?;
        }
        Ok(())
    }

    pub fn write_import_decl(&mut self, imp: &ModuleImport) -> Res {
        trace!("write_import_decl");
        self.write("import ")?;
        if imp.specifiers.len() == 0 {
            self.write("{}")?;
        }
        let mut opened_brace = false;
        let mut specifiers = imp.specifiers.iter();
        if let Some(ref first) = specifiers.next() {
            if let ImportSpecifier::Default(ref ident) = first {
                self.write_ident(ident)?;
            } else if let ImportSpecifier::Namespace(ref imp)= first {
                self.write_namespace_import(imp)?;
            } else {
                self.write("{ ")?;
                opened_brace = true;
                self.write_import_specificer(first)?;
            }
        }

        if !opened_brace {
            if let Some(ref next) = specifiers.next() {
                if let ImportSpecifier::Namespace(ref name) = next {
                    self.write(", ")?;
                    self.write_namespace_import(name)?;
                } else {
                    self.write(", { ")?;
                    self.write_import_specificer(next)?;
                    opened_brace = true;
                }
            }
        }

        while let Some(ref s) = specifiers.next() {
            self.write(", ")?;
            self.write_import_specificer(s)?;
        }
        if opened_brace {
            self.write(" }")?;
        }
        self.write(" from ")?;
        self.write_literal(&imp.source)?;
        self.write_empty_stmt()?;
        Ok(())
    }

    pub fn write_import_specificer(&mut self, spec: &ImportSpecifier) -> Res {
        trace!("write_import_specificer");
        match spec {
            ImportSpecifier::Default(ref i) => self.write_ident(i)?,
            ImportSpecifier::Namespace(ref n) => self.write_namespace_import(n)?,
            ImportSpecifier::Normal(ref n, ref l) => self.write_normal_import(n, l)?,
        }
        Ok(())
    }

    pub fn write_namespace_import(&mut self, name: &Identifier) -> Res {
        trace!("write_namespace_import");
        self.write("* as ")?;
        self.write_ident(name)?;
        Ok(())
    }

    pub fn write_normal_import(&mut self, name: &Identifier, local: &Option<Identifier>) -> Res {
        trace!("write_normal_import");
        self.write_ident(name)?;
        if let Some(ref ident) = local {
            self.write(" as ")?;
            self.write(ident)?;
        }
        Ok(())
    }

    pub fn write_directive(&mut self, dir: &Directive) -> Res {
        trace!("write_directive");
        self.write(&format!("'{}';", dir.directive))?;
        self.write_new_line()?;
        Ok(())
    }

    pub fn write_variable_decl(&mut self, decl: &VariableDecl) -> Result<bool, IoError> {
        trace!("write_variable_decl");
        self.write_pattern(&decl.id)?;
        let mut ret = true;
        if let Some(ref init) = decl.init {
            self.write(" = ")?;
            self.write_expr(init)?;
            ret = match init {
                Expression::Function(_)
                | Expression::Class(_) => false,
                _ => true,
            }
        }
        Ok(ret)
    }

    pub fn write_variable_kind(&mut self, kind: &VariableKind) -> Res {
        trace!("write_variable_kind");
        let s = match kind {
            VariableKind::Const => "const ",
            VariableKind::Let => "let ",
            VariableKind::Var => "var ",
        };
        self.write(s)
    }

    pub fn write_stmt(&mut self, stmt: &Statement) -> Res {
        trace!("write_stmt");
        let mut semi = true;
        let mut new_line = true;
        let cached_state = self.at_top_level;
        match stmt {
            Statement::Empty => (),
            Statement::Debugger => self.write_debugger_stmt()?,
            Statement::Expr(ref stmt) => self.write_expr(stmt)?,
            Statement::Block(ref stmt) => {
                self.at_top_level = false;
                self.write_block_stmt(stmt)?;
                semi = false;
                new_line = false;
                self.at_top_level = cached_state;
            },
            Statement::With(ref stmt) => {
                self.write_with_stmt(stmt)?;
                semi = false;
            },
            Statement::Return(ref stmt) => self.write_return_stmt(stmt)?,
            Statement::Labeled(ref stmt) => {
                self.write_labeled_stmt(stmt)?;
                semi = false;
            },
            Statement::Break(ref stmt) => self.write_break_stmt(stmt)?,
            Statement::Continue(ref stmt) => self.write_continue_stmt(stmt)?,
            Statement::If(ref stmt) => {
                self.write_if_stmt(stmt)?;
                semi = false;
            },
            Statement::Switch(ref stmt) => {
                self.at_top_level = false;
                self.write_switch_stmt(stmt)?;
                semi = false;
                self.at_top_level = cached_state;
            },
            Statement::Throw(ref stmt) => self.write_throw_stmt(stmt)?,
            Statement::Try(ref stmt) => {
                self.write_try_stmt(stmt)?;
                semi = false;
            },
            Statement::While(ref stmt) => {
                new_line = self.write_while_stmt(stmt)?;
                semi = false;
            },
            Statement::DoWhile(ref stmt) => self.write_do_while_stmt(stmt)?,
            Statement::For(ref stmt) => {
                self.at_top_level = false;
                new_line = self.write_for_stmt(stmt)?;
                semi = false;
                self.at_top_level = cached_state;
            },
            Statement::ForIn(ref stmt) => {
                self.at_top_level = false;
                new_line = self.write_for_in_stmt(stmt)?;
                semi = false;
                self.at_top_level = cached_state;
            },
            Statement::ForOf(ref stmt) => {
                self.at_top_level = false;
                new_line = self.write_for_of_stmt(stmt)?;
                semi = false;
                self.at_top_level = cached_state;
            },
            Statement::Var(ref stmt) => self.write_var_stmt(stmt)?,
        };
        if semi {
            self.write_empty_stmt()?;
        }
        if new_line {
            self.write_new_line()?;
        }
        Ok(())
    }

    pub fn write_debugger_stmt(&mut self) -> Res {
        trace!("write_debugger_stmt");
        self.write("debugger;")
    }

    pub fn write_block_stmt(&mut self, block: &[ProgramPart]) -> Res {
        trace!("write_block_stmt");
        self.write_open_brace()?;
        if block.len() == 0 {
            self.write(" \n")?;
            self.write_leading_whitespace()?;
            self.write("\n")?;
        }
        for ref part in block {
            self.write_new_line()?;
            self._write_part(part)?;
        }
        self.write_close_brace()?;
        Ok(())
    }

    pub fn write_with_stmt(&mut self, expr: &WithStatement) -> Res {
        trace!("write_with_stmt");
        self.write("with (")?;
        self.write_expr(&expr.object)?;
        self.write(") ")?;
        self.write_stmt(&expr.body)?;
        Ok(())
    }

    pub fn write_return_stmt(&mut self, expr: &Option<Expression>) -> Res {
        trace!("write_return_stmt");
        self.write("return")?;
        if let Some(ref e) = expr {
            self.write(" ")?;
            self.write_expr(e)?;
        }
        Ok(())
    }

    pub fn write_labeled_stmt(&mut self, expr: &LabeledStatement) -> Res {
        trace!("write_labeled_stmt");
        self.write_ident(&expr.label)?;
        self.write(": ")?;
        self.write_stmt(&expr.body)?;
        Ok(())
    }

    pub fn write_break_stmt(&mut self, expr: &Option<Identifier>) -> Res {
        trace!("write_break_stmt");
        self.write("break")?;
        if let Some(ref i) = expr {
            self.write(" ")?;
            self.write_ident(i)?;
        }
        Ok(())
    }

    pub fn write_continue_stmt(&mut self, expr: &Option<Identifier>) -> Res {
        trace!("write_continue_stmt");
        self.write("continue")?;
        if let Some(ref i) = expr {
            self.write(" ")?;
            self.write_ident(i)?;
        }
        Ok(())
    }

    pub fn write_if_stmt(&mut self, expr: &IfStatement) -> Res {
        trace!("write_if_stmt");
        self.write("if (")?;
        self.write_expr(&expr.test)?;
        self.write(") ")?;
        if let Statement::Empty = &*expr.consequent {
            self.write_block_stmt(&[])?;
        } else {
            self.write_stmt(&expr.consequent)?;
        }
        if let Some(ref alt) = &expr.alternate {
            self.write(" else ")?;
            if let Statement::Empty = &**alt {
                self.write_block_stmt(&[])?;
            } else {
                self.write_stmt(alt)?;
            }
        }
        Ok(())
    }

    pub fn write_switch_stmt(&mut self, switch: &SwitchStatement) -> Res {
        trace!("write_switch_stmt");
        self.write("switch (")?;
        self.write_expr(&switch.discriminant)?;
        self.write(") ")?;
        self.write_open_brace()?;
        if switch.cases.len() > 0 {
            self.write_new_line()?;
        }
        for ref case in &switch.cases {
            self.write_switch_case(case)?;
        }
        self.write_close_brace()?;
        Ok(())
    }

    pub fn write_switch_case(&mut self, case: &SwitchCase) -> Res {
        trace!("write_switch_case");
        self.write_leading_whitespace()?;
        if let Some(ref t) = &case.test {
            self.write("case ")?;
            self.write_expr(t)?;
        } else {
            self.write("default")?;
        }
        self.write(":")?;
        self.write_new_line()?;
        self.current_indent += 1;
        let mut decrease_indent = true;
        for ref part in &case.consequent {
            if let ProgramPart::Statement(Statement::Break(_)) = part {
                self.current_indent = self.current_indent.saturating_sub(1);
                decrease_indent = false;
            }
            self._write_part(part)?;
            self.write_new_line()?;
        }
        if decrease_indent {
            self.current_indent = self.current_indent.saturating_sub(1);
        }
        Ok(())
    }

    pub fn write_throw_stmt(&mut self, expr: &Expression) -> Res {
        trace!("write_throw_stmt");
        self.write("throw ")?;
        self.write_expr(&expr)?;
        Ok(())
    }

    pub fn write_try_stmt(&mut self, stmt: &TryStatement) -> Res {
        trace!("write_try_stmt");
        self.write("try ")?;
        self.write_block_stmt(&stmt.block)?;
        if let Some(ref c) = &stmt.handler {
            self.write(" catch (")?;
            self.write_pattern(&c.param)?;
            self.write(") ")?;
            self.write_block_stmt(&c.body)?;
        }
        if let Some(ref f) = &stmt.finalizer {
            self.write(" finally ")?;
            self.write_block_stmt(&f)?;
        }
        Ok(())
    }

    pub fn write_while_stmt(&mut self, stmt: &WhileStatement) -> Result<bool, IoError> {
        trace!("write_while_stmt");
        let mut ret = false;
        self.write("while (")?;
        self.write_expr(&stmt.test)?;
        self.write(") ")?;
        if let Statement::Block(_) = &*stmt.body {
            ret = true;
        }
        self.write_stmt(&stmt.body)?;
        Ok(ret)
    }

    pub fn write_do_while_stmt(&mut self, stmt: &DoWhileStatement) -> Res {
        trace!("write_do_while_stmt");
        self.write("do ")?;
        self.write_stmt(&stmt.body)?;
        self.write(" while (")?;
        self.write_expr(&stmt.test)?;
        self.write(")")?;
        Ok(())
    }

    pub fn write_for_stmt(&mut self, stmt: &ForStatement) -> Result<bool, IoError> {
        trace!("write_for_stmt");
        self.write("for (")?;
        if let Some(ref init) = &stmt.init {
            self.write_loop_init(init)?;
        }
        self.write_empty_stmt()?;
        if let Some(ref test) = &stmt.test {
            self.write_expr(test)?;
        }
        self.write_empty_stmt()?;
        if let Some(ref update) = &stmt.update {
            self.write_expr(update)?;
        }
        self.write(") ")?;
        let ret = if let Statement::Block(_) = &*stmt.body {
            true
        } else {
            false
        };
        self.write_stmt(&stmt.body)?;
        Ok(ret)
    }

    pub fn write_loop_init(&mut self, init: &LoopInit) -> Res {
        self.in_for_init = true;
        match init {
            LoopInit::Expr(ref e) => self.write_expr(e)?,
            LoopInit::Variable(ref v) => {
                self.write("let ")?;
                let mut after_first = false;
                for ref d in v {
                    if after_first {
                        self.write(", ")?;
                    }
                    self.write_variable_decl(d)?;
                    after_first = true;
                }
            },
        }
        self.in_for_init = false;
        Ok(())
    }

    pub fn write_for_in_stmt(&mut self, stmt: &ForInStatement) -> Result<bool, IoError> {
        trace!("write_for_in_stmt");
        self.write("for (")?;
        self.write_loop_left(&stmt.left)?;
        self.write(" in ")?;
        self.write_expr(&stmt.right)?;
        self.write(") ")?;
        self.write_stmt(&stmt.body)?;
        let ret = if let Statement::Block(_) = &*stmt.body {
            true
        } else {
            false
        };
        Ok(ret)
    }

    pub fn write_for_of_stmt(&mut self, stmt: &ForOfStatement) -> Result<bool, IoError> {
        trace!("write_for_of_stmt");
        self.write("for (")?;
        self.write_loop_left(&stmt.left)?;
        self.write(" of ")?;
        self.write_expr(&stmt.right)?;
        self.write(") ")?;
        self.write_stmt(&stmt.body)?;
        let ret = if let Statement::Block(_) = &*stmt.body {
            true
        } else {
            false
        };
        Ok(ret)
    }

    pub fn write_loop_left(&mut self, left: &LoopLeft) -> Res {
        match left {
            LoopLeft::Pattern(ref pat) => self.write_pattern(pat)?,
            LoopLeft::Variable(ref var) => {
                self.write_variable_decl(var)?;
            },
        }
        Ok(())
    }

    pub fn write_var_stmt(&mut self, expr: &[VariableDecl]) -> Res {
        trace!("write_var_stmt");
        self.write("var ")?;
        let mut after_first = false;
        for ref d in expr {
            if after_first {
                self.write(", ")?;
            }
            self.write_variable_decl(d)?;
            after_first = true;
        }
        Ok(())
    }

    pub fn write_pattern(&mut self, pattern: &Pattern) -> Res {
        trace!("write_pattern");
        match pattern {
            Pattern::Identifier(ref i) => self.write(i),
            Pattern::Object(ref o) => self.write_object_pattern(o),
            Pattern::Array(ref a) => self.write_array_pattern(a),
            Pattern::RestElement(ref r) => self.write_rest_element(r),
            Pattern::Assignment(ref a) => self.write_assignment_pattern(a),
        }
    }

    pub fn write_object_pattern(&mut self, obj: &ObjectPattern) -> Res {
        trace!("write_object_pattern");
        self.write_open_brace()?;
        let mut after_first = false;
        for ref part in obj {
            if after_first {
                self.write(" ")?;
            }
            match part {
                ObjectPatternPart::Assignment(ref prop) => self.write_property(prop)?,
                ObjectPatternPart::Rest(ref pat) => self.write_rest_pattern_part(pat)?,
            }
            self.write(",")?;
            after_first = true;
        }

        self.write_close_brace()?;
        Ok(())
    }

    pub fn write_property(&mut self, prop: &Property) -> Res {
        trace!("write_property");
        match &prop.kind {
            PropertyKind::Init => self.write_init_property(prop),
            PropertyKind::Get => self.write_get_property(prop),
            PropertyKind::Set => self.write_set_property(prop),
            PropertyKind::Method => self.write_property_method(prop),
            PropertyKind::Ctor => self.write_ctor_property(prop),
        }
    }

    pub fn write_init_property(&mut self, prop: &Property) -> Res {
        trace!("write_init_property");
        self.write_property_key(&prop.key, prop.computed)?;
        if !prop.short_hand {
            self.write(": ")?;
            self.write_property_value(&prop.value)?;
        }
        Ok(())
    }

    pub fn write_get_property(&mut self, prop: &Property) -> Res {
        trace!("write_get_property");
        self.write("get ")?;
        self.write_property_method(prop)
    }

    pub fn write_set_property(&mut self, prop: &Property) -> Res {
        trace!("write_set_property");
        self.write("set ")?;
        self.write_property_method(prop)
    }

    pub fn write_property_method(&mut self, prop: &Property) -> Res {
        trace!("write_property_method");
        if let PropertyValue::Expr(Expression::Function(ref func)) = prop.value {
            if func.is_async {
                self.write("async ")?;
            }
            if func.generator {
                self.write("*")?;
            }
            self.write_property_key(&prop.key, prop.computed)?;
            self.write_function_args(&func.params)?;
            self.write_function_body(&func.body)?;
        } else {
            panic!("property method value must be a function expression");
        }
        Ok(())
    }

    pub fn write_function_args(&mut self, args: &[FunctionArg]) -> Res {
        trace!("write_function_args");
        self.write("(")?;
        let mut after_first = false;
        for ref arg in args {
            if after_first {
                self.write(", ")?;
            } else {
                after_first = true;
            }
            self.write_function_arg(arg)?;
        }
        self.write(") ")?;
        Ok(())
    }

    pub fn write_function_arg(&mut self, arg: &FunctionArg) -> Res {
        trace!("write_function_arg");
        match arg {
            FunctionArg::Expr(ref ex) => self.write_expr(ex)?,
            FunctionArg::Pattern(ref pa) => self.write_pattern(pa)?,
        }
        Ok(())
    }

    pub fn write_function_body(&mut self, body: &FunctionBody) -> Res {
        trace!("write_function_body");
        if body.len() == 0 {
            self.write("{ ")?;
        } else {
            self.write_open_brace()?;
            self.write_new_line()?;
        }
        for ref part in body {
            self._write_part(part)?;
        }
        if body.len() == 0 {
            self.write("}")?;
        } else {
            self.write_close_brace()?;
        }
        Ok(())
    }

    pub fn write_ctor_property(&mut self, prop: &Property) -> Res {
        trace!("write_ctor_property");
        self.write("constructor")?;
        if let PropertyValue::Expr(Expression::Function(ref func)) = prop.value {
            self.write_function_args(&func.params)?;
            self.write_function_body(&func.body)?;
        } else {
            panic!("constructor's value must be a function expression");
        }
        Ok(())
    }

    pub fn write_property_key(&mut self, key: &PropertyKey, computed: bool) -> Res {
        trace!("write_property_key");
        if computed {
            self.write("[")?;
        }
        match key {
            PropertyKey::Ident(ref i) => self.write_ident(i)?,
            PropertyKey::Literal(ref l) => self.write_literal(l)?,
            PropertyKey::Pattern(ref p) => self.write_pattern(p)?,
        }
        if computed {
            self.write("]")?;
        }
        Ok(())
    }

    pub fn write_property_value(&mut self, value: &PropertyValue) -> Res {
        trace!("write_property_value");
        match value {
            PropertyValue::Expr(ref e) => self.write_expr(e)?,
            PropertyValue::Pattern(ref p) => self.write_pattern(p)?,
            PropertyValue::None => (),
        }
        Ok(())
    }

    pub fn write_rest_pattern_part(&mut self, pat: &Pattern) -> Res {
        trace!("write_rest_pattern_part");
        self.write("...")?;
        self.write_pattern(pat)?;
        Ok(())
    }

    pub fn write_array_pattern(&mut self, arr: &[Option<Pattern>]) -> Res {
        trace!("write_array_pattern");
        self.write("[")?;
        let after_first = false;
        for ref pat in arr {
            if let Some(ref pat) = pat {
                if after_first {
                    self.write(" ")?;
                }
                self.write_pattern(pat)?;
            }
            self.write(",")?;
        }
        self.write("]")?;
        Ok(())
    }

    pub fn write_rest_element(&mut self, pat: &Pattern) -> Res {
        trace!("write_rest_element");
        self.write("...")?;
        self.write_pattern(pat)?;
        Ok(())
    }

    pub fn write_assignment_pattern(&mut self, assignment: &AssignmentPattern) -> Res {
        trace!("write_assignment_pattern");
        self.write_pattern(&assignment.left)?;
        self.write(" = ")?;
        self.write_expr(&assignment.right)?;
        Ok(())
    }

    pub fn write_expr(&mut self, expr: &Expression) -> Res {
        trace!("write_expr");
        let cached_state = self.at_top_level;
        match expr {
            Expression::Literal(ref expr) => self.write_literal(expr)?,
            Expression::ThisExpression => self.write_this_expr()?,
            Expression::SuperExpression => self.write_super_expr()?,
            Expression::Array(ref expr) => self.write_array_expr(expr)?,
            Expression::Object(ref expr) => self.write_object_expr(expr)?,
            Expression::Function(ref expr) => {
                let wrap = self.at_top_level;
                if wrap {
                    self.write("(")?;
                }
                self.at_top_level = false;
                self.write_function(expr)?;
                if wrap {
                    self.write(")")?;
                }
                self.at_top_level = cached_state;
            },
            Expression::Unary(ref expr) => self.write_unary_expr(expr)?,
            Expression::Update(ref expr) => self.write_update_expr(expr)?,
            Expression::Binary(ref expr) => self.write_binary_expr(expr)?,
            Expression::Assignment(ref expr) => self.write_assignment_expr(expr)?,
            Expression::Logical(ref expr) => self.write_logical_expr(expr)?,
            Expression::Member(ref expr) => self.write_member_expr(expr)?,
            Expression::Conditional(ref expr) => self.write_conditional_expr(expr)?,
            Expression::Call(ref expr) => self.write_call_expr(expr)?,
            Expression::New(ref expr) => self.write_new_expr(expr)?,
            Expression::Sequence(ref expr) => self.write_sequence_expr(expr)?,
            Expression::Spread(ref expr) => self.write_spread_expr(expr)?,
            Expression::ArrowFunction(ref expr) => {
                self.at_top_level = false;
                self.write_arrow_function_expr(expr)?;
                self.at_top_level = cached_state;
            },
            Expression::Yield(ref expr) => self.write_yield_expr(expr)?,
            Expression::Class(ref expr) => {
                self.at_top_level = false;
                self.write_class(expr)?;
                self.at_top_level = cached_state;
            },
            Expression::MetaProperty(ref expr) => self.write_meta_property(expr)?,
            Expression::Await(ref expr) => self.write_await_expr(expr)?,
            Expression::Ident(ref expr) => self.write_ident(expr)?,
            Expression::TaggedTemplate(ref expr) => self.write_tagged_template(expr)?,
            _ => unreachable!(),
        }
        Ok(())
    }

    pub fn write_this_expr(&mut self) -> Res {
        trace!("write_this_expr");
        self.write("this")?;
        Ok(())
    }

    pub fn write_super_expr(&mut self) -> Res {
        trace!("write_super_expr");
        self.write("super")?;
        Ok(())
    }

    pub fn write_array_expr(&mut self, arr: &ArrayExpression) -> Res {
        trace!("write_array_expr");
        self.write("[")?;
        let mut after_first = false;
        for ref e in arr {
            if let Some(ref e) = e {
                if after_first {
                    self.write(" ")?;
                }
                self.write_expr(e)?;
            }
            self.write(",")?;
            after_first = true;
        }
        self.write("]")?;
        Ok(())
    }

    pub fn write_object_expr(&mut self, obj: &ObjectExpression) -> Res {
        trace!("write_object_expr");
        let wrap = self.at_top_level;
        if wrap {
            self.write("(")?;
        }
        self.write_open_brace()?;
        if obj.len() > 0 {
            self.write_new_line()?;
        }
        for ref prop in obj {
            self.write_leading_whitespace()?;
            match prop {
                ObjectProperty::Property(ref p) => self.write_property(p),
                ObjectProperty::Spread(ref e) => self.write_spread_expr(e),
            }?;
            self.write(",")?;
            self.write_new_line()?;
        }
        self.write_close_brace()?;
        if wrap {
            self.write(")")?;
        }
        Ok(())
    }

    pub fn write_function(&mut self, func: &Function) -> Res {
        trace!("write_function");
        if func.is_async {
            self.write("async ")?;
        }
        self.write("function")?;
        if let Some(ref id) = func.id {
            self.write(" ")?;
            if func.generator {
                self.write("*")?;
            }
            self.write(id)?;
        } else if func.generator {
            self.write("*")?;
        }
        self.write_function_args(&func.params)?;
        self.write_function_body(&func.body)
    }

    pub fn write_unary_expr(&mut self, unary: &UnaryExpression) -> Res {
        trace!("write_unary_expr");
        if unary.prefix {
            self.write_unary_operator(&unary.operator)?;
        }
        self.write_expr(&unary.argument)?;
        if !unary.prefix {
            self.write_unary_operator(&unary.operator)?;
        }
        Ok(())
    }

    pub fn write_unary_operator(&mut self, op: &UnaryOperator) -> Res {
        trace!("write_unary_operator");
        match op {
            UnaryOperator::Delete => self.write("delete "),
            UnaryOperator::Minus => self.write("-"),
            UnaryOperator::Not => self.write("!"),
            UnaryOperator::Plus => self.write("+"),
            UnaryOperator::Tilde => self.write("~"),
            UnaryOperator::TypeOf => self.write("typeof "),
            UnaryOperator::Void => self.write("void "),
        }?;
        Ok(())
    }

    pub fn write_update_expr(&mut self, update: &UpdateExpression) -> Res {
        trace!("write_update_expr");
        if update.prefix {
            self.write_update_operator(&update.operator)?;
        }
        self.write_expr(&update.argument)?;
        if !update.prefix {
            self.write_update_operator(&update.operator)?;
        }
        Ok(())
    }

    pub fn write_update_operator(&mut self, op: &UpdateOperator) -> Res {
        let s = match op {
            UpdateOperator::Decrement => "--",
            UpdateOperator::Increment => "++",
        };
        self.write(s)?;
        Ok(())
    }

    pub fn write_binary_expr(&mut self, binary: &BinaryExpression) -> Res {
        trace!("write_binary_expr");
        let wrap =self.in_for_init && binary.operator == BinaryOperator::In;
        if wrap {
            self.write("(")?;
        }
        self.write_expr(&binary.left)?;
        self.write(" ")?;
        self.write_binary_operator(&binary.operator)?;
        self.write(" ")?;
        self.write_expr(&binary.right)?;
        if wrap {
            self.write(")")?;
        }
        Ok(())
    }

    pub fn write_binary_operator(&mut self, op: &BinaryOperator) -> Res {
        let s = match op {
            BinaryOperator::And => "&",
            BinaryOperator::Equal => "==",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanEqual => ">=",
            BinaryOperator::In => "in",
            BinaryOperator::InstanceOf => "instanceof",
            BinaryOperator::LeftShift => "<<",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanEqual => "<=",
            BinaryOperator::Minus => "-",
            BinaryOperator::Mod => "%",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::Or => "|",
            BinaryOperator::Over => "/",
            BinaryOperator::Plus => "+",
            BinaryOperator::PowerOf => "**",
            BinaryOperator::RightShift => ">>",
            BinaryOperator::StrictEqual => "===",
            BinaryOperator::StrictNotEqual => "!==",
            BinaryOperator::Times => "*",
            BinaryOperator::UnsignedRightShift => ">>>",
            BinaryOperator::XOr => "^",
        };
        self.write(s)?;
        Ok(())
    }

    pub fn write_assignment_expr(&mut self, assignment: &AssignmentExpression) -> Res {
        trace!("write_assignment_expr");
        match &assignment.left {
            AssignmentLeft::Expr(ref e) => self.write_expr(e)?,
            AssignmentLeft::Pattern(ref p) => self.write_pattern(p)?,
        }
        self.write(" ")?;
        self.write_assignment_operator(&assignment.operator)?;
        self.write(" ")?;
        self.write_expr(&assignment.right)?;
        Ok(())
    }

    pub fn write_assignment_operator(&mut self, op: &AssignmentOperator) -> Res {
        let s = match op {
            AssignmentOperator::AndEqual => "&=",
            AssignmentOperator::DivEqual => "/=",
            AssignmentOperator::Equal => "=",
            AssignmentOperator::LeftShiftEqual => "<<=",
            AssignmentOperator::MinusEqual => "-=",
            AssignmentOperator::ModEqual => "%=",
            AssignmentOperator::OrEqual => "|=",
            AssignmentOperator::PlusEqual => "+=",
            AssignmentOperator::PowerOfEqual => "**=",
            AssignmentOperator::RightShiftEqual => ">>=",
            AssignmentOperator::TimesEqual => "*=",
            AssignmentOperator::UnsignedRightShiftEqual => ">>>=",
            AssignmentOperator::XOrEqual => "^=",
        };
        self.write(s)?;
        Ok(())
    }

    pub fn write_logical_expr(&mut self, logical: &LogicalExpression) -> Res {
        trace!("write_logical_expr");
        self.write_expr(&logical.left)?;
        self.write(" ")?;
        self.write_logical_operator(&logical.operator)?;
        self.write(" ")?;
        self.write_expr(&logical.right)?;
        Ok(())
    }

    pub fn write_logical_operator(&mut self, op: &LogicalOperator) -> Res {
        trace!("write_logical_operator");
        let s = match op {
            LogicalOperator::And => "&&",
            LogicalOperator::Or => "||",
        };
        self.write(s)?;
        Ok(())
    }

    pub fn write_member_expr(&mut self, member: &MemberExpression) -> Res {
        trace!("write_member_expr");
        self.write_expr(&member.object)?;
        if member.computed {
            self.write("[")?;
        } else {
            self.write(".")?;
        }
        self.write_expr(&member.property)?;
        if member.computed {
            self.write("]")?;
        }
        Ok(())
    }

    pub fn write_conditional_expr(&mut self, conditional: &ConditionalExpression) -> Res {
        trace!("write_conditional_expr");
        self.write_expr(&conditional.test)?;
        self.write(" ? ")?;
        self.write_expr(&conditional.consequent)?;
        self.write(" : ")?;
        self.write_expr(&conditional.alternate)?;
        Ok(())
    }

    pub fn write_call_expr(&mut self, call: &CallExpression) -> Res {
        trace!("write_call_expr");
        self.write_expr(&call.callee)?;
        self.write("(")?;
        self.write_sequence_expr(&call.arguments)?;
        self.write(")")?;
        Ok(())
    }

    pub fn write_new_expr(&mut self, new: &NewExpression) -> Res {
        trace!("write_new_expr");
        self.write("new ")?;
        self.write_expr(&new.callee)?;
        self.write("(")?;
        self.write_sequence_expr(&new.arguments)?;
        self.write(")")?;
        Ok(())
    }

    pub fn write_sequence_expr(&mut self, sequence: &[Expression]) -> Res {
        trace!("write_sequence_expr");
        let mut after_first = false;
        for ref e in sequence {
            if after_first {
                self.write(", ")?;
            }
            self.write_expr(e)?;
            after_first = true;
        }
        Ok(())
    }

    pub fn write_spread_expr(&mut self, spread: &Expression) -> Res {
        trace!("write_spread_expr");
        self.write("...")?;
        self.write_expr(spread)?;
        Ok(())
    }

    pub fn write_arrow_function_expr(&mut self, func: &ArrowFunctionExpression) -> Res {
        trace!("write_arrow_function_expr");
        if func.is_async {
            self.write("async ")?;
        }
        if func.params.len() == 1 {
            if let FunctionArg::Expr(Expression::Object(_)) = &func.params[0] {
                self.write_function_args(&func.params)?;
            } else {
                self.write_function_arg(&func.params[0])?;
            }
        } else {
            self.write_function_args(&func.params)?;
        }
        self.write(" => ")?;
        match &func.body {
            ArrowFunctionBody::FunctionBody(ref b) => self.write_function_body(b)?,
            ArrowFunctionBody::Expr(ref e) => {
                let wrap = if let Expression::Object(_) = &**e {
                    true
                } else {
                    false
                };
                if wrap {
                    self.write("(")?;
                }
                self.write_expr(e)?;
                if wrap {
                    self.write(")")?;
                }
            },
        }
        Ok(())
    }

    pub fn write_yield_expr(&mut self, expr: &YieldExpression) -> Res {
        trace!("write_yield_expr");
        self.write("yield ")?;
        if expr.delegate {
            self.write("*")?;
        }
        if let Some(ref arg) = &expr.argument {
            self.write_expr(arg)?;
        }
        Ok(())
    }

    pub fn write_meta_property(&mut self, meta: &MetaProperty) -> Res {
        trace!("write_meta_property");
        self.write_ident(&meta.meta)?;
        self.write(".")?;
        self.write_ident(&meta.property)?;
        Ok(())
    }

    pub fn write_await_expr(&mut self, expr: &Expression) -> Res {
        trace!("write_await_expr");
        self.write("await ")?;
        self.write_expr(expr)?;
        Ok(())
    }

    pub fn write_ident(&mut self, ident: &str) -> Res {
        trace!("write_ident");
        self.write(ident)
    }

    pub fn write_tagged_template(&mut self, template: &TaggedTemplateExpression) -> Res {
        trace!("write_tagged_template");
        self.write_expr(&template.tag)?;
        self.write_template(&template.quasi)?;
        Ok(())
    }

    pub fn write_literal(&mut self, lit: &Literal) -> Res {
        trace!("write_literal");
        match lit {
            Literal::Boolean(b) => self.write_bool(*b),
            Literal::Null => self.write("null"),
            Literal::Number(n) => self.write(&n),
            Literal::String(s) => self.write_string(s),
            Literal::RegEx(r) => self.write_regex(r),
            Literal::Template(t) => self.write_template(t),
        }
    }

    pub fn write_bool(&mut self, boolean: bool) -> Res {
        trace!("write_bool");
        if boolean {
            self.write("true")
        } else {
            self.write("false")
        }
    }

    pub fn write_string(&mut self, s: &str) -> Res {
        trace!("write_string");
        self.write(s)?;
        Ok(())
    }

    pub fn write_regex(&mut self, regex: &RegEx) -> Res {
        trace!("write_regex");
        self.write("/")?;
        self.write(&regex.pattern)?;
        self.write("/")?;
        self.write(&regex.flags)?;
        Ok(())
    }

    pub fn write_template(&mut self, template: &TemplateLiteral) -> Res {
        trace!("write_template");
        let mut quasis = template.quasis.iter();
        let mut exprs = template.expressions.iter();
        while let Some(quasi) = quasis.next() {
            self.write(&quasi.raw)?;
            if let Some(exp) = exprs.next() {
                self.write_expr(exp)?;
            }
        }
        Ok(())
    }

    pub fn write_empty_stmt(&mut self) -> Res {
        trace!("write_empty_stmt");
        self.write(";")
    }

    pub fn write_open_brace(&mut self) -> Res {
        trace!("write_open_brace");
        self.write("{")?;
        self.current_indent += 1;
        Ok(())
    }

    pub fn write_close_brace(&mut self) -> Res {
        trace!("write_close_brace");
        self.current_indent -= 1;
        self.write_leading_whitespace()?;
        self.write("}")
    }

    pub fn write_leading_whitespace(&mut self) -> Res {
        trace!("write_leading_whitespace");
        self.write(&" ".repeat(4).repeat(self.current_indent))?;
        Ok(())
    }

    pub fn write_new_line(&mut self) -> Res {
        trace!("write_new_line");
        self.write("\n")
    }

    fn write(&mut self, s: &str) -> Res {
        let _ = self.out.write(s.as_bytes())?;
        Ok(())
    }

}


#[cfg(test)]
mod test {
    use super::*;
    use std::fs::{File, remove_file};
    use std::io::Read;
    #[test]
    fn write_empty_expression() {
        let f = File::create("write_empty_expression.js").unwrap();
        let mut w = Writer::new(f);
        w.write_empty_expression().unwrap();
        let mut f = File::open("write_empty_expression.js").unwrap();
        let mut s = String::new();
        f.read_to_string(&mut s).unwrap();
        assert_eq!(s, ";".to_string());
        std::fs::remove_file("write_empty_expression.js").unwrap();
    }

    #[test]
    fn write_debugger_stmt() {
        let f = File::create("write_debugger_stmt.js").unwrap();
        let mut w = Writer::new(f);
        let mut s = String::new();
        w.write_debugger_stmt().unwrap();
        let mut f = File::open("write_debugger_stmt.js").unwrap();
        f.read_to_string(&mut s).unwrap();
        assert_eq!(s, "debugger;");
        remove_file("write_debugger_stmt.js").unwrap();
    }

    #[test]
    fn write_variable_decls() {
        let (mut w, mut r) = files("write_variable_decls");
        w.write_variable_decls(&VariableKind::Var, &[VariableDecl::with_value("thing", Expression::boolean(false))]);
        r.sync_all().unwrap();
        let mut s = String::new();
        r.read_to_string(&mut s).unwrap();
        assert_eq!(s, "var thing = false;\n".to_string());
        let (mut w, mut r) = files("write_variable_decls.2");
        w.write_variable_decls(&VariableKind::Let, &[
            VariableDecl::uninitialized("stuff"),
            VariableDecl::uninitialized("places"),
            VariableDecl::with_value("thing", Expression::boolean(false)),
        ]).unwrap();
        let mut s = String::new();
        r.sync_all().unwrap();
        r.read_to_string(&mut s);
        assert_eq!(s, "let stuff, places, thing = false;\n");
        clean_up("write_variable_decls");
        clean_up("write_variable_decls.2");
    }

    fn files(name: &str) -> (Writer<File>, File) {
        let name = format!("{}.js", name);
        let f1 = File::create(&name).unwrap();
        let w = Writer::new(f1);
        let f2 = File::open(&name).unwrap();
        (w, f2)
    }

    fn clean_up(name: &str) {
        remove_file(format!("{}.js", name)).unwrap();
    }
}