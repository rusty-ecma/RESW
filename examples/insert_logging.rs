use resast::prelude::*;
use ressa::Parser;
use resw::Writer;
use std::{
    fs::{read_to_string, File},
    io::BufWriter,
};

fn main() {
    pretty_env_logger::init();
    let mut args = ::std::env::args();
    let _ = args.next();
    let file_name = args
        .next()
        .unwrap_or(String::from("./examples/insert_logging.js"));
    let js = read_to_string(file_name).expect("Unable to find js file");
    let p = Parser::new(&js).expect("Failed to create parser");
    let f = File::create("./examples/inserted.js").expect("failed to create file");
    let mut w = Writer::new(BufWriter::new(f));
    for ref part in p.map(|r| r.unwrap()).map(|p| map_part(vec![], p)) {
        w.write_part(part).expect("failed to write part");
    }
}

fn map_part<'a>(args: Vec<Expr<'a>>, part: ProgramPart<'a>) -> ProgramPart<'a> {
    match part {
        ProgramPart::Decl(decl) => ProgramPart::Decl(map_decl(args, decl)),
        ProgramPart::Stmt(stmt) => ProgramPart::Stmt(map_stmt(args, stmt)),
        ProgramPart::Dir(_) => part,
    }
}

fn map_decl<'a>(mut args: Vec<Expr<'a>>, decl: Decl<'a>) -> Decl<'a> {
    match decl {
        Decl::Func(f) => Decl::Func(map_func(args, f)),
        Decl::Class(class) => Decl::Class(map_class(args, class)),
        Decl::Var(kind, del) => Decl::Var(
            kind,
            del.into_iter()
                .map(|part| {
                    if let Pat::Ident(ref ident) = part.id {
                        args.push(ident_to_string_lit(ident));
                    }
                    VarDecl {
                        id: part.id,
                        init: part.init.map(|e| map_expr(args.clone(), e)),
                    }
                })
                .collect(),
        ),
        _ => decl.clone(),
    }
}

fn map_stmt<'a>(args: Vec<Expr<'a>>, stmt: Stmt<'a>) -> Stmt<'a> {
    match stmt {
        Stmt::Expr(expr) => Stmt::Expr(map_expr(args, expr)),
        _ => stmt.clone(),
    }
}

fn map_expr<'a>(mut args: Vec<Expr<'a>>, expr: Expr<'a>) -> Expr<'a> {
    match expr {
        Expr::Func(f) => Expr::Func(map_func(args, f)),
        Expr::Class(c) => Expr::Class(map_class(args, c)),
        Expr::ArrowFunc(f) => Expr::ArrowFunc(map_arrow_func(args, f)),
        Expr::Assign(mut assign) => {
            if let Some(expr) = assign_left_to_string_lit(&assign.left) {
                args.push(expr);
            }
            assign.right = Box::new(map_expr(args, *assign.right));
            Expr::Assign(assign.clone())
        }
        _ => expr.clone(),
    }
}

fn map_func<'a>(mut args: Vec<Expr<'a>>, mut func: Func<'a>) -> Func<'a> {
    if let Some(ref id) = &func.id {
        args.push(ident_to_string_lit(id));
    }
    let local_args = extract_idents_from_args(&func.params);
    func.body = FuncBody(
        func.body
            .0
            .into_iter()
            .map(|p| map_part(args.clone(), p))
            .collect(),
    );
    insert_expr_into_func(
        console_log(
            args.clone()
                .into_iter()
                .chain(local_args.into_iter())
                .collect(),
        ),
        &mut func,
    );
    func
}

fn map_class<'a>(mut args: Vec<Expr<'a>>, mut class: Class<'a>) -> Class<'a> {
    if let Some(ref id) = class.id {
        args.push(ident_to_string_lit(id))
    }
    let mut new_body = vec![];
    for item in class.body.0 {
        new_body.push(map_class_prop(args.clone(), item))
    }
    class.body = ClassBody(new_body);
    class
}

fn map_class_prop<'a>(mut args: Vec<Expr<'a>>, mut prop: Prop<'a>) -> Prop<'a> {
    match prop.kind {
        PropKind::Ctor => {
            args.insert(
                args.len().saturating_sub(1),
                Expr::Lit(Lit::String(StringLit::single_from("new"))),
            );
        }
        PropKind::Get => {
            args.push(Expr::Lit(Lit::String(StringLit::single_from("get"))));
        }
        PropKind::Set => {
            args.push(Expr::Lit(Lit::String(StringLit::single_from("set"))));
        }
        _ => (),
    };
    match &prop.key {
        PropKey::Expr(ref expr) => match expr {
            Expr::Ident(ref i) => {
                if i.name != "constructor" {
                    args.push(ident_to_string_lit(i));
                }
            }
            _ => (),
        },
        PropKey::Lit(ref l) => match l {
            Lit::Boolean(_) | Lit::Number(_) | Lit::RegEx(_) | Lit::String(_) => {
                args.push(Expr::Lit(l.clone()))
            }
            Lit::Null => {
                args.push(Expr::Lit(Lit::String(StringLit::Single(
                    ::std::borrow::Cow::Owned(String::from("null")),
                ))));
            }
            _ => (),
        },
        PropKey::Pat(ref p) => match p {
            Pat::Ident(ref i) => args.push(ident_to_string_lit(i)),
            _ => args.extend(extract_idents_from_pat(p).into_iter().filter_map(|e| e)),
        },
    }
    if let PropValue::Expr(expr) = prop.value {
        prop.value = PropValue::Expr(map_expr(args, expr));
    }
    prop
}

fn map_arrow_func<'a>(mut args: Vec<Expr<'a>>, mut f: ArrowFuncExpr<'a>) -> ArrowFuncExpr<'a> {
    args.extend(extract_idents_from_args(&f.params));
    match &mut f.body {
        ArrowFuncBody::FuncBody(ref mut body) => {
            insert_expr_into_func_body(console_log(args), body)
        }
        ArrowFuncBody::Expr(expr) => {
            f.body = ArrowFuncBody::FuncBody(FuncBody(vec![
                console_log(args),
                ProgramPart::Stmt(Stmt::Return(Some(*expr.clone()))),
            ]))
        }
    }
    f
}

fn assign_left_to_string_lit<'a>(left: &AssignLeft<'a>) -> Option<Expr<'a>> {
    match left {
        AssignLeft::Expr(expr) => expr_to_string_lit(expr),
        AssignLeft::Pat(pat) => match pat {
            Pat::Ident(ident) => Some(ident_to_string_lit(ident)),
            _ => None,
        },
    }
}

fn extract_idents_from_args<'a>(args: &[FuncArg<'a>]) -> Vec<Expr<'a>> {
    let mut ret = vec![];
    for arg in args {
        match arg {
            FuncArg::Expr(expr) => ret.push(extract_ident_from_expr(expr)),
            FuncArg::Pat(pat) => ret.extend(extract_idents_from_pat(pat)),
        }
    }
    ret.into_iter().filter_map(|e| e).collect()
}

fn extract_ident_from_expr<'a>(expr: &Expr<'a>) -> Option<Expr<'a>> {
    match expr {
        Expr::Ident(ident) => Some(Expr::Ident(ident.clone())),
        _ => None,
    }
}

fn extract_idents_from_pat<'a>(pat: &Pat<'a>) -> Vec<Option<Expr<'a>>> {
    match pat {
        Pat::Ident(i) => {
            vec![Some(Expr::Ident(i.clone()))]
        }
        Pat::Obj(obj) => obj
            .iter()
            .map(|part| match part {
                ObjPatPart::Rest(pat) => extract_idents_from_pat(pat),
                ObjPatPart::Assign(prop) => match prop.key {
                    PropKey::Pat(ref pat) => extract_idents_from_pat(pat),
                    PropKey::Expr(ref expr) => {
                        vec![extract_ident_from_expr(expr)]
                    }
                    PropKey::Lit(ref lit) => {
                        vec![Some(Expr::Lit(lit.clone()))]
                    }
                },
            })
            .flatten()
            .collect(),
        Pat::Array(arr) => arr
            .iter()
            .map(|p| match p {
                Some(ArrayPatPart::Expr(expr)) => {
                    vec![extract_ident_from_expr(expr)]
                }
                Some(ArrayPatPart::Pat(pat)) => extract_idents_from_pat(pat),
                None => vec![],
            })
            .flatten()
            .collect(),
        Pat::RestElement(pat) => extract_idents_from_pat(pat),
        Pat::Assign(assign) => extract_idents_from_pat(&*assign.left),
    }
}

fn expr_to_string_lit<'a>(e: &Expr<'a>) -> Option<Expr<'a>> {
    let inner = expr_to_string(e)?;
    Some(Expr::Lit(Lit::String(StringLit::Single(
        ::std::borrow::Cow::Owned(inner),
    ))))
}

fn expr_to_string(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(ref ident) => Some(ident.name.to_string()),
        Expr::This => Some("this".to_string()),
        Expr::Member(ref mem) => {
            let prefix = if let Some(s) = expr_to_string(&mem.object) {
                s
            } else {
                return None;
            };
            let suffix = if let Some(s) = expr_to_string(&mem.property) {
                s
            } else {
                return None;
            };
            Some(format!("{}.{}", prefix, suffix))
        }
        Expr::Lit(lit) => match lit {
            Lit::String(s) => Some(s.clone_inner().to_string()),
            Lit::Number(n) => Some(n.to_string()),
            Lit::RegEx(r) => Some(format!("/{}/{}", r.pattern, r.flags)),
            Lit::Boolean(b) => Some(b.to_string()),
            Lit::Null => Some("null".to_string()),
            _ => None,
        },
        _ => None,
    }
}

fn ident_to_string_lit<'a>(i: &Ident<'a>) -> Expr<'a> {
    Expr::Lit(Lit::String(StringLit::Single(i.name.clone())))
}

fn insert_expr_into_func<'a>(expr: ProgramPart<'a>, func: &mut Func<'a>) {
    insert_expr_into_func_body(expr, &mut func.body);
}

fn insert_expr_into_func_body<'a>(expr: ProgramPart<'a>, body: &mut FuncBody<'a>) {
    body.0.insert(0, expr);
}

pub fn console_log<'a>(args: Vec<Expr<'a>>) -> ProgramPart<'a> {
    ProgramPart::Stmt(Stmt::Expr(Expr::Call(CallExpr {
        callee: Box::new(Expr::Member(MemberExpr {
            computed: false,
            object: Box::new(Expr::ident_from("console")),
            property: Box::new(Expr::ident_from("log")),
        })),
        arguments: args,
    })))
}
