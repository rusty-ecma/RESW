use ressa::Parser;
use resw::Writer;
use resast::prelude::*;
use std::{
    fs::{
        read_to_string, 
        File
    },
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
    for ref part in p.map(|r| r.unwrap()).map(map_part) {
        w.write_part(part).expect("failed to write part");
    }
}

fn map_part<'a>(part: ProgramPart<'a>) -> ProgramPart<'a> {
    match part {
        ProgramPart::Decl(decl) => ProgramPart::Decl(map_decl(decl)),
        ProgramPart::Stmt(stmt) => ProgramPart::Stmt(map_stmt(stmt)),
        ProgramPart::Dir(_) => part,
    }
}

fn map_decl<'a>(decl: Decl<'a>) -> Decl<'a> {
    match decl {
        Decl::Func(f) => Decl::Func(map_func(f)),
        Decl::Class(class) => Decl::Class(map_class(class)),
        _ => decl.clone(),
    }
}

fn map_stmt<'a>(stmt: Stmt<'a>) -> Stmt<'a> {
    match stmt {
        Stmt::Expr(expr) => Stmt::Expr(map_expr(expr)),
        _ => stmt.clone(),
    }
}

fn map_expr<'a>(expr: Expr<'a>) -> Expr<'a> {
    match expr {
        Expr::Func(f) => Expr::Func(map_func(f)),
        Expr::Class(c) => Expr::Class(map_class(c)),
        _ => expr.clone(),
    }
}

fn map_func<'a>(mut func: Func<'a>) -> Func<'a> {
    let mut args = vec![];
    if let Some(ref name) = &func.id {
        args.push(Expr::Lit(Lit::String(StringLit::Single(name.name.clone()))));
    }
    for arg in func.params.iter().filter_map(|a| match a {
        FuncArg::Expr(e) => match e {
            Expr::Ident(i) => Some(i),
            _ => None,
        },
        FuncArg::Pat(p) => match p {
            Pat::Ident(i) => Some(i),
            _ => None,
        },
    }) {
        args.push(Expr::Ident(arg.clone()));
    }
    func.body.0.insert(0, console_log(args));
    func.body = FuncBody(func.body.0.into_iter().map(map_part).collect());
    func
}

fn map_class<'a>(mut class: Class<'a>) -> Class<'a> {
    let prefix = if let Some(ref id) = class.id {
        Some(
            Expr::Lit(
                Lit::String(
                    StringLit::Single(
                        id.name.clone()
                    )
                )
            )
        )
    } else {
        None
    };
    let mut new_body = vec![];
    for item in class.body.0 {
        new_body.push(map_class_prop(prefix.clone(), item))
    }
    class.body = ClassBody(new_body);
    class
}

fn map_class_prop<'a>(prefix: Option<Expr<'a>>, mut prop: Prop<'a>) -> Prop<'a> {
    let mut args = match prop.kind {
        PropKind::Ctor => {
            let mut args = vec![
                Expr::Lit(Lit::String(StringLit::single_from("new"))),
            ];
            if let Some(ref s) = prefix {
                args.push(s.clone());
            }
            args
        },
        PropKind::Get => {
            let mut args = vec![];
            if let Some(ref s) = prefix {
                args.push(s.clone());
            }
            args.push(
                Expr::Lit(Lit::String(StringLit::single_from("get")))
            );
            args
        },
        PropKind::Set => {
            let mut args = vec![];
            if let Some(ref s) = prefix {
                args.push(s.clone());
            }
            args.push(
                Expr::Lit(Lit::String(StringLit::single_from("set")))
            );
            args
        },
        PropKind::Method => {
            let mut args = vec![];
            if let Some(ref s) = prefix {
                args.push(s.clone());
            }
            args
        },
        _ => vec![],
    };
    match &prop.key {
        PropKey::Expr(ref expr) => match expr {
            Expr::Ident(ref i) => {
                if i.name != "constructor" {
                    args.push(Expr::Lit(Lit::String(StringLit::Single(i.name.clone()))));
                }
            }
            _ => (),
        },
        PropKey::Lit(ref l) => match l {
            Lit::Boolean(ref b) => {
                args.push(Expr::Lit(Lit::String(StringLit::Single(::std::borrow::Cow::Owned(format!("{}", b))))));
            }
            Lit::Null => {
                args.push(Expr::Lit(Lit::String(StringLit::Single(::std::borrow::Cow::Owned(String::from("null"))))));
            }
            Lit::Number(ref n) => {
                args.push(Expr::Lit(Lit::String(StringLit::Single(::std::borrow::Cow::Owned(format!("{}", n))))));
            }
            Lit::RegEx(ref r) => {
                let mut s = String::from("/");
                s.push_str(&r.pattern);
                s.push('/');
                s.push_str(&r.flags);
                let s = ::std::borrow::Cow::Owned(s);
                args.push(Expr::Lit(Lit::String(StringLit::Single(s))));
            }
            Lit::String(ref s) => {
                match s {
                    StringLit::Double(inner)
                    | StringLit::Single(inner) => {
                        if inner != "constructor" {
                            args.push(Expr::Lit(Lit::String(s.clone())))
                        }
                    }
                }
            }
            _ => (),
        },
        PropKey::Pat(ref p) => match p {
            Pat::Ident(ref i) => {
                args.push(Expr::Lit(Lit::String(StringLit::Single(i.name.clone()))));
            }
            _ => (),
        },
    }
    if let PropValue::Expr(ref mut expr) = prop.value {
        match expr {
            Expr::Func(ref mut f) => {
                for ref arg in &f.params {
                    match arg {
                        FuncArg::Expr(ref expr) => match expr {
                            Expr::Ident(_) => args.push(expr.clone()),
                            _ => (),
                        },
                        FuncArg::Pat(ref pat) => match pat {
                            Pat::Ident(ref ident) => args.push(Expr::Ident(ident.clone())),
                            _ => {}
                        },
                    }
                }
                insert_expr_into_func_body(console_log(args), f)
            }
            Expr::ArrowFunc(ref mut arrow) => match &arrow.body {
                _ => (),
            },
            _ => (),
        }
    }
    prop
}

fn insert_expr_into_func_body<'a>(expr: ProgramPart<'a>, func: &mut Func<'a>) {
    func.body.0.insert(0, expr);
}

pub fn console_log<'a>(args: Vec<Expr<'a>>) -> ProgramPart<'a> {
    ProgramPart::Stmt(Stmt::Expr(Expr::Call(
        CallExpr {
            callee: Box::new(Expr::Member(
                MemberExpr {
                    computed: false,
                    object: Box::new(Expr::ident_from("console")),
                    property: Box::new(Expr::ident_from("log")),
                }
            )),
            arguments: args,
        }
    )))
}
