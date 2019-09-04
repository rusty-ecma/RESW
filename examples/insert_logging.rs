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
        Decl::Var(kind, del) => {
            Decl::Var(kind, del.into_iter().map(|part| {
                let args = extract_idents_from_pat(&part.id).into_iter().filter_map(|e| e).collect();
                VarDecl {
                    id: part.id,
                    init: part.init.map(|e| map_expr(args, e))
                }
            }).collect())
        }
        _ => decl.clone(),
    }
}

fn map_stmt<'a>(stmt: Stmt<'a>) -> Stmt<'a> {
    match stmt {
        Stmt::Expr(expr) => Stmt::Expr(map_expr(vec![], expr)),
        _ => stmt.clone(),
    }
}

fn map_expr<'a>(mut args: Vec<Expr<'a>>, expr: Expr<'a>) -> Expr<'a> {
    match expr {
        Expr::Func(f) => Expr::Func(map_func(f)),
        Expr::Class(c) => Expr::Class(map_class(c)),
        Expr::ArrowFunc(f) => Expr::ArrowFunc(map_arrow_func(vec![], f)),
        Expr::Assign(mut assign) => {
            match assign.left {
                AssignLeft::Expr(ref expr) => if let Some(expr) = extract_ident_from_expr(expr) {
                    args.push(expr);
                },
                AssignLeft::Pat(ref pat) => args.extend(
                    extract_idents_from_pat(pat).into_iter().filter_map(|e| e)
                ),
            }
            assign.right = Box::new(map_expr(args, *assign.right));
            Expr::Assign(
                assign.clone()
            )
        },
        _ => expr.clone(),
    }
}

fn map_func<'a>(mut func: Func<'a>) -> Func<'a> {
    let mut args = vec![];
    if let Some(ref name) = &func.id {
        args.push(Expr::Lit(Lit::String(StringLit::Single(name.name.clone()))));
    }
    args.extend(
        extract_idents_from_args(&func.params)
    );
    func.body.0.insert(0, console_log(args));
    func.body = FuncBody(func.body.0.into_iter().map(map_part).collect());
    func
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
        },
        Pat::Obj(obj) => {
            obj.iter().map(|part| {
                match part {
                    ObjPatPart::Rest(pat) => {
                        extract_idents_from_pat(pat)
                    },
                    ObjPatPart::Assign(prop) => {
                        match prop.key {
                            PropKey::Pat(ref pat) => {
                                extract_idents_from_pat(pat)
                            },
                            PropKey::Expr(ref expr) => {
                                vec![extract_ident_from_expr(expr)]
                            },
                            PropKey::Lit(ref lit) => {
                                vec![Some(Expr::Lit(lit.clone()))]
                            }
                        }
                    },
                }
            }).flatten().collect()
        },
        Pat::Array(arr) => {
            arr.iter().map(|p| {
                match p {
                    Some(ArrayPatPart::Expr(expr)) => {
                        vec![extract_ident_from_expr(expr)]
                    },
                    Some(ArrayPatPart::Pat(pat)) => {
                        extract_idents_from_pat(pat)
                    },
                    None => vec![],
                }
            }).flatten().collect()
        },
        Pat::RestElement(pat) => {
            extract_idents_from_pat(pat)
        },
        Pat::Assign(assign) => {
            extract_idents_from_pat(&*assign.left)
        },
    }
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
                    args.push(idend_to_string_lit(i));
                }
            }
            _ => (),
        },
        PropKey::Lit(ref l) => match l {
            Lit::Boolean(_)
            | Lit::Number(_)
            | Lit::RegEx(_)
            | Lit::String(_) => {
                args.push(Expr::Lit(l.clone()))
            }
            Lit::Null => {
                args.push(Expr::Lit(Lit::String(StringLit::Single(::std::borrow::Cow::Owned(String::from("null"))))));
            }
            _ => (),
        },
        PropKey::Pat(ref p) => {
            match p {
                Pat::Ident(ref i) => args.push(idend_to_string_lit(i)),
                _ => args.extend(extract_idents_from_pat(p).into_iter().filter_map(|e| e)),
            }
        },
    }
    if let PropValue::Expr(ref mut expr) = prop.value {
        match expr {
            Expr::Func(ref mut f) => {
                args.extend(extract_idents_from_args(&f.params));
                insert_expr_into_func(console_log(args), f)
            }
            Expr::ArrowFunc(ref mut arrow) => {
                args.extend(extract_idents_from_args(&arrow.params));
                match &mut arrow.body {
                    ArrowFuncBody::FuncBody(ref mut body) => {
                        insert_expr_into_func_body(console_log(args), body)
                    },
                    ArrowFuncBody::Expr(expr) => {
                        arrow.body = ArrowFuncBody::FuncBody(FuncBody(vec![
                            console_log(args),
                            ProgramPart::Stmt(
                                Stmt::Return(
                                    Some(*expr.clone())
                                )
                            )
                        ]))
                    }
                }
            },
            _ => (),
        }
    }
    prop
}

fn idend_to_string_lit<'a>(i: &Ident<'a>) -> Expr<'a> {
    Expr::Lit(Lit::String(StringLit::Single(i.name.clone())))
}

fn map_arrow_func<'a>(mut args: Vec<Expr<'a>>, mut f: ArrowFuncExpr<'a>) -> ArrowFuncExpr<'a> {
    args.extend(extract_idents_from_args(&f.params));
    match &mut f.body {
        ArrowFuncBody::FuncBody(ref mut body) => {
            insert_expr_into_func_body(console_log(args), body)
        },
        ArrowFuncBody::Expr(expr) => {
            f.body = ArrowFuncBody::FuncBody(FuncBody(vec![
                console_log(args),
                ProgramPart::Stmt(
                    Stmt::Return(
                        Some(*expr.clone())
                    )
                )
            ]))
        }
    }
    f
}

fn insert_expr_into_func<'a>(expr: ProgramPart<'a>, func: &mut Func<'a>) {
    insert_expr_into_func_body(expr, &mut func.body);
}

fn insert_expr_into_func_body<'a>(expr: ProgramPart<'a>, body: &mut FuncBody<'a>) {
    body.0.insert(0, expr);
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
