// use ressa::Parser;
// use resw::Writer;
// use resast::ref_tree::prelude::*;
// use std::fs::{read_to_string, File};

fn main() {
//     pretty_env_logger::init();
//     let mut args = ::std::env::args();
//     let _ = args.next();
//     let file_name = args
//         .next()
//         .unwrap_or(String::from("./examples/insert_logging.js"));
//     let js = read_to_string(file_name).expect("Unable to find js file");
//     let p = Parser::new(&js).expect("Failed to create parser");
//     let f = File::create("./examples/inserted.js").expect("failed to create file");
//     let mut w = Writer::new(f);
//     for ref part in p.map(|r| r.unwrap()).map(map_part) {
//         w.write_part(part).expect("failed to write part");
//     }
}

// fn map_part(part: ProgramPart) -> ProgramPart {
//     match part {
//         ProgramPart::Decl(ref decl) => ProgramPart::Decl(map_decl(decl)),
//         ProgramPart::Stmt(ref stmt) => ProgramPart::Stmt(map_stmt(stmt)),
//         ProgramPart::Dir(_) => part,
//     }
// }

// fn map_decl(decl: &Decl) -> Decl {
//     match decl {
//         Decl::Function(ref f) => Decl::Function(map_func(f)),
//         Decl::Class(ref class) => Decl::Class(map_class(class)),
//         _ => decl.clone(),
//     }
// }

// fn map_stmt(stmt: &Stmt) -> Stmt {
//     match stmt {
//         Stmt::Expr(ref expr) => Stmt::Expr(map_expr(expr)),
//         _ => stmt.clone(),
//     }
// }

// fn map_expr(expr: &Expr) -> Expr {
//     match expr {
//         Expr::Function(ref f) => Expr::Function(map_func(f)),
//         Expr::Class(ref c) => Expr::Class(map_class(c)),
//         _ => expr.clone(),
//     }
// }

// fn map_func(func: &Function) -> Function {
//     let mut f = func.clone();
//     let mut args = vec![];
//     if let Some(ref name) = f.id {
//         args.push(Expr::string(&format!("'{}'", name)));
//     }
//     for arg in f.params.iter().filter_map(|a| match a {
//         FunctionArg::Expr(e) => match e {
//             Expr::Ident(i) => Some(i),
//             _ => None,
//         },
//         FunctionArg::Pat(p) => match p {
//             Pat::Identifier(i) => Some(i),
//             _ => None,
//         },
//     }) {
//         args.push(Expr::ident(arg));
//     }
//     f.body.insert(0, console_log(args));
//     f.body = f.body.into_iter().map(map_part).collect();
//     f
// }

// fn map_class(class: &Class) -> Class {
//     let mut class = class.clone();
//     let prefix = if let Some(ref id) = class.id {
//         id.clone()
//     } else {
//         String::new()
//     };

//     class.body = class
//         .body
//         .iter()
//         .map(|prop| map_class_prop(&prefix, prop))
//         .collect();
//     class
// }

// fn map_class_prop<'a>(prefix: &str, prop: &Property<'a>) -> Property<'a> {
//     let mut prop = prop.clone();
//     let mut args = match prop.kind {
//         PropertyKind::Ctor => vec![Expr::Literal(Literal::String(&format!("'new {}'", prefix))],
//         PropertyKind::Get => vec![
//             Expr::string(&format!("'{}'", prefix)),
//             Expr::string("get"),
//         ],
//         PropertyKind::Set => vec![
//             Expr::string(&format!("'{}'", prefix)),
//             Expr::string("set"),
//         ],
//         PropertyKind::Method => vec![Expr::string(&format!("'{}'", prefix))],
//         _ => vec![],
//     };
//     match &prop.key {
//         PropertyKey::Expr(ref expr) => match expr {
//             Expr::Ident(ref i) => {
//                 if i != "constructor" {
//                     args.push(Expr::string(&format!("'{}'", i)));
//                 }
//             }
//             _ => (),
//         },
//         PropertyKey::Literal(ref l) => match l {
//             Literal::Boolean(ref b) => {
//                 args.push(Expr::string(&format!("'{}'", b)));
//             }
//             Literal::Null => {
//                 args.push(Expr::string("'null'"));
//             }
//             Literal::Number(ref n) => {
//                 args.push(Expr::string(&format!("'{}'", n)));
//             }
//             Literal::RegEx(ref r) => {
//                 args.push(Expr::string(&format!("'/{}/{}'", r.pattern, r.flags)));
//             }
//             Literal::String(ref s) => {
//                 if s != "constructor" {
//                     args.push(Expr::string(s));
//                 }
//             }
//             _ => (),
//         },
//         PropertyKey::Pat(ref p) => match p {
//             Pat::Identifier(ref i) => {
//                 args.push(Expr::string(&format!("'{}'", i)));
//             }
//             _ => (),
//         },
//     }
//     if let PropertyValue::Expr(ref mut expr) = prop.value {
//         match expr {
//             Expr::Function(ref mut f) => {
//                 for ref arg in &f.params {
//                     match arg {
//                         FunctionArg::Expr(ref expr) => match expr {
//                             Expr::Ident(_) => args.push(expr.clone()),
//                             _ => (),
//                         },
//                         FunctionArg::Pat(ref pat) => match pat {
//                             Pat::Identifier(ref ident) => args.push(Expr::ident(ident)),
//                             _ => {}
//                         },
//                     }
//                 }
//                 insert_expr_into_func_body(console_log(args), f)
//             }
//             Expr::ArrowFunction(ref mut arrow) => match &arrow.body {
//                 _ => (),
//             },
//             _ => (),
//         }
//     }
//     prop
// }

// fn insert_expr_into_func_body(expr: ProgramPart, func: &mut Function) {
//     func.body.insert(0, expr);
// }

// pub fn console_log(args: Vec<Expr>) -> ProgramPart {
//     ProgramPart::Stmt(Stmt::Expr(Expr::call(
//         Expr::member(
//             Expr::ident("console"),
//             Expr::ident("log"),
//             false,
//         ),
//         args,
//     )))
// }
