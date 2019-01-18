use ressa::{node::*, Parser};
use resw::Writer;
use std::fs::{read_to_string, File};

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
    let mut w = Writer::new(f);
    for ref part in p.map(|r| r.unwrap()).map(map_part) {
        w.write_part(part).expect("failed to write part");
    }
}

fn map_part(part: ProgramPart) -> ProgramPart {
    match part {
        ProgramPart::Decl(ref decl) => ProgramPart::Decl(map_decl(decl)),
        ProgramPart::Statement(ref stmt) => ProgramPart::Statement(map_stmt(stmt)),
        ProgramPart::Directive(_) => part,
    }
}

fn map_decl(decl: &Declaration) -> Declaration {
    match decl {
        Declaration::Function(ref f) => Declaration::Function(map_func(f)),
        Declaration::Class(ref class) => Declaration::Class(map_class(class)),
        _ => decl.clone(),
    }
}

fn map_stmt(stmt: &Statement) -> Statement {
    match stmt {
        Statement::Expr(ref expr) => Statement::Expr(map_expr(expr)),
        _ => stmt.clone(),
    }
}

fn map_expr(expr: &Expression) -> Expression {
    match expr {
        Expression::Function(ref f) => Expression::Function(map_func(f)),
        Expression::Class(ref c) => Expression::Class(map_class(c)),
        _ => expr.clone(),
    }
}

fn map_func(func: &Function) -> Function {
    let mut f = func.clone();
    let mut args = vec![];
    if let Some(ref name) = f.id {
        args.push(Expression::string(&format!("'{}'", name)));
    }
    for arg in f.params.iter().filter_map(|a| match a {
        FunctionArg::Expr(e) => match e {
            Expression::Ident(i) => Some(i),
            _ => None,
        },
        FunctionArg::Pattern(p) => match p {
            Pattern::Identifier(i) => Some(i),
            _ => None,
        },
    }) {
        args.push(Expression::ident(arg));
    }
    f.body.insert(0, console_log(args));
    f.body = f.body.into_iter().map(map_part).collect();
    f
}

fn map_class(class: &Class) -> Class {
    let mut class = class.clone();
    let prefix = if let Some(ref id) = class.id {
        id.clone()
    } else {
        String::new()
    };

    class.body = class
        .body
        .iter()
        .map(|prop| map_class_prop(&prefix, prop))
        .collect();
    class
}

fn map_class_prop(prefix: &str, prop: &Property) -> Property {
    let mut prop = prop.clone();
    let mut args = match prop.kind {
        PropertyKind::Ctor => vec![Expression::string(&format!("'new {}'", prefix))],
        PropertyKind::Get => vec![
            Expression::string(&format!("'{}'", prefix)),
            Expression::string("get"),
        ],
        PropertyKind::Set => vec![
            Expression::string(&format!("'{}'", prefix)),
            Expression::string("set"),
        ],
        PropertyKind::Method => vec![Expression::string(&format!("'{}'", prefix))],
        _ => vec![],
    };
    match &prop.key {
        PropertyKey::Expr(ref expr) => match expr {
            Expression::Ident(ref i) => {
                if i != "constructor" {
                    args.push(Expression::string(&format!("'{}'", i)));
                }
            }
            _ => (),
        },
        PropertyKey::Literal(ref l) => match l {
            Literal::Boolean(ref b) => {
                args.push(Expression::string(&format!("'{}'", b)));
            }
            Literal::Null => {
                args.push(Expression::string("'null'"));
            }
            Literal::Number(ref n) => {
                args.push(Expression::string(&format!("'{}'", n)));
            }
            Literal::RegEx(ref r) => {
                args.push(Expression::string(&format!("'/{}/{}'", r.pattern, r.flags)));
            }
            Literal::String(ref s) => {
                if s != "constructor" {
                    args.push(Expression::string(s));
                }
            }
            _ => (),
        },
        PropertyKey::Pattern(ref p) => match p {
            Pattern::Identifier(ref i) => {
                args.push(Expression::string(&format!("'{}'", i)));
            }
            _ => (),
        },
    }
    if let PropertyValue::Expr(ref mut expr) = prop.value {
        match expr {
            Expression::Function(ref mut f) => {
                for ref arg in &f.params {
                    match arg {
                        FunctionArg::Expr(ref expr) => match expr {
                            Expression::Ident(_) => args.push(expr.clone()),
                            _ => (),
                        },
                        FunctionArg::Pattern(ref pat) => match pat {
                            Pattern::Identifier(ref ident) => args.push(Expression::ident(ident)),
                            _ => {}
                        },
                    }
                }
                insert_expr_into_func_body(console_log(args), f)
            }
            Expression::ArrowFunction(ref mut arrow) => match &arrow.body {
                _ => (),
            },
            _ => (),
        }
    }
    prop
}

fn insert_expr_into_func_body(expr: ProgramPart, func: &mut Function) {
    func.body.insert(0, expr);
}

pub fn console_log(args: Vec<Expression>) -> ProgramPart {
    ProgramPart::Statement(Statement::Expr(Expression::call(
        Expression::member(
            Expression::ident("console"),
            Expression::ident("log"),
            false,
        ),
        args,
    )))
}
