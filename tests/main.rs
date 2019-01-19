#![cfg(test)]
#[cfg(feature = "moz_central")]
extern crate reqwest;
#[cfg(feature = "moz_central")]
extern crate flate2;
#[cfg(feature = "moz_central")]
extern crate tar;
#[cfg(feature = "moz_central")]
extern crate rayon;
#[cfg(feature = "moz_central")]
mod spider_monkey;

use ressa::Builder;
use resw::{
    Writer,
    write_str::WriteString,    
};

#[test]
fn everything_js_es5() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("./node_modules/everything.js/es5.js").expect("Failed to read js file");
    let (first, second) = double_round_trip(&js, false);
    check_round_trips("everything.es5", &first, &second);
}

#[test]
fn everything_js_es2015_script() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("./node_modules/everything.js/es2015-script.js").expect("failed to read js file");
    let (first, second) = double_round_trip(&js, false);
    check_round_trips("everything.es2015-script", &first, &second);
}

#[test]
fn everything_js_es2015_module() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("./node_modules/everything.js/es2015-module.js").expect("failed to read js file");
    let (first, second) = double_round_trip(&js, true);
    check_round_trips("everything.es2015-module", &first, &second);
}

#[test]
fn jquery() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("./node_modules/jquery/dist/jquery.js").expect("failed to read js file");
    let (first, second) = double_round_trip(&js, false);
    check_round_trips("jquery", &first, &second);
}

#[test]
fn angular() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("./node_modules/angular/angular.js").expect("failed to read js file");
    let (first, second) = double_round_trip(&js, false);
    check_round_trips("angular", &first, &second);
}

#[test]
fn react() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("node_modules/react/cjs/react.development.js").expect("failed to read js file");
    let (first, second) = double_round_trip(&js, false);
    check_round_trips("react", &first, &second);
}
#[test]
fn react_dom() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("node_modules/react-dom/cjs/react-dom.development.js").expect("failed to read js file");
    let (first, second) = double_round_trip(&js, false);
    check_round_trips("react_dom", &first, &second);
}

#[test]
fn vue() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("node_modules/vue/dist/vue.js").expect("failed to read js file");
    let (first, second) = double_round_trip(&js, false);
    check_round_trips("vue", &first, &second);
}

#[test]
fn dexie() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("node_modules/dexie/dist/dexie.js").expect("failed to read js file");
    let (first, second) = double_round_trip(&js, false);
    check_round_trips("dexie", &first, &second);
}

#[test]
fn moment() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("node_modules/moment/moment.js").expect("failed to read js file");
    let (first, second) = double_round_trip(&js, false);
    check_round_trips("moment", &first, &second);
}

fn double_round_trip(js: &str, module: bool) -> (String, Option<String>) {
    let mut first_write = WriteString::new();
    let mut second_write = WriteString::new();
    let first_parser = Builder::new().module(module).js(js).build().expect("Failed to create parser");
    let mut first_writer = Writer::new(first_write.generate_child());
    for part in first_parser {
        let part = part.expect("failed to parse part in first pass");
        if let ressa::node::ProgramPart::Statement(ressa::node::Statement::Expr(ressa::node::Expression::Function(ref func))) = part {
            if func.generator {
                println!("break");
            }
        }
        first_writer.write_part(&part).expect("Failed to write part");
    }
    let first_pass = first_write.get_string().expect("Invalid utf-8 written to first write");
    let second_parser = Builder::new().module(module).js(first_pass.as_str()).build().expect("Failed to create second parser");
    let mut second_writer = Writer::new(second_write.generate_child());
    for part in second_parser {
        let part = match part {
            Ok(part) => part,
            Err(e) => {
                println!("{}", e);
                return (first_pass, None)
            },
        };
        second_writer.write_part(&part).expect("failed to write part");
    }
    let second_pass = second_write.get_string().expect("Invalid utf-8 written to second write");
    (first_pass, Some(second_pass))
}

fn write_failure(name: &str, first: &str, second: &str) {
    use std::io::Write;
    let mut f1 = ::std::fs::File::create(&format!("{}.first.js", name)).expect("Failed to create first failure file");
    f1.write_all(first.as_bytes()).expect("failed to write to first failure file");
    let mut f2 = ::std::fs::File::create(&format!("{}.second.js", name)).expect("Failed to create second failure file");
    f2.write_all(second.as_bytes()).expect("failed to write second failure file");
}

fn ensure_libraries() {
    if !::std::path::PathBuf::from("node_modules").exists() {
        let output = ::std::process::Command::new("npm").arg("i").output().expect("failed to execute npm i");
        assert!(output.status.success());
    }
}

pub(crate) fn check_round_trips(name: &str, first: &str, second: &Option<String>) {
    if let Some(second) = second {
        if first != second {
            write_failure(name, first, second);
            panic!("Double round trip failed for {0}\ncheck ./{0}.first.js and ./{0}.second.js", name);
        }
    } else {
        ::std::fs::write(&format!("{}.first.js", name), first).expect("Failed to write file");
        panic!("Double round trip failed to parse second pass for {0}\n chec ./{0}.first.js", name);
    }
}

