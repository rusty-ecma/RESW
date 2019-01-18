#![cfg(test)]
use ressa::Parser;
use resw::{
    Writer,
    write_str::WriteString,    
};

#[test]
fn everything_js_es5() {
    ensure_libraries();
    let js = ::std::fs::read_to_string("./node_modules/everything.js/es5.js").expect("Failed to read js file");
    let (first, second) = double_round_trip(&js);
    if first != second {
        write_failure("everything.es5", &first, &second)
    }
}

fn double_round_trip(js: &str) -> (String, String) {
    let mut first_write = WriteString::new();
    let mut second_write = WriteString::new();
    let first_parser = Parser::new(js).expect("Failed to create parser");
    let mut first_writer = Writer::new(first_write.generate_child());
    for part in first_parser {
        let part = part.expect("failed to parse part");
        first_writer.write_part(&part).expect("Failed to write part");
    }
    let first_pass = first_write.get_string().expect("Invalid utf-8 written to first write");
    let second_parser = Parser::new(&first_pass).expect("Failed to create second parser");
    let mut second_writer = Writer::new(second_write.generate_child());
    for part in second_parser {
        let part = part.expect("failed to parse part");
        second_writer.write_part(&part).expect("Faield to write part");
    }
    let second_pass = second_write.get_string().expect("Invalid utf-8 written to second write");
    (first_pass, second_pass)
}

fn write_failure(name: &str, first: &str, second: &str) {
    use std::io::Write;
    let mut f1 = ::std::fs::File::create(&format!("{}.first.js", name)).expect("Failed to create first failure file");
    f1.write_all(first.as_bytes()).expect("failed to write to first failure file");
    let mut f2 = ::std::fs::File::create(&format!("{}.second.js", name)).expect("Failed to create second failure file");
    f2.write_all(second.as_bytes()).expect("failed to write second failure file");
}

fn ensure_libraries() {
    let output = ::std::process::Command::new("npm").arg("i").output().expect("failed to execute npm i");
    assert!(output.status.success());
}