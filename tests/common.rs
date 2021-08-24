use ressa::Parser;
use resw::{write_str::WriteString, Writer};
pub fn double_round_trip(js: &str, module: bool) -> (String, Option<String>) {
    let mut first_write = WriteString::new();
    let mut second_write = WriteString::new();
    let first_parser = Parser::builder()
        .module(module)
        .js(js)
        .build()
        .expect("Failed to create parser");
    let mut first_writer = Writer::new(first_write.generate_child());
    for part in first_parser {
        let part = match part {
            Ok(part) => part,
            Err(e) => {
                println!("Error parsing part in first pass {}", e);
                break;
            }
        };
        first_writer
            .write_part(&part)
            .expect("Failed to write part");
    }
    let first_pass = first_write
        .get_string()
        .expect("Invalid utf-8 written to first write");
    let second_parser = Parser::builder()
        .module(module)
        .js(first_pass.as_str())
        .build()
        .expect("Failed to create second parser");
    let mut second_writer = Writer::new(second_write.generate_child());
    for part in second_parser {
        let part = match part {
            Ok(part) => part,
            Err(e) => {
                println!("Error parsing part in second pass {}", e);
                let parsed = second_write
                    .get_string()
                    .expect("Invalid utf-8 written to second write");
                let second_pass = if parsed.len() == 0 {
                    None
                } else {
                    Some(parsed)
                };
                return (first_pass, second_pass);
            }
        };
        second_writer
            .write_part(&part)
            .expect("failed to write part");
    }
    let second_pass = second_write
        .get_string()
        .expect("Invalid utf-8 written to second write");
    (first_pass, Some(second_pass))
}