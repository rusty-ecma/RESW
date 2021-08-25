use resast::{Program, ProgramPart};
use ressa::Parser;
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Ressa(#[from] ressa::Error),
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Utf8(#[from] std::string::FromUtf8Error),
    #[error("Mismatched parts:\n{left}\n{right}")]
    Mismatched { left: String, right: String },
}

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

pub fn round_trip_validate<'a>(
    js: &'a str,
    module: bool,
    name: &'static str,
) -> Result<(), Box<dyn std::error::Error>> {
    let write_failures = std::env::var("RESW_WRITE_FAILURES") == Ok("1".to_string());
    let mut first_parser = Parser::builder().js(js).module(module).build()?;
    let first_parts = parse(&mut first_parser)?;
    let mut write_string = WriteString::new();
    let mut writer = Writer::new(write_string.generate_child());
    for part in &first_parts {
        writer.write_part(part)?;
    }
    let second_js = write_string.get_string()?;
    let mut second_parser = Parser::builder().js(&second_js).module(module).build()?;
    let second_parts = match parse(&mut second_parser) {
        Ok(parts) => parts,
        Err(e) => {
            if write_failures {
                write_failure(name, &second_js, &None);
            }
            return Err(e);
        }
    };
    if first_parts != second_parts {
        if write_failures {
            write_failure(name, &second_js, &None);
        }
        for (lhs, rhs) in first_parts.into_iter().zip(second_parts.into_iter()) {
            assert_eq!(lhs, rhs);
        }
    }

    // assert_eq!(first_parts, second_parts);
    Ok(())
}

pub fn parse<'a>(
    p: &'a mut Parser<'a, ressa::DefaultCommentHandler>,
) -> Result<Vec<ProgramPart<'a>>, Box<dyn std::error::Error>> {
    match p.parse()? {
        Program::Mod(parts) => Ok(parts),
        Program::Script(parts) => Ok(parts),
    }
}

pub fn write_failure(name: &str, first: &str, second: &Option<String>) {
    use std::io::Write;
    let dir = ::std::path::PathBuf::from("test_failures");
    if !dir.exists() {
        ::std::fs::create_dir(&dir).expect("failed to create test_failures");
    }
    let mut f1 = ::std::fs::File::create(dir.join(&format!("{}.first.js", name)))
        .expect("Failed to create first failure file");
    f1.write(format!("//{}\n", name).as_bytes())
        .expect("Failed to write first line");
    f1.write_all(first.as_bytes())
        .expect("failed to write to first failure file");
    if let Some(ref second) = second {
        let mut f2 = ::std::fs::File::create(dir.join(&format!("{}.second.js", name)))
            .expect("Failed to create second failure file");
        f2.write(format!("//{}\n", name).as_bytes())
            .expect("Failed to write first line");
        f2.write_all(second.as_bytes())
            .expect("failed to write second failure file");
    }
}
