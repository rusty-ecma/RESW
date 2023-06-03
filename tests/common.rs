use std::borrow::Cow;

use resast::{Program, ProgramPart};
use ressa::Parser;
use resw::{write_str::WriteString, Writer};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0} <- Ressa")]
    Ressa(#[from] ressa::Error),
    #[error("{0} <- Io")]
    Io(#[from] std::io::Error),
    #[error("{0} <- Uft8")]
    Utf8(#[from] std::string::FromUtf8Error),
    #[error("\n{left}\n{right} <- Mismatched parts")]
    Mismatched { left: String, right: String },
    #[error("{0} <- Panic")]
    Panic(String),
}

pub struct MismatchedError<T, U>
where
    T: std::fmt::Debug,
    U: std::fmt::Debug,
{
    pub left: T,
    pub right: U,
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

pub fn round_trip_validate<'a>(js: &'a str, module: bool, name: &str) -> Result<(), Error> {
    pretty_env_logger::try_init().ok();
    if std::env::var("RESW_SKIP_BARE_TEST") != Ok("1".to_string()) {
        round_trip_validate_bare(js, module, name).map_err(|e| {
            println!("Error validating bare!");
            e
        })?;
    }
    round_trip_validate_spanned(js, module, name).map_err(|e| {
        println!("Error validating spanned!");
        e
    })?;
    Ok(())
}
pub fn round_trip_validate_bare<'a>(js: &'a str, module: bool, name: &str) -> Result<(), Error> {
    let write_failures = std::env::var("RESW_WRITE_FAILURES") == Ok("1".to_string());
    let write_success = std::env::var("RESW_WRITE_SUCCESS") == Ok("1".to_string());
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
        let MismatchedError { left, right } = find_mismatched(&first_parts, &second_parts).unwrap();

        if write_failures {
            let to_write = format!("//{:?}\n//{:?}\n\n{}", left, right, second_js);
            write_failure(name, &to_write, &None);
            write_file(&format!("{:#?}", left), &format!("{}.l.ron", name));
            write_file(&format!("{:#?}", right), &format!("{}.r.ron", name));
        }
        return Err(Error::Mismatched {
            left: format!("{left:?}"),
            right: format!("{right:?}"),
        });
    }
    if write_success {
        write_failure(name, &second_js, &None);
    }
    Ok(())
}

pub fn round_trip_validate_spanned<'a>(js: &'a str, module: bool, name: &str) -> Result<(), Error> {
    use resast::spanned::Program;
    use ressa::spanned::Parser;
    use resw::spanned::SpannedWriter as Writer;
    let write_failures = std::env::var("RESW_WRITE_FAILURES") == Ok("1".to_string());
    let mut first_parser = Parser::builder().js(js).module(module).build()?;
    let first_parts = match first_parser.parse()? {
        Program::Mod(parts) => parts,
        Program::Script(parts) => parts,
    };
    let mut write_string = WriteString::new();
    let mut writer = Writer::new(write_string.generate_child());
    for part in &first_parts {
        writer.write_part(part)?;
    }
    let second_js = write_string.get_string()?;
    let mut second_parser = Parser::builder().js(&second_js).module(module).build()?;
    let res = std::panic::catch_unwind({
        let second_js = second_js.clone();
        move || {
            let res = second_parser.parse();
            let second_parts = match res {
                Ok(prog) => match prog {
                    Program::Mod(parts) => parts,
                    Program::Script(parts) => parts,
                },
                Err(e) => {
                    if write_failures {
                        write_failure(name, &second_js, &None);
                    }
                    return Err(Error::Ressa(e));
                }
            };
            if first_parts != second_parts {
                let mismatched = find_mismatched(&first_parts, &second_parts).unwrap();

                if write_failures {
                    let name = format!("{}-spanned", name);
                    report_mismatch(&name, &second_js, &mismatched);
                }
                return Err(Error::Mismatched {
                    left: format!("{:?}", mismatched.left),
                    right: format!("{:?}", mismatched.right),
                });
            }
            Ok(())
        }
    })
    .map_err(|e| {
        let msg = if let Some(x) = e.downcast_ref::<&dyn std::fmt::Debug>() {
            format!("{:?}", x)
        } else {
            "Panicked while parsing...".to_string()
        };
        eprintln!("panic: {}", msg);
        Error::Panic(msg)
    })?;
    if let Err(e) = res {
        if write_failures {
            write_failure(name, &second_js, &None);
        }
        return Err(e);
    }
    Ok(())
}

pub fn report_mismatch<T: std::fmt::Debug>(
    name: &str,
    first_js: &str,
    mismatched: &MismatchedError<T, T>,
) {
    let MismatchedError { left, right } = mismatched;
    let to_write = format!("//{:?}\n//{:?}\n\n{}", left, right, first_js);
    let name = format!("{}-spanned", name);

    write_failure(&name, &to_write, &None);
    write_file(
        &format!("{:#?}", left),
        &format!("{}-mismatch.left.ron", name),
    );
    write_file(
        &format!("{:#?}", right),
        &format!("{}-mismatch.right.ron", name),
    );
}

pub fn find_mismatched<T>(first_parts: &[T], second_parts: &[T]) -> Option<MismatchedError<T, T>>
where
    T: PartialEq + std::fmt::Debug + Clone,
{
    for (lhs, rhs) in first_parts.into_iter().zip(second_parts.into_iter()) {
        if lhs != rhs {
            return Some(MismatchedError {
                left: lhs.clone(),
                right: rhs.clone(),
            });
        }
    }
    None
}

pub fn parse<'a>(
    p: &'a mut Parser<'a, ressa::DefaultCommentHandler>,
) -> Result<Vec<ProgramPart<Cow<'a, str>>>, Error> {
    match p.parse()? {
        Program::Mod(parts) => Ok(parts),
        Program::Script(parts) => Ok(parts),
    }
}

pub fn write_failure(name: &str, first: &str, second: &Option<String>) {
    write_file(first, &format!("{}.first.js", name));
    if let Some(ref second) = second {
        write_file(second, &format!("{}.second.js", name));
    }
}

fn write_file(content: &str, file_name: &str) {
    let dir = ::std::path::PathBuf::from("test_failures");
    if !dir.exists() {
        ::std::fs::create_dir(&dir).expect("failed to create test_failures");
    }
    let path = dir.join(file_name);
    std::fs::write(path, content).unwrap();
}
