use resast::{Program, ProgramPart};
use ressa::Parser;
use resw::{write_str::WriteString, Writer};

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
    #[error("paniced while parsing: {0}")]
    Panic(String),
}

pub fn round_trip_validate<'a>(js: &'a str, module: bool, name: &str) -> Result<(), Error> {
    pretty_env_logger::try_init().ok();
    round_trip_validate_bare(js, module, name)?;
    round_trip_validate_spanned(js, module, name)
}
pub fn round_trip_validate_bare<'a>(js: &'a str, module: bool, name: &str) -> Result<(), Error> {
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
        let ret = find_mismatched(&first_parts, &second_parts).unwrap();

        if write_failures {
            let to_write = if let Error::Mismatched { left, right } = &ret {
                format!("//{}\n//{}\n\n{}", left, right, second_js)
            } else {
                second_js
            };
            write_failure(name, &to_write, &None);
        }
        return Err(ret);
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
                let ret = find_mismatched(&first_parts, &second_parts).unwrap();
                
                if write_failures {
                    let to_write = if let Error::Mismatched { left, right } = &ret {
                        format!("//{}\n//{}\n\n{}", left, right, second_js)
                    } else {
                        second_js
                    };
                    let name = format!("{}-spanned", name);
                    write_failure(&name, &to_write, &None);
                    write_file(&format!("{:#?}", first_parts), &format!("{}.1.ron", name));
                    write_file(&format!("{:#?}", second_parts), &format!("{}.2.ron", name));
                }
                return Err(ret);
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

pub fn find_mismatched<T>(first_parts: &[T], second_parts: &[T]) -> Option<Error>
where
    T: PartialEq + std::fmt::Debug,
{
    for (lhs, rhs) in first_parts.into_iter().zip(second_parts.into_iter()) {
        if lhs != rhs {
            return Some(Error::Mismatched {
                left: format!("{:?}", lhs),
                right: format!("{:?}", rhs),
            });
        }
    }
    None
}

pub fn parse<'a>(
    p: &'a mut Parser<'a, ressa::DefaultCommentHandler>,
) -> Result<Vec<ProgramPart<'a>>, Error> {
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
