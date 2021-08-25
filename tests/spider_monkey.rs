#![cfg(all(test, feature = "moz_central"))]
use ressa::Parser;
use resw::{write_str::WriteString, Writer};
use std::path::Path;
mod common;

#[test]
fn moz_central() {
    pretty_env_logger::try_init().ok();
    let moz_central_path = Path::new("./moz-central");
    if !moz_central_path.exists() {
        panic!("Please download the spider monkey JIT Test files to run this test (see CONTRIBUTING.md)");
    }
    let msgs = walk(&moz_central_path);
    if !msgs.is_empty() {
        for msg in msgs {
            eprintln!("{}", msg);
        }
        panic!()
    }
}

fn walk(path: &Path) -> Vec<String> {
    let mut ret = vec![];
    let files = path
        .read_dir()
        .unwrap()
        .map(|e| e.unwrap().path())
        .collect::<Vec<_>>();
    for path in files {
        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext == "js" {
                    match run(&path) {
                        Ok(js) => {
                            if let Some(first) = js {
                                match around_once(&first) {
                                    Ok(second) => {
                                        if let Some(msg) =
                                            check_round_trips(&path, &first, &Some(second))
                                        {
                                            ret.push(msg);
                                        }
                                    }
                                    Err(e) => {
                                        ret.push(format!("{}", e));
                                        if let Some(msg) = check_round_trips(&path, &first, &None) {
                                            ret.push(msg)
                                        }
                                    }
                                }
                            }
                        }
                        Err(e) => {
                            let loc = match &e {
                                common::Error::Ressa(
                                    ressa::Error::InvalidGetterParams(ref pos)
                                    | ressa::Error::InvalidSetterParams(ref pos)
                                    | ressa::Error::NonStrictFeatureInStrictContext(ref pos, _)
                                    | ressa::Error::OperationError(ref pos, _)
                                    | ressa::Error::Redecl(ref pos, _)
                                    | ressa::Error::UnableToReinterpret(ref pos, _, _)
                                    | ressa::Error::UnexpectedToken(ref pos, _),
                                ) => {
                                    format!("{}:{}:{}", path.display(), pos.line, pos.column)
                                }
                                _ => format!("{}", path.display()),
                            };
                            log::info!("Failed first pass for\n{}\n{}", loc, e);
                            if let Ok(op) =
                                ::std::process::Command::new("./node_modules/.bin/esparse")
                                    .arg(path)
                                    .output()
                            {
                                if op.status.success() {
                                    ret.push(format!(
                                        "1st pass failure parsed by esprima: {}\n\t{}",
                                        e, loc
                                    ));
                                }
                            }
                        }
                    }
                }
            }
        } else {
            ret.extend(walk(&path))
        }
    }
    ret
}

fn run(file: &Path) -> Result<Option<String>, common::Error> {
    let contents = ::std::fs::read_to_string(file)?;
    if contents.starts_with("// |jit-test| error: SyntaxError")
        || contents.starts_with("|")
        || contents.starts_with("// |jit-test| error:SyntaxError")
    {
        return Ok(None);
    }
    if contents.starts_with("// |jit-test| module") {
        return Ok(None); //these all contain restricted word import as an ident
    }
    let ret = around_once(&contents)?;
    common::round_trip_validate(
        &contents,
        false,
        file.file_stem().unwrap().to_str().unwrap(),
    )?;
    Ok(Some(ret))
}

fn around_once(js: &str) -> Result<String, common::Error> {
    let mut out = WriteString::new();
    let mut writer = Writer::new(out.generate_child());
    for part in Parser::new(&js)? {
        let part = part?;
        writer.write_part(&part).expect("Failed to write part");
    }
    Ok(out
        .get_string()
        .expect("invalid utf8 written to write_string"))
}

fn check_round_trips(path: &Path, first: &str, second: &Option<String>) -> Option<String> {
    let name = path.file_name().unwrap().to_str().unwrap();
    if let Some(ref js) = second {
        if first != js {
            common::write_failure(name, first, second);
            return Some(format!(
                "Double round trip failed for {0}\ncheck ./{1}.first.js and ./{1}.second.js",
                path.display(),
                name
            ));
        }
    } else {
        common::write_failure(name, first, second);
        return Some(format!(
            "Double round trip failed to parse second pass for {}\n chec ./{}.first.js",
            path.display(),
            name
        ));
    }
    None
}
