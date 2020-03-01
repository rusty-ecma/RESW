#![cfg(all(test, feature = "moz_central"))]
use resw::{
    Writer,
    write_str::WriteString, 
};
use std::path::Path;
use ressa::{
    Parser,
    Error
};

#[test]
fn moz_central() {
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
    let files = path.read_dir().unwrap()
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
                                            if let Some(msg) = check_round_trips(&path, &first, &Some(second)) {
                                                ret.push(msg);
                                            }
                                        },
                                        Err(e) => {
                                            ret.push(format!("{}", e));
                                            if let Some(msg) = check_round_trips(&path, &first, &None) {
                                                ret.push(msg)
                                            }
                                        }
                                    }
                                }
                            },
                            Err(e) => {
                                let loc = match &e {
                                    Error::InvalidGetterParams(ref pos)
                                    | Error::InvalidSetterParams(ref pos)
                                    | Error::NonStrictFeatureInStrictContext(ref pos, _)
                                    | Error::OperationError(ref pos, _)
                                    | Error::Redecl(ref pos, _)
                                    | Error::UnableToReinterpret(ref pos, _, _)
                                    | Error::UnexpectedToken(ref pos, _) => format!("{}:{}:{}", path.display(), pos.line, pos.column),
                                    _ => format!("{}", path.display()),
                                };
                                if let Ok(op) = ::std::process::Command::new("./node_modules/.bin/esparse").arg(path).output() {
                                    if op.status.success() {
                                        // eprintln!("possible new whitelist item:\n\t{}", path.display());
                                        ret.push(format!("1st pass failure parsed by esprima: {}\n\t{}", e, loc));
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                ret.extend(
                    walk(&path)
                )
            }
    }
    ret
}

fn run(file: &Path) -> Result<Option<String>, Error> {
    let contents = ::std::fs::read_to_string(file)?;
    if contents.starts_with("// |jit-test| error: SyntaxError")
        || contents.starts_with("|")
        || contents.starts_with("// |jit-test| error:SyntaxError") {
        return Ok(None);
    }
    if contents.starts_with("// |jit-test| module") {
        return Ok(None); //these all contain restricted word import as an ident
    }
    let ret = around_once(&contents)?;
    Ok(Some(ret))
}

fn around_once(js: &str) -> Result<String, Error> {
    let mut out = WriteString::new();
    let mut writer = Writer::new(out.generate_child());
    for part in Parser::new(&js)? {
        let part = part?;
        writer.write_part(&part.as_concrete()).expect("Failed to write part");
    }
    Ok(out.get_string().expect("invalid utf8 written to write_string"))
}

fn check_round_trips(path: &Path, first: &str, second: &Option<String>) -> Option<String> {
    let name = path.file_name().unwrap().to_str().unwrap();
    if let Some(ref js) = second {
        if first != js {
            write_failure(name, first, second);
            return Some(format!("Double round trip failed for {0}\ncheck ./{1}.first.js and ./{1}.second.js", path.display(), name));
        }
    } else {
        write_failure(name, first, second);
        return Some(format!("Double round trip failed to parse second pass for {}\n chec ./{}.first.js", path.display(), name));
    }
    None
}
fn write_failure(name: &str, first: &str, second: &Option<String>) {
    use std::io::Write;
    let dir = ::std::path::PathBuf::from("test_failures");
    if !dir.exists() {
        ::std::fs::create_dir(&dir).expect("failed to create test_failures");
    }
    let mut f1 = ::std::fs::File::create(dir.join(&format!("{}.first.js", name))).expect("Failed to create first failure file");
    f1.write(format!("//{}\n", name).as_bytes()).expect("Failed to write first line");
    f1.write_all(first.as_bytes()).expect("failed to write to first failure file");
    if let Some(ref second) = second {
        let mut f2 = ::std::fs::File::create(dir.join(&format!("{}.second.js", name))).expect("Failed to create second failure file");
        f2.write(format!("//{}\n", name).as_bytes()).expect("Failed to write first line");
        f2.write_all(second.as_bytes()).expect("failed to write second failure file");
    }
}