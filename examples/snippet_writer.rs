use ressa::{Builder};
use resw::Writer;
use std::fs::{read_to_string, File};
use pretty_env_logger::init;

pub fn main() {
    ::std::env::set_var("RUST_LOG", "resw=trace");
    init();
    let mut args = ::std::env::args();
    let _ = args.next();
    let module = if let Some(v) = args.next() {
        v == "module"
    } else {
        false
    };
    let s = read_to_string("./examples/snippets.js")
                .expect("Couldn't read snippet.js");
    let mut b = Builder::new();
    let p = b.module(module).js(&s).build().expect("Failed to create parser");
    let f = File::create("./examples/snippet.out.js").expect("Failed to create out file");
    let mut w = Writer::builder().quote('\'').build(f);

    for part in p {
        let part = part.expect("Failed to get part");
        w.write_part(&part).expect(&format!("Failed to write {:?}", part));
    }
}