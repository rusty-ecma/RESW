mod common;

#[test]
fn nested_ternary() {
    pretty_env_logger::try_init().ok();
    let js = "let a = (a ? b : c) ? d : e;";
    let (a, b) = common::double_round_trip(js, false);
    assert_eq!(a.trim(), js);
    assert_eq!(b.unwrap().trim(), js);
}
#[test]
fn call_member() {
    pretty_env_logger::try_init().ok();
    let js = "new (c(d).e)();";
    let (a, b) = common::double_round_trip(js, false);
    assert_eq!(a.trim(), js);
    assert_eq!(b.unwrap().trim(), js);
}
