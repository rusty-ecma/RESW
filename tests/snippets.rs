mod common;

#[test]
fn nested_conditional() {
    pretty_env_logger::try_init().ok();
    let js = "let a = (a ? b : c) ? d : e;";
    let (a, b) = common::double_round_trip(js, false);
    assert_eq!(a.trim(), js);
    assert_eq!(b.unwrap().trim(), js);
}
#[test]
fn new_call_member() {
    pretty_env_logger::try_init().ok();
    let js = "new (c(d).e)();";
    let (a, b) = common::double_round_trip(js, false);
    assert_eq!(a.trim(), js);
    assert_eq!(b.unwrap().trim(), js);
}
#[test]
fn call_conditional() {
    pretty_env_logger::try_init().ok();
    let js = "(a ? b : c)();";
    let (a, b) = common::double_round_trip(js, false);
    assert_eq!(a.trim(), js);
    assert_eq!(b.unwrap().trim(), js);
}
#[test]
fn call_logical() {
    pretty_env_logger::try_init().ok();
    let js = "(a || c)();";
    let (a, b) = common::double_round_trip(js, false);
    assert_eq!(a.trim(), js);
    assert_eq!(b.unwrap().trim(), js);
}
#[test]
fn assign_prop_in_arg() {
    pretty_env_logger::try_init().ok();
    let js = "let _ = (a, {b} = {b: 3}) => (a + b);";
    let (a, b) = common::double_round_trip(js, false);
    assert_eq!(a.trim(), js);
    assert_eq!(&a, b.as_ref().unwrap());
}
