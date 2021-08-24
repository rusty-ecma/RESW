use super::double_round_trip;

#[test]
fn nested_ternary() {
    pretty_env_logger::try_init().ok();
    let js = "let a = (a ? b : c) ? d : e;";
    let (a, b) = double_round_trip(js, false);
    assert_eq!(a.trim(), js);
    assert_eq!(b.unwrap().trim(), js);
}
