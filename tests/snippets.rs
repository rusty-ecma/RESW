

#[test]
fn nested_ternary() {
    let js = "let a = (a ? b : c) ? d : e;";
    let (a, b) =  super::double_round_trip(js, false);
    assert_eq!(a.trim(), js);
    assert_eq!(b.unwrap(), js);
}