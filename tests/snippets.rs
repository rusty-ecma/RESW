mod common;

#[test]
fn nested_conditional() {
    pretty_env_logger::try_init().ok();
    let js = "let a = (a ? b : c) ? d : e;";
    common::round_trip_validate(js, false, "nested_conditional").unwrap();
}
#[test]
fn new_call_member() {
    pretty_env_logger::try_init().ok();
    let js = "new (c(d).e)();";
    common::round_trip_validate(js, false, "new_call_member").unwrap();
}
#[test]
fn call_conditional() {
    pretty_env_logger::try_init().ok();
    let js = "(a ? b : c)();";
    common::round_trip_validate(js, false, "call_conditional").unwrap();
}
#[test]
fn call_logical() {
    pretty_env_logger::try_init().ok();
    let js = "(a || c)();";
    common::round_trip_validate(js, false, "call_logical").unwrap();
}
#[test]
fn assign_prop_in_arg() {
    pretty_env_logger::try_init().ok();
    let js = "let _ = (a, {b} = {b: 3}) => (a + b);";
    common::round_trip_validate(js, false, "assign_prop_in_arg").unwrap();
}

#[test]
fn for_in_init_in() {
    pretty_env_logger::try_init().ok();
    let js = r#"for(var x=(0 in[])in{});"#;
    common::round_trip_validate(js, false, "for_in_init_in").unwrap();
}

#[test]
fn if_zero_empty() {
    pretty_env_logger::try_init().ok();
    let js = r#"if(0);"#;
    common::round_trip_validate(js, false, "if_zero_empty").unwrap();
}
