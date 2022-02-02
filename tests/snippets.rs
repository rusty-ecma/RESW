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
fn assign_conditional() {
    pretty_env_logger::try_init().ok();
    let js = "let a = (b = c) ? d : e;";
    common::round_trip_validate(js, false, "assign_conditional").unwrap();
}

#[test]
fn conditional_logical() {
    pretty_env_logger::try_init().ok();
    let js = "let a = (b = c) ? d && e : f == g;";
    common::round_trip_validate(js, false, "conditional_logical").unwrap();
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

#[test]
fn complicated_args1() {
    pretty_env_logger::try_init().ok();
    let js = r#"function f({i = 0}){}"#;
    common::round_trip_validate(js, false, "complicated_args1").unwrap();
}

#[test]
fn complicated_args2() {
    pretty_env_logger::try_init().ok();
    let js = r#"function f({a: b = 0}){}"#;
    common::round_trip_validate(js, false, "complicated_args2").unwrap();
}

#[test]
fn complicated_args3() {
    pretty_env_logger::try_init().ok();
    let js = r#"function f({a}){}"#;
    common::round_trip_validate(js, false, "complicated_args3").unwrap();
}

#[test]
fn new_member_expr_failure() {
    let js =
"function isElement(node) {
  return !!(node &&
    (node.nodeName  // We are a direct element.
    || (node.prop && node.attr && node.find)));  // We have an on and find method part of jQuery API.
}";
    common::round_trip_validate(js, false, "new_member_expr_failure").unwrap();
}

#[test]
fn long_args_failure() {
    let js = "function f(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k){}
";
    common::round_trip_validate(js, false, "long_args_failure").unwrap();
}

#[test]
fn new_callee_logical() {
    let js = "let a = {'b': new (c || d)};";
    common::round_trip_validate(js, false, "new_callee_logical").unwrap();
}

#[test]
fn unary_call() {
    let js = "let a = (void 1)()";
    common::round_trip_validate(js, false, "unary_call").unwrap();
}

#[test]
fn re_match_and_member() {
    common::round_trip_validate(
        "var selectAs =  / as /.test(match[0]) && match[1];",
        false,
        "re_match_and_member",
    )
    .unwrap();
}

#[test]
fn regex() {
    let js = "var x = / as /";
    common::round_trip_validate(js, false, "regex").unwrap();
}

#[test]
fn rest_params() {
    let js = "function a(b, c, ...d) {}";
    common::round_trip_validate(js, false, "rest_params").unwrap();
}

#[test]
fn rest_arg() {
    let js = "Math.max(...a)";
    common::round_trip_validate(js, false, "rest_arg").unwrap();
}

#[test]
fn rest_param_obj() {
    let js = "function a({...b}) {}";
    common::round_trip_validate(js, false, "rest_param_obj").unwrap();
}

#[test]
fn assign_spread() {
    let js = "let x = { ...y };";
    common::round_trip_validate(js, false, "assign_spread").unwrap();
}

#[test]
fn double_break_in_switch_case() {
    let js = "switch(0) {
        case 0:
          break;
          break;
        default: {}
    }";
    common::round_trip_validate(js, false, "double_break_in_switch_case").unwrap();
}
