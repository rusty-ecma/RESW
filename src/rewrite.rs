use std::collections::VecDeque;

use std::char;

pub fn re_write(s: &str, new_quote: char) -> Result<String, String> {
    let old_quote = s.chars().next().ok_or(format!("string cannot be 0 length"))?;
    if old_quote == new_quote {
        return Ok(s.to_string())
    }
    let contents = &s[1..s.len().saturating_sub(1)];
    let mut queue: VecDeque<_> = String::from(contents).chars().collect();
    let mut s = format!("{}", new_quote);
    while let Some(c) = queue.pop_front() {
        if c != '\\' {
            if c == new_quote {
                s.push_str(&format!("\\{}", new_quote));
            } else {
                s.push(c);
            }
            continue;
        }
        if let Some(next) = queue.pop_front() {
            match next {
                'b' =>  s.push_str(r#"\u{0008}"#),
                'f' =>  s.push_str(r#"\u{000C}"#),
                'n' =>  s.push_str(r#"\n"#),
                'r' =>  s.push_str(r#"\r"#),
                't' =>  s.push_str(r#"\t"#),
                '\'' => s.push_str(r#"\'"#),
                '\"' => s.push_str(r#"\""#),
                '\\' => s.push_str(r#"\\"#),
                'u' =>  s.push_str(&unescape_unicode(&mut queue)),
                'x' =>  s.push_str(&unescape_hex(&mut queue)),
                'o' =>  s.push_str(&unescape_octal(&mut queue)),
                c if c == old_quote => s.push(old_quote),
                c => s.push_str(&format!("\\{}", c)),
            }
        } else {
            return Err(format!("invalid escape sequence"))
        }
    }
    s.push(new_quote);
    Ok(s)
}

fn unescape_unicode(queue: &mut VecDeque<char>) -> String {
    let mut s = String::new();
    if let Some(c) = queue.pop_front() {
        if c == '{' {
            s.push(c);
            s.push_str(&remaining_until_close_brace(queue));
        } else if c.is_digit(16) {
            s.push(c);
            for _ in 0..3 {
                if let Some(c)  = queue.pop_front() {
                    s.push(c)
                } else {
                    panic!("Invalid unicode sequence no braces: {}", c);
                }
            }
        } else {
            panic!("Invalid unicode sequence \\u followed by non-hex char {}", c);
        }
    }
    s
}

fn remaining_until_close_brace(queue: &mut VecDeque<char>) -> String {
    let mut s = String::new();
    while let Some(c) = queue.pop_front() {
        s.push(c);
        if c == '}' {
            break;
        }
    }
    s
}

fn unescape_hex(queue: &mut VecDeque<char>) -> String {
    let mut s = String::new();
    while let Some(item) = queue.pop_front() {
        if item.is_digit(16) {
            s.push(item)
        } else {
            break;
        }
    }
    s
}

fn unescape_octal(queue: &mut VecDeque<char>) -> String {
    let mut s = String::new();
    while let Some(item) = queue.pop_front() {
        if item.is_digit(8) {
            s.push(item)
        } else {
            break
        }
    }
    s
}


#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn re_write_test() {
        let one = r#""'""#;
        let r = re_write(one, '\'').unwrap();
        assert_eq!(r, r#"'\''"#);
    }
}