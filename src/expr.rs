use std::borrow::Cow;
use std::collections::BTreeMap;

use itertools::Itertools;
use regex::{escape, Regex, RegexBuilder};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("{0}")]
    Regex(#[from] regex::Error),

    #[error("parenthesis {open:?} is matched once with {close:?}, once with {close2:?}. This creates ambiguities when parsing files.")]
    MismatchedParenthesis {
        open: String,
        close: String,
        close2: String,
    },

    #[error("The -p parameter's value must contain one space that delimits the open and closing tag. For example, '( )' or '<a> </a>'.")]
    InvalidPairsLength,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Text(String),
    Open(String, String),
    Close(String, String),
}

impl Token {
    fn as_parenthesis(&self) -> Option<&str> {
        match self {
            Token::Text(_) => None,
            Token::Open(open, _) => Some(open),
            Token::Close(_, close) => Some(close),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expr {
    tokens: Vec<Token>,
}

pub type Pairs = BTreeMap<String, String>;

fn default_pairs() -> Pairs {
    let mut rv = Pairs::new();
    rv.insert("{".to_owned(), "}".to_owned());
    rv.insert("(".to_owned(), ")".to_owned());
    rv.insert("[".to_owned(), "]".to_owned());
    rv.insert("<".to_owned(), ">".to_owned());
    rv.insert("\"".to_owned(), "\"".to_owned());
    rv.insert("`".to_owned(), "`".to_owned());
    rv.insert("\"".to_owned(), "\"".to_owned());
    rv
}

pub fn parse_pairs(input: &[String]) -> Result<Pairs, ParseError> {
    let mut rv = Pairs::new();

    for pair in input {
        let (open, close) = pair
            .split(' ')
            .collect_tuple()
            .ok_or(ParseError::InvalidPairsLength)?;
        if let Some(close2) = rv.insert(open.to_owned(), close.to_owned()) {
            if close2 != close {
                return Err(ParseError::MismatchedParenthesis {
                    open: open.to_owned(),
                    close: close.to_owned(),
                    close2,
                });
            }
        }
    }

    Ok(rv)
}

impl Expr {
    pub fn get_used_pairs(&self, user_defined_pairs: Pairs) -> Result<Pairs, ParseError> {
        let mut rv = user_defined_pairs;

        for token in &self.tokens {
            if let Token::Open(open, close) = token {
                if let Some(close2) = rv.insert(open.clone(), close.clone()) {
                    if &close2 != close {
                        return Err(ParseError::MismatchedParenthesis {
                            open: open.clone(),
                            close: close.clone(),
                            close2,
                        });
                    }
                }
            }
        }

        Ok(rv)
    }

    pub fn parse_fixed_string(input: &str) -> Result<Self, ParseError> {
        Ok(Expr {
            tokens: vec![Token::Text(regex::escape(input))],
        })
    }

    pub fn parse_regex(input: &str) -> Result<Self, ParseError> {
        Ok(Expr {
            tokens: vec![Token::Text(input.to_owned())],
        })
    }

    pub fn parse_expr(input: &str, user_defined_pairs: Pairs) -> Result<Self, ParseError> {
        let pairs = {
            let mut rv = default_pairs();
            rv.extend(user_defined_pairs);
            rv
        };

        let mut tokens = vec![Token::Text(String::new())];
        let mut parens_stack = Vec::new();
        let mut escape = false;

        let mut finish_token = |tokens: &mut Vec<Token>| -> Result<(), ParseError> {
            let token = tokens.last_mut().unwrap();

            if let Token::Text(c) = token {
                if parens_stack.last().and_then(|open| pairs.get(open)) == Some(c) {
                    *token = Token::Close(parens_stack.pop().unwrap(), c.clone());
                } else if let Some(close) = pairs.get(&*c) {
                    parens_stack.push(c.clone());
                    *token = Token::Open(c.clone(), close.clone());
                }
            }

            Ok(())
        };

        for c in input.chars() {
            match c {
                '\\' if !escape => escape = true,
                '\n' | '\t' | ' ' if !escape => {
                    finish_token(&mut tokens)?;
                    match tokens.last() {
                        Some(Token::Text(s)) if s.is_empty() => (),
                        _ => tokens.push(Token::Text(String::new())),
                    }
                }
                c => {
                    match tokens.last_mut() {
                        Some(Token::Text(ref mut s)) => {
                            if escape && c != ' ' && c != '\\' {
                                s.push('\\');
                            }
                            s.push(c);
                        }
                        x => panic!("Unexpected last token: {:?}", x),
                    }
                    escape = c == '\\';
                }
            }
        }

        finish_token(&mut tokens)?;

        Ok(Expr { tokens })
    }

    fn regex_string(&self) -> String {
        let mut rv = String::new();

        for token in &self.tokens {
            if !rv.is_empty() {
                rv.push_str("\\s*?");
            }

            match token {
                Token::Text(regex) => rv.push_str(regex),
                Token::Open(open, _) => {
                    rv.push_str(&regex::escape(&open.to_string()));
                }
                Token::Close(_, close) => {
                    rv.push_str(&regex::escape(&close.to_string()));
                }
            }
        }

        rv
    }

    pub fn get_replacer(
        &self,
        multiline: bool,
        user_defined_pairs: Pairs,
    ) -> Result<Replacer<'_>, ParseError> {
        let pairs = self.get_used_pairs(user_defined_pairs)?;
        let regex_string = self.regex_string();
        let regex = RegexBuilder::new(&regex_string)
            .multi_line(!pairs.is_empty() || multiline)
            .dot_matches_new_line(!pairs.is_empty() || multiline)
            .build()?;

        let reverse_pairs = pairs.iter().map(|(a, b)| (b.clone(), a.clone())).collect();
        let pairs_re = RegexBuilder::new(&format!(
            "({})",
            pairs
                .iter()
                .map(|(a, b)| format!("{}|{}", escape(a), escape(b)))
                .join("|")
        ))
        .multi_line(true)
        .build()?;

        Ok(Replacer {
            expr: self,
            regex,
            pairs,
            reverse_pairs,
            pairs_re,
        })
    }
}

pub struct Replacer<'a> {
    expr: &'a Expr,
    regex: Regex,
    pairs: Pairs,
    reverse_pairs: Pairs,
    pairs_re: Regex,
}

enum MatchingAction {
    /// One match has been replaced.
    Replaced(String),
    /// A match has been found, but the parenthesis are mismatching. `replace_overlapping` should
    /// chop off $0 bytes from input and continue.
    JumpForward(usize),
    /// The replacer fn found a potential match but consumed all parenthesis of the expr before the
    /// capture group ended. The procedure is retried (once) using the provided range:
    /// `&input[$0..$1]`
    RetrySubstring(usize, usize),
}

/// Try and apply the replacement function on various substrings of input. This is a workaround for
/// our regex engine not supporting overlapping matches.
fn replace_overlapping(
    input: &str,
    mut replacer: impl FnMut(&str) -> Option<MatchingAction>,
) -> Cow<'_, str> {
    let mut input = input.to_owned();
    let mut prefix = String::new();

    while let Some(action) = replacer(&input) {
        let i = match action {
            MatchingAction::Replaced(replacement) => {
                input = replacement;
                break;
            }
            MatchingAction::JumpForward(i) => i,
            MatchingAction::RetrySubstring(start, end) => {
                let new_input = &input[..end];
                if let Some(action) = replacer(new_input) {
                    match action {
                        MatchingAction::Replaced(replacement2) => {
                            input = replacement2 + &input[end..];
                            break;
                        }
                        MatchingAction::JumpForward(i2) => i2,
                        MatchingAction::RetrySubstring(start, _) => start,
                    }
                } else {
                    start
                }
            }
        };

        let (add_prefix, new_input) = input.split_at(i);
        prefix.push_str(add_prefix);
        input = new_input.to_owned();

        if input.is_empty() {
            break;
        }
        let c = input.chars().next().unwrap();
        prefix.push(c);
        input = input[c.len_utf8()..].to_owned();
    }

    prefix.push_str(&input);
    prefix.into()
}

impl Replacer<'_> {
    pub fn prefilter_matches(&self, input: &str) -> bool {
        self.regex.is_match(input)
    }

    pub fn replace<'t>(&self, input: &'t str, sub: &str) -> Cow<'t, str> {
        replace_overlapping(input, |input| {
            let captures = self.regex.captures(input)?;

            let full_match = captures.get(0).unwrap();
            let match_str = &input[full_match.start()..full_match.end()];

            let mut expr_parens = self
                .expr
                .tokens
                .iter()
                .enumerate()
                .filter_map(|(i, token)| {
                    Some((i == self.expr.tokens.len() - 1, token.as_parenthesis()?))
                })
                .peekable();

            let mut extra_stack = Vec::new();

            for re_match in self.pairs_re.find_iter(match_str) {
                let i = re_match.end();
                let c = re_match.as_str();

                let (is_open, counterpart) = if let Some(close) = self.pairs.get(c) {
                    (true, close.clone())
                } else if let Some(open) = self.reverse_pairs.get(c) {
                    (false, open.clone())
                } else {
                    continue;
                };

                if !is_open && extra_stack.last() == Some(&counterpart) {
                    extra_stack.pop();
                    continue;
                }

                if let Some((is_last_token, c2)) = expr_parens.peek().cloned() {
                    if c2 == c {
                        expr_parens.next();

                        if is_last_token && match_str.len() > i + 1 {
                            return Some(MatchingAction::RetrySubstring(
                                full_match.start(),
                                full_match.start() + i,
                            ));
                        }

                        continue;
                    }
                }

                if is_open || c == counterpart {
                    if expr_parens.peek().is_none() {
                        return Some(MatchingAction::RetrySubstring(
                            full_match.start(),
                            full_match.start() + i,
                        ));
                    }

                    extra_stack.push(c.to_owned());
                    continue;
                }

                return Some(MatchingAction::JumpForward(full_match.start()));
            }

            if !extra_stack.is_empty() || expr_parens.peek().is_some() {
                return Some(MatchingAction::JumpForward(full_match.start()));
            }

            let mut rv = input[..full_match.start()].to_owned();
            captures.expand(sub, &mut rv);
            rv.push_str(&input[full_match.end()..]);

            Some(MatchingAction::Replaced(rv))
        })
    }
}

#[test]
fn test_basic_regex() {
    insta::assert_debug_snapshot!(Expr::parse_expr(r#"foo { [a-zA-Z0-9]+\ bam } baz"#, Default::default()), @r###"
    Ok(
        Expr {
            tokens: [
                Text(
                    "foo",
                ),
                Open(
                    "{",
                    "}",
                ),
                Text(
                    "[a-zA-Z0-9]+ bam",
                ),
                Close(
                    "{",
                    "}",
                ),
                Text(
                    "baz",
                ),
            ],
        },
    )
    "###);
}

#[test]
fn test_basic_regex_with_backslash() {
    insta::assert_debug_snapshot!(Expr::parse_expr(r#"foo { [a-zA-Z0-9]+\\ bam } baz"#, Default::default()), @r###"
    Ok(
        Expr {
            tokens: [
                Text(
                    "foo",
                ),
                Open(
                    "{",
                    "}",
                ),
                Text(
                    "[a-zA-Z0-9]+\\ bam",
                ),
                Close(
                    "{",
                    "}",
                ),
                Text(
                    "baz",
                ),
            ],
        },
    )
    "###);
}

#[test]
fn test_nested_expr() {
    insta::assert_debug_snapshot!(Expr::parse_expr("foo { { bam } bar { baz } }", Default::default()), @r###"
    Ok(
        Expr {
            tokens: [
                Text(
                    "foo",
                ),
                Open(
                    "{",
                    "}",
                ),
                Open(
                    "{",
                    "}",
                ),
                Text(
                    "bam",
                ),
                Close(
                    "{",
                    "}",
                ),
                Text(
                    "bar",
                ),
                Open(
                    "{",
                    "}",
                ),
                Text(
                    "baz",
                ),
                Close(
                    "{",
                    "}",
                ),
                Close(
                    "{",
                    "}",
                ),
            ],
        },
    )
    "###);
}

#[cfg(test)]
macro_rules! replacer_test {
    ($input:expr, $search:expr, $replace:expr $(, $open:expr => $close:expr)*, @$output:expr) => {{
        #[allow(unused_mut)]
        let mut pairs = Pairs::new();
        $(
            pairs.insert($open.to_owned(), $close.to_owned());
        )*

        let search = Expr::parse_expr($search, pairs.clone()).unwrap();
        let replacer = search.get_replacer(false, pairs).unwrap();
        let mut file = $input.to_owned();

        println!("running replacer");

        let mut steps = 0;

        loop {
            assert!(steps < 10);
            steps += 1;
            let new_file = replacer.replace(&*file, $replace);
            if new_file == file {
                break;
            }

            file = new_file.into_owned();
        }

        insta::assert_snapshot!(
            format!("// spacemod: steps_taken={steps}\n{file}"),
            @$output
        );
    }}
}

#[test]
fn test_simple_string() {
    replacer_test!("foo { bar }", "foo", "xxx", @r###"
    // spacemod: steps_taken=2
    xxx { bar }
    "###);
}

#[test]
fn test_basic() {
    replacer_test!("foo { bar }", "foo { bar }", "xxx", @r###"
    // spacemod: steps_taken=2
    xxx
    "###);
}

#[test]
fn test_xml_without_balancing() {
    replacer_test!(
        "<div><div>hello world</div></div>",
        "([^^])<div>(.*)</div>",
        "$1<span>$2</span>",
        @r###"
    // spacemod: steps_taken=2
    <div><span>hello world</div></span>
    "###);
}

#[test]
fn test_xml_without_parens() {
    replacer_test!(
        "<div><div>hello world</div></div>",
        "([^^]) <div> (.*) </div>",
        "$1<span>$2</span>",
        @r###"
    // spacemod: steps_taken=2
    <div><span>hello world</div></span>
    "###);
}

#[test]
fn test_xml_balanced() {
    replacer_test!(
        "<div><div>hello world</div></div>",
        "([^^]) <div> (.*) </div>",
        "$1<span>$2</span>",
        "<div>" => "</div>",
        @r###"
    // spacemod: steps_taken=2
    <div><span>hello world</span></div>
    "###
    );
}

#[test]
fn test_relay_code() {
    replacer_test!(
        r###"use foo::Bar;

        fn main() {
            let outdir = match env::var_os("OUT_DIR") {
                None => return,
                Some(outdir) => outdir,
            };
        }
"###,

        "fn main ( ) { (.*) }",
        "fn poopy() {$1}",

        @r###"
    // spacemod: steps_taken=2
    use foo::Bar;

            fn poopy() {
                let outdir = match env::var_os("OUT_DIR") {
                    None => return,
                    Some(outdir) => outdir,
                };
            }
    "###);
}

#[test]
fn test_example_good() {
    let file = r#"vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]"#;
    let search = r#"" (.*) " \.to_string ( )"#;
    let replace = r#"String::from("$1")"#;
    replacer_test!(
        file, search, replace,
        @r###"
    // spacemod: steps_taken=4
    vec![String::from("foo"), String::from("bar"), String::from("baz")]
    "###);
}

#[test]
fn test_example_bad() {
    let file = r#"vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]"#;
    let search = r#""(.*)"\.to_string\(\)"#;
    let replace = r#"String::from("$1")"#;
    replacer_test!(
        file, search, replace,
        @r###"
    // spacemod: steps_taken=4
    vec![String::from(String::from(String::from("foo"), "bar"), "baz")]
    "###);
}

#[test]
fn test_no_match() {
    let file = r"foo bar baz";
    let search = "hello";
    let replace = "oh no";

    replacer_test!(file, search, replace, @r###"
    // spacemod: steps_taken=1
    foo bar baz
    "###);
}

#[test]
fn test_regression1() {
    let file = r###"fn foo() {
    self.0.push(format!("process_value({})", state.path()));
    self.0.push("before_process_child_values".to_string());
    self.0.push("after_process_child_values".to_string());
}"###;
    let search = r#"" (.*) " \.to_string ( )"#;
    let replace = r#"String::from("$1")"#;

    replacer_test!(
        file, search, replace,
        @r###"
    // spacemod: steps_taken=3
    fn foo() {
        self.0.push(format!("process_value({})", state.path()));
        self.0.push(String::from("before_process_child_values"));
        self.0.push(String::from("after_process_child_values"));
    }
    "###);
}

#[test]
fn test_regression_extra_parens() {
    let file = r#"pytestmark = pytest.mark.skip(reason="foobar") ; def foo(): pass"#;
    let search = r"pytestmark\s*=\s*pytest.mark.skip ( .* )";
    let replace = "";

    replacer_test!( file, search, replace, @r###"
    // spacemod: steps_taken=2
     ; def foo(): pass
    "###);
}

#[test]
fn test_garbage_string() {
    let file = r###""do not ,./<>?!@#$%^&*())'ßtrip'".to_string(),
Foo(Bar(Baz("foo".to_string()))),
);
map.insert("###;
    let search = r#"" (.*) " \.to_string ( )"#;
    let replace = r#"String::from("$1")"#;

    replacer_test!(
    file, search, replace, @r###"
    // spacemod: steps_taken=2
    "do not ,./<>?!@#$%^&*())'ßtrip'".to_string(),
    Foo(Bar(Baz(String::from("foo")))),
    );
    map.insert(
    "###);
}

#[test]
fn test_remaining_expr_parens() {
    let file = r#"// "foo"
("some".to_string())"#;

    let search = r#"" (.*) " \.to_string ( )"#;
    let replace = r#"String::from("$1")"#;

    replacer_test!(file, search, replace, @r###"
    // spacemod: steps_taken=2
    // "foo"
    (String::from("some"))
    "###);
}

#[test]
fn test_nested_parens() {
    let file = r#"str(uuid.uuid4())"#;

    let search = r#"str ( uuid.uuid4 ( ) )"#;
    let replace = r#"uuid.uuid4().hex"#;

    replacer_test!(file, search, replace, @r###"
    // spacemod: steps_taken=2
    uuid.uuid4().hex
    "###);
}

#[test]
fn test_regex_and_regular_parens() {
    let file = r#"str(uuid.uuid4());"#;

    let search = r#"str\( uuid.uuid4 ( ) \);"#;
    let replace = r#"uuid.uuid4().hex;"#;

    replacer_test!(file, search, replace, @r###"
    // spacemod: steps_taken=2
    uuid.uuid4().hex;
    "###);
}

#[test]
fn test_regex_and_regular_parens2() {
    let file = r#"str(uuid.uuid4())"#;

    let search = r#"str ( uuid.uuid4\(\) )"#;
    let replace = r#"uuid.uuid4().hex"#;

    replacer_test!(file, search, replace, @r###"
    // spacemod: steps_taken=2
    uuid.uuid4().hex
    "###);
}

#[test]
fn test_regex_and_regular_parens3() {
    let file = r#"str{uuid.uuid4()};"#;

    let search = r#"str\{ uuid.uuid4 ( ) \};"#;
    let replace = r#"uuid.uuid4().hex;"#;

    replacer_test!(file, search, replace, @r###"
    // spacemod: steps_taken=2
    uuid.uuid4().hex;
    "###);
}

#[test]
fn test_regex_and_regular_parens4() {
    let file = r#"str{uuid.uuid4()}"#;

    let search = r#"str { uuid.uuid4\(\) }"#;
    let replace = r#"uuid.uuid4().hex"#;

    replacer_test!(file, search, replace, @r###"
    // spacemod: steps_taken=2
    uuid.uuid4().hex
    "###);
}

#[test]
fn test_html5gum() {
    let file = r#"
    unread_char(c);
    continue
    emitter();
    continue
    "#;

    let search = r#"unread_char ( (.+) ) ; continue"#;
    let replace = r#"reconsume_in!($1)"#;

    replacer_test!(file, search, replace, @r###"
    // spacemod: steps_taken=2

        reconsume_in!(c)
        emitter();
        continue
        
    "###);
}

#[test]
fn test_copy_file() {
    let file = r#"
    copy_file(to=from, from=to)

    copy_file(
        to=get_file(filepath, mode),
        from=get_file_writer(other_filepath, other_mode)
    )
    "#;

    let search = r#"copy_file ( to= (.*) , from= (.*) )"#;
    let replace = r#"copy_file($2, $1)"#;

    replacer_test!(file, search, replace, @r###"
    // spacemod: steps_taken=3

        copy_file(to, from)

        copy_file(get_file_writer(other_filepath, other_mode)
        , get_file(filepath, mode))
        
    "###);
}

#[test]
fn test_commas() {
    insta::assert_debug_snapshot!(Expr::parse_expr(r#""request_data": json.dumps ( { "options": self.options } ) ,"#, Default::default()), @r###"
    Ok(
        Expr {
            tokens: [
                Text(
                    "\"request_data\":",
                ),
                Text(
                    "json.dumps",
                ),
                Open(
                    "(",
                    ")",
                ),
                Open(
                    "{",
                    "}",
                ),
                Text(
                    "\"options\":",
                ),
                Text(
                    "self.options",
                ),
                Close(
                    "{",
                    "}",
                ),
                Close(
                    "(",
                    ")",
                ),
                Text(
                    ",",
                ),
            ],
        },
    )
    "###);
}

#[test]
fn test_multi_whitespace() {
    insta::assert_debug_snapshot!(Expr::parse_expr("{ \t\n{ foo } }", Default::default()), @r###"
    Ok(
        Expr {
            tokens: [
                Open(
                    "{",
                    "}",
                ),
                Open(
                    "{",
                    "}",
                ),
                Text(
                    "foo",
                ),
                Close(
                    "{",
                    "}",
                ),
                Close(
                    "{",
                    "}",
                ),
            ],
        },
    )
    "###);
}

#[test]
fn test_regression_parens() {
    insta::assert_debug_snapshot!(Expr::parse_expr(r#"self.create_user ( " (.*) " (.*) )"#, Default::default()), @r###"
    Ok(
        Expr {
            tokens: [
                Text(
                    "self.create_user",
                ),
                Open(
                    "(",
                    ")",
                ),
                Open(
                    "\"",
                    "\"",
                ),
                Text(
                    "(.*)",
                ),
                Close(
                    "\"",
                    "\"",
                ),
                Text(
                    "(.*)",
                ),
                Close(
                    "(",
                    ")",
                ),
            ],
        },
    )
    "###);
}

#[test]
fn test_parse_xml() {
    let mut pairs = Pairs::new();
    pairs.insert("<div>".to_owned(), "</div>".to_owned());
    insta::assert_debug_snapshot!(Expr::parse_expr(r#"<div> foo </div>"#, pairs), @r###"
    Ok(
        Expr {
            tokens: [
                Open(
                    "<div>",
                    "</div>",
                ),
                Text(
                    "foo",
                ),
                Close(
                    "<div>",
                    "</div>",
                ),
            ],
        },
    )
    "###);
}
