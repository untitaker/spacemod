use std::borrow::Cow;
use std::collections::BTreeMap;

use regex::{Regex, RegexBuilder};

use pest::error::Error;
use pest::iterators::Pair;
use pest::Parser;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("{0}")]
    Pest(#[from] Error<parser::Rule>),

    #[error("expected expression, found {0:?}")]
    ExpectedExpression(parser::Rule),

    #[error("{0}")]
    Regex(#[from] regex::Error),

    #[error("parenthesis {open:?} is matched once with {close:?}, once with {close2:?}. This creates ambiguities when parsing files.")]
    MismatchedParenthesis {
        open: char,
        close: char,
        close2: char,
    },
}

mod parser {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "expr.pest"]
    pub struct Parser;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Text(String),
    Open(char, char),
    Close(char, char),
}

impl Token {
    fn as_char(&self) -> Option<char> {
        match self {
            Token::Text(_) => None,
            Token::Open(open, _) => Some(*open),
            Token::Close(_, close) => Some(*close),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expr {
    tokens: Vec<Token>,
}

pub type Pairs = BTreeMap<char, char>;

impl Expr {
    pub fn get_parenthesis_pairs(&self) -> Result<Pairs, ParseError> {
        let mut rv = BTreeMap::new();

        for token in &self.tokens {
            if let Token::Open(open, close) = token {
                let open = *open;
                let close = *close;
                if let Some(close2) = rv.insert(open, close) {
                    if close2 != close {
                        return Err(ParseError::MismatchedParenthesis {
                            open,
                            close,
                            close2,
                        });
                    }
                }
            }
        }

        Ok(rv)
    }

    pub fn parse_expr(input: &str) -> Result<Self, ParseError> {
        use parser::Rule;

        fn handle_expr(pair: Pair<Rule>, tokens: &mut Vec<Token>) -> Result<(), ParseError> {
            match pair.as_rule() {
                Rule::Parens => {
                    let mut inner = pair.into_inner();
                    let open = inner.next().unwrap().as_str().chars().next().unwrap();
                    let mut child_tokens = Vec::new();
                    for child in inner.next().unwrap().into_inner() {
                        handle_expr(child, &mut child_tokens)?;
                    }
                    let close = inner.next().unwrap().as_str().chars().next().unwrap();
                    debug_assert!(inner.next().is_none());

                    tokens.push(Token::Open(open, close));
                    tokens.extend(child_tokens);
                    tokens.push(Token::Close(open, close));
                    Ok(())
                }
                Rule::Regex => {
                    let mut regex = String::new();
                    for token in pair.into_inner() {
                        regex.push_str(token.as_str());
                    }
                    tokens.push(Token::Text(regex));
                    Ok(())
                }
                rule => Err(ParseError::ExpectedExpression(rule)),
            }
        }

        let mut tokens = Vec::new();

        for pair in parser::Parser::parse(parser::Rule::Input, input)?
            .next()
            .unwrap()
            .into_inner()
        {
            handle_expr(pair, &mut tokens)?;
        }

        Ok(Expr { tokens })
    }

    fn regex_string(&self) -> String {
        let mut rv = String::new();

        for token in &self.tokens {
            if !rv.is_empty() {
                rv.push_str("\\s*?");
            }

            match token {
                Token::Text(regex) => rv.push_str(&regex),
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

    pub fn get_replacer(&self, multiline: bool) -> Result<Replacer<'_>, ParseError> {
        let pairs = self.get_parenthesis_pairs()?;
        let regex_string = self.regex_string();
        let regex = RegexBuilder::new(&regex_string)
            .multi_line(!pairs.is_empty() || multiline)
            .dot_matches_new_line(!pairs.is_empty())
            .build()?;

        let reverse_pairs = pairs.iter().map(|(a, b)| (*b, *a)).collect();

        Ok(Replacer {
            expr: self,
            regex,
            pairs,
            reverse_pairs,
        })
    }
}

pub struct Replacer<'a> {
    expr: &'a Expr,
    regex: Regex,
    pairs: Pairs,
    reverse_pairs: Pairs,
}

enum MatchingAction {
    /// Common case: replacer fn did all possible replacements for input ($1),
    /// `replace_overlapping` should chop off $0 bytes from input and continue.
    ContinueAt(usize, String),
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
        let (i, replacement) = match action {
            MatchingAction::ContinueAt(i, replacement) => (i, replacement),
            MatchingAction::RetrySubstring(start, end) => {
                let new_input = &input[..end];
                if let Some(action) = replacer(&new_input) {
                    match action {
                        MatchingAction::ContinueAt(i2, replacement2) => {
                            (i2, replacement2 + &input[end..])
                        }
                        MatchingAction::RetrySubstring(start, _) => (start, input),
                    }
                } else {
                    (start, input)
                }
            }
        };

        let (add_prefix, new_input) = replacement.split_at(i);
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

impl<'a> Replacer<'a> {
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
                .filter_map(|(i, token)| Some((i == self.expr.tokens.len() - 1, token.as_char()?)))
                .peekable();

            let mut extra_stack = Vec::new();

            for (i, c) in match_str.char_indices() {
                let (is_open, counterpart) = if let Some(close) = self.pairs.get(&c) {
                    (true, *close)
                } else if let Some(open) = self.reverse_pairs.get(&c) {
                    (false, *open)
                } else {
                    continue;
                };

                if !is_open && extra_stack.last().cloned() == Some(counterpart) {
                    extra_stack.pop();
                    continue;
                }

                if let Some((is_last_token, c2)) = expr_parens.peek().cloned() {
                    if c2 == c {
                        expr_parens.next();

                        if is_last_token && match_str.len() > i + 1 {
                            return Some(MatchingAction::RetrySubstring(
                                full_match.start(),
                                full_match.start() + i + 1,
                            ));
                        }

                        continue;
                    }
                }

                if is_open || c == counterpart {
                    extra_stack.push(c);
                    continue;
                }

                return Some(MatchingAction::ContinueAt(
                    full_match.start(),
                    input.to_owned(),
                ));
            }

            if !extra_stack.is_empty() || expr_parens.peek().is_some() {
                return Some(MatchingAction::ContinueAt(
                    full_match.start(),
                    input.to_owned(),
                ));
            }

            let mut rv = input[..full_match.start()].to_owned();
            captures.expand(sub, &mut rv);
            rv.push_str(&input[full_match.end()..]);

            Some(MatchingAction::ContinueAt(rv.len(), rv))
        })
    }
}

#[test]
fn test_basic_regex() {
    insta::assert_debug_snapshot!(Expr::parse_expr("foo { [a-zA-Z0-9]+\\ bam } baz"), @r###"
    Ok(
        Expr {
            tokens: [
                Text(
                    "foo",
                ),
                Open(
                    '{',
                    '}',
                ),
                Text(
                    "[a-zA-Z0-9]+ bam",
                ),
                Close(
                    '{',
                    '}',
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
    insta::assert_debug_snapshot!(Expr::parse_expr("foo { { bam } bar { baz } }"), @r###"
    Ok(
        Expr {
            tokens: [
                Text(
                    "foo",
                ),
                Open(
                    '{',
                    '}',
                ),
                Open(
                    '{',
                    '}',
                ),
                Text(
                    "bam",
                ),
                Close(
                    '{',
                    '}',
                ),
                Text(
                    "bar",
                ),
                Open(
                    '{',
                    '}',
                ),
                Text(
                    "baz",
                ),
                Close(
                    '{',
                    '}',
                ),
                Close(
                    '{',
                    '}',
                ),
            ],
        },
    )
    "###);
}

#[cfg(test)]
macro_rules! replacer_test {
    ($input:expr, $search:expr, $replace:expr, @$output:expr) => {{
        let search = Expr::parse_expr($search).unwrap();
        let replacer = search.get_replacer(false).unwrap();
        let mut file = $input.to_owned();

        println!("running replacer");

        loop {
            let new_file = replacer.replace(&*file, $replace);
            if new_file == file {
                break;
            }

            file = new_file.into_owned();
        }

        insta::assert_snapshot!(
            file,
            @$output
        );
    }}
}

#[test]
fn test_simple_string() {
    replacer_test!("foo { bar }", "foo", "xxx", @"xxx { bar }");
}

#[test]
fn test_basic() {
    replacer_test!("foo { bar }", "foo { bar }", "xxx", @"xxx");
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

        @r###"use foo::Bar;

        fn poopy() {
            let outdir = match env::var_os("OUT_DIR") {
                None => return,
                Some(outdir) => outdir,
            };
        }
"###
    );
}

#[test]
fn test_example_good() {
    let file = r#"vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]"#;
    let search = r#"" (.*) " \.to_string ( )"#;
    let replace = r#"String::from("$1")"#;
    replacer_test!(
        file, search, replace,
        @r###"vec![String::from("foo"), String::from("bar"), String::from("baz")]"###
    );
}

#[test]
fn test_example_bad() {
    let file = r#"vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]"#;
    let search = r#""(.*)"\.to_string\(\)"#;
    let replace = r#"String::from("$1")"#;
    replacer_test!(
        file, search, replace,
        @r###"vec![String::from(String::from(String::from("foo"), "bar"), "baz")]"###
    );
}

#[test]
fn test_no_match() {
    let file = r"foo bar baz";
    let search = "hello";
    let replace = "oh no";

    replacer_test!(file, search, replace, @"foo bar baz");
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
    fn foo() {
        self.0.push(format!("process_value({})", state.path()));
        self.0.push(String::from("before_process_child_values"));
        self.0.push(String::from("after_process_child_values"));
    }
    "###
    );
}

#[test]
fn test_regression_extra_parens() {
    let file = r#"pytestmark = pytest.mark.skip(reason="foobar") ; def foo(): pass"#;
    let search = r"pytestmark\s*=\s*pytest.mark.skip ( .* )";
    let replace = "";

    replacer_test!( file, search, replace, @" ; def foo(): pass");
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
    // "foo"
    (String::from("some"))
    "###);
}

#[test]
fn test_nested_parens() {
    let file = r#"str(uuid.uuid4())"#;

    let search = r#"str ( uuid.uuid4 ( ) )"#;
    let replace = r#"uuid.uuid4().hex"#;

    replacer_test!(file, search, replace, @"uuid.uuid4().hex");
}

#[test]
fn test_regex_and_regular_parens() {
    let file = r#"str(uuid.uuid4());"#;

    let search = r#"str\( uuid.uuid4 ( ) \);"#;
    let replace = r#"uuid.uuid4().hex;"#;

    replacer_test!(file, search, replace, @"uuid.uuid4().hex;");
}

#[test]
fn test_regex_and_regular_parens2() {
    let file = r#"str(uuid.uuid4())"#;

    let search = r#"str ( uuid.uuid4\(\) )"#;
    let replace = r#"uuid.uuid4().hex"#;

    replacer_test!(file, search, replace, @"uuid.uuid4().hex");
}

#[test]
fn test_regex_and_regular_parens3() {
    let file = r#"str{uuid.uuid4()};"#;

    let search = r#"str\{ uuid.uuid4 ( ) \};"#;
    let replace = r#"uuid.uuid4().hex;"#;

    replacer_test!(file, search, replace, @"uuid.uuid4().hex;");
}

#[test]
fn test_regex_and_regular_parens4() {
    let file = r#"str{uuid.uuid4()}"#;

    let search = r#"str { uuid.uuid4\(\) }"#;
    let replace = r#"uuid.uuid4().hex"#;

    replacer_test!(file, search, replace, @"uuid.uuid4().hex");
}
