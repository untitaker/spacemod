use std::borrow::Cow;
use std::collections::BTreeMap;

use regex::{Captures, Regex, RegexBuilder};

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
                    return Err(ParseError::MismatchedParenthesis {
                        open,
                        close,
                        close2,
                    });
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

    pub fn get_replacer(&self) -> Result<Replacer<'_>, ParseError> {
        let pairs = self.get_parenthesis_pairs()?;
        let regex_string = self.regex_string();
        let regex = RegexBuilder::new(&regex_string)
            .multi_line(!pairs.is_empty())
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

enum ReplaceResult<'t> {
    /// The underlying regex engine returned no matches, there is no point in trimming
    NoMatches,
    /// The underlying regex engine returned matches but they have mismatching parenthesis
    NoReplacements,
    /// Something got replaced, don't continue trimming
    Replaced(Cow<'t, str>),
}

/// Try and apply the replacement function on various substrings of input. This is a workaround for
/// our regex engine not supporting overlapping matches.
fn replace_substrings<'t>(
    mut input: &'t str,
    mut replacer: impl FnMut(&'t str) -> ReplaceResult<'t>,
) -> Cow<'t, str> {
    let orig_input = input;
    let mut suffix = String::new();
    loop {
        match replacer(input) {
            ReplaceResult::NoMatches => break,
            ReplaceResult::NoReplacements => {
                let popped_suffix = input.chars().rev().next().unwrap();
                suffix.push(popped_suffix);
                input = &input[..input.len() - popped_suffix.len_utf8()];
            }
            ReplaceResult::Replaced(rv) => {
                let mut rv = rv.into_owned();
                for c in suffix.chars().rev() {
                    rv.push(c);
                }

                return Cow::Owned(rv);
            }
        }
    }

    input = orig_input;
    let mut prefix = suffix;
    prefix.clear();

    loop {
        match replacer(input) {
            ReplaceResult::NoMatches => break,
            ReplaceResult::NoReplacements => {
                let popped_prefix = input.chars().next().unwrap();
                prefix.push(popped_prefix);
                input = &input[popped_prefix.len_utf8()..];
            }
            ReplaceResult::Replaced(rv) => {
                for c in rv.chars() {
                    prefix.push(c);
                }

                return Cow::Owned(prefix);
            }
        }
    }

    input.into()
}

impl<'a> Replacer<'a> {
    pub fn replace<'t>(&self, input: &'t str, sub: &str) -> Cow<'t, str> {
        replace_substrings(input, |input| {
            let mut potential_matches = false;
            let mut replaced_anything = false;

            let rv = self.regex.replace(input, |captures: &Captures<'_>| {
                potential_matches = true;

                let full_match = captures.get(0).unwrap();
                let match_str = &input[full_match.start()..full_match.end()];

                let mut expr_parens = self
                    .expr
                    .tokens
                    .iter()
                    .filter_map(Token::as_char)
                    .peekable();
                let mut extra_stack = Vec::new();

                for c in match_str.chars() {
                    let (is_open, counterpart) = if let Some(close) = self.pairs.get(&c) {
                        (true, *close)
                    } else if let Some(open) = self.reverse_pairs.get(&c) {
                        (false, *open)
                    } else {
                        continue;
                    };

                    if expr_parens.peek().cloned() == Some(c) {
                        expr_parens.next();
                        continue;
                    }

                    if !is_open && extra_stack.last().cloned() == Some(counterpart) {
                        extra_stack.pop();
                    } else if is_open || c == counterpart {
                        extra_stack.push(c);
                    } else {
                        return match_str.to_owned();
                    }
                }

                if expr_parens.peek().is_some() || !extra_stack.is_empty() {
                    return match_str.to_owned();
                }

                replaced_anything = true;

                let mut rv = String::new();
                captures.expand(sub, &mut rv);
                rv
            });

            if replaced_anything {
                ReplaceResult::Replaced(rv)
            } else if potential_matches {
                ReplaceResult::NoReplacements
            } else {
                ReplaceResult::NoMatches
            }
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
        let replacer = search.get_replacer().unwrap();

        insta::assert_snapshot!(
            replacer.replace($input, $replace),
            @$output
        );

        $output
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
    let mut file = r#"vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]"#;
    let search = r#"" (.*) " \.to_string ( )"#;
    let replace = r#"String::from("$1")"#;
    file = replacer_test!(
        file, search, replace,
        @r###"vec![String::from("foo"), "bar".to_string(), "baz".to_string()]"###
    );

    file = replacer_test!(
        file, search, replace,
        @r###"vec![String::from("foo"), "bar".to_string(), String::from("baz")]"###
    );

    replacer_test!(
        file, search, replace,
        @r###"vec![String::from("foo"), String::from("bar"), String::from("baz")]"###
    );
}

#[test]
fn test_example_bad() {
    let mut file = r#"vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]"#;
    let search = r#""(.*)"\.to_string\(\)"#;
    let replace = r#"String::from("$1")"#;
    file = replacer_test!(
        file, search, replace,
        @r###"vec![String::from("foo".to_string(), "bar".to_string(), "baz")]"###
    );

    file = replacer_test!(
        file, search, replace,
        @r###"vec![String::from(String::from("foo".to_string(), "bar"), "baz")]"###
    );

    replacer_test!(
        file, search, replace,
        @r###"vec![String::from(String::from(String::from("foo"), "bar"), "baz")]"###
    );
}
