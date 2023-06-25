use std::fs::{read_to_string, write};

use anyhow::Error;

mod acceptance_helpers;
use acceptance_helpers::{assert_cmd, setup};
use insta::assert_snapshot;

#[test]
fn test_basic() -> Result<(), Error> {
    let harness = setup()?;

    write(harness.join("content"), "hello world")?;

    assert_cmd!(harness, spacemod "--accept-all" "world" "lard", @r###"
    success: true
    exit_code: 0
    ----- stdout -----
    Automatically changed ./content

    ----- stderr -----
    "###);

    assert_snapshot!(read_to_string(harness.join("content"))?, @"hello lard");
    Ok(())
}
