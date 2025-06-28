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

    1 diffs applied (1 automatically)
    0 diffs rejected (0 automatically)
    0 diffs undone

    ----- stderr -----
    "###);

    assert_snapshot!(read_to_string(harness.join("content"))?, @"hello lard");
    Ok(())
}

#[test]
fn test_parenthesis() -> Result<(), Error> {
    let harness = setup()?;

    write(harness.join("content"), "hello ( world ) lard )")?;

    assert_cmd!(harness, spacemod "--accept-all" "-S" "hello ( (.*) )" "goodbye", @r###"
    success: true
    exit_code: 0
    ----- stdout -----
    Automatically changed ./content

    1 diffs applied (1 automatically)
    0 diffs rejected (0 automatically)
    0 diffs undone

    ----- stderr -----
    "###);

    assert_snapshot!(read_to_string(harness.join("content"))?, @"goodbye lard )");

    Ok(())
}

#[test]
fn test_no_parenthesis() -> Result<(), Error> {
    let harness = setup()?;

    write(harness.join("content"), "hello ( world ) lard )")?;

    assert_cmd!(harness, spacemod "--accept-all" "hello ( (.*) )" "goodbye", @r###"
    success: true
    exit_code: 0
    ----- stdout -----

    0 diffs applied (0 automatically)
    0 diffs rejected (0 automatically)
    0 diffs undone

    ----- stderr -----
    "###);

    assert_snapshot!(read_to_string(harness.join("content"))?, @"hello ( world ) lard )");

    Ok(())
}

#[test]
fn test_fixed_string() -> Result<(), Error> {
    let harness = setup()?;

    write(harness.join("content"), "hello ( (.*) ) lard )")?;

    assert_cmd!(harness, spacemod "--accept-all" "-F" "hello ( (.*) )" "goodbye", @r###"
    success: true
    exit_code: 0
    ----- stdout -----
    Automatically changed ./content

    1 diffs applied (1 automatically)
    0 diffs rejected (0 automatically)
    0 diffs undone

    ----- stderr -----
    "###);

    assert_snapshot!(read_to_string(harness.join("content"))?, @"goodbye lard )");

    Ok(())
}

#[test]
fn test_file_size_limit() -> Result<(), Error> {
    let harness = setup()?;

    let large_content = "x".repeat(6 * 1024 * 1024); // 6MB
    write(harness.join("large"), &large_content)?;

    assert_cmd!(harness, spacemod "--accept-all" "x" "y" "large", @r###"
    success: false
    exit_code: 1
    ----- stdout -----

    ----- stderr -----
    Error: File large is 6MB, exceeding limit of 5MB. Use --max-file-size to increase or 0 to disable.
    "###);

    write(harness.join("small"), "hello world")?;
    assert_cmd!(harness, spacemod "--accept-all" "--max-file-size" "10" "world" "universe" "small", @r###"
    success: true
    exit_code: 0
    ----- stdout -----
    Automatically changed small

    1 diffs applied (1 automatically)
    0 diffs rejected (0 automatically)
    0 diffs undone

    ----- stderr -----
    "###);

    assert_cmd!(harness, spacemod "--accept-all" "--max-file-size" "0" "hello" "hi" "small", @r###"
    success: true
    exit_code: 0
    ----- stdout -----
    Automatically changed small

    1 diffs applied (1 automatically)
    0 diffs rejected (0 automatically)
    0 diffs undone

    ----- stderr -----
    "###);

    Ok(())
}
