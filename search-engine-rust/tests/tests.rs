//! Integration tests. Call and interact with the command line interface.

use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn command_without_path() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("toy-search-engine")?;
    cmd.write_stdin(":quit")
        .assert()
        .stdout(predicate::str::contains("Hello, world!"))
        .stdout(predicate::str::contains("Path used: ."))
        .stdout(predicate::str::contains("search> "))
        .success();

    Ok(())
}

#[test]
fn command_with_path() -> Result<(), Box<dyn std::error::Error>> {
    let dir = "tests/assets";
    let mut cmd = Command::cargo_bin("toy-search-engine")?;
    cmd.arg(dir);
    cmd.write_stdin(":quit")
        .assert()
        .stdout("Hello, world!\nPath used: tests/assets\n4 file(s) read in directory\nsearch> ")
        .success();

    Ok(())
}

#[test]
fn no_readable_files() -> Result<(), Box<dyn std::error::Error>> {
    let dir = assert_fs::TempDir::new()?;
    let mut cmd = Command::cargo_bin("toy-search-engine")?;
    cmd.arg(dir.path());
    cmd.assert()
        .stderr("error: no text files found\n")
        .failure()
        .code(1);

    Ok(())
}

#[test]
fn pass_query() -> Result<(), Box<dyn std::error::Error>> {
    let dir = "tests/assets";
    let mut cmd = Command::cargo_bin("toy-search-engine")?;
    cmd.arg(dir);
    cmd.write_stdin("to be dreary\n:quit")
        .assert()
        .stdout(predicate::str::contains(
            "tests/assets/dickinson-I'm nobody! who are you?.txt:100%",
        ))
        .stdout(predicate::str::contains(
            "tests/assets/yeats-the arrow.txt:66%",
        ))
        .stdout(predicate::str::contains(
            "tests/assets/blake-the tyger.txt:33%",
        ))
        .stdout(predicate::str::contains(
            "tests/assets/yeats-an irish airman foresees his death.txt:33%",
        ))
        .success();

    Ok(())
}
