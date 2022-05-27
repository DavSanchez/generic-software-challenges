//! Main executable module.

use std::{io, process::exit};

use toy_search_engine::*;

/// Main function. Greets user and executes the application. Any error during the application run gets printed and the program exits with a non-zero exit code.
fn main() -> io::Result<()> {
    println!("Hello, world!");
    exit(match run_application() {
        Ok(_) => 0,
        Err(err) => {
            eprintln!("error: {}", err);
            1
        }
    });
}
