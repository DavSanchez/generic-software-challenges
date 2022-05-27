//! Main library. The entry point of the application (`main.rs`) calls a single function from this module.

use std::{
    collections::HashMap,
    env, fs,
    io::{self, Error, ErrorKind, Write},
    path::{Path, PathBuf},
};

mod app_command;
mod file_ranking;
mod inverted_index;
use crate::{
    app_command::{parse_input, AppCommand},
    file_ranking::print_ranking,
    inverted_index::build_inverted_index,
};

/// Main entry point of the application.
/// Evaluates the command line arguments and calls the appropriate functions. In this case:
/// 1. Gets all the files in the directory specified by the first argument (additional ones are ignored). If no directory is specified, the current working directory is used.
/// 2. If files were read, builds the inverted index from these files.
/// 3. Enters a loop that prompts the user for commands.
/// 4. Calls the appropriate function depending on the command entered by the user.
/// 5. Exits the loop when the user enters `:quit`
/// An error is printed and the program exits with a non-zero exit code if an error occurs.
pub fn run_application() -> io::Result<()> {
    let path = env::args().nth(1).unwrap_or_else(|| ".".to_string());
    println!("Path used: {path}");

    let files = get_text_files(Path::new(&path))?;
    if files.len() == 0 {
        return Err(Error::new(ErrorKind::NotFound, "no text files found"));
    }
    println!("{} file(s) read in directory", files.len());

    let inv_idx = build_inverted_index(&files);
    loop {
        search_prompt()?;
        let mut search = String::new();
        io::stdin().read_line(&mut search)?;
        match parse_input(search) {
            AppCommand::Quit => break,
            AppCommand::Search(words) => print_ranking(&inv_idx, &words),
        }
    }
    Ok(())
}

/// The prompt shown when the user is requested to enter a command.
fn search_prompt() -> io::Result<()> {
    print!("search> ");
    Ok(io::stdout().flush()?)
}

/// Recursively traverse the provided `path` and return all the files that can be read into a String data type.
fn get_text_files(path: &Path) -> io::Result<HashMap<PathBuf, String>> {
    let mut entries = HashMap::new();

    get_text_files_rec(path, &mut entries)?;

    fn get_text_files_rec(path: &Path, entries: &mut HashMap<PathBuf, String>) -> io::Result<()> {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                get_text_files_rec(&path, entries)?;
            } else {
                if let Ok(contents) = fs::read_to_string(&path) {
                    entries.insert(path, contents);
                }
            }
        }
        Ok(())
    }

    Ok(entries)
}
