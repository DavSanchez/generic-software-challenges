//! Module containing the application command enum type and a simple parser that maps inputs to instances of the command type.

use crate::inverted_index::*;

/// The application command enum type.
/// Currently only two commands are supported:
/// - `Quit`: Exits the application.
/// - `Search`: Searches for a query in the inverted index. This command is parameterized by a vector of words representing the query.
#[derive(PartialEq, Debug)]
pub enum AppCommand {
    Quit,
    Search(Vec<String>),
}

/// Parses a string into an instance of the enum type `AppCommand`.
pub fn parse_input(input: String) -> AppCommand {
    match input.trim() {
        ":quit" => AppCommand::Quit,
        s => AppCommand::Search(tokenize(&s.to_string())),
    }
}

// It is considered idiomatic Rust that the unit tests for a module reside in the same file as the module itself.
#[cfg(test)]
mod tests {

    use super::*;

    fn string_vec(v: Vec<&str>) -> Vec<String> {
        v.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn quit_command_parse_test() {
        // Correctly formatted quit string:
        let quit_string = String::from(":quit");
        assert_eq!(parse_input(quit_string), AppCommand::Quit);
    }

    #[test]
    fn search_command_parse_test() {
        // Test string with spacing characters:
        assert_eq!(
            parse_input(String::from(" \t  test string   \t\r\n")),
            AppCommand::Search(string_vec(vec!["test", "string"]))
        );
        // Numbers, letters and non-alphanumeric characters:
        assert_eq!(
            parse_input(String::from("1 2 34 test '' ñ")),
            AppCommand::Search(string_vec(vec!["1", "2", "34", "test", "ñ"]))
        );
        // Valid token within non-alphanumeric characters:
        assert_eq!(
            parse_input(String::from("% · (%test%) *+")),
            AppCommand::Search(string_vec(vec!["test"]))
        );
        // No valid character produces empty vector:
        assert_eq!(
            parse_input(String::from("% · (%%) *+")),
            AppCommand::Search(string_vec(vec![]))
        );
    }
}
