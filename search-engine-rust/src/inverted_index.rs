//! Module for creating and querying inverted index structures.
use char;
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

/// Type alias for the inverted index, a map of words to a set of files containing the word.
pub type InvertedIndex = HashMap<String, HashSet<PathBuf>>;

/// Build the inverted index given a map of file paths to their contents.
pub fn build_inverted_index(files: &HashMap<PathBuf, String>) -> InvertedIndex {
    let mut index = HashMap::new();
    for (path, content) in files {
        let tokens = tokenize(content);
        for token in tokens {
            index
                .entry(token)
                .or_insert(HashSet::new())
                .insert(path.clone());
        }
    }
    index
}

/// Queries an inverted index for files containing the given words.
/// Returns a vector of file sets for each word queries by the `query`parameter.
pub fn search_inverted_index(index: &InvertedIndex, query: &Vec<String>) -> Vec<HashSet<PathBuf>> {
    let mut results = vec![];
    for token in query {
        if let Some(paths) = index.get(token) {
            results.push(paths.iter().cloned().collect());
        }
    }
    results
}

/// Tokenizes a string into a vector of words.
/// Words are defined as a sequence of characters that are alphanumerical, whitespace and other special characters are filtered out.
pub fn tokenize(input: &String) -> Vec<String> {
    input
        .split(|c: char| !c.is_alphanumeric())
        .map(|s| s.to_string())
        .filter(|s| s != "")
        .map(|s| s.to_lowercase())
        .collect()
}

// It is considered idiomatic Rust that the unit tests for a module reside in the same file as the module itself.
#[cfg(test)]
mod tests {
    use super::*;

    fn string_vec(v: Vec<&str>) -> Vec<String> {
        v.iter().map(|s| s.to_string()).collect()
    }

    fn inv_index() -> InvertedIndex {
        let mut files = HashMap::new();
        files.insert(
            PathBuf::from("/home/user/test/file1.txt"),
            String::from("This is a test file"),
        );
        files.insert(
            PathBuf::from("/home/user/test/file2.txt"),
            String::from("This is another test file"),
        );
        files.insert(
            PathBuf::from("/home/user/test/file3.txt"),
            String::from("This is a test file"),
        );
        build_inverted_index(&files)
    }

    #[test]
    fn tokenize_test() {
        // Test string with spacing characters:
        assert_eq!(
            tokenize(&String::from(" \t  test string   \t\r\n")),
            (string_vec(vec!["test", "string"]))
        );
        // Numbers, letters and non-alphanumeric characters:
        assert_eq!(
            tokenize(&String::from("1 2 34 test '' ñ")),
            string_vec(vec!["1", "2", "34", "test", "ñ"])
        );
        // Valid token within non-alphanumeric characters:
        assert_eq!(
            tokenize(&String::from("% · (%test%) *+")),
            string_vec(vec!["test"])
        );
        // No valid character produces empty vector:
        assert_eq!(tokenize(&String::from("% · (%%) *+")), string_vec(vec![]));
    }

    #[test]
    fn build_inverted_index_test() {
        let expected = HashMap::from_iter(vec![
            (
                String::from("test"),
                HashSet::from_iter(vec![
                    PathBuf::from("/home/user/test/file1.txt"),
                    PathBuf::from("/home/user/test/file2.txt"),
                    PathBuf::from("/home/user/test/file3.txt"),
                ]),
            ),
            (
                String::from("file"),
                HashSet::from_iter(vec![
                    PathBuf::from("/home/user/test/file1.txt"),
                    PathBuf::from("/home/user/test/file2.txt"),
                    PathBuf::from("/home/user/test/file3.txt"),
                ]),
            ),
            (
                String::from("a"),
                HashSet::from_iter(vec![
                    PathBuf::from("/home/user/test/file1.txt"),
                    PathBuf::from("/home/user/test/file3.txt"),
                ]),
            ),
            (
                String::from("is"),
                HashSet::from_iter(vec![
                    PathBuf::from("/home/user/test/file1.txt"),
                    PathBuf::from("/home/user/test/file2.txt"),
                    PathBuf::from("/home/user/test/file3.txt"),
                ]),
            ),
            (
                String::from("this"),
                HashSet::from_iter(vec![
                    PathBuf::from("/home/user/test/file1.txt"),
                    PathBuf::from("/home/user/test/file2.txt"),
                    PathBuf::from("/home/user/test/file3.txt"),
                ]),
            ),
            (
                String::from("another"),
                HashSet::from_iter(vec![PathBuf::from("/home/user/test/file2.txt")]),
            ),
        ]);
        let actual = inv_index();
        assert_eq!(expected, actual);
    }

    #[test]
    fn search_inverted_index_test() {
        let inverted_index = inv_index();
        let query = string_vec(vec!["test", "another"]);
        let expected: Vec<HashSet<PathBuf>> = vec![
            HashSet::from_iter(vec![
                PathBuf::from("/home/user/test/file1.txt"),
                PathBuf::from("/home/user/test/file2.txt"),
                PathBuf::from("/home/user/test/file3.txt"),
            ]),
            HashSet::from_iter(vec![PathBuf::from("/home/user/test/file2.txt")]),
        ];
        let actual = search_inverted_index(&inverted_index, &query);
        assert_eq!(expected, actual);
    }
}
