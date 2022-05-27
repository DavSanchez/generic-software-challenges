//! This module contains the implementation of a simple file ranking algorithm.

use std::{cmp::Ordering, collections::HashSet, path::PathBuf};

use crate::inverted_index::{search_inverted_index, InvertedIndex};

/// File ranking abstraction. This represents an entry in the ranking, with the file path and the score (percentage).
/// The score is currently represented as an integer percentage value, with the rounding done by the type coercion.
#[derive(PartialEq, Debug)]
struct FileRank {
    pub path: PathBuf,
    pub rank: usize,
}

/// Build the ranking of files given the query and an inverted index. Files are ordered first alphabetically (ascending), then by score (descending).
/// The score is calculated by the number of matching words divided by the total number of words in the original query.
/// Ranking items will only be included if the query is not empty.
fn build_ranking(index: &InvertedIndex, query: &Vec<String>) -> Vec<FileRank> {
    let mut file_ranking = vec![];
    let results = search_inverted_index(index, query)
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    // Vector will only be populated if the query is not empty.
    for unique in results.clone().into_iter().collect::<HashSet<PathBuf>>() {
        let unique_occurrences = results.clone().into_iter().filter(|p| p == &unique).count();
        let rank = (unique_occurrences * 100).checked_div(query.len());
        if let Some(_) = rank {
            file_ranking.push(FileRank {
                path: unique,
                rank: (unique_occurrences * 100) / query.len(),
            });
        }
    }
    // Order first alphabetically (asc), then by rank (desc):
    file_ranking.sort_by(|a, b| a.path.partial_cmp(&b.path).unwrap_or(Ordering::Equal));
    file_ranking.sort_by(|a, b| b.rank.partial_cmp(&a.rank).unwrap_or(Ordering::Equal));
    file_ranking
}

/// Called from the main library, builds the ranking of files given the query and an inverted index calling `build_ranking`, then formats and prints the results.
pub fn print_ranking(index: &InvertedIndex, query: &Vec<String>) {
    let ranking = build_ranking(index, query);
    if ranking.len() == 0 {
        println!("mo matches found");
        return;
    }
    for file_rank in ranking {
        println!(
            "{}:{}%",
            file_rank.path.as_path().display().to_string(),
            file_rank.rank
        );
    }
}

// It is considered idiomatic Rust that the unit tests for a module reside in the same file as the module itself.
#[cfg(test)]
mod tests {

    use super::*;
    use crate::inverted_index::build_inverted_index;
    use std::collections::HashMap;

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
    fn build_ranking_test() {
        let inverted_index = inv_index();
        let query = string_vec(vec!["test", "another"]);
        let expected = vec![
            FileRank {
                path: PathBuf::from("/home/user/test/file2.txt"),
                rank: 100,
            },
            FileRank {
                path: PathBuf::from("/home/user/test/file1.txt"),
                rank: 50,
            },
            FileRank {
                path: PathBuf::from("/home/user/test/file3.txt"),
                rank: 50,
            },
        ];
        let actual = build_ranking(&inverted_index, &query);
        assert_eq!(expected, actual);
    }
}
