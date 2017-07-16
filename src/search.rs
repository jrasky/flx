// Copyright 2015 Jerome Rasky <jerome@rasky.co>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT
// or http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.
use std::collections::{HashMap, BinaryHeap};
use std::cmp::Ordering;
use std::iter::FromIterator;

use unicode_normalization::UnicodeNormalization;

use constants::*;

/// Contains the searchable database
#[derive(Debug)]
pub struct SearchBase {
    lines: Vec<LineInfo>,
}

/// Parsed information about a line, ready to be searched by a SearchBase.
#[derive(Debug)]
pub struct LineInfo {
    line: String,
    char_map: HashMap<char, Vec<usize>>,
    heat_map: Vec<f32>,
    factor: f32,
}

#[derive(PartialEq, Eq)]
enum CharClass {
    Whitespace,
    Numeric,
    Alphabetic,
    First,
    Other,
}

#[derive(Debug)]
struct LineMatch<'a> {
    score: f32,
    factor: f32,
    line: &'a str,
}

impl<'a> Ord for LineMatch<'a> {
    fn cmp(&self, other: &LineMatch) -> Ordering {
        match self.score.partial_cmp(&other.score) {
            Some(Ordering::Equal) | None => {
                self.factor
                    .partial_cmp(&other.factor)
                    .unwrap_or(Ordering::Equal)
            }
            Some(order) => order,
        }
    }
}

impl<'a> PartialOrd for LineMatch<'a> {
    fn partial_cmp(&self, other: &LineMatch) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> PartialEq for LineMatch<'a> {
    fn eq(&self, other: &LineMatch) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<'a> Eq for LineMatch<'a> {}

/// Creates a LineInfo object with a factor of zero
impl<T: Into<String>> From<T> for LineInfo {
    fn from(item: T) -> LineInfo {
        LineInfo::new(item, 0.0)
    }
}

impl<V: Into<LineInfo>> FromIterator<V> for SearchBase {
    fn from_iter<T: IntoIterator<Item = V>>(iterator: T) -> SearchBase {
        SearchBase::new(iterator.into_iter().map(|item| item.into()).collect())
    }
}

impl SearchBase {
    /// Construct a new SearchBase from a Vec of LineInfos.
    pub fn new(lines: Vec<LineInfo>) -> SearchBase {
        SearchBase { lines: lines }
    }

    /// Perform a query of the SearchBase.
    ///
    /// number limits the number of matches returned.
    ///
    /// Matches any supersequence of the given query, with heuristics to order
    /// matches based on how close they are to the given query.
    pub fn query<'a, T: AsRef<str>>(&'a self, query: T, number: usize) -> Vec<&'a str> {
        let query = query.as_ref();
        if query.is_empty() {
            // non-matching query
            return vec![];
        }

        let mut matches: BinaryHeap<LineMatch> = BinaryHeap::with_capacity(number);

        let composed: Vec<char> = query.nfkc().filter(|ch| !ch.is_whitespace()).collect();

        for item in self.lines.iter() {
            let score = match item.score(&composed) {
                None => {
                    // non-matching line
                    continue;
                }
                Some(score) => score,
            };

            let match_item = LineMatch {
                score: -score,
                factor: -item.factor,
                line: &item.line,
            };

            if matches.len() < number {
                matches.push(match_item);
            } else if let Some(mut other_item) = matches.peek_mut() {
                if &match_item < &*other_item {
                    // replace the "greatest" item with ours
                    *other_item = match_item;
                }
            } else {
                unreachable!("No item to peek at, but number of items greater than zero");
            }
        }

        matches.into_sorted_vec().into_iter().map(|x| x.line).collect()
    }
}

impl LineInfo {
    /// Constructs a new LineInfo objects from the given item.
    ///
    /// Factor is a "tie-breaker," or something to weight the matches in a way
    /// beyond the matching already done in flx. The greater the factor, the
    /// more greatly matching favors the item.
    pub fn new<T: Into<String>>(item: T, factor: f32) -> LineInfo {
        let mut map: HashMap<char, Vec<usize>> = HashMap::new();
        let mut heat = vec![];
        let line = item.into();

        let mut ws_score: f32 = 0.0;
        let mut cs_score: f32 = 0.0;
        let mut cur_class = CharClass::First;
        let mut cs_change = false;

        for (idx, c) in line.nfkc().enumerate() {
            if idx > MAX_LEN {
                break;
            }

            if !c.is_whitespace() {
                if cur_class == CharClass::First {
                    cs_score += FIRST_FACTOR;
                }
            }

            if c.is_whitespace() {
                cur_class = CharClass::Whitespace;
                ws_score = WHITESPACE_FACTOR;
            } else if c.is_numeric() {
                if cur_class != CharClass::Numeric {
                    cur_class = CharClass::Numeric;
                    if !cs_change {
                        cs_score += CLASS_FACTOR;
                        cs_change = true;
                    }
                } else {
                    cs_change = false;
                }
            } else if c.is_alphabetic() {
                if cur_class != CharClass::Alphabetic {
                    cur_class = CharClass::Alphabetic;
                    if !cs_change {
                        cs_score += CLASS_FACTOR;
                        cs_change = true;
                    }
                } else {
                    cs_change = false;
                }
            } else {
                if cur_class != CharClass::Other {
                    cur_class = CharClass::Other;
                    if !cs_change {
                        cs_score += CLASS_FACTOR;
                        cs_change = true;
                    }
                } else {
                    cs_change = false;
                }
            }

            if cur_class != CharClass::Whitespace {
                map.entry(c).or_insert(Vec::default()).push(idx);
                if c.is_uppercase() {
                    for lc in c.to_lowercase() {
                        map.entry(lc).or_insert(Vec::default()).push(idx);
                    }
                }
            }

            heat.push(ws_score + cs_score);

            ws_score *= WHITESPACE_REDUCE;
            if !cs_change {
                cs_score *= CLASS_REDUCE;
            }
        }

        LineInfo {
            line: line,
            char_map: map,
            heat_map: heat,
            factor: factor,
        }
    }

    /// Sets the factor for the line info
    ///
    /// Changes the factor after the creation of the line
    pub fn set_factor(&mut self, factor: f32) {
        self.factor = factor;
    }

    /// Gets the factor for the line info
    ///
    /// Produces the factor for the line info
    pub fn get_factor(&self) -> f32 {
        self.factor
    }

    fn score_position(&self, position: &[usize]) -> f32 {
        let avg_dist: f32;

        if position.len() < 2 {
            avg_dist = 0.0;
        } else {
            avg_dist = position.windows(2)
                               .map(|pair| pair[1] as f32 - pair[0] as f32)
                               .sum::<f32>() / position.len() as f32;
        }

        let heat_sum: f32 = position.iter()
                                    .map(|idx| self.heat_map[*idx])
                                    .sum();

        avg_dist * DIST_WEIGHT + heat_sum * HEAT_WEIGHT + self.factor * FACTOR_REDUCE
    }

    fn score<'a>(&self, query: &'a [char]) -> Option<f32> {
        let mut position = vec![0; query.len()];

        let mut lists: Vec<&[usize]> = Vec::with_capacity(query.len());

        if query.iter().any(|ch| {
            if let Some(list) = self.char_map.get(ch) {
                // Use a side effect here to save time
                lists.push(list);
                false
            } else {
                true
            }
        }) {
            return None;
        }

        self.score_inner(query, &mut position, 0, &lists)
    }

    fn score_inner<'a>(&self, query: &'a [char], position: &mut [usize], idx: usize, lists: &[&[usize]]) -> Option<f32> {
        if idx + 1 == query.len() {
            Some(self.score_position(position))
        } else {
            let mut best = None;

            for sub_position in lists[idx].iter() {
                if idx > 0 && *sub_position <= position[idx - 1] {
                    // not a valid position
                    continue;
                }

                position[idx] = *sub_position;

                if let Some(score) = self.score_inner(query, position, idx + 1, lists) {
                    if score > best.unwrap_or(::std::f32::NEG_INFINITY) {
                        best = Some(score);
                    }
                }
            }

            best
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter::FromIterator;

    use rand::Rng;

    use rand;
    use test;

    use super::*;

    #[test]
    fn test_matches() {
        // create a simple search set
        let test_strings = vec!["test1", "test2", "test3"];
        let base = SearchBase::from_iter(test_strings);

        // search for something deinitely not in it
        let result = base.query("abc", 1);

        assert!(result.is_empty());
    }

    #[test]
    fn test_one_long() {
        let test_strings = vec!["a", "b", "ab"];
        let base = SearchBase::from_iter(test_strings);

        let result = base.query("a", 1);

        assert!(result.contains(&"a"));
    }

    #[test]
    fn test_simple_matches() {
        // create a simple search set
        let test_strings = vec!["test", "hello", "hello2"];
        let base = SearchBase::from_iter(test_strings);

        // search
        let result = base.query("hello", 3);

        assert!(result.contains(&"hello"));
        assert!(result.contains(&"hello2"));
        assert!(!result.contains(&"test"));
    }

    #[test]
    fn test_truncate() {
        let test_strings = vec!["test", "toast"];
        let base = SearchBase::from_iter(test_strings);

        // tt matches test more closely than toast
        let result = base.query("tt", 1);

        assert_eq!(result.len(), 1);
        assert!(result.contains(&"test"));
    }

    #[bench]
    fn bench_search(b: &mut test::Bencher) {
        let mut rng = rand::thread_rng();

        let test_strings = vec!["touaoeuaoeeaoeuaoeuaoeusaoeuaoeuaoeuoeautaoeuaoeuaoeu",
                                "aoeuaoeuhaoeuaoeuaoeueaoeuaoeuaoeulaoeuaoeuaoeuloaeuoaeuoeauooea\
                                 ua",
                                "aoeuaoeuahoeuaouaoeuoaeeuaoeuoaeuaoeulaoeuoaeuaoeulaoeuaoeuaoeuo\
                                 aoeuoaeuaoeu2aoeuoae"];
        let mut test_set = Vec::with_capacity(1000);

        for _ in 0..1000 {
            let num = rng.gen::<usize>() % test_strings.len();
            test_set.push(test_strings[num].clone());
        }

        let base = SearchBase::from_iter(test_set);

        b.iter(|| base.query("hello", 10));
    }
}
